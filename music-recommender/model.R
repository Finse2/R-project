load_model <- function(path = "model") {
  artifact <- if (dir.exists(path) || !grepl("\\.duckdb$", path, ignore.case = TRUE)) {
    file.path(path, "artists.duckdb")
  } else {
    path
  }
  if (!file.exists(artifact)) {
    stop("Artist model not found at ", artifact, ". Run train.R first.", call. = FALSE)
  }
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = artifact, read_only = TRUE)
  opened <- FALSE
  on.exit(if (!opened) DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "SET threads = 4")
  info <- DBI::dbReadTable(con, "model_info")
  if (nrow(info) != 1L || info$schema_version != 1L) {
    stop("This model version is unsupported. Rebuild it with train.R.", call. = FALSE)
  }
  opened <- TRUE
  list(con = con, catalog_path = info$catalog_path, info = info)
}

close_model <- function(model) {
  if (DBI::dbIsValid(model$con)) DBI::dbDisconnect(model$con, shutdown = TRUE)
  invisible(NULL)
}

recommend_artists <- function(model, artist_id, limit = 10L) {
  if (length(artist_id) != 1L || !is.numeric(artist_id) || is.na(artist_id) ||
      artist_id < 1 || artist_id > .Machine$integer.max || artist_id != floor(artist_id)) {
    stop("artist_id must be a MusicBrainz catalogue artist ID.", call. = FALSE)
  }
  if (length(limit) != 1L || !is.numeric(limit) || is.na(limit) ||
      limit < 1 || limit > 100 || limit != floor(limit)) {
    stop("limit must be an integer from 1 to 100.", call. = FALSE)
  }
  DBI::dbGetQuery(model$con, "
    WITH seed AS (
      SELECT * FROM artists WHERE artist_id = ?
    ), seed_features AS (
      SELECT f.genre, f.weight FROM artist_features f JOIN seed s USING (artist_id)
    ), overlap AS (
      SELECT c.artist_id, sum(c.weight * s.weight) AS cosine, count(*) AS shared_count,
             array_to_string(list(c.genre ORDER BY c.weight * s.weight DESC, c.genre)[1:4], ', ') AS shared_tags
      FROM seed_features s JOIN artist_features c USING (genre)
      GROUP BY c.artist_id
    ), direct_credits AS (
      SELECT c.artist_b AS artist_id, c.credit_count FROM collaborations c JOIN seed s ON c.artist_a = s.artist_id
      UNION ALL
      SELECT c.artist_a AS artist_id, c.credit_count FROM collaborations c JOIN seed s ON c.artist_b = s.artist_id
    ), candidate_ids AS (
      SELECT artist_id FROM overlap UNION SELECT artist_id FROM direct_credits
    ), weights AS (
      SELECT max(weight) FILTER (WHERE term = '(Intercept)') AS intercept,
             max(weight) FILTER (WHERE term = 'cosine') AS cosine,
             max(weight) FILTER (WHERE term = 'log_shared') AS log_shared,
             max(weight) FILTER (WHERE term = 'year_gap') AS year_gap,
             max(weight) FILTER (WHERE term = 'same_country') AS same_country
      FROM coefficients
    ), features AS (
      SELECT a.artist_id, a.name, a.gid, a.comment, a.country, a.main_genre, a.first_release_year,
             a.rating_count, a.n_release_groups, coalesce(o.shared_tags, '') AS shared_tags,
             coalesce(c.credit_count, 0) AS shared_credits, coalesce(o.cosine, 0) AS cosine,
             w.intercept + w.cosine * coalesce(o.cosine, 0) +
             w.log_shared * ln(1.0 + coalesce(o.shared_count, 0)) +
             w.year_gap * least(60.0, coalesce(abs(s.first_release_year - a.first_release_year), 25)) / 25 +
             w.same_country * CAST(coalesce(s.country_code = a.country_code, false) AS INTEGER) AS logit
      FROM candidate_ids i JOIN artists a USING (artist_id)
      LEFT JOIN overlap o USING (artist_id)
      LEFT JOIN direct_credits c USING (artist_id)
      CROSS JOIN seed s CROSS JOIN weights w
      WHERE a.artist_id <> s.artist_id
    )
    SELECT artist_id, name, gid,
           (0.65 * cosine + 0.25 / (1.0 + exp(-logit)) +
            0.10 * least(1.0, ln(1.0 + shared_credits) / ln(6.0))) *
           (0.70 + 0.30 * least(1.0, ln(1.0 + coalesce(rating_count, 0)) / ln(31.0))) AS score,
           shared_tags, shared_credits, comment, country, main_genre, first_release_year,
           rating_count, n_release_groups
    FROM features
    ORDER BY score DESC, rating_count DESC NULLS LAST, name, artist_id
    LIMIT ?
  ", params = list(as.integer(artist_id), as.integer(limit)))
}
