train_catalog <- function(catalog_path, model_path) {
  catalog_path <- normalizePath(catalog_path, winslash = "/", mustWork = TRUE)
  dir.create(model_path, recursive = TRUE, showWarnings = FALSE)
  model_path <- normalizePath(model_path, winslash = "/", mustWork = TRUE)
  destination <- file.path(model_path, "artists.duckdb")
  building <- tempfile("artists-build-", tmpdir = model_path, fileext = ".duckdb")
  on.exit(unlink(c(building, paste0(building, ".wal")), force = TRUE), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = building)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "SET threads = 4")
  DBI::dbExecute(con, paste0("ATTACH ", DBI::dbQuoteString(con, catalog_path),
                            " AS catalog (READ_ONLY)"))

  DBI::dbExecute(con, "
    CREATE TABLE artists AS
    SELECT id AS artist_id, CAST(gid AS VARCHAR) AS gid, name,
           strip_accents(lower(name)) AS search_name, comment, country, country_code,
           main_genre, first_release_year, rating_count, n_release_groups
    FROM catalog.artist_profile
    WHERE NOT coalesce(is_placeholder, false) AND name IS NOT NULL
    ORDER BY artist_id
  ")
  DBI::dbExecute(con, "
    CREATE TABLE genre_vocabulary AS
    SELECT genre, count(*) AS document_frequency,
           1 + ln((1.0 + (SELECT count(DISTINCT artist) FROM catalog.artist_genre WHERE votes > 0)) /
                  (1.0 + count(*))) AS idf
    FROM catalog.artist_genre
    WHERE votes > 0
    GROUP BY genre
  ")
  DBI::dbExecute(con, "
    CREATE TABLE raw_weights AS
    SELECT a.artist AS artist_id, a.genre, ln(1.0 + a.votes) * g.idf AS weight
    FROM catalog.artist_genre a
    JOIN genre_vocabulary g USING (genre)
    JOIN artists p ON p.artist_id = a.artist
    WHERE a.votes > 0
  ")
  DBI::dbExecute(con, "
    CREATE TABLE artist_features AS
    SELECT artist_id, genre,
           weight / sqrt(sum(weight * weight) OVER (PARTITION BY artist_id)) AS weight
    FROM raw_weights
    ORDER BY genre, artist_id
  ")
  DBI::dbExecute(con, "DROP TABLE raw_weights")

  # Shared credits with two to four artists form weak positive labels.
  DBI::dbExecute(con, "
    CREATE TABLE collaborations AS
    WITH credited AS (
      SELECT DISTINCT n.artist_credit, n.artist
      FROM catalog.artist_credit_name n
      JOIN catalog.artist_credit c ON c.id = n.artist_credit
      JOIN artists p ON p.artist_id = n.artist
      WHERE c.artist_count BETWEEN 2 AND 4
    )
    SELECT a.artist AS artist_a, b.artist AS artist_b, count(*) AS credit_count
    FROM credited a
    JOIN credited b ON a.artist_credit = b.artist_credit AND a.artist < b.artist
    GROUP BY a.artist, b.artist
    ORDER BY artist_a, artist_b
  ")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE genre_artists AS
    SELECT artist_id, main_genre,
           row_number() OVER (PARTITION BY main_genre ORDER BY artist_id) AS position,
           count(*) OVER (PARTITION BY main_genre) AS genre_size
    FROM artists
    WHERE main_genre IS NOT NULL
  ")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE training_positives AS
    SELECT c.artist_a, c.artist_b, ga.main_genre, ga.genre_size
    FROM collaborations c
    JOIN genre_artists ga ON ga.artist_id = c.artist_a
    JOIN genre_artists gb ON gb.artist_id = c.artist_b
    WHERE ga.genre_size >= 20
    ORDER BY hash(c.artist_a, c.artist_b, 177)
    LIMIT 24000
  ")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE labeled_pairs AS
    SELECT artist_a, artist_b, 1 AS label FROM training_positives
    UNION ALL
    SELECT DISTINCT p.artist_a, n.artist_id AS artist_b, 0 AS label
    FROM training_positives p
    JOIN genre_artists n ON n.main_genre = p.main_genre
      AND n.position = 1 + hash(p.artist_a, p.artist_b, 819) % p.genre_size
    ANTI JOIN collaborations c ON c.artist_a = least(p.artist_a, n.artist_id)
      AND c.artist_b = greatest(p.artist_a, n.artist_id)
    WHERE n.artist_id <> p.artist_a
  ")

  examples <- DBI::dbGetQuery(con, "
    WITH overlap AS (
      SELECT p.artist_a, p.artist_b, sum(a.weight * b.weight) AS cosine,
             count(*) AS shared_count
      FROM labeled_pairs p
      JOIN artist_features a ON a.artist_id = p.artist_a
      JOIN artist_features b ON b.artist_id = p.artist_b AND b.genre = a.genre
      GROUP BY p.artist_a, p.artist_b
    )
    SELECT p.label, hash(least(p.artist_a, p.artist_b), greatest(p.artist_a, p.artist_b), 211) % 5 = 0 AS holdout,
           coalesce(o.cosine, 0) AS cosine,
           ln(1.0 + coalesce(o.shared_count, 0)) AS log_shared,
           least(60.0, coalesce(abs(a.first_release_year - b.first_release_year), 25)) / 25 AS year_gap,
           CAST(coalesce(a.country_code = b.country_code, false) AS INTEGER) AS same_country
    FROM labeled_pairs p
    JOIN artists a ON a.artist_id = p.artist_a
    JOIN artists b ON b.artist_id = p.artist_b
    LEFT JOIN overlap o ON o.artist_a = p.artist_a AND o.artist_b = p.artist_b
    ORDER BY p.artist_a, p.artist_b
  ")

  training <- examples[!examples$holdout, ]
  holdout <- examples[examples$holdout, ]
  stopifnot(nrow(training) >= 100L, length(unique(training$label)) == 2L,
            length(unique(holdout$label)) == 2L)
  fit <- stats::glm(label ~ cosine + log_shared + year_gap + same_country,
                    data = training, family = stats::binomial())
  stopifnot(isTRUE(fit$converged), all(is.finite(stats::coef(fit))))
  coefficients <- stats::coef(fit)
  held_scores <- stats::predict(fit, newdata = holdout, type = "response")
  positives <- sum(holdout$label == 1L)
  negatives <- sum(holdout$label == 0L)
  auc <- (sum(rank(held_scores)[holdout$label == 1L]) - positives * (positives + 1) / 2) /
    (positives * negatives)

  counts <- DBI::dbGetQuery(con, "
    SELECT (SELECT count(*) FROM artists) AS artists,
           (SELECT count(DISTINCT artist_id) FROM artist_features) AS tagged_artists,
           (SELECT count(*) FROM genre_vocabulary) AS genres,
           (SELECT count(*) FROM collaborations) AS collaborator_pairs
  ")
  DBI::dbWriteTable(con, "coefficients", data.frame(term = names(coefficients),
                                                   weight = unname(coefficients)))
  info <- data.frame(schema_version = 1L, catalog_path = catalog_path,
                     training_pairs = nrow(training), holdout_pairs = nrow(holdout),
                     holdout_auc = auc, counts)
  DBI::dbWriteTable(con, "model_info", info)
  DBI::dbExecute(con, "CHECKPOINT")
  DBI::dbDisconnect(con, shutdown = TRUE)

  backup <- tempfile("artists-backup-", tmpdir = model_path, fileext = ".duckdb")
  if (file.exists(destination) && !file.rename(destination, backup)) {
    stop("Could not replace the model. Close any app using it and try again.", call. = FALSE)
  }
  if (!file.rename(building, destination)) {
    if (file.exists(backup) && !file.rename(backup, destination)) {
      stop("Could not publish the new model. The previous model is at ", backup, call. = FALSE)
    }
    stop("Could not publish the new model.", call. = FALSE)
  }
  if (file.exists(backup)) unlink(backup)

  print(info[, setdiff(names(info), c("schema_version", "catalog_path"))], row.names = FALSE)
  cat(sprintf("Model saved to %s\n", destination))
  invisible(info)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 2L) stop("Usage: Rscript train.R [catalog.duckdb] [model_directory]", call. = FALSE)
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  catalog <- if (length(args)) args[1L] else {
    Sys.getenv("MUSICBRAINZ_DB", unset = file.path(dirname(dirname(normalizePath(script))),
                                                  "data", "musicbrainz.duckdb"))
  }
  model_directory <- if (length(args) == 2L) args[2L] else file.path(dirname(script), "model")
  train_catalog(catalog, model_directory)
}
