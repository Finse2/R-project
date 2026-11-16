open_catalog <- function(path) {
  if (!file.exists(path)) stop("MusicBrainz database not found: ", path, call. = FALSE)
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = path, read_only = TRUE)
}

checked_limit <- function(limit, maximum = 50L) {
  if (length(limit) != 1L || !is.numeric(limit) || !is.finite(limit) ||
      limit < 1 || limit > maximum || limit != floor(limit)) {
    stop("Limit must be a whole number between 1 and ", maximum, ".", call. = FALSE)
  }
  as.integer(limit)
}

find_artists <- function(model, query, limit = 12L) {
  DBI::dbGetQuery(model$con, "
    WITH q AS (SELECT strip_accents(lower(trim(?))) AS term),
    matches AS (
      SELECT artist_id, name, CAST(gid AS VARCHAR) AS gid, comment, country,
             main_genre, coalesce(rating_count, 0) AS rating_count,
             coalesce(n_release_groups, 0) AS releases,
             search_name
      FROM artists
    )
    SELECT artist_id, name, gid, comment, country, main_genre, rating_count
    FROM matches, q
    WHERE term <> '' AND contains(search_name, term)
    ORDER BY CASE WHEN search_name = term THEN 0
                  WHEN starts_with(search_name, term) THEN 1 ELSE 2 END,
             rating_count DESC, releases DESC, name, artist_id
    LIMIT ?", params = list(query, checked_limit(limit)))
}

find_songs <- function(con, query, artist = "", limit = 12L) {
  # Prefer an exact title. Scan for partial matches when needed.
  for (exact in c(TRUE, FALSE)) {
    title_filter <- if (exact) "search_title = term" else "contains(search_title, term)"
    matches <- DBI::dbGetQuery(con, paste0("
      WITH q AS (
        SELECT strip_accents(lower(trim(?))) AS term,
               strip_accents(lower(trim(?))) AS performer
      ), matching_titles AS (
        SELECT id, gid, name, artist_credit, length, comment,
               strip_accents(lower(name)) AS search_title
        FROM recording_clean
        WHERE NOT coalesce(video, FALSE)
      ), candidates AS (
        SELECT r.id AS recording_id, CAST(r.gid AS VARCHAR) AS gid,
               r.name, ac.name AS artist, r.artist_credit, r.length AS length_ms,
               y.year, r.comment, coalesce(m.rating_count, 0) AS rating_count,
               r.search_title, q.term, q.performer,
               strip_accents(lower(ac.name)) AS search_artist
        FROM q CROSS JOIN matching_titles r
        JOIN artist_credit ac ON ac.id = r.artist_credit
        LEFT JOIN recording_meta m ON m.id = r.id
        LEFT JOIN recording_year y ON y.recording = r.id
        WHERE q.term <> '' AND ", title_filter, "
          AND (q.performer = '' OR contains(strip_accents(lower(ac.name)), q.performer))
      )
      SELECT recording_id, gid, name, artist, artist_credit, length_ms, year, comment, rating_count
      FROM candidates
      QUALIFY row_number() OVER (
        PARTITION BY search_title, search_artist
        ORDER BY rating_count DESC, length_ms IS NULL, year NULLS LAST, recording_id
      ) = 1
      ORDER BY CASE WHEN search_title = term THEN 0
                    WHEN starts_with(search_title, term) THEN 1 ELSE 2 END,
               CASE WHEN search_artist = performer THEN 0 ELSE 1 END,
               rating_count DESC, year NULLS LAST, recording_id
      LIMIT ?"), params = list(query, artist, checked_limit(limit)))
    if (nrow(matches) || !exact) return(matches)
  }
}

suggest_songs <- function(con, query, artist = "", limit = 4L) {
  # Cache the rated subset once per connection. The submitted search still
  # uses the complete recording table, including recordings without ratings.
  DBI::dbExecute(con, "
    CREATE TEMP TABLE IF NOT EXISTS recommender_song_suggestions AS
    SELECT r.id, r.gid, r.name, r.artist_credit, r.length, r.comment,
           strip_accents(lower(r.name)) AS search_title, m.rating_count
    FROM recording_clean r
    JOIN recording_meta m ON m.id = r.id
    WHERE m.rating_count > 0 AND NOT coalesce(r.video, FALSE)
    ORDER BY search_title, r.id")
  DBI::dbGetQuery(con, "
    WITH q AS (
      SELECT strip_accents(lower(trim(?))) AS term,
             strip_accents(lower(trim(?))) AS performer
    ), by_title AS (
      SELECT r.*, q.term, q.performer, ac.name AS artist,
             strip_accents(lower(ac.name)) AS search_artist
      FROM q CROSS JOIN recommender_song_suggestions r
      JOIN artist_credit ac ON ac.id = r.artist_credit
      WHERE q.term <> '' AND contains(r.search_title, q.term)
    ), top_matches AS (
      SELECT * FROM by_title
      WHERE performer = '' OR contains(search_artist, performer)
      QUALIFY row_number() OVER (
        PARTITION BY search_title, search_artist
        ORDER BY rating_count DESC, length IS NULL, id
      ) = 1
      ORDER BY CASE WHEN search_title = term THEN 0
                    WHEN starts_with(search_title, term) THEN 1 ELSE 2 END,
               CASE WHEN search_artist = performer THEN 0 ELSE 1 END,
               rating_count DESC, id
      LIMIT ?
    )
    SELECT r.id AS recording_id, CAST(r.gid AS VARCHAR) AS gid, r.name, r.artist,
           r.artist_credit, r.length AS length_ms, y.year, r.comment, r.rating_count
    FROM top_matches r
    LEFT JOIN recording_year y ON y.recording = r.id
    ORDER BY CASE WHEN search_title = term THEN 0
                  WHEN starts_with(search_title, term) THEN 1 ELSE 2 END,
             CASE WHEN search_artist = performer THEN 0 ELSE 1 END,
             rating_count DESC, r.id", params = list(query, artist, checked_limit(limit)))
}

find_albums <- function(con, query, artist = "", limit = 12L) {
  DBI::dbGetQuery(con, "
    WITH q AS (
      SELECT strip_accents(lower(trim(?))) AS term,
             strip_accents(lower(trim(?))) AS performer
    ), albums AS (
      SELECT id, gid, name, artist_credit, first_release_year AS year, comment,
             coalesce(rating_count, 0) AS rating_count,
             coalesce(release_count, 0) AS release_count,
             strip_accents(lower(name)) AS search_title
      FROM release_group_clean
      WHERE type_name = 'Album'
    ), matches AS (
      SELECT albums.*, ac.name AS artist, q.term, q.performer,
             strip_accents(lower(ac.name)) AS search_artist
      FROM q CROSS JOIN albums JOIN artist_credit ac ON ac.id = albums.artist_credit
      WHERE q.term <> '' AND contains(search_title, q.term)
    )
    SELECT id AS release_group_id, CAST(gid AS VARCHAR) AS gid, name, artist,
           artist_credit, year, comment, rating_count
    FROM matches
    WHERE performer = '' OR contains(search_artist, performer)
    ORDER BY CASE WHEN search_title = term THEN 0
                  WHEN starts_with(search_title, term) THEN 1 ELSE 2 END,
             CASE WHEN search_artist = performer THEN 0 ELSE 1 END,
             rating_count DESC, release_count DESC, year NULLS LAST, id
    LIMIT ?", params = list(query, artist, checked_limit(limit)))
}

suggest_albums <- function(con, query, artist = "", limit = 4L) {
  find_albums(con, query, artist, limit)
}

credited_artists <- function(con, artist_credit) {
  DBI::dbGetQuery(con, "
    SELECT artist AS artist_id FROM artist_credit_name
    WHERE artist_credit = ? ORDER BY position, artist",
    params = list(as.integer(artist_credit)))$artist_id
}

recommend_songs <- function(con, artists, seed = NULL, limit = 10L, seed_year = NA_integer_) {
  limit <- checked_limit(limit)
  ids <- as.integer(artists$artist_id)
  affinities <- as.numeric(artists$score)
  if (!length(ids)) return(data.frame())
  source_id <- if (is.null(seed)) -1L else as.integer(seed$recording_id[[1]])
  source_title <- if (is.null(seed)) "" else seed$name[[1]]
  source_year <- if (is.null(seed)) seed_year else as.integer(seed$year[[1]])
  source_length <- if (is.null(seed)) NA_integer_ else as.integer(seed$length_ms[[1]])

  artist_values <- paste(rep("(?::INTEGER, ?::DOUBLE)", length(ids)), collapse = ", ")
  artist_params <- as.list(as.vector(rbind(ids, affinities)))
  DBI::dbGetQuery(con, paste0("
    WITH source AS (
      SELECT ?::INTEGER AS id, strip_accents(lower(?)) AS title,
             ?::INTEGER AS year, ?::INTEGER AS duration
    ), affinity(artist_id, similarity) AS (VALUES ", artist_values, "),
    credits AS (
      SELECT acn.artist_credit, max(a.similarity) AS similarity,
             arg_max(a.artist_id, a.similarity ORDER BY a.artist_id) AS linked_artist
      FROM affinity a JOIN artist_credit_name acn ON acn.artist = a.artist_id
      GROUP BY acn.artist_credit
    ), candidates AS MATERIALIZED (
      SELECT r.id, r.gid, r.name, r.length, ac.name AS artist, c.linked_artist,
             c.similarity, y.year, coalesce(m.rating_count, 0) AS rating_count
      FROM credits c JOIN recording_clean r ON r.artist_credit = c.artist_credit
      JOIN artist_credit ac ON ac.id = r.artist_credit
      LEFT JOIN recording_year y ON y.recording = r.id
      LEFT JOIN recording_meta m ON m.id = r.id
      CROSS JOIN source s
      WHERE NOT coalesce(r.video, FALSE) AND r.id <> s.id
        AND strip_accents(lower(r.name)) <> s.title
      QUALIFY row_number() OVER (
        PARTITION BY c.linked_artist
        ORDER BY coalesce(m.rating_count, 0) DESC,
                 abs(y.year - s.year) NULLS LAST, r.id
      ) <= 160
    ), shared AS (
      SELECT c.id, count(DISTINCT t.id) AS tag_count,
             string_agg(DISTINCT t.name, ', ' ORDER BY t.name) AS shared_tags
      FROM source s JOIN recording_tag_clean st ON st.recording = s.id AND st.count > 0
      JOIN recording_tag_clean rt ON rt.tag = st.tag AND rt.count > 0
      JOIN candidates c ON c.id = rt.recording
      JOIN tag_clean t ON t.id = rt.tag
      GROUP BY c.id
    ), ranked AS (
      SELECT c.*, shared.shared_tags,
             0.58 * c.similarity
             + 0.22 * least(1., ln(1. + c.rating_count) / ln(26.))
             + 0.10 * coalesce(exp(-abs(c.year - s.year) / 12.), 0.)
             + 0.04 * coalesce(exp(-abs(c.length - s.duration) / 120000.), 0.)
             + 0.06 * least(1., coalesce(shared.tag_count, 0) / 3.) AS score
      FROM candidates c CROSS JOIN source s LEFT JOIN shared ON shared.id = c.id
    ), unique_titles AS (
      SELECT *
      FROM ranked
      QUALIFY row_number() OVER (
        PARTITION BY strip_accents(lower(name)), strip_accents(lower(artist))
        ORDER BY score DESC, id
      ) = 1
    ), diverse AS (
      SELECT *,
             row_number() OVER (PARTITION BY linked_artist ORDER BY score DESC, id) AS artist_rank
      FROM unique_titles
    )
    SELECT id AS recording_id, CAST(gid AS VARCHAR) AS gid, name, artist,
           linked_artist AS artist_id, year, score, shared_tags, rating_count
    FROM diverse
    ORDER BY score - 0.10 * (artist_rank - 1) DESC, id
    LIMIT ?"), params = c(list(source_id, source_title, source_year, source_length),
                           artist_params, list(limit)))
}

recommend_from_match <- function(model, con, kind, seed, recommend, limit = 10L) {
  limit <- checked_limit(limit)
  if (kind %in% c("song", "album")) {
    seed_ids <- head(unique(credited_artists(con, seed$artist_credit[[1]])), 5L)
  } else {
    seed_ids <- as.integer(seed$artist_id[[1]])
  }
  seed_song <- if (kind == "song") seed else NULL
  seed_year <- if (kind == "album") as.integer(seed$year[[1]]) else NA_integer_

  neighbors <- do.call(rbind, lapply(seed_ids, function(id) {
    recommend_artists(model, id, limit = max(limit, 25L))
  }))
  if (!is.null(neighbors) && nrow(neighbors)) {
    neighbors <- neighbors[!neighbors$artist_id %in% seed_ids, , drop = FALSE]
    neighbors <- neighbors[order(-neighbors$score, neighbors$artist_id), , drop = FALSE]
    neighbors <- neighbors[!duplicated(neighbors$artist_id), , drop = FALSE]
  }
  if (recommend == "artist") {
    return(if (is.null(neighbors)) data.frame() else head(neighbors, limit))
  }

  # Prefer discovery across artists, with a fallback to the seed's discography
  # when sparse metadata gives the model no artist neighbors.
  scores <- if (is.null(neighbors)) data.frame() else {
    head(neighbors[, c("artist_id", "score"), drop = FALSE], max(limit, 25L))
  }
  if (!nrow(scores)) scores <- data.frame(artist_id = seed_ids, score = rep(0.5, length(seed_ids)))
  songs <- recommend_songs(con, scores, seed_song, limit, seed_year = seed_year)
  if (nrow(songs) && !is.null(neighbors) && nrow(neighbors)) {
    artist_tags <- neighbors$shared_tags[match(songs$artist_id, neighbors$artist_id)]
    songs$shared_artist_tags <- artist_tags
  }
  songs
}
