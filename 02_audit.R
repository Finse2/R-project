# =============================================================================
# 02_audit.R  -  Stage 3 (part 1): data quality checks
# -----------------------------------------------------------------------------
# Nothing is modified here. This script only MEASURES the problems so that the
# cleaning decisions in 03_clean.R are based on numbers:
#   A. explicit missing values (NA)
#   B. implicit missing values (empty text, placeholder artists)
#   C. duplicates
#   D. outliers and impossible values (dates, song lengths, tag votes)
#   E. inconsistencies between tables (references to rows that do not exist)
#   F. coverage of the variables we want to analyse
#
# All checks are written with dplyr. The tables stay inside DuckDB; only the
# small result of each check is brought into R with collect().
#
# Run with the same working directory as 01_load.R.
# Packages: DBI, duckdb, dplyr, dbplyr, tidyr, readr
# =============================================================================



library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(tidyr)   # pivot_longer()
library(readr)   # read_csv()

options(pillar.sigfig = 10, width = 110)   # print full counts, not 3.04e6

# All outputs (data/, output/) are written inside the project folder
# Run from the project folder. On this PC the folder is set automatically;
# elsewhere, open the project folder (or setwd() to it) before running.
project_dir <- "C:/Users/Usuario/Desktop/R project"
if (dir.exists(project_dir)) setwd(project_dir)

db_path   <- "data/musicbrainz.duckdb"
# The loaded database is several GB; a missing or tiny file means wrong folder.
if (!file.exists(db_path) || file.size(db_path) < 1e9) {
  stop("Database not found at '", db_path, "'. Build it first: ",
       "run 01_load.R from the project folder. ",
       "Current folder: ", getwd())
}
this_year <- 2026L          # year of the dump
first_recording <- 1877L    # year the phonograph was invented

con    <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
schema <- read_csv("schema/mb_schema.csv", show_col_types = FALSE)

audit <- list()             # every result is stored here and saved at the end

show <- function(name, x, n = 60) {
  audit[[name]] <<- x
  cat("\n\n==== ", name, " ====\n", sep = "")
  print(x, n = n)
  invisible(x)
}

# Shortcuts to the main tables (references, nothing is loaded)
artist        <- tbl(con, "artist")
recording     <- tbl(con, "recording")
release_group <- tbl(con, "release_group")

main_tables <- c("artist", "area", "release_group", "release_group_meta",
                 "release", "release_country", "medium", "recording", "track",
                 "work", "work_language", "label")

# ---- A. Explicit missing values ----------------------------------------------

na_profile <- function(tbl_name) {
  d     <- tbl(con, tbl_name)
  total <- d %>% summarise(n = n()) %>% pull(n)
  d %>%
    summarise(across(everything(),
                     ~ sum(if_else(is.na(.x), 1L, 0L), na.rm = TRUE))) %>%
    collect() %>%
    pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
    mutate(table = tbl_name, rows = total,
           pct_missing = round(100 * n_missing / rows, 2)) %>%
    select(table, column, rows, n_missing, pct_missing)
}

show("A. Explicit missing values (columns with at least one NA)",
     bind_rows(lapply(main_tables, na_profile)) %>% filter(n_missing > 0),
     n = Inf)

# ---- B. Implicit missing values ----------------------------------------------

# B1. Empty text: the dump distinguishes \N (unknown) from "" (empty).
empty_profile <- function(tbl_name) {
  text_cols <- schema %>%
    filter(table == tbl_name, duckdb_type == "VARCHAR") %>%
    pull(column)
  if (length(text_cols) == 0) return(NULL)
  tbl(con, tbl_name) %>%
    summarise(across(all_of(text_cols),
                     ~ sum(if_else(.x == "", 1L, 0L), na.rm = TRUE))) %>%
    collect() %>%
    pivot_longer(everything(), names_to = "column", values_to = "n_empty") %>%
    mutate(table = tbl_name, .before = 1)
}

show("B1. Empty strings in text columns",
     bind_rows(lapply(main_tables, empty_profile)) %>% filter(n_empty > 0),
     n = Inf)

# B2. Placeholder artists: rows that exist only to say "we do not know".
#     MusicBrainz writes them in square brackets, e.g. [unknown].
show("B2. Number of artists whose name starts with '['",
     artist %>% filter(name %like% "[%") %>% summarise(artists = n()) %>% collect())

# How much are they used? ref_count = number of albums/songs using a credit.
credit_use <- tbl(con, "artist_credit_name") %>%
  inner_join(tbl(con, "artist_credit") %>% select(artist_credit = id, ref_count),
             by = "artist_credit") %>%
  group_by(artist) %>%
  summarise(uses = sum(ref_count, na.rm = TRUE))

show("B2. The 20 most credited artists (placeholders appear at the top)",
     credit_use %>%
       inner_join(artist %>% select(artist = id, name, type), by = "artist") %>%
       arrange(desc(uses)) %>%
       head(20) %>%
       collect())

# B3. Artist type and gender: gender only makes sense for a Person, so a
#     missing gender for a Group is not a data problem.
type_names   <- tbl(con, "artist_type") %>% select(type = id, type_name = name) %>% collect()
gender_names <- tbl(con, "gender") %>% select(gender = id, gender_name = name) %>% collect()

show("B3. Artists by type and gender",
     artist %>%
       count(type, gender, name = "artists") %>%
       collect() %>%
       left_join(type_names, by = "type") %>%
       left_join(gender_names, by = "gender") %>%
       select(type_name, gender_name, artists) %>%
       arrange(desc(artists)),
     n = Inf)

# ---- C. Duplicates -----------------------------------------------------------

# C1. Is the identifier really unique?
key_check <- function(tbl_name, key = "id") {
  tbl(con, tbl_name) %>%
    summarise(rows = n(), distinct_keys = n_distinct(!!sym(key))) %>%
    collect() %>%
    mutate(table = tbl_name, duplicated_keys = rows - distinct_keys, .before = 1)
}

show("C1. Duplicated identifiers",
     bind_rows(lapply(c("artist", "area", "release_group", "release", "medium",
                        "recording", "track", "work", "label", "tag"),
                      key_check)))

# C2. Tables whose key is a pair of columns
pair_check <- function(tbl_name, k1, k2) {
  tbl(con, tbl_name) %>%
    count(!!sym(k1), !!sym(k2), name = "times") %>%
    filter(times > 1) %>%
    summarise(duplicated_pairs = n()) %>%
    collect() %>%
    mutate(table = tbl_name, key = paste(k1, k2, sep = " + "), .before = 1)
}

show("C2. Duplicated key pairs",
     bind_rows(
       pair_check("artist_tag", "artist", "tag"),
       pair_check("release_group_tag", "release_group", "tag"),
       pair_check("recording_tag", "recording", "tag"),
       pair_check("release_country", "release", "country"),
       pair_check("work_language", "work", "language")
     ))

# C3. Same name is NOT a duplicate by itself (two different bands can share a
#     name; the comment column tells them apart). Same name AND same comment
#     is a possible true duplicate.
show("C3. Artists sharing a name",
     bind_rows(
       artist %>%
         count(name, name = "times") %>%
         filter(times > 1) %>%
         summarise(groups = n(), artists_involved = sum(times, na.rm = TRUE)) %>%
         collect() %>%
         mutate(check = "same name", .before = 1),
       artist %>%
         count(name, comment, name = "times") %>%
         filter(times > 1) %>%
         summarise(groups = n(), artists_involved = sum(times, na.rm = TRUE)) %>%
         collect() %>%
         mutate(check = "same name and same comment", .before = 1)
     ))

# ---- D. Outliers and impossible values ---------------------------------------

# D1. Artist dates
show("D1. Artist begin / end dates",
     artist %>%
       summarise(
         artists              = n(),
         with_begin_year      = sum(if_else(!is.na(begin_date_year), 1L, 0L), na.rm = TRUE),
         with_end_year        = sum(if_else(!is.na(end_date_year), 1L, 0L), na.rm = TRUE),
         min_begin_year       = min(begin_date_year, na.rm = TRUE),
         max_begin_year       = max(begin_date_year, na.rm = TRUE),
         begin_in_future      = sum(if_else(begin_date_year > this_year, 1L, 0L), na.rm = TRUE),
         begin_before_1000    = sum(if_else(begin_date_year < 1000L, 1L, 0L), na.rm = TRUE),
         end_in_future        = sum(if_else(end_date_year > this_year, 1L, 0L), na.rm = TRUE),
         month_not_1_to_12    = sum(if_else(begin_date_month < 1L | begin_date_month > 12L, 1L, 0L), na.rm = TRUE),
         day_not_1_to_31      = sum(if_else(begin_date_day < 1L | begin_date_day > 31L, 1L, 0L), na.rm = TRUE),
         end_before_begin     = sum(if_else(end_date_year < begin_date_year, 1L, 0L), na.rm = TRUE),
         ended_but_no_end_date = sum(if_else(ended & is.na(end_date_year), 1L, 0L), na.rm = TRUE)
       ) %>%
       collect() %>%
       pivot_longer(everything(), names_to = "check", values_to = "value"))

show("D1. Artists with the most extreme begin years",
     artist %>%
       filter(begin_date_year < 1000L | begin_date_year > this_year) %>%
       left_join(tbl(con, "artist_type") %>% select(type = id, type_name = name), by = "type") %>%
       select(name, type_name, begin_date_year, end_date_year, comment) %>%
       arrange(begin_date_year) %>%
       head(40) %>%
       collect())

# D2. Lifespan of persons (type 1) with both years known
show("D2. Lifespan of persons, in years",
     artist %>%
       filter(type == 1L, !is.na(begin_date_year), !is.na(end_date_year)) %>%
       mutate(lifespan = end_date_year - begin_date_year) %>%
       summarise(
         persons   = n(),
         shortest  = min(lifespan, na.rm = TRUE),
         q1        = quantile(lifespan, 0.25, na.rm = TRUE),
         median    = median(lifespan, na.rm = TRUE),
         q3        = quantile(lifespan, 0.75, na.rm = TRUE),
         longest   = max(lifespan, na.rm = TRUE),
         negative  = sum(if_else(lifespan < 0L, 1L, 0L), na.rm = TRUE),
         over_110  = sum(if_else(lifespan > 110L, 1L, 0L), na.rm = TRUE)
       ) %>%
       collect() %>%
       pivot_longer(everything(), names_to = "statistic", values_to = "value"))

# D3. Release years
year_check <- function(d, col, label) {
  d %>%
    summarise(
      rows        = n(),
      no_year     = sum(if_else(is.na(!!sym(col)), 1L, 0L), na.rm = TRUE),
      min_year    = min(!!sym(col), na.rm = TRUE),
      max_year    = max(!!sym(col), na.rm = TRUE),
      in_future   = sum(if_else(!!sym(col) > this_year, 1L, 0L), na.rm = TRUE),
      before_1877 = sum(if_else(!!sym(col) < first_recording, 1L, 0L), na.rm = TRUE)
    ) %>%
    collect() %>%
    mutate(variable = label, .before = 1)
}

show("D3. Release years",
     bind_rows(
       year_check(tbl(con, "release_group_meta"), "first_release_date_year",
                  "album: first release year"),
       year_check(tbl(con, "release_country"), "date_year",
                  "release in a country: year")
     ))

# D4. Song length (milliseconds), with the 1.5 x IQR rule for outliers
len <- recording %>%
  summarise(
    songs        = n(),
    no_length    = sum(if_else(is.na(length), 1L, 0L), na.rm = TRUE),
    zero_or_less = sum(if_else(length <= 0L, 1L, 0L), na.rm = TRUE),
    under_5_sec  = sum(if_else(length < 5000L, 1L, 0L), na.rm = TRUE),
    over_1_hour  = sum(if_else(length > 3600000L, 1L, 0L), na.rm = TRUE),
    shortest     = min(length, na.rm = TRUE),
    q1           = quantile(length, 0.25, na.rm = TRUE),
    median       = median(length, na.rm = TRUE),
    q3           = quantile(length, 0.75, na.rm = TRUE),
    longest      = max(length, na.rm = TRUE)
  ) %>%
  collect()

iqr         <- len$q3 - len$q1
lower_fence <- len$q1 - 1.5 * iqr
upper_fence <- len$q3 + 1.5 * iqr

outside <- recording %>%
  summarise(
    below_lower_fence = sum(if_else(length < lower_fence, 1L, 0L), na.rm = TRUE),
    above_upper_fence = sum(if_else(length > upper_fence, 1L, 0L), na.rm = TRUE)
  ) %>%
  collect()

show("D4. Song length in milliseconds",
     bind_cols(len, tibble(lower_fence = lower_fence, upper_fence = upper_fence), outside) %>%
       pivot_longer(everything(), names_to = "statistic", values_to = "value"))

# D5. Tag votes: count = upvotes minus downvotes, so 0 or negative means the
#     community rejected the tag.
tag_votes <- function(tbl_name) {
  tbl(con, tbl_name) %>%
    summarise(rows = n(),
              zero_or_negative = sum(if_else(count <= 0L, 1L, 0L), na.rm = TRUE),
              max_votes = max(count, na.rm = TRUE)) %>%
    collect() %>%
    mutate(table = tbl_name, .before = 1)
}

show("D5. Tag votes",
     bind_rows(lapply(c("artist_tag", "release_group_tag", "recording_tag"), tag_votes)))

# D6. Text problems in names: backslash escapes left by the export format,
#     and spaces at the start or end.
name_check <- function(tbl_name) {
  tbl(con, tbl_name) %>%
    summarise(
      rows                = n(),
      contains_backslash  = sum(if_else(name %like% "%\\%", 1L, 0L), na.rm = TRUE),
      leading_or_trailing_space = sum(if_else(name %like% " %" | name %like% "% ", 1L, 0L), na.rm = TRUE)
    ) %>%
    collect() %>%
    mutate(table = tbl_name, .before = 1)
}

show("D6. Text problems in the name column",
     bind_rows(lapply(c("artist", "release_group", "recording", "work", "label", "tag"),
                      name_check)))

# ---- E. Inconsistencies between tables ---------------------------------------
# Rows that point to an identifier that does not exist in the other table.

orphans <- function(child, col, parent, key = "id") {
  n_orphans <- tbl(con, child) %>%
    filter(!is.na(!!sym(col))) %>%
    anti_join(tbl(con, parent), by = setNames(key, col)) %>%
    summarise(n = n()) %>%
    pull(n)
  tibble(reference = paste0(child, ".", col, " -> ", parent, ".", key),
         orphan_rows = n_orphans)
}

show("E. References to rows that do not exist",
     bind_rows(
       orphans("artist", "area", "area"),
       orphans("artist", "type", "artist_type"),
       orphans("artist", "gender", "gender"),
       orphans("artist_credit_name", "artist", "artist"),
       orphans("release_group", "artist_credit", "artist_credit"),
       orphans("release", "release_group", "release_group"),
       orphans("medium", "release", "release"),
       orphans("track", "medium", "medium"),
       orphans("track", "recording", "recording"),
       orphans("artist_tag", "tag", "tag"),
       orphans("release_label", "label", "label"),
       orphans("work_language", "language", "language")
     ))

# ---- F. Coverage of the variables we want to analyse -------------------------

# Tags that are recognised genres (same name in the curated genre list)
genre_tags <- tbl(con, "tag") %>%
  inner_join(tbl(con, "genre") %>% select(name), by = "name") %>%
  select(tag = id, genre = name)

n_artists <- artist %>% summarise(n = n()) %>% pull(n)

coverage <- bind_rows(
  artist %>%
    summarise(
      has_type       = sum(if_else(!is.na(type), 1L, 0L), na.rm = TRUE),
      has_area       = sum(if_else(!is.na(area), 1L, 0L), na.rm = TRUE),
      has_begin_year = sum(if_else(!is.na(begin_date_year), 1L, 0L), na.rm = TRUE),
      has_gender     = sum(if_else(!is.na(gender), 1L, 0L), na.rm = TRUE)
    ) %>%
    collect() %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n") %>%
    mutate(variable = paste("artist:", variable)) %>%
    mutate(total = n_artists),
  tibble(
    variable = "artist: has at least one genre tag (votes > 0)",
    n = tbl(con, "artist_tag") %>%
      filter(count > 0L) %>%
      inner_join(genre_tags, by = "tag") %>%
      summarise(n = n_distinct(artist)) %>%
      pull(n),
    total = n_artists
  ),
  tibble(
    variable = "album: has at least one genre tag (votes > 0)",
    n = tbl(con, "release_group_tag") %>%
      filter(count > 0L) %>%
      inner_join(genre_tags, by = "tag") %>%
      summarise(n = n_distinct(release_group)) %>%
      pull(n),
    total = release_group %>% summarise(n = n()) %>% pull(n)
  ),
  tibble(
    variable = "work: has a lyrics language",
    n = tbl(con, "work_language") %>% summarise(n = n_distinct(work)) %>% pull(n),
    total = tbl(con, "work") %>% summarise(n = n()) %>% pull(n)
  )
) %>%
  mutate(pct = round(100 * n / total, 2))

show("F. Coverage", coverage)

# ---- Save ---------------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
saveRDS(audit, "output/audit.rds")
message("\nAudit saved to output/audit.rds")

dbDisconnect(con, shutdown = TRUE)
