# =============================================================================
# 03_clean.R  -  Stage 3 (part 2): data cleaning
# -----------------------------------------------------------------------------
# Applies the decisions that follow from 02_audit.R. The raw tables are never
# changed: every cleaned table is written next to them with the suffix _clean,
# so each step can be re-run and compared with the original.
#
# Summary of decisions (numbers come from the audit of the 2026-11-14 dump):
#
#  MISSING VALUES
#   * Dates, area, song length: left as NA. They cannot be estimated from
#     other columns, and filling 80% of birth years with a mean would invent
#     data. Each analysis uses the rows that have the variable it needs.
#   * Artist type (22.7% NA), album type (2.2% NA), label type, work type:
#     kept, labelled "Unknown", so the rows still count in totals.
#   * Gender: NA is "Not applicable" for groups, orchestras and choirs
#     (the field does not exist for them) and "Unknown" otherwise.
#   * Empty text ("") in comment columns: converted to NA.
#   * Placeholder artists (Various Artists, [unknown], [no artist], ...):
#     kept but flagged with is_placeholder, so they can be excluded when
#     counting real artists. They are 3 of the 7 most credited "artists".
#
#  DUPLICATES
#   * None found: identifiers and key pairs are unique, and no two artists
#     share both name and comment. 132,003 names are shared by several
#     artists, but these are different artists, so nothing is removed.
#
#  OUTLIERS AND IMPOSSIBLE VALUES
#   * Artist dates: only values that are impossible are set to NA (year in
#     the future, year 0, end before begin, a person living more than 110
#     years, a person born before the year 1000 who is not marked as ended).
#     Old but possible dates (Sophocles, medieval choirs) are kept.
#   * Release years outside 1877-2026 (before the phonograph or in the
#     future) are set to NA.
#   * Label founding years before 1450 (the printing press) or in the future
#     are set to NA.
#   * Song length: values above 24 hours are set to NA. The 1.5 x IQR rule on
#     the raw scale would flag 2 million normal long tracks (the distribution
#     is right-skewed), so it is applied on the log scale and only used to
#     LABEL songs as unusually short or long. Nothing else is removed.
#   * Tags with 0 or negative votes were rejected by the community: removed.
#
#  INCONSISTENCIES
#   * All 12 references between tables are intact (0 orphan rows).
#   * Names containing "\\" (the export format doubles every backslash) are
#     restored to a single backslash.
#
# Packages: DBI, duckdb, dplyr, dbplyr
# =============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)

options(pillar.sigfig = 10, width = 110)

# All outputs (data/, output/) are written inside the project folder
# Run from the project folder. On this PC the folder is set automatically;
# elsewhere, open the project folder (or setwd() to it) before running.
project_dir <- "C:/Users/Usuario/Desktop/R project"
if (dir.exists(project_dir)) setwd(project_dir)

db_path          <- "data/musicbrainz.duckdb"
# The loaded database is several GB; a missing or tiny file means wrong folder.
if (!file.exists(db_path) || file.size(db_path) < 1e9) {
  stop("Database not found at '", db_path, "'. Build it first: ",
       "run 01_load.R from the project folder. ",
       "Current folder: ", getwd())
}
this_year        <- 2026L                       # year of the dump
first_recording  <- 1877L                       # invention of the phonograph
first_press      <- 1450L                       # printing press (music publishers)
max_plausible_ms <- 24 * 60 * 60 * 1000         # 24 hours in milliseconds

con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

clean_log <- list()   # effect of every rule, saved at the end

show <- function(name, x, n = 40) {
  clean_log[[name]] <<- x
  cat("\n==== ", name, " ====\n", sep = "")
  print(x, n = n)
  invisible(x)
}

# Write the result of a dplyr pipeline as a table inside the database.
# sql_render() gives the SQL that dplyr generated for the pipeline.
save_table <- function(d, name) {
  dbExecute(con, paste0("CREATE OR REPLACE TABLE ", dbQuoteIdentifier(con, name),
                        " AS ", sql_render(d)))
  n <- tbl(con, name) %>% summarise(n = n()) %>% pull(n)
  message(sprintf("%-26s written, %10.0f rows", name, n))
  tbl(con, name)
}

# The dump writes one backslash as two. dplyr has no verb for a literal text
# replacement in the database, so DuckDB's replace() is called directly.
unescape_name <- function(d) {
  d %>% mutate(name = sql("replace(name, '\\\\', '\\')"))
}

# ---- 1. Artists --------------------------------------------------------------

artist_clean <- tbl(con, "artist") %>%
  unescape_name() %>%
  left_join(tbl(con, "artist_type") %>% select(type = id, type_name = name), by = "type") %>%
  left_join(tbl(con, "gender") %>% select(gender = id, gender_name = name), by = "gender") %>%
  mutate(
    # missing categories become explicit labels
    type_name = coalesce(type_name, "Unknown"),
    gender_name = case_when(
      !is.na(gender_name)                              ~ gender_name,
      type_name %in% c("Group", "Orchestra", "Choir")  ~ "Not applicable",
      TRUE                                             ~ "Unknown"
    ),
    # placeholder artists: "Various Artists" (id 1) and the bracketed special
    # names, which MusicBrainz stores with type "Other". Real bands with a
    # bracketed name ([spunge], [PHYSICS]) have another type or none.
    is_placeholder = id == 1L | (name %like% "[%]" & type_name == "Other")
  ) %>%
  mutate(
    # impossible dates (NA comparisons count as "not a problem")
    begin_bad = coalesce(
      begin_date_year > this_year | begin_date_year == 0L |
        (type_name == "Person" & begin_date_year < 1000L & !ended), FALSE),
    end_bad = coalesce(end_date_year > this_year | end_date_year == 0L, FALSE),
    pair_bad = coalesce(
      end_date_year < begin_date_year |
        (type_name == "Person" & end_date_year - begin_date_year > 110L), FALSE)
  ) %>%
  mutate(
    date_issue = case_when(
      begin_bad & end_bad ~ "begin and end impossible",
      begin_bad           ~ "begin impossible",
      end_bad             ~ "end impossible",
      pair_bad            ~ "begin and end inconsistent"
    )
  ) %>%
  transmute(
    id, gid, name,
    comment     = na_if(comment, ""),
    type_name, gender_name, is_placeholder,
    area, begin_area, end_area,
    begin_year  = if_else(begin_bad | pair_bad, NA_integer_, begin_date_year),
    begin_month = if_else(begin_bad | pair_bad, NA_integer_, begin_date_month),
    begin_day   = if_else(begin_bad | pair_bad, NA_integer_, begin_date_day),
    end_year    = if_else(end_bad | pair_bad, NA_integer_, end_date_year),
    end_month   = if_else(end_bad | pair_bad, NA_integer_, end_date_month),
    end_day     = if_else(end_bad | pair_bad, NA_integer_, end_date_day),
    ended,
    date_issue
  ) %>%
  save_table("artist_clean")

show("Artist: type after cleaning",
     artist_clean %>% count(type_name, sort = TRUE) %>% collect())

show("Artist: gender after cleaning",
     artist_clean %>% count(gender_name, sort = TRUE) %>% collect())

show("Artist: dates set to NA, by reason",
     artist_clean %>% count(date_issue, sort = TRUE) %>% collect())

show("Artist: begin year after cleaning",
     artist_clean %>%
       summarise(with_begin_year = sum(if_else(!is.na(begin_year), 1L, 0L), na.rm = TRUE),
                 earliest = min(begin_year, na.rm = TRUE),
                 latest   = max(begin_year, na.rm = TRUE),
                 before_1000 = sum(if_else(begin_year < 1000L, 1L, 0L), na.rm = TRUE)) %>%
       collect())

# Placeholders: check the rule by looking at what it catches and what it leaves
credit_use <- tbl(con, "artist_credit_name") %>%
  inner_join(tbl(con, "artist_credit") %>% select(artist_credit = id, ref_count),
             by = "artist_credit") %>%
  group_by(artist) %>%
  summarise(uses = sum(ref_count, na.rm = TRUE))

show("Artist: number of placeholders",
     artist_clean %>% count(is_placeholder) %>% collect())

show("Artist: most used placeholders (flagged)",
     artist_clean %>%
       filter(is_placeholder) %>%
       left_join(credit_use, by = c("id" = "artist")) %>%
       select(id, name, type_name, uses) %>%
       arrange(desc(uses)) %>%
       head(25) %>%
       collect())

show("Artist: bracketed names NOT flagged (should be real artists)",
     artist_clean %>%
       filter(!is_placeholder, name %like% "[%") %>%
       left_join(credit_use, by = c("id" = "artist")) %>%
       select(id, name, type_name, uses) %>%
       arrange(desc(uses)) %>%
       head(15) %>%
       collect())

# ---- 2. Albums (release groups) ----------------------------------------------
# release_group_meta has exactly one row per release group (first release date,
# number of releases, rating), so it is attached here.

release_group_clean <- tbl(con, "release_group") %>%
  unescape_name() %>%
  left_join(tbl(con, "release_group_primary_type") %>% select(type = id, type_name = name),
            by = "type") %>%
  left_join(tbl(con, "release_group_meta"), by = "id") %>%
  mutate(
    year_bad = coalesce(first_release_date_year < first_recording |
                          first_release_date_year > this_year, FALSE)
  ) %>%
  transmute(
    id, gid, name, artist_credit,
    type_name           = coalesce(type_name, "Unknown"),
    comment             = na_if(comment, ""),
    first_release_year  = if_else(year_bad, NA_integer_, first_release_date_year),
    first_release_month = if_else(year_bad, NA_integer_, first_release_date_month),
    first_release_day   = if_else(year_bad, NA_integer_, first_release_date_day),
    release_count, rating, rating_count,
    year_issue          = year_bad
  ) %>%
  save_table("release_group_clean")

show("Album: type after cleaning",
     release_group_clean %>% count(type_name, sort = TRUE) %>% collect())

show("Album: first release year",
     release_group_clean %>%
       summarise(albums        = n(),
                 set_to_na     = sum(if_else(year_issue, 1L, 0L), na.rm = TRUE),
                 with_year     = sum(if_else(!is.na(first_release_year), 1L, 0L), na.rm = TRUE),
                 earliest      = min(first_release_year, na.rm = TRUE),
                 latest        = max(first_release_year, na.rm = TRUE)) %>%
       collect())

# ---- 3. Release dates by country ---------------------------------------------

release_country_clean <- tbl(con, "release_country") %>%
  mutate(year_bad = coalesce(date_year < first_recording | date_year > this_year, FALSE)) %>%
  transmute(
    release, country,
    year  = if_else(year_bad, NA_integer_, date_year),
    month = if_else(year_bad, NA_integer_, date_month),
    day   = if_else(year_bad, NA_integer_, date_day),
    year_issue = year_bad
  ) %>%
  save_table("release_country_clean")

show("Release in a country: year",
     release_country_clean %>%
       summarise(rows      = n(),
                 set_to_na = sum(if_else(year_issue, 1L, 0L), na.rm = TRUE),
                 earliest  = min(year, na.rm = TRUE),
                 latest    = max(year, na.rm = TRUE)) %>%
       collect())

# ---- 4. Songs (recordings) ---------------------------------------------------

recording <- tbl(con, "recording")

# Could a missing length be recovered from the tracks of the song?
show("Song: missing lengths that some track could fill",
     recording %>%
       filter(is.na(length)) %>%
       select(recording = id) %>%
       inner_join(tbl(con, "track") %>% filter(!is.na(length)) %>% distinct(recording),
                  by = "recording") %>%
       summarise(recoverable = n()) %>%
       collect())

# 1.5 x IQR rule on log10(length): the distribution is right-skewed, so the
# rule is applied on the log scale and converted back to milliseconds.
log_q <- recording %>%
  filter(!is.na(length), length <= max_plausible_ms) %>%
  mutate(log_length = log10(length)) %>%
  summarise(q1 = quantile(log_length, 0.25, na.rm = TRUE),
            q3 = quantile(log_length, 0.75, na.rm = TRUE)) %>%
  collect()

short_fence_ms <- 10^(log_q$q1 - 1.5 * (log_q$q3 - log_q$q1))
long_fence_ms  <- 10^(log_q$q3 + 1.5 * (log_q$q3 - log_q$q1))
message(sprintf("Length fences: shorter than %.0f s or longer than %.1f min",
                short_fence_ms / 1000, long_fence_ms / 60000))

recording_clean <- recording %>%
  unescape_name() %>%
  mutate(
    length_class = case_when(
      is.na(length)             ~ "unknown",
      length > max_plausible_ms ~ "impossible, set to NA",
      length < short_fence_ms   ~ "unusually short",
      length > long_fence_ms    ~ "unusually long",
      TRUE                      ~ "typical"
    )
  ) %>%
  mutate(length = if_else(length_class == "impossible, set to NA", NA_integer_, length)) %>%
  transmute(id, gid, name, artist_credit, length, length_class,
            comment = na_if(comment, ""), video) %>%
  save_table("recording_clean")

show("Song: length classes",
     recording_clean %>% count(length_class, sort = TRUE) %>% collect())

# ---- 5. Labels and works -----------------------------------------------------

label_clean <- tbl(con, "label") %>%
  unescape_name() %>%
  left_join(tbl(con, "label_type") %>% select(type = id, type_name = name), by = "type") %>%
  mutate(year_bad = coalesce(begin_date_year < first_press | begin_date_year > this_year, FALSE)) %>%
  transmute(
    id, gid, name,
    type_name  = coalesce(type_name, "Unknown"),
    area,
    begin_year = if_else(year_bad, NA_integer_, begin_date_year),
    end_year   = if_else(coalesce(end_date_year > this_year, FALSE), NA_integer_, end_date_year),
    ended,
    comment    = na_if(comment, ""),
    year_issue = year_bad
  ) %>%
  save_table("label_clean")

show("Label: founding year",
     label_clean %>%
       summarise(labels    = n(),
                 set_to_na = sum(if_else(year_issue, 1L, 0L), na.rm = TRUE),
                 earliest  = min(begin_year, na.rm = TRUE),
                 latest    = max(begin_year, na.rm = TRUE)) %>%
       collect())

work_clean <- tbl(con, "work") %>%
  unescape_name() %>%
  left_join(tbl(con, "work_type") %>% select(type = id, type_name = name), by = "type") %>%
  transmute(id, gid, name,
            type_name = coalesce(type_name, "Unknown"),
            comment   = na_if(comment, "")) %>%
  save_table("work_clean")

# ---- 6. Tags -----------------------------------------------------------------

tag_clean <- tbl(con, "tag") %>%
  unescape_name() %>%
  select(id, name) %>%
  save_table("tag_clean")

clean_votes <- function(tbl_name, entity) {
  before <- tbl(con, tbl_name) %>% summarise(n = n()) %>% pull(n)
  after  <- tbl(con, tbl_name) %>%
    filter(count > 0L) %>%
    select(all_of(c(entity, "tag", "count"))) %>%
    save_table(paste0(tbl_name, "_clean")) %>%
    summarise(n = n()) %>%
    pull(n)
  tibble(table = tbl_name, rows_before = before, rows_after = after,
         removed = before - after)
}

show("Tags: rows with 0 or negative votes removed",
     bind_rows(
       clean_votes("artist_tag", "artist"),
       clean_votes("release_group_tag", "release_group"),
       clean_votes("recording_tag", "recording")
     ))

# ---- 7. Check: backslashes in names after cleaning ---------------------------
# with_backslash should equal the audit count (the names keep ONE backslash).
# two_in_a_row should be close to 0: only names that really contain two.

still_doubled <- function(tbl_name) {
  tbl(con, tbl_name) %>%
    summarise(with_backslash = sum(if_else(name %like% "%\\%", 1L, 0L), na.rm = TRUE),
              two_in_a_row   = sum(if_else(name %like% "%\\\\%", 1L, 0L), na.rm = TRUE)) %>%
    collect() %>%
    mutate(table = tbl_name, .before = 1)
}

show("Names: backslashes after cleaning",
     bind_rows(lapply(c("artist_clean", "release_group_clean", "recording_clean",
                        "work_clean", "label_clean", "tag_clean"), still_doubled)))

# ---- Save --------------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
saveRDS(clean_log, "output/cleaning_log.rds")
message("\nCleaning log saved to output/cleaning_log.rds")
message("Database file: ", round(file.size(db_path) / 1024^3, 2), " GB")

dbDisconnect(con, shutdown = TRUE)
