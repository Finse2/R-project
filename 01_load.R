# =============================================================================
# 01_load.R  -  Stage 2: loading the MusicBrainz dump
# -----------------------------------------------------------------------------
# Source : MusicBrainz PostgreSQL full export (mbdump), dump of 2026-11-14,
#          schema sequence 31. 213 tables, ~28 GB of text.
# Scope  : 48 tables (~20 GB) covering artists, genres, albums, band members
#          and instruments, record labels, song languages and songs.
#
# Why DuckDB and not read_tsv()/read.table():
#   The song tables alone are 4.7 GB (recording, 41 M rows) and 8 GB (track,
#   59 M rows). A data frame lives entirely in RAM, so these cannot be loaded
#   the usual way on a normal laptop. DuckDB is a database engine that runs
#   inside R, reads the text files directly, and stores them compressed in ONE
#   file on disk. Afterwards every table is used with ordinary dplyr verbs
#   (through dbplyr); only the small results are brought into memory.
#
# File format (checked by opening the raw files):
#   * one file per table, no extension, no header row
#   * tab-separated, one row per line, UTF-8
#   * missing values are written as \N   (PostgreSQL COPY text format)
#   * empty text is an empty field, which is NOT the same as \N
#   * no quoting: a " inside a name is just a character
#   * booleans are written as t / f
#
# Column names, order and types come from the official schema
#   github.com/metabrainz/musicbrainz-server  ->  admin/sql/CreateTables.sql
# summarised in schema/mb_schema.csv (one row per column).
#
# Project folder layout:
#   project/
#     01_load.R
#     schema/mb_schema.csv
#     data/                 <- created here, will hold musicbrainz.duckdb
#
# Packages: install.packages(c("DBI", "duckdb", "readr", "dplyr", "dbplyr"))
# =============================================================================

required_packages <- c("DBI", "duckdb", "readr", "dplyr", "dbplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(DBI)      # dbConnect(), dbExecute(), dbGetQuery()
library(duckdb)   # the database engine
library(readr)    # read_csv() for the schema file
library(dplyr)    # filter(), arrange(), bind_rows(), tbl(), ...

# ---- 1. Paths and scope ------------------------------------------------------

# All outputs (data/, output/) are written inside the project folder
# Run from the project folder. On this PC the folder is set automatically;
# elsewhere, open the project folder (or setwd() to it) before running.
project_dir <- "C:/Users/Usuario/Desktop/R project"
if (dir.exists(project_dir)) setwd(project_dir)

mb_dir      <- "C:/Users/Usuario/MusicBrainzRaw/mbdump"   # raw dump (never modified)
schema_file <- "schema/mb_schema.csv"
db_path     <- "data/musicbrainz.duckdb"                  # needs roughly 8-10 GB free
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

# Groups of tables to load. "songs" is 15 of the 20 GB: comment it out for a
# quick first run, then put it back and run again (tables already loaded are
# skipped, so nothing is repeated).
themes <- c(
  "artists",    # artist, type, gender, area, country codes, ratings
  "genres",     # tags and the curated genre list, for artists / albums / songs
  "credits",    # artist_credit: links albums and songs to their artist(s)
  "albums",     # release groups, releases, release dates and countries, media
  "members",    # artist-artist relationships (member of band) and instruments
  "labels",     # record labels and which releases they published
  "languages",  # works (compositions), their lyrics language, links to songs
  "songs"       # recordings, tracks, performer relationships
)

# ---- 2. Schema ---------------------------------------------------------------
# A comma-separated file WITH a header, so read_csv() is the right reader here.
# Types are given explicitly so nothing is guessed.

schema <- read_csv(
  schema_file,
  col_types = cols(
    theme         = col_character(),
    table         = col_character(),
    position      = col_integer(),
    column        = col_character(),
    postgres_type = col_character(),
    duckdb_type   = col_character()
  )
) %>%
  filter(theme %in% themes) %>%
  arrange(table, position)

tables <- unique(schema$table)

# ---- 3. Format verification before loading -----------------------------------
# For every file: does it exist, and do its first 1000 lines have exactly as
# many tab-separated fields as the schema says? This takes a few seconds and
# catches a wrong schema version before a long load starts.

# Number of tab characters per line (bytes are safe: a tab is 1 byte in UTF-8)
count_tabs <- function(x) {
  nchar(x, type = "bytes") - nchar(gsub("\t", "", x, fixed = TRUE), type = "bytes")
}

check_file <- function(tbl_name) {
  path     <- file.path(mb_dir, tbl_name)
  expected <- sum(schema$table == tbl_name)
  if (!file.exists(path)) {
    return(tibble(table = tbl_name, size_mb = NA_real_, expected = expected,
                  found = "file missing", ok = FALSE))
  }
  head_lines <- readLines(path, n = 1000, encoding = "UTF-8", warn = FALSE)
  found      <- unique(count_tabs(head_lines) + 1)
  tibble(
    table    = tbl_name,
    size_mb  = round(file.size(path) / 1024^2, 1),
    expected = expected,
    found    = paste(found, collapse = "/"),
    ok       = length(found) == 1 && found == expected
  )
}

preflight <- bind_rows(lapply(tables, check_file))
print(preflight, n = Inf)

if (!all(preflight$ok)) {
  stop("Format check failed for: ",
       paste(preflight$table[!preflight$ok], collapse = ", "))
}
message("Format check passed: ", nrow(preflight), " files, ",
        round(sum(preflight$size_mb) / 1024, 1), " GB")

# ---- 4. Load into DuckDB -----------------------------------------------------

con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

# Rows do not need to keep the file order; this lowers memory use on big loads.
dbExecute(con, "SET preserve_insertion_order = false")

load_table <- function(tbl_name, overwrite = FALSE) {
  if (!overwrite && dbExistsTable(con, tbl_name)) {
    message(sprintf("%-34s already loaded, skipped", tbl_name))
    return(invisible(NULL))
  }

  spec <- schema %>% filter(table == tbl_name) %>% arrange(position)

  # {'id': 'INTEGER', 'gid': 'UUID', ...}  column names and types, in file order
  columns_sql <- paste0("'", spec$column, "': '", spec$duckdb_type, "'",
                        collapse = ", ")
  path_sql <- gsub("'", "''", file.path(mb_dir, tbl_name), fixed = TRUE)

  sql <- paste0(
    "CREATE OR REPLACE TABLE ", dbQuoteIdentifier(con, tbl_name), " AS ",
    "SELECT * FROM read_csv('", path_sql, "', ",
    "delim = '\t', ",         # tab-separated
    "header = false, ",       # first line is data, not column names
    "nullstr = '\\N', ",      # ONLY \N is missing; empty text stays empty
    "quote = '', ",           # the dump has no quoting
    "escape = '', ",
    "auto_detect = false, ",  # no guessing: names and types come from the schema
    "columns = {", columns_sql, "})"
  )
  # ignore_errors is left at its default (false): one malformed line stops the
  # load with an error, so a table that loads has had every line parsed.

  elapsed <- system.time(dbExecute(con, sql))[["elapsed"]]
  message(sprintf("%-34s loaded in %6.1f s", tbl_name, elapsed))
  invisible(NULL)
}

# Smallest files first, so problems show up early
for (tbl_name in preflight$table[order(preflight$size_mb)]) {
  load_table(tbl_name)
}

# ---- 5. Summary of what was loaded -------------------------------------------

count_rows <- function(tbl_name) {
  as.numeric(dbGetQuery(con, paste0("SELECT count(*) AS n FROM ",
                                    dbQuoteIdentifier(con, tbl_name)))$n)
}

load_summary <- preflight %>%
  select(table, size_mb, columns = expected) %>%
  mutate(rows = unname(vapply(table, count_rows, numeric(1)))) %>%
  left_join(distinct(schema, table, theme), by = "table") %>%
  arrange(theme, desc(rows))

print(load_summary, n = Inf)
message("Database file: ", round(file.size(db_path) / 1024^3, 2), " GB")

# ---- 6. First look through dplyr ---------------------------------------------
# tbl() does not load the table: it is a reference to it inside the database.
# dplyr verbs are translated to SQL and run by DuckDB; collect() brings the
# (small) result into R as a normal tibble.

artist <- tbl(con, "artist")
glimpse(artist)

artist %>%
  count(type, sort = TRUE) %>%
  collect() %>%
  print()

dbDisconnect(con, shutdown = TRUE)
