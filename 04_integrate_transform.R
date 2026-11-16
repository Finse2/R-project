# =============================================================================
# 04_integrate_transform.R  -  Stages 4 and 5: integration and transformation
# -----------------------------------------------------------------------------
# PART A  INTEGRATION: the cleaned tables are joined into analysis-ready tables
#   A1 area_country    every area (city, region...) mapped to its country
#   A2 artist_genre    genres of each artist, from artist tags AND album tags
#   A3 artist_profile  one row per artist: country, main genre, discography
#   A4 recording_year  year each song first appeared on a release
#
# PART B  TRANSFORMATION: dplyr summaries that feed the graphs and the app.
#   Each one is small, so it is collected into R and saved as .rds in
#   data/analysis/. The Shiny app only needs these files, not the database.
#   B1 genre_by_country     B2 genre_by_year        B3 albums_by_year
#   B4 band_roles           B5 song_length_by_year  B6 language_by_country
#   B7 top_labels           B8 song_length_by_genre (+ by decade)
#
# dplyr functions used: select, filter, mutate, transmute, rename, arrange,
#   distinct, count, group_by, summarise, inner_join, left_join, anti_join,
#   union_all, bind_rows, case_when, if_else, coalesce, row_number, n_distinct,
#   window_order (dbplyr), collect.
#
# Run after 03_clean.R, same working directory.
# Packages: DBI, duckdb, dplyr, dbplyr
# =============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)

options(pillar.sigfig = 10, width = 110, dplyr.summarise.inform = FALSE)

# All outputs (data/, output/) are written inside the project folder
# Run from the project folder. On this PC the folder is set automatically;
# elsewhere, open the project folder (or setwd() to it) before running.
project_dir <- "C:/Users/Usuario/Desktop/R project"
if (dir.exists(project_dir)) setwd(project_dir)

db_path <- "data/musicbrainz.duckdb"
# The loaded database is several GB; a missing or tiny file means wrong folder.
if (!file.exists(db_path) || file.size(db_path) < 1e9) {
  stop("Database not found at '", db_path, "'. Build it first: ",
       "run 01_load.R from the project folder. ",
       "Current folder: ", getwd())
}
out_dir <- "data/analysis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

# Write a dplyr pipeline as a table inside the database
save_table <- function(d, name) {
  dbExecute(con, paste0("CREATE OR REPLACE TABLE ", dbQuoteIdentifier(con, name),
                        " AS ", sql_render(d)))
  n <- tbl(con, name) %>% summarise(n = n()) %>% pull(n)
  message(sprintf("table  %-22s %10.0f rows", name, n))
  tbl(con, name)
}

# Save a small result (already in R) for the graphs and the app
save_result <- function(x, name) {
  saveRDS(x, file.path(out_dir, paste0(name, ".rds")))
  message(sprintf("result %-22s %10.0f rows  -> %s.rds", name, nrow(x), name))
  cat("\n==== ", name, " ====\n", sep = "")
  print(x, n = 12)
  invisible(x)
}

# =============================================================================
# PART A  -  INTEGRATION
# =============================================================================

# ---- A1. Every area mapped to its country ------------------------------------
# artist.area can be a city or a region. l_area_area says "child is part of
# parent", so we walk up from each area until we reach one that has an ISO
# country code. Both tables are small (120,000 rows), so this is done in R:
# start from the countries, then repeatedly attach their children.

areas_local  <- tbl(con, "area") %>% select(area = id, area_name = name) %>% collect()
parent_local <- tbl(con, "l_area_area") %>% select(parent = entity0, child = entity1) %>% collect()
countries    <- tbl(con, "iso_3166_1") %>%
  select(country_area = area, country_code = code) %>%
  collect() %>%
  distinct(country_area, .keep_all = TRUE)

area_country <- countries %>% transmute(area = country_area, country_area, level = 0L)
frontier     <- area_country

while (nrow(frontier) > 0) {
  frontier <- parent_local %>%
    inner_join(frontier, by = c("parent" = "area")) %>%      # children of the last level
    transmute(area = child, country_area, level = level + 1L) %>%
    anti_join(area_country, by = "area") %>%                 # not resolved yet
    distinct(area, .keep_all = TRUE)                         # one country per area
  area_country <- bind_rows(area_country, frontier)
}

area_country <- area_country %>%
  left_join(countries, by = "country_area") %>%
  left_join(areas_local %>% rename(country_area = area, country = area_name),
            by = "country_area") %>%
  select(area, country_code, country, level)

dbWriteTable(con, "area_country", area_country, overwrite = TRUE)
message(sprintf("table  %-22s %10.0f rows (of %d areas; deepest level %d)",
                "area_country", nrow(area_country), nrow(areas_local),
                max(area_country$level)))

# ---- A2. Genres of each artist -----------------------------------------------
# A tag is a genre when its name is in MusicBrainz's curated genre list.
# Only 7% of artists carry a genre tag themselves, but 35% of albums do, so an
# artist also inherits the genre tags of the albums credited to them.

genre_tag <- tbl(con, "tag_clean") %>%
  inner_join(tbl(con, "genre") %>% select(name), by = "name") %>%
  select(tag = id, genre = name)

# artist <-> album, through the artist credit (an album can have several artists)
album_artist <- tbl(con, "release_group_clean") %>%
  select(release_group = id, artist_credit, type_name, year = first_release_year) %>%
  inner_join(tbl(con, "artist_credit_name") %>% select(artist_credit, artist),
             by = "artist_credit") %>%
  distinct(release_group, artist, type_name, year)

genre_direct <- tbl(con, "artist_tag_clean") %>%
  inner_join(genre_tag, by = "tag") %>%
  select(artist, genre, votes = count)

genre_from_albums <- tbl(con, "release_group_tag_clean") %>%
  inner_join(genre_tag, by = "tag") %>%
  inner_join(album_artist %>% select(release_group, artist), by = "release_group") %>%
  group_by(artist, genre) %>%
  summarise(votes = sum(count, na.rm = TRUE), .groups = "drop")

artist_genre <- union_all(genre_direct, genre_from_albums) %>%
  group_by(artist, genre) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  save_table("artist_genre")

# Main genre = the genre with most votes (ties broken alphabetically)
artist_main_genre <- artist_genre %>%
  group_by(artist) %>%
  window_order(desc(votes), genre) %>%
  mutate(genre_rank = row_number()) %>%
  ungroup() %>%
  filter(genre_rank == 1L) %>%
  select(artist, main_genre = genre)

artist_n_genres <- artist_genre %>% count(artist, name = "n_genres")

# ---- A3. One row per artist ---------------------------------------------------

discography <- album_artist %>%
  group_by(artist) %>%
  summarise(
    n_release_groups   = n_distinct(release_group),
    n_albums           = sum(if_else(type_name == "Album", 1L, 0L), na.rm = TRUE),
    first_release_year = min(year, na.rm = TRUE),
    last_release_year  = max(year, na.rm = TRUE),
    .groups = "drop"
  )

artist_profile <- tbl(con, "artist_clean") %>%
  left_join(tbl(con, "area_country") %>% select(area, country_code, country), by = "area") %>%
  left_join(artist_main_genre, by = c("id" = "artist")) %>%
  left_join(artist_n_genres, by = c("id" = "artist")) %>%
  left_join(discography, by = c("id" = "artist")) %>%
  left_join(tbl(con, "artist_meta") %>% select(id, rating, rating_count), by = "id") %>%
  mutate(
    n_genres         = coalesce(n_genres, 0L),
    n_release_groups = coalesce(n_release_groups, 0L),
    n_albums         = coalesce(n_albums, 0L),
    # derived variables
    begin_decade     = as.integer(floor(begin_year / 10) * 10),
    lifespan         = if_else(type_name == "Person", end_year - begin_year, NA_integer_),
    career_years     = last_release_year - first_release_year + 1L,
    activity         = case_when(
      n_release_groups == 0L  ~ "no releases",
      n_release_groups == 1L  ~ "1 release",
      n_release_groups <= 5L  ~ "2-5 releases",
      n_release_groups <= 20L ~ "6-20 releases",
      TRUE                    ~ "more than 20"
    )
  ) %>%
  save_table("artist_profile")

cat("\n==== artist_profile: coverage after integration ====\n")
artist_profile %>%
  filter(!is_placeholder) %>%
  summarise(
    artists          = n(),
    with_country     = sum(if_else(!is.na(country_code), 1L, 0L), na.rm = TRUE),
    with_main_genre  = sum(if_else(!is.na(main_genre), 1L, 0L), na.rm = TRUE),
    with_releases    = sum(if_else(n_release_groups > 0L, 1L, 0L), na.rm = TRUE),
    with_both        = sum(if_else(!is.na(country_code) & !is.na(main_genre), 1L, 0L), na.rm = TRUE)
  ) %>%
  collect() %>%
  print()

# Real artists with a known country: the population for the country graphs
artists_located <- artist_profile %>%
  filter(!is_placeholder, !is.na(country_code)) %>%
  select(artist = id, country_code, country)

# ---- A4. Year each song first appeared ---------------------------------------
# A recording has no date of its own. Its year is the earliest first-release
# year among the albums it appears on: track -> medium -> release -> album.

recording_year <- tbl(con, "track") %>%
  select(recording, medium) %>%
  inner_join(tbl(con, "medium") %>% select(medium = id, release), by = "medium") %>%
  inner_join(tbl(con, "release") %>% select(release = id, release_group), by = "release") %>%
  inner_join(tbl(con, "release_group_clean") %>%
               filter(!is.na(first_release_year)) %>%
               select(release_group = id, year = first_release_year),
             by = "release_group") %>%
  group_by(recording) %>%
  summarise(year = min(year, na.rm = TRUE), .groups = "drop") %>%
  save_table("recording_year")

# =============================================================================
# PART B  -  TRANSFORMATION (summaries for the graphs)
# =============================================================================

# ---- B1. Genres by country ---------------------------------------------------
# Number of artists of each country that have each genre, and the share of the
# country's genre-tagged artists that this represents.

country_totals <- artist_profile %>%
  filter(!is_placeholder, !is.na(country_code), !is.na(main_genre)) %>%
  count(country_code, name = "artists_with_genre") %>%
  collect()

genre_by_country <- artist_genre %>%
  inner_join(artists_located, by = "artist") %>%
  group_by(country_code, country, genre) %>%
  summarise(artists = n(), .groups = "drop") %>%
  collect() %>%
  left_join(country_totals, by = "country_code") %>%
  mutate(share = artists / artists_with_genre) %>%
  arrange(country, desc(artists)) %>%
  save_result("genre_by_country")

# ---- B2. Genres over time (albums) -------------------------------------------

albums_dated <- tbl(con, "release_group_clean") %>%
  filter(!is.na(first_release_year)) %>%
  select(release_group = id, year = first_release_year, type_name)

albums_tagged <- tbl(con, "release_group_tag_clean") %>%
  inner_join(genre_tag, by = "tag") %>%
  inner_join(albums_dated, by = "release_group")

year_totals <- albums_tagged %>%
  distinct(release_group, year) %>%
  count(year, name = "tagged_albums") %>%
  collect()

genre_by_year <- albums_tagged %>%
  group_by(year, genre) %>%
  summarise(albums = n(), .groups = "drop") %>%
  collect() %>%
  left_join(year_totals, by = "year") %>%
  mutate(share  = albums / tagged_albums,
         decade = (year %/% 10) * 10) %>%
  arrange(year, desc(albums)) %>%
  save_result("genre_by_year")

# ---- B3. Album output per year, by type --------------------------------------

albums_by_year <- albums_dated %>%
  count(year, type_name, name = "release_groups") %>%
  collect() %>%
  mutate(decade = (year %/% 10) * 10) %>%
  arrange(year, type_name) %>%
  save_result("albums_by_year")

# ---- B4. Roles in bands (instruments and vocals), by gender ------------------
# l_artist_artist holds artist-artist relationships; the link says which kind
# ("member of band") and link_attribute says what the member plays.

member_links <- tbl(con, "link") %>%
  inner_join(tbl(con, "link_type") %>%
               filter(name == "member of band",
                      entity_type0 == "artist", entity_type1 == "artist") %>%
               select(link_type = id),
             by = "link_type") %>%
  select(link = id)

role_families <- tbl(con, "link_attribute_type") %>%
  filter(name %in% c("instrument", "vocal"), id == root) %>%   # the two top-level families
  select(root = id, role_family = name)

link_roles <- tbl(con, "link_attribute") %>%
  select(link, attribute_type) %>%
  inner_join(tbl(con, "link_attribute_type") %>% select(attribute_type = id, role = name, root),
             by = "attribute_type") %>%
  inner_join(role_families, by = "root") %>%
  select(link, role, role_family)

band_roles <- tbl(con, "l_artist_artist") %>%
  select(link, member = entity0, band = entity1) %>%
  inner_join(member_links, by = "link") %>%
  inner_join(link_roles, by = "link") %>%
  inner_join(artist_profile %>%
               filter(!is_placeholder) %>%
               select(member = id, gender_name, country),
             by = "member") %>%
  distinct(member, band, role, role_family, gender_name, country) %>%   # a member can rejoin
  count(role_family, role, gender_name, country, name = "memberships") %>%
  collect() %>%
  mutate(country = coalesce(country, "Unknown")) %>%
  arrange(desc(memberships)) %>%
  save_result("band_roles")

# ---- B5. Song length over time -----------------------------------------------

song_length_by_year <- tbl(con, "recording_clean") %>%
  filter(!is.na(length)) %>%
  select(recording = id, length, length_class) %>%
  inner_join(recording_year, by = "recording") %>%
  group_by(year) %>%
  summarise(
    songs           = n(),
    mean_min        = mean(length, na.rm = TRUE) / 60000,
    q1_min          = quantile(length, 0.25, na.rm = TRUE) / 60000,
    median_min      = median(length, na.rm = TRUE) / 60000,
    q3_min          = quantile(length, 0.75, na.rm = TRUE) / 60000,
    unusually_short = sum(if_else(length_class == "unusually short", 1L, 0L), na.rm = TRUE),
    unusually_long  = sum(if_else(length_class == "unusually long", 1L, 0L), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect() %>%
  mutate(decade = (year %/% 10) * 10) %>%
  arrange(year) %>%
  save_result("song_length_by_year")

# ---- B6. Lyrics languages by country of the writer ---------------------------
# work = a composition. l_artist_work links it to who wrote the words
# ("lyricist" or the general "writer"); work_language gives the language.

writer_links <- tbl(con, "link") %>%
  inner_join(tbl(con, "link_type") %>%
               filter(entity_type0 == "artist", entity_type1 == "work",
                      name %in% c("lyricist", "writer")) %>%
               select(link_type = id),
             by = "link_type") %>%
  select(link = id)

language_by_country <- tbl(con, "l_artist_work") %>%
  select(link, artist = entity0, work = entity1) %>%
  inner_join(writer_links, by = "link") %>%
  inner_join(tbl(con, "work_language") %>% select(work, language), by = "work") %>%
  inner_join(tbl(con, "language") %>% select(language = id, language_name = name),
             by = "language") %>%
  inner_join(artists_located, by = "artist") %>%
  distinct(work, language_name, country_code, country) %>%   # count each work once per country
  count(country_code, country, language_name, name = "works") %>%
  collect() %>%
  group_by(country_code) %>%
  mutate(share = works / sum(works)) %>%
  ungroup() %>%
  arrange(country, desc(works)) %>%
  save_result("language_by_country")

# ---- B7. Record labels by number of releases ---------------------------------

label_releases <- tbl(con, "release_label") %>%
  filter(!is.na(label)) %>%
  distinct(release, label) %>%
  count(label, name = "releases")

top_labels <- tbl(con, "label_clean") %>%
  inner_join(label_releases, by = c("id" = "label")) %>%
  left_join(tbl(con, "area_country") %>% select(area, country_code, country), by = "area") %>%
  mutate(is_placeholder = name %like% "[%]") %>%       # e.g. [no label] = self-released
  select(id, name, type_name, country_code, country, begin_year, releases, is_placeholder) %>%
  arrange(desc(releases)) %>%
  head(5000) %>%
  collect() %>%
  mutate(country = coalesce(country, "Unknown")) %>%
  save_result("top_labels")

# ---- B8. Song length by the main genre of the artist --------------------------
# Links the three things together: song -> its artist(s), through the artist
# credit -> the artist's main genre from artist_profile. A song by two artists
# of different genres counts once in each genre.

song_genre <- tbl(con, "recording_clean") %>%
  filter(!is.na(length)) %>%
  select(recording = id, artist_credit, length) %>%
  inner_join(tbl(con, "artist_credit_name") %>% select(artist_credit, artist),
             by = "artist_credit") %>%
  inner_join(artist_profile %>%
               filter(!is_placeholder, !is.na(main_genre)) %>%
               select(artist = id, genre = main_genre),
             by = "artist") %>%
  distinct(recording, genre, length)

cat("\n==== songs that get a genre from their artist ====\n")
song_genre %>%
  summarise(songs = n_distinct(recording), genres = n_distinct(genre)) %>%
  collect() %>%
  print()

song_length_by_genre <- song_genre %>%
  group_by(genre) %>%
  summarise(
    songs      = n(),
    mean_min   = mean(length, na.rm = TRUE) / 60000,
    q1_min     = quantile(length, 0.25, na.rm = TRUE) / 60000,
    median_min = median(length, na.rm = TRUE) / 60000,
    q3_min     = quantile(length, 0.75, na.rm = TRUE) / 60000,
    .groups = "drop"
  ) %>%
  collect() %>%
  arrange(desc(songs)) %>%
  save_result("song_length_by_genre")

song_length_by_genre_decade <- song_genre %>%
  inner_join(recording_year, by = "recording") %>%
  mutate(decade = as.integer(floor(year / 10) * 10)) %>%
  group_by(genre, decade) %>%
  summarise(
    songs      = n(),
    mean_min   = mean(length, na.rm = TRUE) / 60000,
    q1_min     = quantile(length, 0.25, na.rm = TRUE) / 60000,
    median_min = median(length, na.rm = TRUE) / 60000,
    q3_min     = quantile(length, 0.75, na.rm = TRUE) / 60000,
    .groups = "drop"
  ) %>%
  collect() %>%
  arrange(genre, decade) %>%
  save_result("song_length_by_genre_decade")

# ---- Done --------------------------------------------------------------------

cat("\n==== files for the graphs and the app ====\n")
print(data.frame(file = list.files(out_dir),
                 kb = round(file.size(file.path(out_dir, list.files(out_dir))) / 1024)))
message("Database file: ", round(file.size(db_path) / 1024^3, 2), " GB")

dbDisconnect(con, shutdown = TRUE)
