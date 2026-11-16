# =============================================================================
# 05_explore.R  -  Stage 6: data exploration
# -----------------------------------------------------------------------------
# Ten ggplot2 graphs, each with the numbers needed to interpret it printed in
# the console, plus descriptive statistics and a correlation matrix.
#
#   G1 releases per year by type          (trend over time, 4 series)
#   G2 genre share over time              (trend over time, 6 series)
#   G3 genres by country                  (heatmap)
#   G4 share of women by role in bands    (ranked bars)
#   G5 song length over time              (median with interquartile band)
#   G6 lyrics languages by country        (stacked share bars)
#   G7 largest record labels              (ranked bars)
#   G8 correlations between artist variables (correlation matrix)
#   G9 career length vs number of releases   (median and middle half per band)
#   G10 song length by the artist's genre    (median and middle half per genre)
#
# Figures are saved as PNG in output/figures/.
# Input: data/analysis/*.rds (from 04) and the artist_profile table.
# Packages: ggplot2, dplyr, tidyr, scales, DBI, duckdb, dbplyr
# =============================================================================


library(ggplot2)
library(dplyr)
library(tidyr)
library(DBI)
library(duckdb)
library(dbplyr)

options(pillar.sigfig = 10, width = 130, dplyr.summarise.inform = FALSE)

# All outputs (data/, output/) are written inside the project folder
# Run from the project folder. On this PC the folder is set automatically;
# elsewhere, open the project folder (or setwd() to it) before running.
project_dir <- "C:/Users/Usuario/Desktop/R project"
if (dir.exists(project_dir)) setwd(project_dir)

in_dir  <- "data/analysis"
fig_dir <- "output/figures"
db_path <- "data/musicbrainz.duckdb"
# The loaded database is several GB; a missing or tiny file means wrong folder.
if (!file.exists(db_path) || file.size(db_path) < 1e9) {
  stop("Database not found at '", db_path, "'. Build it first: ",
       "run 01_load.R from the project folder. ",
       "Current folder: ", getwd())
}
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

first_year <- 1950L   # before this the yearly counts are small and noisy
last_year  <- 2025L   # 2026 is incomplete: the dump is from November 2026
not_countries <- c("[Worldwide]", "Europe")   # areas with an ISO code that are not countries

# ---- Look and feel -----------------------------------------------------------
# One fixed palette for categories (always used in this order), one blue ramp
# for magnitudes, blue/red around a neutral grey for correlations.

pal <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a",
         yellow = "#eda100", magenta = "#e87ba4", green = "#008300")
grey_other <- "#898781"
seq_low    <- "#cde2fb"
seq_high   <- "#0d366b"
ink        <- "#0b0b0b"
ink_soft   <- "#52514e"
surface    <- "#fcfcfb"

theme_mb <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background       = element_rect(fill = surface, colour = NA),
      panel.grid.minor      = element_blank(),
      panel.grid.major      = element_line(colour = "#e1e0d9", linewidth = 0.3),
      plot.title            = element_text(face = "bold", colour = ink),
      plot.title.position   = "plot",
      plot.subtitle         = element_text(colour = ink_soft),
      plot.caption          = element_text(colour = grey_other, hjust = 0),
      plot.caption.position = "plot",
      axis.text             = element_text(colour = ink_soft),
      axis.title            = element_text(colour = ink_soft),
      legend.position       = "top",
      legend.justification  = "left",
      legend.title          = element_blank()
    )
}

save_plot <- function(p, name, width = 9, height = 5.5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p,
         width = width, height = height, dpi = 150, bg = surface)
  message("figure saved: ", name, ".png")
  invisible(p)
}

show <- function(title, x, n = 40) {
  cat("\n==== ", title, " ====\n", sep = "")
  print(x, n = n)
  invisible(x)
}

read_result <- function(name) readRDS(file.path(in_dir, paste0(name, ".rds")))

# =============================================================================
# G1. Releases per year, by type
# =============================================================================

albums_by_year <- read_result("albums_by_year")

g1_data <- albums_by_year %>%
  filter(year >= first_year, year <= last_year) %>%
  mutate(type = if_else(type_name %in% c("Album", "Single", "EP"), type_name, "Other")) %>%
  group_by(year, type) %>%
  summarise(release_groups = sum(release_groups), .groups = "drop") %>%
  complete(year, type, fill = list(release_groups = 0)) %>%
  mutate(type = factor(type, levels = c("Album", "Single", "EP", "Other")))

g1 <- ggplot(g1_data, aes(year, release_groups, colour = type)) +
  geom_line(linewidth = 0.8) +
  geom_text(data = filter(g1_data, year == last_year), aes(label = type),
            colour = ink_soft, hjust = 0, nudge_x = 0.8, size = 3.4, show.legend = FALSE) +
  scale_colour_manual(values = c(Album = pal[["blue"]], Single = pal[["orange"]],
                                 EP = pal[["aqua"]], Other = grey_other)) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), limits = c(first_year, last_year + 7)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Singles overtook albums in 2019",
       subtitle = "Release groups by year of first release, 1950-2025",
       x = NULL, y = "Release groups",
       caption = "Source: MusicBrainz dump 2026-11-14. Other = broadcast, other and unknown type.") +
  theme_mb()
save_plot(g1, "g1_releases_by_year")

show("G1. Release groups per decade and type",
     albums_by_year %>%
       mutate(type = if_else(type_name %in% c("Album", "Single", "EP"), type_name, "Other")) %>%
       group_by(decade, type) %>%
       summarise(n = sum(release_groups), .groups = "drop") %>%
       pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
       mutate(total = Album + Single + EP + Other,
              pct_single = round(100 * Single / total, 1)) %>%
       arrange(decade))

show("G1. Last 12 years in detail",
     g1_data %>%
       filter(year >= last_year - 11) %>%
       pivot_wider(names_from = type, values_from = release_groups))

show("G1. Anomaly check: years before 1900",
     albums_by_year %>% filter(year < 1900) %>% select(-decade) %>% arrange(year))

# =============================================================================
# G2. Genre share over time
# =============================================================================

genre_by_year <- read_result("genre_by_year") %>%
  filter(year >= 1960, year <= last_year)

genre_rank <- genre_by_year %>%
  group_by(genre) %>%
  summarise(albums = sum(albums)) %>%
  arrange(desc(albums))

top6 <- head(genre_rank$genre, 6)

g2_data <- genre_by_year %>%
  filter(genre %in% top6) %>%
  mutate(genre = factor(genre, levels = top6))

g2 <- ggplot(g2_data, aes(year, share, colour = genre)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = setNames(unname(pal), top6)) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(labels = scales::percent) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Pop and jazz declined while electronic music caught up with rock",
       subtitle = "Share of genre-tagged release groups carrying each genre, by year of first release",
       x = NULL, y = "Share of tagged release groups",
       caption = "A release group can carry several genres, so shares do not add up to 100%.") +
  theme_mb()
save_plot(g2, "g2_genre_share_by_year")

decade_totals <- genre_by_year %>%
  distinct(year, decade, tagged_albums) %>%
  group_by(decade) %>%
  summarise(tagged = sum(tagged_albums))

show("G2. Tagged release groups per decade (the denominator)", decade_totals)

show("G2. Share (%) of tagged release groups per decade, 14 most common genres",
     genre_by_year %>%
       filter(genre %in% head(genre_rank$genre, 14)) %>%
       group_by(decade, genre) %>%
       summarise(albums = sum(albums), .groups = "drop") %>%
       left_join(decade_totals, by = "decade") %>%
       mutate(pct = round(100 * albums / tagged, 1)) %>%
       select(genre, decade, pct) %>%
       pivot_wider(names_from = decade, values_from = pct))

# =============================================================================
# G3. Genres by country
# =============================================================================

genre_by_country <- read_result("genre_by_country") %>%
  filter(!country %in% not_countries)

top_countries <- genre_by_country %>%
  distinct(country, artists_with_genre) %>%
  arrange(desc(artists_with_genre)) %>%
  head(12) %>%
  pull(country)

top_genres_c <- genre_by_country %>%
  filter(country %in% top_countries) %>%
  group_by(genre) %>%
  summarise(artists = sum(artists)) %>%
  arrange(desc(artists)) %>%
  head(10) %>%
  pull(genre)

g3_data <- genre_by_country %>%
  filter(country %in% top_countries, genre %in% top_genres_c) %>%
  mutate(country = factor(country, levels = rev(top_countries)),
         genre   = factor(genre, levels = top_genres_c))

g3 <- ggplot(g3_data, aes(genre, country, fill = share)) +
  geom_tile(colour = surface, linewidth = 1) +
  scale_fill_gradient(low = seq_low, high = seq_high, labels = scales::percent) +
  guides(fill = guide_colourbar(barwidth = 14, barheight = 0.5)) +
  labs(title = "Rock or electronic leads in 11 of the 12 countries; in France it is hip hop",
       subtitle = "Share of a country's genre-tagged artists that carry each genre (12 countries with most artists)",
       x = NULL, y = NULL,
       caption = "An artist can carry several genres. Genres come from artist tags and from the tags of their albums.") +
  theme_mb() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))
save_plot(g3, "g3_genres_by_country", height = 6)

show("G3. Artists with a genre, per country (top 12)",
     genre_by_country %>%
       distinct(country, artists_with_genre) %>%
       arrange(desc(artists_with_genre)) %>%
       head(12))

show("G3. Share (%) of each country's tagged artists, by genre",
     g3_data %>%
       mutate(pct = round(100 * share, 1)) %>%
       select(country, genre, pct) %>%
       pivot_wider(names_from = genre, values_from = pct) %>%
       arrange(desc(country)))

show("G3. The most distinctive genre of each country (highest share relative to the 12-country average)",
     genre_by_country %>%
       filter(country %in% top_countries, artists >= 50) %>%
       group_by(genre) %>%
       mutate(avg_share = mean(share)) %>%
       ungroup() %>%
       mutate(times_average = round(share / avg_share, 1), pct = round(100 * share, 1)) %>%
       group_by(country) %>%
       slice_max(times_average, n = 3, with_ties = FALSE) %>%
       ungroup() %>%
       select(country, genre, artists, pct, times_average))

# =============================================================================
# G4. Share of women by role in bands
# =============================================================================

band_roles <- read_result("band_roles")

roles <- band_roles %>%
  filter(gender_name %in% c("Male", "Female")) %>%
  group_by(role_family, role, gender_name) %>%
  summarise(memberships = sum(memberships), .groups = "drop") %>%
  pivot_wider(names_from = gender_name, values_from = memberships, values_fill = 0) %>%
  mutate(total = Male + Female, pct_female = Female / total)

overall_female <- sum(roles$Female) / sum(roles$total)

g4_data <- roles %>% arrange(desc(total)) %>% head(15)

g4 <- ggplot(g4_data, aes(pct_female, reorder(role, pct_female))) +
  geom_col(fill = pal[["blue"]], width = 0.6) +
  geom_vline(xintercept = overall_female, colour = ink_soft, linewidth = 0.4) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Violinists are close to half women; drummers and guitarists are over 90% men",
       subtitle = sprintf("Share of women among band members with each role (15 most common roles). Line: all roles, %.1f%%",
                          100 * overall_female),
       x = "Share of members who are women", y = NULL,
       caption = "Band memberships with a recorded role, members of known gender (male or female).") +
  theme_mb() +
  theme(panel.grid.major.y = element_blank())
save_plot(g4, "g4_women_by_band_role")

show("G4. Gender recorded for band members with a role (all genders)",
     band_roles %>%
       group_by(gender_name) %>%
       summarise(memberships = sum(memberships)) %>%
       mutate(pct = round(100 * memberships / sum(memberships), 1)) %>%
       arrange(desc(memberships)))

show("G4. 25 most common roles",
     roles %>%
       arrange(desc(total)) %>%
       head(25) %>%
       mutate(pct_female = round(100 * pct_female, 1)))

# =============================================================================
# G5. Song length over time
# =============================================================================

song_length <- read_result("song_length_by_year")

g5_data <- song_length %>% filter(year >= first_year, year <= last_year)

g5 <- ggplot(g5_data, aes(year)) +
  geom_ribbon(aes(ymin = q1_min, ymax = q3_min), fill = seq_low) +
  geom_line(aes(y = median_min), colour = pal[["blue"]], linewidth = 0.8) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Songs grew longer until about 2012 and have been getting shorter since",
       subtitle = "Median length in minutes (line) and the middle 50% of songs (band), by year the song first appeared",
       x = NULL, y = "Minutes",
       caption = "Songs with a known length and a dated release. Year = earliest first-release year of the albums containing the song.") +
  theme_mb()
save_plot(g5, "g5_song_length_by_year")

show("G5. Song length every 5 years",
     g5_data %>%
       filter(year %% 5 == 0) %>%
       transmute(year, songs,
                 q1 = round(q1_min, 2), median = round(median_min, 2), q3 = round(q3_min, 2),
                 mean = round(mean_min, 2),
                 pct_short = round(100 * unusually_short / songs, 1),
                 pct_long  = round(100 * unusually_long / songs, 1)))

show("G5. Longest and shortest median, and correlation with the year",
     tibble(
       year_of_longest_median  = g5_data$year[which.max(g5_data$median_min)],
       longest_median          = round(max(g5_data$median_min), 2),
       year_of_shortest_median = g5_data$year[which.min(g5_data$median_min)],
       shortest_median         = round(min(g5_data$median_min), 2),
       cor_1950_1995 = round(cor(g5_data$year[g5_data$year <= 1995],
                                 g5_data$median_min[g5_data$year <= 1995]), 3),
       cor_1995_2025 = round(cor(g5_data$year[g5_data$year >= 1995],
                                 g5_data$median_min[g5_data$year >= 1995]), 3)
     ))

# =============================================================================
# G6. Lyrics languages by country of the writer
# =============================================================================

language_by_country <- read_result("language_by_country") %>%
  filter(!country %in% not_countries,
         !language_name %in% c("[No linguistic content]", "[Multiple languages]"))

lang_countries <- language_by_country %>%
  group_by(country) %>%
  summarise(works = sum(works)) %>%
  arrange(desc(works)) %>%
  head(12)

# Three parts per country: English, the most common other language (named in
# the row label), and everything else. A fixed set of "top languages" would
# hide the main language of small countries (Dutch, Swedish, Finnish) in "Other".
main_other <- language_by_country %>%
  filter(language_name != "English") %>%
  group_by(country) %>%
  slice_max(works, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(country, other_language = language_name, other_works = works)

lang3 <- language_by_country %>%
  group_by(country) %>%
  summarise(total = sum(works),
            english = sum(works[language_name == "English"])) %>%
  left_join(main_other, by = "country") %>%
  mutate(other_works = coalesce(other_works, 0),
         rest  = total - english - other_works,
         label = paste0(country, " (", coalesce(other_language, "-"), ")"))

part_levels <- c("English", "Most common other language (in brackets)", "All other languages")

g6_data <- lang3 %>%
  filter(country %in% lang_countries$country) %>%
  mutate(label = reorder(label, english / total)) %>%
  select(label, total, English = english,
         `Most common other language (in brackets)` = other_works,
         `All other languages` = rest) %>%
  pivot_longer(c(-label, -total), names_to = "part", values_to = "works") %>%
  mutate(share = works / total, part = factor(part, levels = part_levels))

g6 <- ggplot(g6_data, aes(share, label, fill = part)) +
  geom_col(width = 0.6, colour = surface, linewidth = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = setNames(c(pal[["blue"]], pal[["orange"]], grey_other), part_levels)) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "English is the second lyrics language of northern Europe, and the first in Sweden",
       subtitle = "Works by lyrics language, by country of the writer (12 countries with most works)",
       x = "Share of works", y = NULL,
       caption = "Works with a lyricist or writer credit and a recorded language. Instrumental and multi-language works excluded.") +
  theme_mb() +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = margin(5.5, 24, 5.5, 5.5))
save_plot(g6, "g6_language_by_country")

show("G6. Three most common languages per country (top 12 countries)",
     language_by_country %>%
       filter(country %in% lang_countries$country) %>%
       group_by(country) %>%
       mutate(country_works = sum(works), pct = round(100 * works / country_works, 1)) %>%
       slice_max(works, n = 3, with_ties = FALSE) %>%
       ungroup() %>%
       arrange(desc(country_works), desc(works)) %>%
       select(country, country_works, language_name, works, pct))

show("G6. Spain in detail",
     language_by_country %>%
       filter(country == "Spain") %>%
       mutate(pct = round(100 * works / sum(works), 1)) %>%
       arrange(desc(works)) %>%
       select(language_name, works, pct) %>%
       head(10))

# =============================================================================
# G7. Largest record labels
# =============================================================================

top_labels <- read_result("top_labels")

g7_data <- top_labels %>%
  filter(!is_placeholder) %>%
  arrange(desc(releases)) %>%
  head(15) %>%
  mutate(label = paste0(name, " (", coalesce(country_code, "?"), ")"))

g7 <- ggplot(g7_data, aes(releases, reorder(label, releases))) +
  geom_col(fill = pal[["blue"]], width = 0.6) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Columbia has published far more releases than any other label",
       subtitle = "Number of releases published under each label (country code in brackets)",
       x = "Releases", y = NULL,
       caption = "The placeholder [no label], used for self-released music, is excluded.") +
  theme_mb() +
  theme(panel.grid.major.y = element_blank())
save_plot(g7, "g7_top_labels")

show("G7. Top 20 labels, including placeholders",
     top_labels %>%
       arrange(desc(releases)) %>%
       head(20) %>%
       select(name, type_name, country_code, begin_year, releases, is_placeholder))

show("G7. Releases of the 5,000 largest labels, by label country",
     top_labels %>%
       filter(!is_placeholder) %>%
       group_by(country) %>%
       summarise(labels = n(), releases = sum(releases)) %>%
       mutate(pct_releases = round(100 * releases / sum(releases), 1)) %>%
       arrange(desc(releases)) %>%
       head(12))

# =============================================================================
# G10. Song length by the main genre of the artist
# =============================================================================

song_genre <- read_result("song_length_by_genre")
song_genre_decade <- read_result("song_length_by_genre_decade")

g10_data <- song_genre %>%
  arrange(desc(songs)) %>%
  head(20) %>%
  mutate(genre = reorder(genre, median_min))

g10 <- ggplot(g10_data, aes(median_min, genre)) +
  geom_linerange(aes(xmin = q1_min, xmax = q3_min), colour = seq_low, linewidth = 4) +
  geom_point(colour = pal[["blue"]], size = 3) +
  scale_x_continuous(limits = c(0, NA), breaks = 0:12) +
  labs(title = "Song length by the main genre of the artist",
       subtitle = "Median length in minutes (dot) and middle 50% of songs (bar), 20 genres with most songs",
       x = "Minutes", y = NULL,
       caption = "Each song takes the main genre of its artist(s). Songs with a known length.") +
  theme_mb() +
  theme(panel.grid.major.y = element_blank())
save_plot(g10, "g10_song_length_by_genre", height = 6.5)

show("G10. Song length (minutes) for the 25 genres with most songs",
     song_genre %>%
       arrange(desc(songs)) %>%
       head(25) %>%
       transmute(genre, songs, q1 = round(q1_min, 2), median = round(median_min, 2),
                 q3 = round(q3_min, 2), mean = round(mean_min, 2)))

show("G10. Shortest and longest median among genres with at least 20,000 songs",
     bind_rows(
       song_genre %>% filter(songs >= 20000) %>% arrange(median_min) %>% head(8),
       song_genre %>% filter(songs >= 20000) %>% arrange(desc(median_min)) %>% head(8)
     ) %>%
       transmute(genre, songs, median = round(median_min, 2)))

show("G10. Median song length (minutes) by decade, 10 genres with most songs",
     song_genre_decade %>%
       filter(genre %in% head(song_genre$genre, 10), decade >= 1950, songs >= 500) %>%
       mutate(median = round(median_min, 2)) %>%
       select(genre, decade, median) %>%
       arrange(decade) %>%
       pivot_wider(names_from = decade, values_from = median))

# =============================================================================
# G8 and G9. Relationships between artist variables
# =============================================================================
# One row per real artist with at least one release, numeric variables only.

con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
artists_num <- tbl(con, "artist_profile") %>%
  filter(!is_placeholder, n_release_groups > 0L) %>%
  select(type_name, begin_year, n_release_groups, n_albums, career_years,
         n_genres, rating, rating_count) %>%
  collect()
dbDisconnect(con, shutdown = TRUE)

num_vars <- c("begin_year", "n_release_groups", "n_albums", "career_years",
              "n_genres", "rating", "rating_count")

describe <- function(v) {
  x <- artists_num[[v]]
  tibble(variable = v,
         n      = sum(!is.na(x)),
         mean   = round(mean(x, na.rm = TRUE), 2),
         sd     = round(sd(x, na.rm = TRUE), 2),
         min    = min(x, na.rm = TRUE),
         q1     = unname(quantile(x, 0.25, na.rm = TRUE)),
         median = median(x, na.rm = TRUE),
         q3     = unname(quantile(x, 0.75, na.rm = TRUE)),
         max    = max(x, na.rm = TRUE))
}

show("Descriptive statistics: artists with at least one release",
     bind_rows(lapply(num_vars, describe)))

show("Releases per artist, by artist type",
     artists_num %>%
       group_by(type_name) %>%
       summarise(artists = n(),
                 median_releases = median(n_release_groups),
                 mean_releases   = round(mean(n_release_groups), 1),
                 max_releases    = max(n_release_groups),
                 median_career   = median(career_years, na.rm = TRUE)) %>%
       arrange(desc(artists)))

# Spearman (rank) correlation: the counts are very skewed, so ranks are more
# appropriate than Pearson's linear correlation.
cor_mat <- cor(as.matrix(artists_num[num_vars]),
               use = "pairwise.complete.obs", method = "spearman")

cat("\n==== G8. Spearman correlation matrix ====\n")
print(round(cor_mat, 2))
saveRDS(cor_mat, file.path(in_dir, "artist_correlations.rds"))

cor_long <- as.data.frame(as.table(cor_mat))
names(cor_long) <- c("x", "y", "r")

g8 <- ggplot(cor_long, aes(x, y, fill = r)) +
  geom_tile(colour = surface, linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", r)), colour = ink, size = 3.3) +
  scale_fill_gradient2(low = "#e34948", mid = "#f0efec", high = pal[["blue"]],
                       midpoint = 0, limits = c(-1, 1)) +
  scale_y_discrete(limits = rev) +
  guides(fill = guide_colourbar(barwidth = 14, barheight = 0.5)) +
  labs(title = "How artist variables move together",
       subtitle = "Spearman rank correlation, artists with at least one release",
       x = NULL, y = NULL,
       caption = "Blue: the two variables rise together. Red: one rises as the other falls. Grey: no relationship.") +
  theme_mb() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))
save_plot(g8, "g8_artist_correlations", width = 7.5, height = 6.5)

g9_data <- artists_num %>%
  filter(!is.na(career_years), career_years >= 1) %>%
  mutate(career = cut(career_years, breaks = c(0, 1, 5, 10, 20, 40, Inf),
                      labels = c("1 year", "2-5", "6-10", "11-20", "21-40", "over 40"))) %>%
  group_by(career) %>%
  summarise(artists         = n(),
            q1_releases     = unname(quantile(n_release_groups, 0.25)),
            median_releases = median(n_release_groups),
            q3_releases     = unname(quantile(n_release_groups, 0.75)),
            max_releases    = max(n_release_groups))

g9 <- ggplot(g9_data, aes(career, median_releases)) +
  geom_linerange(aes(ymin = q1_releases, ymax = q3_releases),
                 colour = seq_low, linewidth = 5) +
  geom_point(colour = pal[["blue"]], size = 3.5) +
  geom_text(aes(label = median_releases), colour = ink_soft, nudge_x = 0.28, size = 3.4) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Longer careers mean more releases, and a much wider spread",
       subtitle = "Release groups per artist by career length: median (dot, labelled) and middle 50% of artists (bar)",
       x = "Career length in years (first to last dated release)", y = "Release groups per artist",
       caption = "Artists with at least one dated release. Career length is measured from the releases themselves.") +
  theme_mb() +
  theme(panel.grid.major.x = element_blank())
save_plot(g9, "g9_career_vs_releases")

show("G9. Releases by career length", g9_data)

message("\nAll figures are in ", normalizePath(fig_dir))
