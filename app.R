# =============================================================================
# app.R  -  Stage 7: web application (Shiny)
# -----------------------------------------------------------------------------
# Interactive versions of the graphs from 05_explore.R. Every tab has controls
# that change what is drawn, and every chart shows the exact values on hover.
#
# The chart tabs read summary files in data/analysis/. Recommendations also
# use the MusicBrainz database and the model in music-recommender/.
#
# How to run: put this file in the project folder (the one that contains the
# "data" folder) and run   shiny::runApp("path/to/that/folder")
#
# Packages: shiny, bslib, ggplot2, dplyr, tidyr, plotly, scales

# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(bslib)

recommender_dir <- "music-recommender"
# The recommender reads the database in data/ unless MUSICBRAINZ_DB says otherwise,
# so the project folder can be moved without retraining the model.
if (!nzchar(Sys.getenv("MUSICBRAINZ_DB")) && file.exists("data/musicbrainz.duckdb")) {
  Sys.setenv(MUSICBRAINZ_DB = normalizePath("data/musicbrainz.duckdb", winslash = "/"))
}
recommender <- new.env()
for (file in c("model.R", "catalog.R", "llm.R", "recommendations.R")) {
  source(file.path(recommender_dir, file), local = recommender)
}

# ---- Data --------------------------------------------------------------------

data_dir <- "data/analysis"
if (!dir.exists(data_dir)) {
  stop("Folder '", data_dir, "' not found. app.R must be in the project folder ",
       "that contains 'data'. Current folder: ", getwd())
}
read_result <- function(name) readRDS(file.path(data_dir, paste0(name, ".rds")))

last_year     <- 2025L                        # 2026 is incomplete in the dump
not_countries <- c("[Worldwide]", "Europe")   # ISO areas that are not countries

# Releases: four types, with the share of each within its year
releases <- read_result("albums_by_year") %>%
  filter(year <= last_year) %>%
  mutate(type = if_else(type_name %in% c("Album", "Single", "EP"), type_name, "Other")) %>%
  group_by(year, type) %>%
  summarise(n = sum(release_groups), .groups = "drop") %>%
  complete(year, type, fill = list(n = 0)) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

genre_year <- read_result("genre_by_year") %>% filter(year <= last_year)
genre_choices <- genre_year %>%
  filter(year >= 1960) %>%
  group_by(genre) %>%
  summarise(albums = sum(albums)) %>%
  arrange(desc(albums)) %>%
  head(80) %>%
  pull(genre)

genre_country <- read_result("genre_by_country") %>% filter(!country %in% not_countries)
gc_countries <- genre_country %>%
  distinct(country, artists_with_genre) %>%
  filter(artists_with_genre >= 300) %>%
  arrange(desc(artists_with_genre)) %>%
  pull(country)

band_roles <- read_result("band_roles") %>% filter(gender_name %in% c("Male", "Female"))
br_countries <- band_roles %>%
  filter(!country %in% c("Unknown", not_countries)) %>%
  group_by(country) %>%
  summarise(m = sum(memberships)) %>%
  filter(m >= 500) %>%
  arrange(desc(m)) %>%
  pull(country)

song_length <- read_result("song_length_by_year") %>% filter(year <= last_year)

# Song length by the main genre of the artist, overall and per decade
song_genre <- bind_rows(
  read_result("song_length_by_genre") %>% mutate(period = "All years"),
  read_result("song_length_by_genre_decade") %>%
    filter(decade >= 1950, decade <= 2020) %>%
    mutate(period = paste0(decade, "s")) %>%
    select(-decade)
)
sg_periods <- c("All years", paste0(seq(1950, 2020, 10), "s"))

# Languages: English, the most common other language, and the rest
languages <- read_result("language_by_country") %>%
  filter(!country %in% not_countries,
         !language_name %in% c("[No linguistic content]", "[Multiple languages]"))
main_other <- languages %>%
  filter(language_name != "English") %>%
  group_by(country) %>%
  slice_max(works, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(country, other_language = language_name, other_works = works)
lang3 <- languages %>%
  group_by(country) %>%
  summarise(total = sum(works), english = sum(works[language_name == "English"])) %>%
  left_join(main_other, by = "country") %>%
  mutate(other_works    = coalesce(other_works, 0),
         other_language = coalesce(other_language, "none"),
         rest           = total - english - other_works) %>%
  filter(total >= 300) %>%
  arrange(desc(total))

labels_all <- read_result("top_labels")
label_countries <- labels_all %>%
  filter(!country %in% c("Unknown", not_countries)) %>%
  count(country, sort = TRUE) %>%
  filter(n >= 10) %>%
  pull(country)

cor_mat <- read_result("artist_correlations")
var_names <- c(begin_year       = "Year the artist began",
               n_release_groups = "Release groups",
               n_albums         = "Albums",
               career_years     = "Career length (years)",
               n_genres         = "Number of genres",
               rating           = "Average rating",
               rating_count     = "Number of ratings")

# ---- Look and feel (same as 05_explore.R) ------------------------------------

pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
blue       <- pal[1]
orange     <- pal[2]
grey_other <- "#898781"
seq_low    <- "#cde2fb"
seq_high   <- "#0d366b"
ink_soft   <- "#52514e"
surface    <- "#fcfcfb"
type_colours <- c(Album = pal[1], Single = pal[2], EP = pal[3], Other = grey_other)
part_levels  <- c("English", "Most common other language", "All other languages")
part_colours <- setNames(c(blue, orange, grey_other), part_levels)

theme_mb <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.3),
          axis.text        = element_text(colour = ink_soft),
          axis.title       = element_text(colour = ink_soft),
          legend.title     = element_blank())
}

# ggplot -> interactive plotly chart; the tooltip shows the "text" aesthetic.
# Transparent background so the chart sits on its card, same type as the page.
to_plotly <- function(p) {
  ggplotly(p, tooltip = "text") %>%
    layout(font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif",
                       color = ink_soft),
           legend = list(orientation = "h", x = 0, y = 1.1, font = list(size = 13),
                         title = list(text = "")),
           hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#e1e0d9",
                             font = list(color = "#0b0b0b", size = 13)),
           margin = list(t = 40, r = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
    config(displayModeBar = FALSE)
}

# Colour follows the item, not its position: an item keeps its colour while it
# stays selected, and a newly selected item takes the first free colour.
keep_slots <- function(slots, selected, n = length(pal)) {
  slots <- slots[names(slots) %in% selected]
  new   <- setdiff(selected, names(slots))
  free  <- setdiff(seq_len(n), unname(slots))
  if (length(new) > 0) slots[new] <- free[seq_along(new)]
  slots
}

num <- function(x) formatC(round(x), format = "d", big.mark = ",")
pct <- function(x) paste0(format(round(100 * x, 1), nsmall = 1, trim = TRUE), "%")

# ---- User interface ----------------------------------------------------------

# Headline numbers for the overview, from the audit (02_audit.R). These tiles are
# left out if the audit file is not there.
audit_path <- "output/audit.rds"
table_rows <- if (file.exists(audit_path)) {
  ids <- readRDS(audit_path)[["C1. Duplicated identifiers"]]
  setNames(ids$rows, ids$table)
} else NULL
compact <- function(x) if (x >= 1e6) paste0(format(round(x / 1e6, 1), nsmall = 1), " M") else num(x)

stat_tiles <- c(
  if (!is.null(table_rows)) c(
    "Artists"        = compact(table_rows[["artist"]]),
    "Release groups" = compact(table_rows[["release_group"]]),
    "Recordings"     = compact(table_rows[["recording"]]),
    "Record labels"  = compact(table_rows[["label"]])
  ),
  "Years of releases" = paste0(min(releases$year), "–", last_year)
)

# One entry per chart page: menu, page title, headline finding, what it shows
stories <- list(
  rel  = list("Music over time", "Releases", "Singles overtook albums in 2019",
              "Release groups by year of first release, split into albums, singles, EPs and other types."),
  gen  = list("Music over time", "Genres over time", "Pop and jazz declined while electronic music caught up with rock",
              "Share of genre-tagged release groups that carry each genre, year by year."),
  sl   = list("Music over time", "Song length", "Songs grew longer until 2012 and have been getting shorter since",
              "Median song length and the middle 50% of songs, by the year a song first appeared."),
  gc   = list("Around the world", "Genres by country", "Rock or electronic leads almost everywhere. In France it is hip hop",
              "Share of each country's genre-tagged artists that carry each genre."),
  lang = list("Around the world", "Lyrics languages", "English is the first lyrics language in Sweden and the second across northern Europe",
              "Works by lyrics language, by the country of the person who wrote the words."),
  lab  = list("Around the world", "Labels", "Columbia has published far more releases than any other label",
              "Record labels ranked by the number of releases they published."),
  br   = list("Artists", "Band roles", "Violinists are close to half women. Drummers and guitarists are over 90% men",
              "Share of women among band members, for each instrument and vocal role."),
  sg   = list("Artists", "Songs by genre", "How long songs are in each genre",
              "Median song length and its spread for each artist genre, overall or by decade."),
  cor  = list("Artists", "Artist variables", "Long careers mean many releases",
              "Spearman correlations between career length, releases, genres and ratings.")
)

chart_tab <- function(id, note, ...) {
  story <- stories[[id]]
  nav_panel(
    story[[2]], value = id,
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(width = 290, title = "Filters", ...),
      card(
        full_screen = TRUE, class = "chart-card",
        card_header(
          div(class = "chart-eyebrow", paste(story[[1]], "·", story[[2]])),
          h2(class = "chart-headline", story[[3]])
        ),
        navset_underline(
          nav_panel("Chart", plotlyOutput(paste0(id, "_plot"), height = "520px")),
          nav_panel("Data", div(class = "table-wrap", tableOutput(paste0(id, "_table"))))
        ),
        card_footer(class = "chart-note", note)
      )
    )
  )
}

story_card <- function(id) {
  story <- stories[[id]]
  div(class = "story-card",
      div(class = "chart-eyebrow", story[[2]]),
      h3(story[[3]]),
      p(story[[4]]),
      actionLink(paste0("go_", id), "Explore →", class = "story-link"))
}

overview <- nav_panel(
  "Overview", value = "overview",
  div(class = "hero",
      div(class = "hero-inner",
          div(class = "hero-eyebrow", "MusicBrainz · full database dump of 14 November 2026"),
          h1("Nearly 150 years of recorded music, in numbers"),
          p(class = "hero-lead",
            "Every artist, album, song and record label in the open MusicBrainz encyclopedia, ",
            "cleaned and summarised. Pick a finding below to explore it, or ask for recommendations."),
          div(class = "hero-actions",
              actionButton("go_explore", "Start exploring", class = "btn-light btn-lg"),
              actionButton("go_recommend", "Find similar music", class = "btn-outline-light btn-lg")))),
  div(class = "page-body",
      div(class = "stat-row",
          lapply(names(stat_tiles), function(label) {
            div(class = "stat-tile",
                div(class = "stat-value", stat_tiles[[label]]),
                div(class = "stat-label", label))
          })),
      lapply(unique(vapply(stories, `[[`, character(1), 1)), function(section) {
        ids <- names(stories)[vapply(stories, function(x) x[[1]] == section, logical(1))]
        tagList(h2(class = "section-title", section),
                div(class = "story-grid", lapply(ids, story_card)))
      }),
      p(class = "source-note",
        "Source: MusicBrainz database dump, 2026-11-14 (CC0). Loaded with DuckDB, cleaned and ",
        "summarised in R (scripts 01–05). Charts are interactive: hover for exact values ",
        "and use the filters beside each chart.")))

recommender_ready <- file.exists(file.path(recommender_dir, "model", "artists.duckdb")) &&
  file.exists(Sys.getenv("MUSICBRAINZ_DB", unset = "data/musicbrainz.duckdb"))

app_css <- "
  :root { --ink: #0b0b0b; --ink-soft: #52514e; --muted: #898781; --line: #e1e0d9;
          --surface: #fcfcfb; --plane: #f9f9f7; --blue: #2a78d6; }
  body { background: var(--plane); }
  /* the overview banner runs edge to edge, right under the navbar */
  .container-fluid:has(> .tab-content > .tab-pane.active[data-value='overview']) { padding: 0; margin: 0; }
  .tab-pane[data-value='overview'] { margin-top: 0 !important; }
  .btn-primary { --bs-btn-color: #fff; --bs-btn-hover-color: #fff; --bs-btn-active-color: #fff; }

  /* Recommendations page, in the same style as the chart cards */
  .music-recommender h2 { font-size: 1.7rem; font-weight: 700; letter-spacing: -.01em; color: var(--ink); }
  .music-recommender .match-panel h2, .music-recommender .recommendations h2 { font-size: 1.2rem; }
  .music-recommender .recommendations ol { background: var(--surface); border: 1px solid rgba(11,11,11,.08);
    border-radius: 12px; box-shadow: 0 4px 18px rgba(13,27,46,.05); padding: 6px 24px 6px 56px; }
  .music-recommender .recommendation:last-child { border-bottom: 0; }
  .music-recommender .recommendation::marker { color: var(--muted); font-weight: 600; }
  .music-recommender .recommendation-form { background: var(--surface); border-radius: 12px;
    border: 1px solid rgba(11,11,11,.08); box-shadow: 0 4px 18px rgba(13,27,46,.05); padding: 24px; }  .navbar-brand { display: flex; align-items: center; gap: 10px; font-weight: 700; letter-spacing: -.01em; }
  .brand-mark { width: 26px; height: 26px; }

  /* Overview */
  .hero { background: radial-gradient(1200px 420px at 85% -10%, #3987e5 0%, transparent 60%),
                      linear-gradient(135deg, #0d1b2e 0%, #0d366b 55%, #1c5cab 100%);
          color: #fff; padding: 72px 24px 96px; }
  .hero-inner { max-width: 1100px; margin: 0 auto; }
  .hero-eyebrow { text-transform: uppercase; letter-spacing: .08em; font-size: .78rem;
                  font-weight: 600; color: #b7d3f6; margin-bottom: 14px; }
  .hero h1 { font-size: clamp(2rem, 4.5vw, 3.3rem); font-weight: 800; letter-spacing: -.025em;
             line-height: 1.08; max-width: 820px; margin-bottom: 18px; color: #fff; }
  .hero-lead { font-size: 1.15rem; color: #dbe8fa; max-width: 680px; margin-bottom: 30px; }
  .hero-actions { display: flex; gap: 12px; flex-wrap: wrap; }
  .page-body { max-width: 1100px; margin: 0 auto; padding: 0 16px 48px; }
  .stat-row { display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
              gap: 14px; margin-top: -48px; position: relative; }
  .stat-tile { background: var(--surface); border: 1px solid rgba(11,11,11,.08); border-radius: 12px;
               padding: 18px 20px; box-shadow: 0 6px 24px rgba(13,27,46,.08); }
  .stat-value { font-size: 1.9rem; font-weight: 700; color: var(--ink); letter-spacing: -.02em; }
  .stat-label { color: var(--ink-soft); font-size: .9rem; margin-top: 2px; }
  .section-title { font-size: 1.25rem; font-weight: 700; color: var(--ink); margin: 40px 0 14px; }
  .story-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 14px; }
  .story-card { background: var(--surface); border: 1px solid rgba(11,11,11,.08); border-radius: 12px;
                padding: 20px 22px; display: flex; flex-direction: column;
                transition: box-shadow .15s ease, transform .15s ease, border-color .15s ease; }
  .story-card:hover { box-shadow: 0 10px 28px rgba(13,27,46,.10); transform: translateY(-2px);
                      border-color: rgba(42,120,214,.35); }
  .story-card h3 { font-size: 1.08rem; font-weight: 700; color: var(--ink); line-height: 1.3; margin: 6px 0 8px; }
  .story-card p { color: var(--ink-soft); font-size: .93rem; flex: 1; margin-bottom: 14px; }
  .story-link { font-weight: 600; text-decoration: none; color: var(--blue); align-self: flex-start; }
  .story-link:hover { text-decoration: underline; }
  .source-note { color: var(--muted); font-size: .85rem; margin-top: 40px; border-top: 1px solid var(--line);
                 padding-top: 16px; }

  /* Chart pages */
  .chart-eyebrow { text-transform: uppercase; letter-spacing: .07em; font-size: .72rem;
                   font-weight: 600; color: var(--muted); }
  .chart-card { border-radius: 12px; border: 1px solid rgba(11,11,11,.08);
                box-shadow: 0 4px 18px rgba(13,27,46,.05); }
  .chart-card .card-header { display: block; background: var(--surface); border-bottom: 0; padding: 18px 22px 4px; }
  .chart-headline { font-size: 1.35rem; font-weight: 700; color: var(--ink); letter-spacing: -.01em;
                    margin: 4px 0 0; }
  .chart-card .nav-underline { padding: 0 22px; }
  .chart-card .tab-content { padding: 6px 14px 0; }
  .chart-note { background: var(--surface); color: var(--ink-soft); font-size: .88rem;
                border-top: 1px solid var(--line); padding: 12px 22px; }
  .bslib-sidebar-layout > .sidebar { background: var(--surface); }
  .bslib-sidebar-layout .sidebar-title { font-size: .78rem; text-transform: uppercase; letter-spacing: .07em;
                                         color: var(--muted); font-weight: 700; }
  .table-wrap { max-height: 540px; overflow: auto; padding: 4px 8px 12px; }
  .table-wrap table { width: 100%; font-size: .9rem; font-variant-numeric: tabular-nums; }
  .table-wrap thead th { position: sticky; top: 0; background: var(--surface); color: var(--ink-soft);
                         font-weight: 600; border-bottom: 1px solid #c3c2b7; }
  .table-wrap tbody tr:hover { background: #f0efec; }
  .irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single
    { background: var(--blue); border-color: var(--blue); }
  .recommender-page { padding: 8px 16px 40px; }
"

brand <- tags$span(
  HTML('<svg class="brand-mark" viewBox="0 0 32 32" aria-hidden="true">
          <circle cx="16" cy="16" r="15" fill="#2a78d6"/>
          <circle cx="16" cy="16" r="10.5" fill="none" stroke="#86b6ef" stroke-width="1"/>
          <circle cx="16" cy="16" r="7" fill="none" stroke="#86b6ef" stroke-width="1"/>
          <circle cx="16" cy="16" r="3.2" fill="#fff"/></svg>'),
  "MusicBrainz Explorer")

ui <- page_navbar(
  id = "main",
  title = brand,
  window_title = "MusicBrainz Explorer",
  theme = bs_theme(
    version = 5, bg = "#f9f9f7", fg = "#0b0b0b", primary = "#2a78d6",
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
    heading_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")
  ),
  navbar_options = navbar_options(bg = "#0d1b2e", theme = "dark"),
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css))),

  overview,

  nav_menu(
    "Music over time",
    chart_tab(
      "rel",
      "Release groups by year of first release. 2026 is left out because the data ends in November 2026.",
      sliderInput("rel_years", "Years", min = 1900, max = last_year,
                  value = c(1950, last_year), sep = ""),
      checkboxGroupInput("rel_types", "Types", choices = names(type_colours),
                         selected = names(type_colours)),
      radioButtons("rel_measure", "Show",
                   choices = c("Number of release groups" = "n",
                               "Share of the year's releases" = "share"))
    ),
    chart_tab(
      "gen",
      "Share of genre-tagged release groups that carry each genre. A release group can carry several genres, so shares do not add up to 100%.",
      selectizeInput("gen_genres", "Genres (up to 6)", choices = genre_choices,
                     selected = genre_choices[1:6], multiple = TRUE,
                     options = list(maxItems = 6)),
      sliderInput("gen_years", "Years", min = 1950, max = last_year,
                  value = c(1960, last_year), sep = "")
    ),
    chart_tab(
      "sl",
      "Length of songs by the year they first appeared on a release. The band covers the middle 50% of songs.",
      sliderInput("sl_years", "Years", min = 1900, max = last_year,
                  value = c(1950, last_year), sep = ""),
      checkboxInput("sl_mean", "Also show the mean", value = FALSE)
    )
  ),

  nav_menu(
    "Around the world",
    chart_tab(
      "gc",
      "Share of a country's genre-tagged artists that carry each genre. Genres come from artist tags and from the tags of their albums.",
      selectizeInput("gc_countries", "Countries (up to 15)", choices = gc_countries,
                     selected = gc_countries[1:12], multiple = TRUE,
                     options = list(maxItems = 15)),
      sliderInput("gc_n", "Number of genres", min = 5, max = 15, value = 10, step = 1)
    ),
    chart_tab(
      "lang",
      "Works with a lyricist or writer credit, by country of the writer. The language in brackets is the most common one after English. Instrumental and multi-language works are excluded.",
      selectizeInput("lang_countries", "Countries (up to 20)", choices = lang3$country,
                     selected = lang3$country[1:12], multiple = TRUE,
                     options = list(maxItems = 20)),
      radioButtons("lang_order", "Order countries by",
                   choices = c("Share of English" = "english", "Number of works" = "total"))
    ),
    chart_tab(
      "lab",
      "Labels ranked by number of releases. [no label] is the placeholder MusicBrainz uses for self-released music.",
      selectInput("lab_country", "Country of the label",
                  choices = c("All countries", label_countries)),
      sliderInput("lab_n", "Number of labels", min = 5, max = 30, value = 15, step = 1),
      checkboxInput("lab_placeholder", "Include [no label]", value = FALSE)
    )
  ),

  nav_menu(
    "Artists",
    chart_tab(
      "br",
      "Share of women among band members with each role (members recorded as male or female). The line is the share across all roles in the selection.",
      selectInput("br_country", "Country of the member",
                  choices = c("All countries", br_countries)),
      checkboxGroupInput("br_family", "Role family",
                         choices = c("Instruments" = "instrument", "Vocals" = "vocal"),
                         selected = c("instrument", "vocal")),
      sliderInput("br_n", "Number of roles", min = 5, max = 25, value = 15, step = 1),
      sliderInput("br_min", "Minimum members per role", min = 5, max = 200, value = 20, step = 5)
    ),
    chart_tab(
      "sg",
      "Each song takes the main genre of its artist(s). Dot: median length. Bar: the middle 50% of songs. Genres are the ones with most songs in the chosen period.",
      selectInput("sg_period", "Period (year the song first appeared)", choices = sg_periods),
      sliderInput("sg_n", "Number of genres", min = 5, max = 30, value = 20, step = 1),
      radioButtons("sg_order", "Order genres by",
                   choices = c("Median length" = "median_min", "Number of songs" = "songs"))
    ),
    chart_tab(
      "cor",
      "Spearman rank correlation between artist variables, for artists with at least one release. Blue: the two rise together. Red: one rises as the other falls.",
      checkboxGroupInput("cor_vars", "Variables", choices = setNames(names(var_names), var_names),
                         selected = names(var_names))
    )
  ),

  nav_panel(
    "Recommendations", value = "recommend",
    div(class = "recommender-page",
        if (!recommender_ready) {
          div(class = "alert alert-warning", style = "max-width: 990px; margin: 20px auto 0;",
              strong("Recommendations need the full database. "),
              "This page uses data/musicbrainz.duckdb (built by 01_load.R) and the model built by ",
              "music-recommender/train.R. The chart pages work without them.")
        },
        recommender$recommendations_ui("recommendations"))
  ),

  nav_spacer(),
  nav_item(tags$a(href = "https://musicbrainz.org", target = "_blank", rel = "noopener noreferrer",
                  "musicbrainz.org ↗"))
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  recommender$recommendations_server("recommendations", file.path(recommender_dir, "model"))

  # Overview buttons and story cards switch to their page
  observeEvent(input$go_explore, nav_select("main", "rel"))
  observeEvent(input$go_recommend, nav_select("main", "recommend"))
  lapply(names(stories), function(id) {
    observeEvent(input[[paste0("go_", id)]], nav_select("main", id))
  })

  # -- Releases
  rel_data <- reactive({
    releases %>%
      filter(year >= input$rel_years[1], year <= input$rel_years[2],
             type %in% input$rel_types) %>%
      mutate(type = factor(type, levels = names(type_colours)))
  })
  output$rel_plot <- renderPlotly({
    d <- rel_data()
    validate(need(nrow(d) > 0, "Select at least one type."))
    as_share <- input$rel_measure == "share"
    d <- d %>%
      mutate(value = if (as_share) share else n,
             tip = paste0(type, ", ", year, ": ", num(n), " release groups (", pct(.data$share), ")"))
    p <- ggplot(d, aes(year, value, colour = type, group = type, text = tip)) +
      geom_line(linewidth = 0.8) +
      scale_colour_manual(values = type_colours) +
      scale_y_continuous(labels = if (as_share) scales::percent else scales::comma) +
      labs(x = NULL, y = if (as_share) "Share of the year's release groups" else "Release groups") +
      theme_mb()
    to_plotly(p)
  })
  output$rel_table <- renderTable({
    rel_data() %>%
      transmute(Year = as.integer(year), Type = type, `Release groups` = num(n), Share = pct(share)) %>%
      arrange(desc(Year), Type)
  })

  # -- Genres over time
  slots <- new.env()
  slots$genre <- integer(0)
  gen_colours <- reactive({
    slots$genre <- keep_slots(slots$genre, input$gen_genres)
    setNames(pal[slots$genre], names(slots$genre))
  })
  gen_data <- reactive({
    genre_year %>%
      filter(genre %in% input$gen_genres,
             year >= input$gen_years[1], year <= input$gen_years[2])
  })
  output$gen_plot <- renderPlotly({
    validate(need(length(input$gen_genres) > 0, "Select at least one genre."))
    cols <- gen_colours()
    d <- gen_data() %>%
      mutate(genre = factor(genre, levels = names(cols)),
             tip = paste0(genre, ", ", year, ": ", pct(share), " (", num(albums),
                          " of ", num(tagged_albums), " tagged release groups)"))
    p <- ggplot(d, aes(year, share, colour = genre, group = genre, text = tip)) +
      geom_line(linewidth = 0.8) +
      scale_colour_manual(values = cols) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "Share of tagged release groups") +
      theme_mb()
    to_plotly(p)
  })
  output$gen_table <- renderTable({
    gen_data() %>%
      transmute(Year = as.integer(year), Genre = genre, `Release groups` = num(albums),
                `Tagged that year` = num(tagged_albums), Share = pct(share)) %>%
      arrange(desc(Year), Genre)
  })

  # -- Genres by country
  gc_data <- reactive({
    d <- genre_country %>% filter(country %in% input$gc_countries)
    top <- d %>%
      group_by(genre) %>%
      summarise(a = sum(artists)) %>%
      arrange(desc(a)) %>%
      head(input$gc_n) %>%
      pull(genre)
    d %>%
      filter(genre %in% top) %>%
      mutate(genre   = factor(genre, levels = top),
             country = factor(country, levels = rev(input$gc_countries)))
  })
  output$gc_plot <- renderPlotly({
    validate(need(length(input$gc_countries) > 0, "Select at least one country."))
    d <- gc_data() %>%
      mutate(tip = paste0(country, ", ", genre, ": ", pct(share), " (", num(artists),
                          " of ", num(artists_with_genre), " tagged artists)"))
    p <- ggplot(d, aes(genre, country, fill = share, text = tip)) +
      geom_tile(colour = surface, linewidth = 1) +
      scale_fill_gradient(low = seq_low, high = seq_high, labels = scales::percent) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_mb() +
      theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1))
    to_plotly(p)
  })
  output$gc_table <- renderTable({
    gc_data() %>%
      arrange(desc(country), genre) %>%
      transmute(Country = country, Genre = genre, Artists = num(artists), Share = pct(share))
  })

  # -- Band roles
  br_data <- reactive({
    d <- band_roles %>% filter(role_family %in% input$br_family)
    if (input$br_country != "All countries") d <- d %>% filter(country == input$br_country)
    d <- d %>%
      group_by(role, gender_name) %>%
      summarise(m = sum(memberships), .groups = "drop") %>%
      pivot_wider(names_from = gender_name, values_from = m, values_fill = 0)
    if (!"Female" %in% names(d)) d$Female <- rep(0, nrow(d))
    if (!"Male" %in% names(d)) d$Male <- rep(0, nrow(d))
    d %>% mutate(total = Male + Female, pct_female = Female / total)
  })
  output$br_plot <- renderPlotly({
    all_roles <- br_data()
    d <- all_roles %>%
      filter(total >= input$br_min) %>%
      arrange(desc(total)) %>%
      head(input$br_n)
    validate(need(nrow(d) > 0, "No role has enough members for this selection. Lower the minimum."))
    overall <- sum(all_roles$Female) / sum(all_roles$total)
    d <- d %>%
      mutate(role = reorder(role, pct_female),
             tip = paste0(role, ": ", pct(pct_female), " women (", num(Female), " women, ",
                          num(Male), " men)"))
    p <- ggplot(d, aes(role, pct_female, text = tip)) +
      geom_col(fill = blue, width = 0.6) +
      geom_hline(yintercept = overall, colour = ink_soft, linewidth = 0.4) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = paste0("Share of members who are women (line: all roles, ", pct(overall), ")")) +
      theme_mb() +
      theme(panel.grid.major.y = element_blank())
    to_plotly(p)
  })
  output$br_table <- renderTable({
    br_data() %>%
      filter(total >= input$br_min) %>%
      arrange(desc(total)) %>%
      head(input$br_n) %>%
      transmute(Role = role, Women = num(Female), Men = num(Male), `Share of women` = pct(pct_female))
  })

  # -- Song length
  sl_data <- reactive({
    song_length %>% filter(year >= input$sl_years[1], year <= input$sl_years[2])
  })
  output$sl_plot <- renderPlotly({
    d <- sl_data()
    validate(need(nrow(d) > 0, "No data for these years."))
    lines <- d %>%
      select(year, songs, Median = median_min, Mean = mean_min) %>%
      pivot_longer(c(Median, Mean), names_to = "series", values_to = "minutes") %>%
      filter(series == "Median" | input$sl_mean) %>%
      mutate(tip = paste0(year, ": ", tolower(series), " ", formatC(minutes, format = "f", digits = 2),
                          " min (", num(songs), " songs)"))
    p <- ggplot() +
      geom_ribbon(data = d, aes(year, ymin = q1_min, ymax = q3_min), fill = seq_low) +
      geom_line(data = lines, aes(year, minutes, colour = series, group = series, text = tip),
                linewidth = 0.8) +
      scale_colour_manual(values = c(Median = blue, Mean = orange)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = NULL, y = "Minutes") +
      theme_mb()
    to_plotly(p)
  })
  output$sl_table <- renderTable({
    sl_data() %>%
      arrange(desc(year)) %>%
      transmute(Year = as.integer(year), Songs = num(songs), `25%` = round(q1_min, 2),
                Median = round(median_min, 2), `75%` = round(q3_min, 2), Mean = round(mean_min, 2))
  })

  # -- Songs by genre
  sg_data <- reactive({
    song_genre %>%
      filter(period == input$sg_period, songs >= 200) %>%
      arrange(desc(songs)) %>%
      head(input$sg_n)
  })
  output$sg_plot <- renderPlotly({
    d <- sg_data()
    validate(need(nrow(d) > 0, "No genre has enough songs in this period."))
    d <- d %>%
      mutate(order_value = if (input$sg_order == "songs") songs else median_min,
             genre = reorder(genre, order_value),
             tip = paste0(genre, ": median ", formatC(median_min, format = "f", digits = 2),
                          " min, middle half ", formatC(q1_min, format = "f", digits = 2), " to ",
                          formatC(q3_min, format = "f", digits = 2), " min (", num(songs), " songs)"))
    p <- ggplot(d, aes(genre, median_min, text = tip)) +
      geom_linerange(aes(ymin = q1_min, ymax = q3_min), colour = seq_low, linewidth = 3) +
      geom_point(colour = blue, size = 2.5) +
      coord_flip() +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = NULL, y = "Minutes") +
      theme_mb() +
      theme(panel.grid.major.y = element_blank())
    to_plotly(p)
  })
  output$sg_table <- renderTable({
    sg_data() %>%
      transmute(Genre = genre, Songs = num(songs), `25%` = round(q1_min, 2),
                Median = round(median_min, 2), `75%` = round(q3_min, 2), Mean = round(mean_min, 2))
  })

  # -- Lyrics languages
  lang_data <- reactive({
    lang3 %>%
      filter(country %in% input$lang_countries) %>%
      mutate(label = paste0(country, " (", other_language, ")"),
             order_value = if (input$lang_order == "english") english / total else total)
  })
  output$lang_plot <- renderPlotly({
    validate(need(length(input$lang_countries) > 0, "Select at least one country."))
    d <- lang_data() %>%
      mutate(label = reorder(label, order_value)) %>%
      select(label, total, other_language, English = english,
             `Most common other language` = other_works, `All other languages` = rest) %>%
      pivot_longer(c(English, `Most common other language`, `All other languages`),
                   names_to = "part", values_to = "works") %>%
      mutate(share = works / total,
             part_name = if_else(part == "Most common other language", other_language, part),
             part = factor(part, levels = part_levels),
             tip = paste0(label, " - ", part_name, ": ", pct(share), " (", num(works), " works)"))
    p <- ggplot(d, aes(label, share, fill = part, text = tip)) +
      geom_col(width = 0.6, colour = surface, linewidth = 0.5,
               position = position_stack(reverse = TRUE)) +
      coord_flip() +
      scale_fill_manual(values = part_colours) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "Share of works") +
      theme_mb() +
      theme(panel.grid.major.y = element_blank())
    to_plotly(p)
  })
  output$lang_table <- renderTable({
    lang_data() %>%
      arrange(desc(order_value)) %>%
      transmute(Country = country, Works = num(total), English = pct(english / total),
                `Most common other language` = other_language,
                `Its share` = pct(other_works / total), `All others` = pct(rest / total))
  })

  # -- Labels
  lab_data <- reactive({
    d <- labels_all
    if (!input$lab_placeholder) d <- d %>% filter(!is_placeholder)
    if (input$lab_country != "All countries") d <- d %>% filter(country == input$lab_country)
    d %>% arrange(desc(releases)) %>% head(input$lab_n)
  })
  output$lab_plot <- renderPlotly({
    d <- lab_data()
    validate(need(nrow(d) > 0, "No labels for this selection."))
    d <- d %>%
      mutate(label = paste0(name, " (", coalesce(country_code, "?"), ") #", id),
             label = reorder(label, releases),
             tip = paste0(name, ": ", num(releases), " releases. ", type_name, ", ", country,
                          if_else(is.na(begin_year), "", paste0(", founded ", begin_year))))
    p <- ggplot(d, aes(label, releases, text = tip)) +
      geom_col(fill = blue, width = 0.6) +
      coord_flip() +
      scale_x_discrete(labels = function(x) sub(" #[0-9]+$", "", x)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Releases") +
      theme_mb() +
      theme(panel.grid.major.y = element_blank())
    to_plotly(p)
  })
  output$lab_table <- renderTable({
    lab_data() %>%
      transmute(Label = name, Type = type_name, Country = country,
                Founded = as.integer(begin_year), Releases = num(releases))
  })

  # -- Artist variables
  cor_data <- reactive({
    v <- input$cor_vars
    m <- cor_mat[v, v, drop = FALSE]
    d <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
    names(d) <- c("x", "y", "r")
    d %>%
      mutate(x = factor(unname(var_names[x]), levels = unname(var_names[v])),
             y = factor(unname(var_names[y]), levels = rev(unname(var_names[v]))))
  })
  output$cor_plot <- renderPlotly({
    validate(need(length(input$cor_vars) >= 2, "Select at least two variables."))
    d <- cor_data() %>%
      mutate(tip = paste0(x, " and ", y, ": ", sprintf("%.2f", r)))
    p <- ggplot(d, aes(x, y, fill = r, text = tip)) +
      geom_tile(colour = surface, linewidth = 1) +
      geom_text(aes(label = sprintf("%.2f", r)), size = 3.5) +
      scale_fill_gradient2(low = "#e34948", mid = "#f0efec", high = blue,
                           midpoint = 0, limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_mb() +
      theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1))
    to_plotly(p)
  })
  output$cor_table <- renderTable({
    validate(need(length(input$cor_vars) >= 2, "Select at least two variables."))
    cor_data() %>%
      filter(as.integer(x) < length(input$cor_vars) + 1 - as.integer(y)) %>%
      arrange(desc(abs(r))) %>%
      transmute(`Variable 1` = x, `Variable 2` = y, Correlation = sprintf("%.2f", r))
  })
}

shinyApp(ui, server)
