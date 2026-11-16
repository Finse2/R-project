value_text <- function(row, column) {
  value <- row[[column]]
  if (!length(value) || is.na(value[[1L]])) "" else trimws(as.character(value[[1L]]))
}

match_details <- function(kind, row) {
  if (identical(kind, "artist")) {
    c(value_text(row, "country"), value_text(row, "main_genre"))
  } else {
    c(value_text(row, "artist"), value_text(row, "year"))
  }
}

match_label <- function(candidate, annotate_kind) {
  row <- candidate$row
  fields <- c(value_text(row, "name"), match_details(candidate$kind, row), value_text(row, "comment"))
  label <- paste(fields[nzchar(fields)], collapse = " \u00b7 ")
  if (annotate_kind) {
    kind_label <- c(artist = "Artist", song = "Song", album = "Album")
    paste(kind_label[[candidate$kind]], label, sep = " \u2014 ")
  } else {
    label
  }
}

styles <- "
  .music-recommender { max-width: 990px; margin: 28px auto 42px; }
  .music-recommender h2 { margin: 0 0 12px; }
  .music-recommender .recommendation-form {
    padding: 20px; background: #fcfcfb; border: 1px solid #deded7; border-radius: 6px;
  }
  .music-recommender .shiny-input-container { width: 100%; max-width: 100%; }
  .music-recommender .recommendation-fields {
    display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px;
  }
  .music-recommender .catalog-suggestions { margin: -8px 0 16px; }
  .music-recommender .catalog-suggestions ul { list-style: none; margin-bottom: 0; padding-left: 0; }
  .music-recommender .catalog-suggestions button {
    text-align: left; width: 100%; white-space: normal;
  }
  .music-recommender .catalog-suggestions button:focus-visible {
    outline: 2px solid #2467a4; outline-offset: -2px;
  }
  .music-recommender .match-panel { margin-top: 20px; }
  .music-recommender .recommendation-status { margin-top: 16px; }
  .music-recommender .recommendations { margin-top: 24px; }
  .music-recommender .recommendations ol { padding-left: 25px; }
  .music-recommender .recommendation { border-bottom: 1px solid #e1e0d9; padding: 10px 0; }
  .music-recommender .recommendation a { font-weight: 600; }
  .music-recommender .recommendation-meta,
  .music-recommender .recommendation-evidence { color: #52514e; margin-top: 4px; }
  @media (max-width: 600px) {
    .music-recommender .recommendation-fields { grid-template-columns: 1fr; gap: 0; }
  }
"

recommendations_ui <- function(id) {
  ns <- NS(id)
  tags$section(
    class = "music-recommender",
    tags$head(tags$style(HTML(styles))),
    tags$h2("Find similar music"),
    div(class = "recommendation-form",
      radioButtons(ns("mode"), "Search by",
                   c("All" = "auto", "Artist" = "artist", "Song" = "song",
                     "Album" = "album", "Ask local AI" = "ai"),
                   inline = TRUE),
      div(class = "recommendation-fields",
        div(
          conditionalPanel("input.mode !== 'ai'",
            textInput(ns("query"), "Artist, song, or album", placeholder = "Portishead"),
            div(role = "region", `aria-label` = "Catalog suggestions", uiOutput(ns("suggestions"))),
            ns = ns),
          conditionalPanel("input.mode === 'ai'",
            textInput(ns("prompt"), "Ask a question", placeholder = "Songs like Teardrop by Massive Attack"), ns = ns)
        ),
        conditionalPanel("input.mode === 'song' || input.mode === 'album' || input.mode === 'auto'",
          textInput(ns("performer"), "Artist (optional)", placeholder = "Massive Attack"), ns = ns)
      ),
      radioButtons(ns("result_kind"), "Recommend",
                   c("Automatic" = "auto", "Artists" = "artist", "Songs" = "song"),
                   inline = TRUE),
      actionButton(ns("search"), "Find recommendations", class = "btn-primary")
    ),
    uiOutput(ns("matches")),
    div(class = "recommendation-status", role = "status", `aria-live` = "polite",
        uiOutput(ns("status"))),
    uiOutput(ns("recommendations"))
  )
}

recommendations_server <- function(id, model_path) {
  model_path <- Sys.getenv("MUSIC_RECOMMENDER_MODEL", unset = model_path)
  moduleServer(id, function(input, output, session) {
    model <- NULL
    con <- NULL
    candidates <- reactiveVal(list())
    results <- reactiveVal(NULL)
    status <- reactiveVal(NULL)
    current_request <- reactiveVal(NULL)
    dismissed_suggestions <- reactiveVal(NULL)

    session$onSessionEnded(function() {
      if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
      if (!is.null(model)) close_model(model)
    })

    ensure_catalog <- function() {
      if (is.null(con)) {
        path <- Sys.getenv("MUSICBRAINZ_DB", unset = model$catalog_path)
        con <<- open_catalog(path)
      }
    }

    ensure_model <- function() {
      if (is.null(model)) model <<- load_model(model_path)
    }

    search_kind <- function(request, kind, suggest = FALSE) {
      limit <- if (suggest) 4L else 12L
      if (identical(kind, "artist")) {
        rows <- find_artists(model, request$query, limit)
      } else {
        ensure_catalog()
        lookup <- switch(kind,
          song = if (suggest) suggest_songs else find_songs,
          album = if (suggest) suggest_albums else find_albums
        )
        rows <- lookup(con, request$query, request$artist, limit)
      }
      lapply(seq_len(nrow(rows)), function(index) {
        list(kind = kind, row = rows[index, , drop = FALSE])
      })
    }

    search_catalog <- function(request, suggest = FALSE) {
      ensure_model()
      kinds <- if (identical(request$kind, "auto")) c("artist", "song", "album") else request$kind
      if (identical(request$kind, "auto") && nzchar(request$artist)) {
        kinds <- c("song", "album")
      }
      unlist(lapply(kinds, function(kind) search_kind(request, kind, suggest)), recursive = FALSE)
    }

    typed_request <- reactive({
      mode <- input$mode
      if (is.null(mode) || identical(mode, "ai")) return(NULL)
      query <- input$query
      performer <- input$performer
      list(query = if (is.null(query)) "" else trimws(query), kind = mode, recommend = "auto",
           artist = if (identical(mode, "artist") || is.null(performer)) "" else trimws(performer))
    })
    paused_request <- debounce(typed_request, 320)
    suggestions <- reactive({
      request <- paused_request()
      if (is.null(request) || nchar(request$query) < 3L ||
          identical(request, dismissed_suggestions())) {
        return(NULL)
      }
      tryCatch(
        list(request = request, matches = search_catalog(request, suggest = TRUE)),
        error = function(err) list(request = request, error = conditionMessage(err))
      )
    })

    fetch <- function(candidate, request) {
      requested_kind <- if (identical(input$result_kind, "auto")) request$recommend else input$result_kind
      default_target <- if (identical(candidate$kind, "artist")) "artist" else "song"
      target <- if (identical(requested_kind, "auto")) default_target else requested_kind
      if (!identical(candidate$kind, "artist") || identical(target, "song")) ensure_catalog()
      rows <- withProgress(
        message = "Ranking recommendations",
        recommend_from_match(model, con, candidate$kind, candidate$row, target, 10L)
      )
      if (!nrow(rows)) {
        status(list(text = "This match has too little catalog information for recommendations.",
                    error = FALSE))
        return(invisible(NULL))
      }
      status(NULL)
      results(list(rows = rows, target = target, seed = value_text(candidate$row, "name")))
    }

    show_error <- function(err) {
      status(list(text = conditionMessage(err), error = TRUE))
    }

    output$suggestions <- renderUI({
      live <- typed_request()
      if (is.null(live) || nchar(live$query) < 3L || identical(live, dismissed_suggestions())) return(NULL)
      if (!identical(live, paused_request())) return(NULL)
      found <- suggestions()
      if (is.null(found) || !identical(found$request, live)) return(NULL)
      if (!is.null(found$error)) return(tags$p(class = "text-danger", found$error))
      if (!length(found$matches)) {
        return(tags$p(class = "text-muted", "No suggestions. Use Find recommendations to search the full catalog."))
      }

      event_id <- jsonlite::toJSON(session$ns("suggestion_pick"), auto_unbox = TRUE)
      div(class = "catalog-suggestions",
          tags$ul(class = "list-group",
            lapply(found$matches, function(candidate) {
              key <- paste(candidate$kind, value_text(candidate$row, "gid"), sep = ":")
              onclick <- sprintf("Shiny.setInputValue(%s, %s, {priority: 'event'});", event_id,
                                 jsonlite::toJSON(key, auto_unbox = TRUE))
              tags$li(class = "list-group-item",
                tags$button(type = "button", class = "btn btn-link",
                  onclick = onclick, match_label(candidate, annotate_kind = TRUE)))
            })))
    })

    observeEvent(input$suggestion_pick, {
      found <- suggestions()
      req(!is.null(found), identical(found$request, typed_request()),
          length(found$matches))
      keys <- vapply(found$matches, function(candidate) {
        paste(candidate$kind, value_text(candidate$row, "gid"), sep = ":")
      }, character(1))
      index <- match(input$suggestion_pick, keys)
      req(length(index) == 1L, !is.na(index))
      candidate <- found$matches[[index]]
      dismissed_suggestions(found$request)
      candidates(list())
      results(NULL)
      status(NULL)
      tryCatch(fetch(candidate, found$request), error = show_error)
    })

    observeEvent(input$search, {
      candidates(list())
      results(NULL)
      status(NULL)
      dismissed_suggestions(typed_request())

      tryCatch({
        request <- if (identical(input$mode, "ai")) {
          withProgress(message = "Reading your request",
            interpret_request(
              input$prompt,
              model = Sys.getenv("MUSIC_RECOMMENDER_OLLAMA_MODEL", unset = "qwen3:4b"),
              endpoint = Sys.getenv("MUSIC_RECOMMENDER_OLLAMA_URL",
                                    unset = "http://127.0.0.1:11434")
            )
          )
        } else typed_request()
        if (is.null(request) || !nzchar(request$query)) {
          stop("Enter an artist, song, or album.", call. = FALSE)
        }

        current_request(request)
        found <- withProgress(message = "Searching MusicBrainz", search_catalog(request))
        candidates(found)

        if (!length(found)) {
          detail <- paste0("No matches for \u201c", request$query, "\u201d. Check the spelling or try a shorter title.")
          if (nzchar(request$artist)) detail <- paste(detail, "You can also clear the artist filter.")
          status(list(text = detail, error = FALSE))
        } else if (length(found) == 1L) {
          fetch(found[[1L]], request)
        }
      }, error = show_error)
    })

    observeEvent(input$recommend, {
      found <- candidates()
      req(length(found) > 1L, input$match)
      index <- suppressWarnings(as.integer(input$match))
      req(!is.na(index), index >= 1L, index <= length(found))
      results(NULL)
      status(NULL)
      tryCatch(fetch(found[[index]], current_request()), error = show_error)
    })

    output$matches <- renderUI({
      found <- candidates()
      if (length(found) <= 1L) return(NULL)
      multiple_kinds <- length(unique(vapply(found, function(candidate) candidate$kind, character(1)))) > 1L
      labels <- vapply(found, match_label, character(1), annotate_kind = multiple_kinds)
      div(class = "match-panel",
          tags$h2("Choose a match"),
          selectInput(session$ns("match"), "Catalog matches", choices = setNames(seq_along(labels), labels),
                      selectize = FALSE),
          actionButton(session$ns("recommend"), "Use this match"))
    })

    output$status <- renderUI({
      note <- status()
      if (is.null(note)) return(NULL)
      tags$p(class = if (note$error) "text-danger" else "text-muted", note$text)
    })

    output$recommendations <- renderUI({
      answer <- results()
      if (is.null(answer)) return(NULL)
      is_artist <- identical(answer$target, "artist")
      section <- if (is_artist) "artist" else "recording"
      rows <- answer$rows

      tags$section(class = "recommendations", `aria-labelledby` = session$ns("recommendations-title"),
        tags$h2(id = session$ns("recommendations-title"),
                paste(if (is_artist) "Artists" else "Songs", "related to", answer$seed)),
        tags$ol(lapply(seq_len(nrow(rows)), function(index) {
          row <- rows[index, , drop = FALSE]
          details <- match_details(answer$target, row)
          details <- details[nzchar(details)]
          shared <- value_text(row, "shared_tags")
          artist_tags <- value_text(row, "shared_artist_tags")
          credits <- value_text(row, "shared_credits")
          evidence <- if (nzchar(shared)) {
            paste(if (is_artist) "Shared genres" else "Shared recording tags", shared, sep = " \u00b7 ")
          } else if (nzchar(artist_tags)) {
            paste("Shared artist genres", artist_tags, sep = " \u00b7 ")
          }
          credit_note <- if (is_artist && nzchar(credits) && as.numeric(credits) > 0) {
            paste("Shared catalog credits", credits, sep = " \u00b7 ")
          }
          gid <- value_text(row, "gid")
          href <- paste0("https://musicbrainz.org/", section, "/", URLencode(gid, reserved = TRUE))
          tags$li(class = "recommendation",
            if (nzchar(gid)) tags$a(href = href, target = "_blank", rel = "noopener noreferrer",
                                    value_text(row, "name")) else tags$strong(value_text(row, "name")),
            if (length(details)) div(class = "recommendation-meta", paste(details, collapse = " \u00b7 ")),
            if (length(evidence)) div(class = "recommendation-evidence", evidence),
            if (length(credit_note)) div(class = "recommendation-evidence", credit_note)
          )
        }))
      )
    })
  })
}
