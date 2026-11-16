interpret_request <- function(prompt, model = "qwen3:4b",
                              endpoint = "http://127.0.0.1:11434") {
  if (!is.character(prompt) || length(prompt) != 1L || is.na(prompt) ||
      !nzchar(trimws(prompt))) {
    stop("Enter a request that names an artist, song, or album.", call. = FALSE)
  }
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(trimws(model))) {
    stop("Natural-language search needs a local Ollama model.", call. = FALSE)
  }
  model <- trimws(model)
  if (grepl("(^cloud/|[:-]cloud$)", model, ignore.case = TRUE)) {
    stop("Choose an Ollama model that runs on this computer.", call. = FALSE)
  }

  local_endpoint <- "^http://(127\\.0\\.0\\.1|localhost|\\[::1\\])(:[0-9]{1,5})?/?$"
  if (!is.character(endpoint) || length(endpoint) != 1L || is.na(endpoint) ||
      !grepl(local_endpoint, endpoint)) {
    stop("The Ollama URL must be a local HTTP address.", call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Natural-language search requires the R packages httr and jsonlite.",
         call. = FALSE)
  }

  result_kinds <- c("auto", "artist", "song")
  seed_kinds <- c(result_kinds, "album")
  schema <- list(
    type = "object",
    properties = list(
      query = list(type = "string"),
      artist = list(type = "string"),
      kind = list(type = "string", enum = seed_kinds),
      recommend = list(type = "string", enum = result_kinds)
    ),
    required = c("query", "artist", "kind", "recommend"),
    additionalProperties = FALSE
  )
  instructions <- paste(
    "Extract one catalog search from the user's text. Return JSON only.",
    "Put the exact artist name, song title, or album title mentioned in query, preserving spelling and accents.",
    "Leave query empty if the request names no recognizable seed. Never supply an example or guess a seed.",
    "Set kind to artist, song, or album when clear, or auto when ambiguous.",
    "For a song or album, use artist only when the user names the credited artist. Otherwise return an empty string.",
    "Set recommend to song or artist when the user specifies what they want back, otherwise auto.",
    "Examples:",
    "'Songs like Numb by Linkin Park' => {\"query\":\"Numb\",\"artist\":\"Linkin Park\",\"kind\":\"song\",\"recommend\":\"song\"}.",
    "'Artists similar to Bjork' => {\"query\":\"Bjork\",\"artist\":\"\",\"kind\":\"artist\",\"recommend\":\"artist\"}.",
    "'Songs like the album OK Computer by Radiohead' => {\"query\":\"OK Computer\",\"artist\":\"Radiohead\",\"kind\":\"album\",\"recommend\":\"song\"}.",
    "'What is similar to Genesis?' => {\"query\":\"Genesis\",\"artist\":\"\",\"kind\":\"auto\",\"recommend\":\"auto\"}."
  )
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = instructions),
      list(role = "user", content = trimws(prompt))
    ),
    format = schema,
    stream = FALSE,
    think = FALSE,
    options = list(temperature = 0, num_ctx = 2048L, num_predict = 128L)
  )
  response <- tryCatch(
    httr::POST(
      paste0(sub("/$", "", endpoint), "/api/chat"),
      httr::config(proxy = "", followlocation = FALSE, connecttimeout = 4L), httr::timeout(90),
      body = body, encode = "json"
    ),
    error = function(err) {
      if (inherits(err, "curl_error_operation_timedout")) {
        stop("Ollama timed out. Try again after the model finishes loading.", call. = FALSE)
      }
      stop("Couldn't reach the local Ollama model. Start Ollama and try again.",
           call. = FALSE)
    }
  )
  status <- httr::status_code(response)
  if (status == 404L) {
    stop(sprintf("Local Ollama model '%s' is unavailable.", model), call. = FALSE)
  }
  if (status != 200L) {
    stop(sprintf("Local Ollama returned HTTP %d. Check the server and selected model.", status),
         call. = FALSE)
  }

  fields <- tryCatch({
    envelope <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    jsonlite::fromJSON(envelope$message$content)
  }, error = function(err) NULL)
  valid_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }
  if (!is.list(fields) || !all(schema$required %in% names(fields)) ||
      !all(vapply(fields[schema$required], valid_text, logical(1))) ||
      !fields$kind %in% seed_kinds || !fields$recommend %in% result_kinds) {
    stop("Ollama returned a response the app couldn't read. Try an artist, song, or album.",
         call. = FALSE)
  }
  query <- trimws(fields$query)
  if (!nzchar(query)) {
    stop("Name an artist, song, or album to start the recommendations.", call. = FALSE)
  }
  list(query = query, artist = trimws(fields$artist), kind = fields$kind,
       recommend = fields$recommend)
}
