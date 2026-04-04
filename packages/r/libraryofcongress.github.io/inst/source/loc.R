



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://www.loc.gov"

#' Search Library of Congress
#' @param query Search term
#' @param collection Collection (optional, e.g. 'newspapers', 'photos')
#' @return tibble of search results
#' @export
loc_search <- function(query, collection = NULL) {
  if (!is.null(collection)) {
    url <- sprintf("%s/%s/?q=%s&fo=json&c=25", .base, collection, URLencode(query, TRUE))
  } else {
    url <- sprintf("%s/search/?q=%s&fo=json&c=25", .base, URLencode(query, TRUE))
  }
  raw <- .fetch_json(url)
  results <- raw$results
  if (length(results) == 0) return(tibble::tibble(id = character(), title = character()))
  tibble::tibble(
    id = vapply(results, function(x) x$id %||% NA_character_, character(1)),
    title = vapply(results, function(x) {
      if (is.list(x$title) || length(x$title) > 1) x$title[[1]] else x$title %||% NA_character_
    }, character(1)),
    date = vapply(results, function(x) x$date %||% NA_character_, character(1)),
    type = vapply(results, function(x) {
      if (is.list(x$original_format)) paste(unlist(x$original_format), collapse = ", ") else x$original_format %||% NA_character_
    }, character(1))
  )
}

#' Generate LLM context for libraryofcongress.github.io
#' @return Character string
#' @export
loc_context <- function() { .build_context("libraryofcongress.github.io") }


# == Context ===================================================================

#' Generate LLM-friendly context for libraryofcongress.github.io
#'
#' @return Character string with full function signatures and bodies
#' @export
libraryofcongress_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/libraryofcongress.github.io.R"
  if (!file.exists(src_file)) {
    cat("# libraryofcongress.github.io context - source not found\n")
    return(invisible("# libraryofcongress.github.io context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

