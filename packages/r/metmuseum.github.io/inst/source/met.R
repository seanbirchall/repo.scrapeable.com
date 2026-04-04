



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://collectionapi.metmuseum.org/public/collection/v1"

#' Search Met Museum artworks
#' @param query Search term
#' @param limit Max results to fetch details for (default 20)
#' @return tibble of matching artworks (IDs and basic info)
#' @export
met_search <- function(query, limit = 20L) {
  url <- sprintf("%s/search?q=%s", .base, URLencode(query, TRUE))
  raw <- .fetch_json(url)
  ids <- unlist(raw$objectIDs)
  if (length(ids) == 0) return(tibble::tibble(object_id = integer(), title = character()))
  ids <- ids[seq_len(min(limit, length(ids)))]
  rows <- lapply(ids, function(id) {
    tryCatch({
      obj <- .fetch_json(sprintf("%s/objects/%d", .base, id))
      tibble::tibble(object_id = obj$objectID %||% NA_integer_,
                     title = obj$title %||% NA_character_,
                     artist = obj$artistDisplayName %||% NA_character_,
                     department = obj$department %||% NA_character_,
                     medium = obj$medium %||% NA_character_)
    }, error = function(e) NULL)
  })
  dplyr::bind_rows(rows[!vapply(rows, is.null, logical(1))])
}

#' Get a specific Met Museum object
#' @param object_id Object ID
#' @return tibble with one row of artwork metadata
#' @export
met_object <- function(object_id) {
  url <- sprintf("%s/objects/%d", .base, as.integer(object_id))
  raw <- .fetch_json(url)
  tibble::tibble(
    object_id = raw$objectID %||% NA_integer_,
    title = raw$title %||% NA_character_,
    artist = raw$artistDisplayName %||% NA_character_,
    department = raw$department %||% NA_character_,
    medium = raw$medium %||% NA_character_,
    date = raw$objectDate %||% NA_character_,
    culture = raw$culture %||% NA_character_,
    period = raw$period %||% NA_character_,
    image_url = raw$primaryImage %||% NA_character_
  )
}

#' Generate LLM context for metmuseum.github.io
#' @return Character string
#' @export
met_context <- function() {
  .build_context("metmuseum.github.io")
}


# == Context ===================================================================

#' Generate LLM-friendly context for metmuseum.github.io
#'
#' @return Character string with full function signatures and bodies
#' @export
metmuseum_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/metmuseum.github.io.R"
  if (!file.exists(src_file)) {
    cat("# metmuseum.github.io context - source not found\n")
    return(invisible("# metmuseum.github.io context - source not found"))
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

