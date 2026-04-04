# == Data access ===============================================================

#' List available Hartford datasets
#'
#' @return tibble: key, name, item_id, layer
#' @export
htfd_datasets <- function() {
  tibble(
    key     = names(.htfd_datasets),
    name    = vapply(.htfd_datasets, function(x) x$name, character(1)),
    item_id = vapply(.htfd_datasets, function(x) x$id, character(1)),
    layer   = vapply(.htfd_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Hartford open data portal
#'
#' @param q Search query
#' @return tibble: id, type, title, description, url
#' @export
htfd_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .htfd_base, utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(tibble())
  d <- raw$data
  tibble(
    id          = as.character(d$id),
    type        = as.character(d$type %||% NA_character_),
    title       = as.character(d$attributes$name %||% NA_character_),
    description = as.character(d$attributes$searchDescription %||% NA_character_),
    url         = as.character(d$attributes$url %||% NA_character_)
  )
}

#' Download a Hartford dataset as CSV
#'
#' @param key Dataset key from htfd_datasets(), or a raw item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return tibble with dataset records
#' @export
htfd_data <- function(key, layer = NULL) {
  if (key %in% names(.htfd_datasets)) {
    ds <- .htfd_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .htfd_base, item_id, lyr)
  .fetch_csv(url)
}

#' Download Hartford dataset as GeoJSON
#'
#' @param key Dataset key or item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return list (parsed GeoJSON)
#' @export
htfd_geojson <- function(key, layer = NULL) {
  if (key %in% names(.htfd_datasets)) {
    ds <- .htfd_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/geojson?layers=%d", .htfd_base, item_id, lyr)
  tryCatch(.fetch_json(url), error = function(e) NULL)
}

# == Context ===================================================================

#' Generate LLM-friendly context for hartford.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
htfd_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/hartford.gov.R"
  if (!file.exists(src_file)) { cat("# hartford.gov context - source not found\n"); return(invisible(NULL)) }
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
    blocks[[length(blocks) + 1]] <- c(rox, lines[fi:end_line], "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
