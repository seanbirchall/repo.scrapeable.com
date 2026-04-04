# == Data access ===============================================================

#' List available Ferndale datasets
#'
#' @return tibble: key, name, item_id, layer
#' @export
fern_datasets <- function() {
  tibble(
    key     = names(.fern_datasets),
    name    = vapply(.fern_datasets, function(x) x$name, character(1)),
    item_id = vapply(.fern_datasets, function(x) x$id, character(1)),
    layer   = vapply(.fern_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Ferndale open data portal
#'
#' @param q Search query
#' @return tibble: id, type, title, description, url
#' @export
fern_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .fern_base, utils::URLencode(q))
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

#' Download a Ferndale dataset as CSV
#'
#' @param key Dataset key from fern_datasets(), or a raw item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return tibble with dataset records
#' @export
fern_data <- function(key, layer = NULL) {
  if (key %in% names(.fern_datasets)) {
    ds <- .fern_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .fern_base, item_id, lyr)
  .fetch_csv(url)
}

#' Get Ferndale drug incident and NARCAN data
#'
#' @return tibble with drug incident records (2007-2017)
#' @export
fern_drug_incidents <- function() {
  fern_data("drug_incidents")
}

# == Context ===================================================================

#' Generate LLM-friendly context for ferndalemi.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
fern_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/ferndalemi.gov.R"
  if (!file.exists(src_file)) { cat("# ferndalemi.gov context - source not found\n"); return(invisible(NULL)) }
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
