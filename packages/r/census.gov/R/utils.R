#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# census-gov.R
# Self-contained US Census Bureau client.
# All public functions return tibbles. All columns return as character —
# caller decides types (Census uses "-", "N", "(X)" for suppressed data).
#
# Dependencies: httr2, jsonlite, dplyr, tidyr, tibble
# Auth: optional API key (query param). Get one at api.census.gov/data/key_signup.html


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.census_cache <- new.env(parent = emptyenv())
.census_base <- "https://api.census.gov/data"

# Max variables per Census API request (hard limit is 50)
.census_var_limit <- 49

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Census response parser ----------------------------------------------------
# Census API returns JSON array-of-arrays: [[headers], [row1], [row2], ...]
# Returns ALL columns as character — no auto-type guessing.

.parse_census_response <- function(raw) {
  if (is.matrix(raw)) {
    if (nrow(raw) < 2) return(tibble())
    headers <- tolower(raw[1, ])
    mat <- raw[-1, , drop = FALSE]
    colnames(mat) <- headers
    as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
  } else if (is.list(raw) && length(raw) >= 2) {
    headers <- tolower(unlist(raw[[1]]))
    rows <- raw[-1]
    if (length(rows) == 0)
      return(tibble(!!!setNames(rep(list(character()), length(headers)), headers)))
    mat <- do.call(rbind, lapply(rows, function(r) {
      r[vapply(r, is.null, logical(1))] <- NA_character_
      unlist(r)
    }))
    colnames(mat) <- headers
    as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
  } else {
    tibble()
  }
}

# -- Variable caching ----------------------------------------------------------

.cached_variables <- function(dataset, year, key = NULL) {
  cache_key <- paste0("vars_", dataset, "_", year)
  if (!is.null(.census_cache[[cache_key]])) return(.census_cache[[cache_key]])
  result <- census_variables(dataset, year, key)
  .census_cache[[cache_key]] <- result
  result
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  title = character(), description = character(),
  dataset = character(), year = integer()
)

.schema_variables <- tibble(
  name = character(), label = character(), concept = character(),
  predicate_type = character(), group = character()
)

.schema_groups <- tibble(name = character(), description = character())

.schema_geography <- tibble(
  name = character(), geo_level = character(),
  requires = character(), wildcard = character()
)


