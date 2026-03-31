# data-cityofchicago-org.R
# Self-contained Chicago Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app tokens optional for higher rate limits)
# Rate limits: 1000 requests/hour without app token

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.chi_base <- "https://data.cityofchicago.org"

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

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(),
  updated_at = as.POSIXct(character()), view_count = integer()
)

.schema_query <- tibble()


# == Dataset discovery =========================================================

#' List available datasets on Chicago Open Data
#'
#' Returns a catalog of datasets available on data.cityofchicago.org.
#' Filter by category to narrow results.
#'
#' @param limit Number of datasets to return (default 50, max 200)
#' @param category Optional category filter (e.g. "Public Safety")
#' @return tibble: id, name, description, category, type, updated_at, view_count
chi_datasets <- function(limit = 50, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .chi_base, limit)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)
  if (!is.data.frame(raw)) return(.schema_datasets)

  df <- as_tibble(raw)
  result <- df |>
    transmute(
      id          = as.character(id),
      name        = as.character(name),
      description = as.character(description %||% NA),
      category    = as.character(category %||% NA),
      type        = as.character(displayType %||% NA),
      updated_at  = as.POSIXct(as.numeric(viewLastModified %||% NA),
                                origin = "1970-01-01", tz = "UTC"),
      view_count  = as.integer(viewCount %||% NA)
    )

  if (!is.null(category)) {
    result <- result |> filter(grepl(!!category, .data$category, ignore.case = TRUE))
  }
  result
}


# == Query a dataset ===========================================================

#' Query a Socrata dataset on Chicago Open Data
#'
#' Runs a SoQL query against a specific dataset. Uses the SODA 2.0 API.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "ijzp-q8t2")
#' @param where Optional SoQL WHERE clause (e.g. "primary_type='THEFT'")
#' @param select Optional SoQL SELECT clause (e.g. "date, primary_type")
#' @param order Optional SoQL ORDER BY clause (e.g. "date DESC")
#' @param limit Number of rows to return (default 1000, max 50000)
#' @param offset Offset for pagination (default 0)
#' @return tibble with columns from the dataset
chi_query <- function(dataset_id, where = NULL, select = NULL,
                      order = NULL, limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order

  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .chi_base, dataset_id, query_str)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}


# == Context ===================================================================

#' Show Chicago Open Data package context for LLM integration
#'
#' Prints a summary of all public functions, their signatures, and
#' roxygen documentation. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
chi_context <- function() {
  header <- c(
    "# data.cityofchicago.org - Chicago Open Data Client (Socrata SODA)",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (app tokens optional for higher rate limits)",
    "# Rate limits: 1000 requests/hour without app token",
    "#",
    "# Popular dataset IDs:",
    "#   ijzp-q8t2 - Crimes - 2001 to Present",
    "#   85ca-t3if - Crimes - 2001 to Present (Map)",
    "#   ydr8-5enu - Strategic Subject List (SSL)",
    "#   6zsd-86xi - Chicago Traffic Crashes",
    "#   97t2-u7y7 - Red Light Camera Violations",
    "#",
    "# SoQL reference: https://dev.socrata.com/docs/queries/"
  )
  .build_context("data.cityofchicago.org", header_lines = header)
}
