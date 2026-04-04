# nces-ed-gov-ccd.R
# Self-contained NCES Common Core of Data (CCD) client.
# Uses the Urban Institute Education Data API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ccd_base <- "https://educationdata.urban.org/api/v1"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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

.schema_schools <- tibble(
  ncessch = character(), school_name = character(), leaid = character(),
  lea_name = character(), state_location = character(), city_location = character(),
  zip_location = character(), enrollment = integer(), year = integer()
)

.schema_districts <- tibble(
  leaid = character(), lea_name = character(), state_leaid = character(),
  state_location = character(), city_location = character(),
  zip_location = character(), enrollment = integer(), year = integer()
)

# == Public functions ==========================================================

#' Search CCD school directory
#'
#' Returns school-level directory data from the NCES Common Core of Data (CCD)
#' via the Urban Institute Education Data API. Includes school names, locations,
#' district information, and enrollment counts for public schools in the US.
#'
#' @param year School year as integer (e.g. 2021 for the 2021-22 school year).
#'   Default 2021. Available years vary; recent data typically covers 2009-2022.
#' @param state Two-digit FIPS state code as string (e.g. "06" for California,
#'   "36" for New York). When NULL (default), returns schools from all states.
#' @param limit Maximum number of records to return (default 100). The API
#'   may return more rows than requested due to pagination behavior.
#' @return A tibble with columns:
#'   \describe{
#'     \item{ncessch}{NCES school ID (12-digit unique identifier)}
#'     \item{school_name}{School name}
#'     \item{leaid}{Local education agency (district) ID}
#'     \item{lea_name}{District name}
#'     \item{state_location}{Two-letter state abbreviation}
#'     \item{city_location}{City name}
#'     \item{zip_location}{ZIP code}
#'     \item{enrollment}{Total enrollment count (integer)}
#'     \item{year}{School year}
#'   }
#' @examples
#' ccd_schools(year = 2021, state = "06", limit = 10)
#' @seealso [ccd_districts()], [ccd_context()]
#' @source <https://educationdata.urban.org/documentation/>
#' @export
ccd_schools <- function(year = 2021, state = NULL, limit = 100) {
  url <- sprintf("%s/schools/ccd/directory/%d/?limit=%d",
                 .ccd_base, as.integer(year), as.integer(limit))
  if (!is.null(state)) url <- paste0(url, "&fips=", state)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || nrow(results) == 0) return(.schema_schools)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      ncessch = as.character(if ("ncessch" %in% nms) ncessch else NA_character_),
      school_name = as.character(if ("school_name" %in% nms) school_name else NA_character_),
      leaid = as.character(if ("leaid" %in% nms) leaid else NA_character_),
      lea_name = as.character(if ("lea_name" %in% nms) lea_name else NA_character_),
      state_location = as.character(if ("state_location" %in% nms) state_location else NA_character_),
      city_location = as.character(if ("city_location" %in% nms) city_location else NA_character_),
      zip_location = as.character(if ("zip_location" %in% nms) zip_location else NA_character_),
      enrollment = as.integer(if ("enrollment" %in% nms) enrollment else NA_integer_),
      year = as.integer(year)
    )
}

#' Search CCD school district directory
#'
#' Returns district-level (local education agency) data from the NCES Common
#' Core of Data via the Urban Institute Education Data API. Includes district
#' names, locations, and aggregate enrollment counts.
#'
#' @param year School year as integer (e.g. 2021 for the 2021-22 school year).
#'   Default 2021.
#' @param state Two-digit FIPS state code as string (e.g. "06" for California).
#'   When NULL (default), returns districts from all states.
#' @param limit Maximum number of records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{leaid}{NCES local education agency ID (7-digit unique identifier)}
#'     \item{lea_name}{District name}
#'     \item{state_leaid}{State-assigned district ID}
#'     \item{state_location}{Two-letter state abbreviation}
#'     \item{city_location}{City name}
#'     \item{zip_location}{ZIP code}
#'     \item{enrollment}{Total district enrollment (integer)}
#'     \item{year}{School year}
#'   }
#' @examples
#' ccd_districts(year = 2021, state = "06", limit = 10)
#' @seealso [ccd_schools()], [ccd_context()]
#' @source <https://educationdata.urban.org/documentation/>
#' @export
ccd_districts <- function(year = 2021, state = NULL, limit = 100) {
  url <- sprintf("%s/school-districts/ccd/directory/%d/?limit=%d",
                 .ccd_base, as.integer(year), as.integer(limit))
  if (!is.null(state)) url <- paste0(url, "&fips=", state)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || nrow(results) == 0) return(.schema_districts)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      leaid = as.character(if ("leaid" %in% nms) leaid else NA_character_),
      lea_name = as.character(if ("lea_name" %in% nms) lea_name else NA_character_),
      state_leaid = as.character(if ("state_leaid" %in% nms) state_leaid else NA_character_),
      state_location = as.character(if ("state_location" %in% nms) state_location else NA_character_),
      city_location = as.character(if ("city_location" %in% nms) city_location else NA_character_),
      zip_location = as.character(if ("zip_location" %in% nms) zip_location else NA_character_),
      enrollment = as.integer(if ("enrollment" %in% nms) enrollment else NA_integer_),
      year = as.integer(year)
    )
}

#' Get nces-ed-gov-ccd client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the NCES CCD (K-12 schools) client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' ccd_context()
#' @export
ccd_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ccd_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nces-ed-gov-ccd.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nces-ed-gov-ccd")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nces-ed-gov-ccd context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# nces-ed-gov-ccd", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
