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
#' Returns school-level data from the NCES Common Core of Data via the
#' Urban Institute Education Data API.
#'
#' @param year School year (e.g., 2021 for 2021-22). Default 2021.
#' @param state Two-digit FIPS code as string (e.g., "06" for CA), or NULL for all.
#' @param limit Maximum records to return. Default 100.
#' @return tibble: ncessch, school_name, leaid, lea_name, state_location,
#'   city_location, zip_location, enrollment, year
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
#' Returns district-level data from the NCES Common Core of Data.
#'
#' @param year School year (e.g., 2021 for 2021-22). Default 2021.
#' @param state Two-digit FIPS code as string (e.g., "06" for CA), or NULL for all.
#' @param limit Maximum records to return. Default 100.
#' @return tibble: leaid, lea_name, state_leaid, state_location,
#'   city_location, zip_location, enrollment, year
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

#' Print CCD context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
ccd_context <- function() {
  .build_context(
    pkg_name = "nces.ed.gov",
    header_lines = c(
      "# Package: nces.ed.gov",
      "# NCES Common Core of Data (CCD) via Urban Institute Education Data API",
      "# Auth: none",
      "# Rate limits: none documented",
      "#",
      "# Data: K-12 public school and district directory information",
      "# Years available: approximately 1986-2022",
      "# FIPS state codes: 06=CA, 36=NY, 48=TX, 17=IL, etc."
    )
  )
}
