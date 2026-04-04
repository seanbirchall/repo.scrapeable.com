


# nces-ed-gov-ccd.R
# Self-contained NCES Common Core of Data (CCD) client.
# Uses the Urban Institute Education Data API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ccd_base <- "https://educationdata.urban.org/api/v1"

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

# == Context ===================================================================

#' Generate LLM-friendly context for nces.ed.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
ccd_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/nces.ed.gov.R"
  if (!file.exists(src_file)) {
    cat("# nces.ed.gov context - source not found\n")
    return(invisible("# nces.ed.gov context - source not found"))
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

