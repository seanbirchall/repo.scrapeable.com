# exoplanetarchive-caltech-edu.R
# Self-contained NASA Exoplanet Archive TAP API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented (be respectful)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.exo_base <- "https://exoplanetarchive.ipac.caltech.edu/TAP/sync"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
    j <- fi - 1; rox_start <- fi
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

# == Schemas ===================================================================

.schema_query <- tibble(
  pl_name = character(), hostname = character(), disc_year = integer(),
  pl_bmassj = numeric()
)

.schema_planets <- tibble(
  pl_name = character(), hostname = character(), disc_year = integer(),
  pl_orbper = numeric(), pl_rade = numeric(), pl_bmassj = numeric(),
  pl_eqt = numeric(), disc_facility = character(),
  discoverymethod = character()
)

# == Public functions ==========================================================

#' Run an ADQL query against the Exoplanet Archive TAP service
#'
#' Executes an arbitrary ADQL (Astronomical Data Query Language) query against
#' the NASA Exoplanet Archive TAP service. ADQL is similar to SQL and allows
#' querying the full catalog of confirmed planets, stellar properties, and
#' transit parameters.
#'
#' @param sql ADQL query string. For example:
#'   \code{"select pl_name, hostname from ps where disc_year = 2024"}.
#'   Available tables include \code{ps} (planetary systems), \code{pscomppars}
#'   (composite parameters), and \code{stellarhosts}.
#' @param format Output format: "json" (default, returns tibble), "csv"
#'   (returns tibble from CSV), or "votable" (saves file, returns path).
#' @return A tibble of query results. Column names and types depend on the
#'   query. For "votable" format, returns a tibble with a single \code{file}
#'   column containing the path to the downloaded file.
#' @examples
#' exo_query("select top 5 pl_name, hostname, disc_year from ps order by disc_year desc")
#' @seealso [exo_planets()], [exo_context()]
#' @source <https://exoplanetarchive.ipac.caltech.edu/docs/TAP/usingTAP.html>
#' @export
exo_query <- function(sql, format = "json") {
  url <- sprintf("%s?query=%s&format=%s",
                 .exo_base,
                 utils::URLencode(sql, reserved = TRUE),
                 format)
  if (format == "json") {
    raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0) return(tibble())
    as_tibble(raw)
  } else {
    path <- .fetch(url, ext = paste0(".", format))
    if (format == "csv") {
      as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
    } else {
      message("File saved to: ", path)
      tibble(file = path)
    }
  }
}

#' Get confirmed exoplanets, optionally filtered by discovery year
#'
#' Returns confirmed exoplanets from the NASA Exoplanet Archive Planetary
#' Systems (ps) table. Each row is a unique planet with key physical and
#' orbital parameters. Results are ordered by discovery year descending.
#'
#' @param year Discovery year filter (optional integer, e.g. 2024). When NULL
#'   (default), returns planets from all years.
#' @param limit Maximum number of rows to return (default 100). Set to NULL
#'   for no limit (use with caution -- the full catalog has 5000+ planets).
#' @return A tibble with columns:
#'   \describe{
#'     \item{pl_name}{Planet name (e.g. "Kepler-22 b")}
#'     \item{hostname}{Host star name}
#'     \item{disc_year}{Year of discovery}
#'     \item{pl_orbper}{Orbital period in days}
#'     \item{pl_rade}{Planet radius in Earth radii}
#'     \item{pl_bmassj}{Planet mass in Jupiter masses}
#'     \item{pl_eqt}{Equilibrium temperature in Kelvin}
#'     \item{disc_facility}{Discovery facility name}
#'     \item{discoverymethod}{Discovery method (e.g. "Transit", "Radial Velocity")}
#'   }
#' @examples
#' exo_planets(year = 2024, limit = 10)
#' exo_planets(limit = 5)
#' @seealso [exo_query()], [exo_context()]
#' @source <https://exoplanetarchive.ipac.caltech.edu>
#' @export
exo_planets <- function(year = NULL, limit = 100) {
  where <- if (!is.null(year)) sprintf(" where disc_year = %d", as.integer(year)) else ""
  sql <- sprintf(
    "select distinct pl_name, hostname, disc_year, pl_orbper, pl_rade, pl_bmassj, pl_eqt, disc_facility, discoverymethod from ps%s order by disc_year desc",
    where
  )
  if (!is.null(limit)) sql <- paste0(sql, sprintf(" top %d", as.integer(limit)))

  url <- sprintf("%s?query=%s&format=json",
                 .exo_base, utils::URLencode(sql, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_planets)

  as_tibble(raw) |>
    mutate(
      pl_name         = as.character(pl_name),
      hostname        = as.character(hostname),
      disc_year       = as.integer(disc_year),
      pl_orbper       = as.numeric(pl_orbper),
      pl_rade         = as.numeric(pl_rade),
      pl_bmassj       = as.numeric(pl_bmassj),
      pl_eqt          = as.numeric(pl_eqt),
      disc_facility   = as.character(disc_facility),
      discoverymethod = as.character(discoverymethod)
    )
}

# == Context ===================================================================

#' Get exoplanetarchive-caltech-edu client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the NASA Exoplanet Archive client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' exo_context()
#' @export
exo_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(exo_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/exoplanetarchive-caltech-edu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "exoplanetarchive-caltech-edu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# exoplanetarchive-caltech-edu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# exoplanetarchive-caltech-edu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
