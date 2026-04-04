# exoplanetarchive.ipac.caltech.edu.R - Self-contained exoplanetarchive.ipac.caltech.edu client



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

.exo_base <- "https://exoplanetarchive.ipac.caltech.edu/TAP/sync"

.exo_query <- function(query) {
  url <- sprintf("%s?query=%s&format=json", .exo_base, utils::URLencode(query, reserved = TRUE))
  tryCatch(.fetch_json(url), error = function(e) list())
}

# == Schemas ===================================================================

.schema_planets <- tibble(
  pl_name = character(), hostname = character(), disc_year = integer(),
  mass_earth = numeric(), radius_earth = numeric(),
  orbital_period = numeric(), distance_pc = numeric()
)

#' Query confirmed exoplanets
#'
#' Queries the NASA Exoplanet Archive TAP service for confirmed exoplanets
#' from the Planetary Systems (PS) table. Returns key physical and orbital
#' parameters for each planet.
#'
#' @param where Character or NULL. SQL WHERE clause to filter results (appended
#'   with AND to \code{default_flag=1}). Examples: \code{"disc_year>2020"},
#'   \code{"pl_rade<2"}, \code{"hostname='Kepler-442'"},
#'   \code{"discoverymethod='Transit'"}.
#' @param top Integer. Maximum number of results to return (default 50).
#' @return A tibble with columns:
#'   \describe{
#'     \item{pl_name}{Character. Planet name (e.g. "Kepler-442 b").}
#'     \item{hostname}{Character. Host star name (e.g. "Kepler-442").}
#'     \item{disc_year}{Integer. Year of discovery.}
#'     \item{mass_earth}{Numeric. Planet mass in Earth masses.}
#'     \item{radius_earth}{Numeric. Planet radius in Earth radii.}
#'     \item{orbital_period}{Numeric. Orbital period in days.}
#'     \item{distance_pc}{Numeric. Distance to system in parsecs.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' exo_planets(top = 10)
#' exo_planets(where = "disc_year>2020", top = 100)
#' exo_planets(where = "pl_rade<1.5 AND pl_orbper>100")
#' }
exo_planets <- function(where = NULL, top = 50L) {
  q <- sprintf("SELECT TOP %d pl_name,hostname,disc_year,pl_bmasse,pl_rade,pl_orbper,sy_dist FROM ps WHERE default_flag=1", as.integer(top))
  if (!is.null(where)) q <- paste(q, "AND", where)
  raw <- .exo_query(q)
  if (length(raw) == 0) return(.schema_planets)
  tibble(
    pl_name = vapply(raw, function(x) x$pl_name %||% NA_character_, character(1)),
    hostname = vapply(raw, function(x) x$hostname %||% NA_character_, character(1)),
    disc_year = vapply(raw, function(x) as.integer(x$disc_year %||% NA_integer_), integer(1)),
    mass_earth = vapply(raw, function(x) as.numeric(x$pl_bmasse %||% NA_real_), numeric(1)),
    radius_earth = vapply(raw, function(x) as.numeric(x$pl_rade %||% NA_real_), numeric(1)),
    orbital_period = vapply(raw, function(x) as.numeric(x$pl_orbper %||% NA_real_), numeric(1)),
    distance_pc = vapply(raw, function(x) as.numeric(x$sy_dist %||% NA_real_), numeric(1))
  )
}

#' Get host star properties for exoplanet systems
#'
#' Queries distinct host star properties from the NASA Exoplanet Archive.
#' Useful for studying the stellar environments of exoplanetary systems.
#'
#' @param where Character or NULL. SQL WHERE clause to filter results.
#'   Examples: \code{"st_spectype LIKE 'G%'"} (Sun-like stars),
#'   \code{"st_teff BETWEEN 5000 AND 6000"}, \code{"sy_dist<50"} (nearby).
#' @param top Integer. Maximum number of results to return (default 50).
#' @return A tibble with columns:
#'   \describe{
#'     \item{hostname}{Character. Host star name.}
#'     \item{st_spectype}{Character. Spectral type (e.g. "G2 V", "K0").}
#'     \item{st_teff}{Numeric. Effective temperature in Kelvin.}
#'     \item{st_rad}{Numeric. Stellar radius in solar radii.}
#'     \item{st_mass}{Numeric. Stellar mass in solar masses.}
#'     \item{st_met}{Numeric. Stellar metallicity [Fe/H] in dex.}
#'     \item{sy_dist}{Numeric. Distance in parsecs.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' exo_stars(top = 20)
#' exo_stars(where = "st_spectype LIKE 'G%'", top = 50)
#' }
exo_stars <- function(where = NULL, top = 50L) {
  schema <- tibble(hostname = character(), st_spectype = character(),
                   st_teff = numeric(), st_rad = numeric(), st_mass = numeric(),
                   st_met = numeric(), sy_dist = numeric())
  q <- sprintf("SELECT DISTINCT TOP %d hostname,st_spectype,st_teff,st_rad,st_mass,st_met,sy_dist FROM ps WHERE default_flag=1", as.integer(top))
  if (!is.null(where)) q <- paste(q, "AND", where)
  raw <- .exo_query(q)
  if (length(raw) == 0) return(schema)
  tibble(
    hostname = vapply(raw, function(x) x$hostname %||% NA_character_, character(1)),
    st_spectype = vapply(raw, function(x) x$st_spectype %||% NA_character_, character(1)),
    st_teff = vapply(raw, function(x) as.numeric(x$st_teff %||% NA_real_), numeric(1)),
    st_rad = vapply(raw, function(x) as.numeric(x$st_rad %||% NA_real_), numeric(1)),
    st_mass = vapply(raw, function(x) as.numeric(x$st_mass %||% NA_real_), numeric(1)),
    st_met = vapply(raw, function(x) as.numeric(x$st_met %||% NA_real_), numeric(1)),
    sy_dist = vapply(raw, function(x) as.numeric(x$sy_dist %||% NA_real_), numeric(1))
  )
}

#' Find potentially habitable exoplanets
#'
#' Returns Earth-like exoplanets with radius 0.5--2.0 Earth radii and orbital
#' period > 10 days, sorted by radius ascending. These criteria select rocky
#' planets in or near the habitable zone. Includes host star temperature for
#' assessing habitability.
#'
#' @param top Integer. Maximum number of results to return (default 50).
#' @return A tibble with columns:
#'   \describe{
#'     \item{pl_name}{Character. Planet name.}
#'     \item{hostname}{Character. Host star name.}
#'     \item{disc_year}{Integer. Year of discovery.}
#'     \item{mass_earth}{Numeric. Planet mass in Earth masses.}
#'     \item{radius_earth}{Numeric. Planet radius in Earth radii (0.5--2.0).}
#'     \item{orbital_period}{Numeric. Orbital period in days (>10).}
#'     \item{st_teff}{Numeric. Host star effective temperature in Kelvin.}
#'     \item{distance_pc}{Numeric. Distance to system in parsecs.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' exo_habitable(top = 20)
#' }
exo_habitable <- function(top = 50L) {
  schema <- tibble(pl_name = character(), hostname = character(),
                   disc_year = integer(), mass_earth = numeric(),
                   radius_earth = numeric(), orbital_period = numeric(),
                   st_teff = numeric(), distance_pc = numeric())
  q <- sprintf(
    "SELECT TOP %d pl_name,hostname,disc_year,pl_bmasse,pl_rade,pl_orbper,st_teff,sy_dist FROM ps WHERE default_flag=1 AND pl_rade>=0.5 AND pl_rade<=2.0 AND pl_orbper>10 ORDER BY pl_rade ASC",
    as.integer(top)
  )
  raw <- .exo_query(q)
  if (length(raw) == 0) return(schema)
  tibble(
    pl_name = vapply(raw, function(x) x$pl_name %||% NA_character_, character(1)),
    hostname = vapply(raw, function(x) x$hostname %||% NA_character_, character(1)),
    disc_year = vapply(raw, function(x) as.integer(x$disc_year %||% NA_integer_), integer(1)),
    mass_earth = vapply(raw, function(x) as.numeric(x$pl_bmasse %||% NA_real_), numeric(1)),
    radius_earth = vapply(raw, function(x) as.numeric(x$pl_rade %||% NA_real_), numeric(1)),
    orbital_period = vapply(raw, function(x) as.numeric(x$pl_orbper %||% NA_real_), numeric(1)),
    st_teff = vapply(raw, function(x) as.numeric(x$st_teff %||% NA_real_), numeric(1)),
    distance_pc = vapply(raw, function(x) as.numeric(x$sy_dist %||% NA_real_), numeric(1))
  )
}

#' Get discovery statistics by year and method
#'
#' Returns exoplanet discovery counts grouped by year and detection method.
#' Useful for tracking how exoplanet detection has evolved over time (e.g.,
#' the rise of transit surveys like Kepler and TESS).
#'
#' @param top Integer. Maximum number of year/method rows to return (default 30).
#'   Note: this limits total rows, not years. Use larger values for complete
#'   coverage.
#' @return A tibble with columns:
#'   \describe{
#'     \item{disc_year}{Integer. Year of discovery.}
#'     \item{discoverymethod}{Character. Detection method (e.g. "Transit",
#'       "Radial Velocity", "Imaging", "Microlensing").}
#'     \item{count}{Integer. Number of confirmed planets discovered.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' exo_discoveries(top = 100)
#' }
exo_discoveries <- function(top = 30L) {
  schema <- tibble(disc_year = integer(), discoverymethod = character(), count = integer())
  q <- sprintf(
    "SELECT TOP %d disc_year,discoverymethod,count(*) as cnt FROM ps WHERE default_flag=1 GROUP BY disc_year,discoverymethod ORDER BY disc_year DESC",
    as.integer(top)
  )
  raw <- .exo_query(q)
  if (length(raw) == 0) return(schema)
  tibble(
    disc_year = vapply(raw, function(x) as.integer(x$disc_year %||% NA_integer_), integer(1)),
    discoverymethod = vapply(raw, function(x) x$discoverymethod %||% NA_character_, character(1)),
    count = vapply(raw, function(x) as.integer(x$cnt %||% NA_integer_), integer(1))
  )
}

# == Context ===================================================================

#' Get exoplanetarchive.ipac.caltech.edu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
exoplanetarchive_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(exoplanetarchive_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/exoplanetarchive.ipac.caltech.edu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "exoplanetarchive.ipac.caltech.edu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# exoplanetarchive.ipac.caltech.edu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# exoplanetarchive.ipac.caltech.edu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
