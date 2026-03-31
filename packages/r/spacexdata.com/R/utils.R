#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# spacexdata-com.R
# Self-contained SpaceX launch and rocket data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.spacex_base <- "https://api.spacexdata.com/v4"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_launches <- tibble(
  id = character(), name = character(), flight_number = integer(),
  date_utc = as.POSIXct(character()), success = logical(),
  details = character(), rocket = character(),
  launchpad = character(), upcoming = logical()
)

.schema_rockets <- tibble(
  id = character(), name = character(), type = character(),
  active = logical(), stages = integer(), boosters = integer(),
  cost_per_launch = numeric(), first_flight = as.Date(character()),
  country = character(), company = character(), description = character()
)

# -- Parse helpers -------------------------------------------------------------

.parse_launches <- function(raw) {
  if (length(raw) == 0) return(.schema_launches)

  rows <- lapply(raw, function(l) {
    tibble(
      id = as.character(l$id %||% NA),
      name = as.character(l$name %||% NA),
      flight_number = as.integer(l$flight_number %||% NA),
      date_utc = as.POSIXct(l$date_utc %||% NA, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      success = as.logical(l$success %||% NA),
      details = as.character(l$details %||% NA),
      rocket = as.character(l$rocket %||% NA),
      launchpad = as.character(l$launchpad %||% NA),
      upcoming = as.logical(l$upcoming %||% NA)
    )
  })
  bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

