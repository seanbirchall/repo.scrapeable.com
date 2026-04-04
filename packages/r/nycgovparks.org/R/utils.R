#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_retry
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# nycgovparks.org
# Self-contained NYC Parks client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required. All endpoints are public.

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.nycparks_base <- "https://www.nycgovparks.org"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)

# -- Context generator ---------------------------------------------------------

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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# -- JSON fetch helper ---------------------------------------------------------

.nycparks_fetch <- function(path) {
  url <- paste0(.nycparks_base, "/", path)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

# -- Type columns helper -------------------------------------------------------

.type_cols <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      if (grepl("date|Date|Start|Completion|Updated", col, ignore.case = TRUE) &&
          length(vals) > 0 && sum(grepl("^\\d{4}-\\d{2}-\\d{2}", vals)) > length(vals) * 0.3) {
        df[[col]] <- as.Date(substr(df[[col]], 1, 10))
        next
      }
      if (col %in% c("lat", "lon", "latitude", "longitude") && length(vals) > 0 &&
          all(grepl("^-?[0-9]+\\.?[0-9]*$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
        next
      }
    }
  }
  df
}

# Known directory feeds
.nycparks_dirs <- list(
  pools_indoor    = "bigapps/DPR_Pools_indoor_001.json",
  pools_outdoor   = "bigapps/DPR_Pools_outdoor_001.json",
  dog_runs        = "bigapps/DPR_DogRuns_001.json",
  basketball      = "bigapps/DPR_Basketball_001.json",
  tennis          = "bigapps/DPR_Tennis_001.json",
  handball        = "bigapps/DPR_Handball_001.json",
  cricket         = "bigapps/DPR_Cricket_001.json",
  bocce           = "bigapps/DPR_Bocce_001.json",
  running_tracks  = "bigapps/DPR_RunningTracks_001.json",
  playgrounds     = "bigapps/DPR_Playgrounds_001.json",
  beaches         = "bigapps/DPR_Beaches_001.json",
  barbecue        = "bigapps/DPR_Barbecue_001.json",
  kayak           = "bigapps/DPR_Kayak_001.json",
  hiking          = "bigapps/DPR_Hiking_001.json",
  horseback       = "bigapps/DPR_Horseback_001.json",
  ice_skating     = "bigapps/DPR_IceSkating_001.json",
  rec_centers     = "bigapps/DPR_RecreationCenter_001.json",
  nature_centers  = "bigapps/DPR_NatureCenters_001.json",
  nature_preserves = "bigapps/DPR_NaturePreserves_001.json",
  zoos            = "bigapps/DPR_Zoos_001.json",
  historic_houses = "bigapps/DPR_HistoricHouses_001.json",
  eateries        = "bigapps/DPR_Eateries_001.json",
  concessions     = "bigapps/DPR_Concessions_001.json",
  computer_centers = "bigapps/DPR_PublicComputerResourceCenter_001.json"
)
