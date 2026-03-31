# data-giss-nasa-gov.R
# Self-contained NASA GISS Surface Temperature (GISTEMP) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.giss_base <- "https://data.giss.nasa.gov/gistemp/tabledata_v4"

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

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url, ext = ".json"))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_global_temp <- tibble(
  year = integer(), jan = numeric(), feb = numeric(), mar = numeric(),
  apr = numeric(), may = numeric(), jun = numeric(), jul = numeric(),
  aug = numeric(), sep = numeric(), oct = numeric(), nov = numeric(),
  dec = numeric(), j_d = numeric(), d_n = numeric(), djf = numeric(),
  mam = numeric(), jja = numeric(), son = numeric()
)

# == Public functions ==========================================================

#' Download and parse GISTEMP global temperature anomalies
#'
#' Fetches the GLB.Ts+dSST.csv file from NASA GISS, which contains monthly
#' global mean surface temperature anomalies (degrees C) relative to the
#' 1951-1980 base period.
#'
#' @param dataset One of "global" (land+ocean), "land", "southern", "northern".
#'   Default "global".
#' @return tibble with year and monthly/seasonal temperature anomaly columns
#' @export
giss_global_temp <- function(dataset = "global") {
  file_map <- list(
    global   = "GLB.Ts+dSST.csv",
    land     = "GLB.Ts.csv",
    northern = "NH.Ts+dSST.csv",
    southern = "SH.Ts+dSST.csv"
  )
  fname <- file_map[[dataset]]
  if (is.null(fname)) stop("dataset must be one of: global, land, northern, southern")

  url <- paste0(.giss_base, "/", fname)
  tmp <- .fetch(url, ext = ".csv")
  raw_lines <- readLines(tmp, warn = FALSE)

  # Find the header row (starts with "Year")
  header_idx <- grep("^Year", raw_lines)[1]
  if (is.na(header_idx)) stop("Could not find header row in CSV")

  # Read from header row, replacing *** with NA
  data_lines <- raw_lines[header_idx:length(raw_lines)]
  data_lines <- gsub("\\*\\*\\*", "NA", data_lines)

  # Remove any trailing non-data lines (like "Year" appearing again)

  con <- textConnection(data_lines)
  df <- tryCatch(
    read.csv(con, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  close(con)

  if (is.null(df) || nrow(df) == 0) return(.schema_global_temp)

  # Clean column names
  names(df) <- tolower(names(df))
  names(df) <- gsub("-", "_", names(df))

  # Remove non-numeric year rows

  df <- df[grepl("^\\d{4}$", trimws(df$year)), , drop = FALSE]

  df <- as_tibble(df) |>
    mutate(
      year = as.integer(year),
      across(-year, ~ suppressWarnings(as.numeric(.x)))
    )

  df
}

#' Print GISTEMP context for LLM integration
#'
#' Outputs package metadata, available functions with full roxygen documentation
#' and function signatures. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
giss_context <- function() {
  .build_context(
    pkg_name = "data.giss.nasa.gov",
    header_lines = c(
      "# Package: data.giss.nasa.gov",
      "# NASA GISS Surface Temperature Analysis (GISTEMP)",
      "# Auth: none",
      "# Rate limits: none",
      "#",
      "# Datasets: global (land+ocean), land, northern, southern",
      "# Values are temperature anomalies in degrees C relative to 1951-1980 base",
      "# Columns: year, jan-dec monthly, j_d (Jan-Dec annual), d_n (Dec-Nov annual),",
      "#   djf/mam/jja/son (seasonal averages)"
    )
  )
}
