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
