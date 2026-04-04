# nyctmc.org.R
# Self-contained NYC Traffic Management Center client.
# Returns real-time EZ-Pass traffic speed data from Midtown in Motion sensors.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Data refreshes every 5 minutes

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nyctmc_base <- "https://linkdata.nyctmc.org/data"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_speeds <- tibble(

  sid = character(),
  link_name = character(),
  borough = character(),
  link_length_ft = numeric(),
  aggregation_period_sec = integer(),
  n_samples = integer(),
  timestamp = as.POSIXct(character()),
  median_tt_sec = numeric(),
  median_speed_fps = numeric(),
  median_speed_mph = numeric()
)

# == Public functions ==========================================================

#' Get real-time NYC traffic speeds from EZ-Pass sensors
#'
#' Returns the latest Midtown in Motion traffic speed readings across
#' all sensor links in Manhattan and other boroughs.
#'
#' @return tibble: sid, link_name, borough, link_length_ft,
#'   aggregation_period_sec, n_samples, timestamp, median_tt_sec,
#'   median_speed_fps, median_speed_mph
nyctmc_speeds <- function() {
  url <- sprintf("%s/mim_data_opendata.csv", .nyctmc_base)
  tmp <- .fetch(url, ext = ".csv")
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_speeds)

  as_tibble(raw) |>
    transmute(
      sid = as.character(sid),
      link_name = as.character(link_name),
      borough = as.character(borough),
      link_length_ft = as.numeric(link_length_ft),
      aggregation_period_sec = as.integer(aggregation_period_sec),
      n_samples = as.integer(n_samples),
      timestamp = as.POSIXct(median_calculation_timestamp,
                             format = "%m/%d/%Y %H:%M:%S"),
      median_tt_sec = as.numeric(median_tt_sec),
      median_speed_fps = as.numeric(median_speed_fps),
      median_speed_mph = round(as.numeric(median_speed_fps) * 0.681818, 2)
    )
}

#' List unique traffic sensor links
#'
#' Returns distinct sensor link segments with their names, boroughs,
#' and lengths.
#'
#' @return tibble: sid, link_name, borough, link_length_ft
nyctmc_links <- function() {
  nyctmc_speeds() |>
    distinct(sid, link_name, borough, link_length_ft) |>
    arrange(borough, link_name)
}

#' Get traffic speeds filtered by borough
#'
#' @param borough Borough name (e.g., "Manhattan", "Brooklyn")
#' @return tibble: same columns as nyctmc_speeds()
nyctmc_borough <- function(borough) {
  pat <- borough[[1]]
  nyctmc_speeds() |>
    filter(grepl(pat, .data$borough, ignore.case = TRUE))
}

#' Search traffic links by name
#'
#' @param query Search string matched against link names (case-insensitive)
#' @return tibble: same columns as nyctmc_speeds()
nyctmc_search <- function(query) {
  nyctmc_speeds() |>
    filter(grepl(query, .data$link_name, ignore.case = TRUE))
}

#' Show context for the nyctmc.org client
#'
#' Reads its own source and returns function signatures and documentation.
#'
#' @return Invisible string of context
nyctmc_context <- function() {
  src_dir <- system.file("source", package = "nyctmc.org")
  if (src_dir == "") {
    this_file <- sys.frame(1)$ofile %||%
      attr(body(nyctmc_speeds), "srcfile")$filename %||%
      ""
    if (this_file != "" && file.exists(this_file)) {
      lines <- readLines(this_file, warn = FALSE)
    } else {
      cat("# nyctmc.org - NYC Traffic Management Center client\n")
      cat("# Source not found. Use ?nyctmc_speeds for help.\n")
      return(invisible(""))
    }
  } else {
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) {
      cat("# No R source found.\n")
      return(invisible(""))
    }
    lines <- readLines(src_files[1], warn = FALSE)
  }

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
    sig <- lines[fi]
    k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) {
      k <- k + 1
      sig <- paste(sig, trimws(lines[k]))
    }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c(
    "# nyctmc.org - NYC Traffic Management Center",
    "# Real-time EZ-Pass traffic speed data",
    "#",
    "# == Functions ==",
    "#",
    unlist(blocks)
  ), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
