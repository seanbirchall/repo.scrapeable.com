#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode read.csv
#' @keywords internal
NULL

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.exim_csv_url <- "https://img.exim.gov/s3fs-public/dataset/vbhv-d8am/Data.Gov+-+FY25+Q4.csv"

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.exim_cache <- new.env(parent = emptyenv())

.exim_load <- function(force = FALSE) {
  if (!force && exists("data", envir = .exim_cache)) {
    return(get("data", envir = .exim_cache))
  }
  tmp <- .fetch_csv(.exim_csv_url)
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  df <- tibble::as_tibble(raw)
  names(df) <- trimws(names(df))

  df <- df |>
    mutate(
      `Fiscal Year` = as.integer(`Fiscal Year`),
      `Decision Date` = suppressWarnings(as.Date(`Decision Date`, format = "%m/%d/%Y")),
      `Effective Date` = suppressWarnings(as.Date(`Effective Date`, format = "%m/%d/%Y")),
      `Expiration Date` = suppressWarnings(as.Date(`Expiration Date`, format = "%m/%d/%Y")),
      `Approved/Declined Amount` = suppressWarnings(as.numeric(gsub(",", "", `Approved/Declined Amount`))),
      `Disbursed/Shipped Amount` = suppressWarnings(as.numeric(gsub(",", "", `Disbursed/Shipped Amount`))),
      `Undisbursed Exposure Amount` = suppressWarnings(as.numeric(gsub(",", "", `Undisbursed Exposure Amount`))),
      `Outstanding Exposure Amount` = suppressWarnings(as.numeric(gsub(",", "", `Outstanding Exposure Amount`))),
      `Small Business Authorized Amount` = suppressWarnings(as.numeric(gsub(",", "", `Small Business Authorized Amount`))),
      `Woman Owned Authorized Amount` = suppressWarnings(as.numeric(gsub(",", "", `Woman Owned Authorized Amount`))),
      `Minority Owned Authorized Amount` = suppressWarnings(as.numeric(gsub(",", "", `Minority Owned Authorized Amount`)))
    )

  assign("data", df, envir = .exim_cache)
  df
}

`%||%` <- function(x, y) if (is.null(x)) y else x

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
