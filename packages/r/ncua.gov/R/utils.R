#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ncua_base <- "https://www.ncua.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".zip") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_zip_csv <- function(url, max_rows = 10000) {
  zip_path <- .fetch(url, ext = ".zip")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  utils::unzip(zip_path, exdir = tmp_dir)
  csvs <- list.files(tmp_dir, pattern = "\\.(csv|txt)$", full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  if (length(csvs) == 0) {
    warning("No CSV/TXT files found in ZIP")
    return(tibble())
  }
  target <- csvs[which.max(file.size(csvs))]
  df <- tryCatch(
    utils::read.csv(target, stringsAsFactors = FALSE, check.names = FALSE,
                    nrows = max_rows),
    error = function(e) {
      tryCatch(
        utils::read.delim(target, stringsAsFactors = FALSE, check.names = FALSE,
                          nrows = max_rows),
        error = function(e2) data.frame()
      )
    }
  )
  tibble::as_tibble(df)
}


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
