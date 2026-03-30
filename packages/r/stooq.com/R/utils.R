#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @keywords internal
NULL

# stooq-com.R
# Self-contained Stooq historical price data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required (public CSV downloads)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: US/global stocks, indices, commodities, forex, bonds, crypto


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.stooq_base <- "https://stooq.com/q/d/l/"

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

# -- CSV fetch engine ----------------------------------------------------------

.stooq_fetch <- function(symbol, start = NULL, end = NULL, interval = "d") {
  params <- list(s = tolower(symbol), i = interval)
  if (!is.null(start)) params$d1 <- format(as.Date(start), "%Y%m%d")
  if (!is.null(end))   params$d2 <- format(as.Date(end), "%Y%m%d")

  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.stooq_base, "?", query)

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  # Check if file has content
  lines <- readLines(tmp, n = 2, warn = FALSE)
  if (length(lines) < 2 || lines[1] == "No data") return(.schema_history)

  df <- read.csv(tmp, stringsAsFactors = FALSE)
  if (nrow(df) == 0 || !("Date" %in% names(df))) return(.schema_history)

  result <- as_tibble(df) |>
    transmute(
      date   = as.Date(Date),
      open   = as.numeric(Open),
      high   = as.numeric(High),
      low    = as.numeric(Low),
      close  = as.numeric(Close),
      volume = if ("Volume" %in% names(df)) as.numeric(Volume) else NA_real_,
      ticker = symbol
    ) |>
    arrange(date)
  result
}


# == Schemas ===================================================================

.schema_history <- tibble(
  date = as.Date(character()), open = numeric(), high = numeric(),
  low = numeric(), close = numeric(), volume = numeric(),
  ticker = character()
)


