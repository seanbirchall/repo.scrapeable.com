#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# norges-bank-no.R
# Self-contained Norges Bank (Norwegian central bank) SDMX API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous
# Format: SDMX-CSV


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nb_base <- "https://data.norges-bank.no/api/data"

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

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url) {
  f <- .fetch(url, ext = ".csv")
  lines <- readLines(f, warn = FALSE)
  # SDMX-CSV headers include descriptions like "FREQ:Frequency" — strip to bare key
  if (length(lines) > 0) {
    header <- strsplit(lines[1], ",")[[1]]
    header <- sub(":.*", "", header)
    lines[1] <- paste(header, collapse = ",")
  }
  tc <- textConnection(lines)
  on.exit(close(tc))
  utils::read.csv(tc, stringsAsFactors = FALSE)
}

# == Schemas ===================================================================

.schema_exchange_rates <- tibble(
  date = as.Date(character()), currency = character(),
  value = numeric(), frequency = character(),
  unit_mult = character(), decimals = integer()
)
