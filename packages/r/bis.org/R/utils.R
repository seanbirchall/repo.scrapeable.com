#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom xml2 read_xml xml_ns xml_find_all xml_find_first xml_attr xml_text
#' @keywords internal
NULL

# bis-org.R
# Self-contained Bank for International Settlements (BIS) data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tibble
# Auth: none required (public SDMX REST API)
# Rate limits: undocumented, be polite
# Docs: https://data.bis.org/topics


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.bis_base <- "https://stats.bis.org/api/v1"

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

# -- SDMX CSV fetch engine ----------------------------------------------------

.bis_fetch <- function(dataflow, key, params = list()) {
  params$format <- "csv"
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/data/%s/%s?%s", .bis_base, dataflow, key, query)

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  df <- read.csv(tmp, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(tibble())

  as_tibble(df) |>
    mutate(
      TIME_PERIOD = {
        tp <- TIME_PERIOD
        tp <- gsub("-Q1$", "-01-01", tp)
        tp <- gsub("-Q2$", "-04-01", tp)
        tp <- gsub("-Q3$", "-07-01", tp)
        tp <- gsub("-Q4$", "-10-01", tp)
        tp <- ifelse(grepl("^\\d{4}$", tp), paste0(tp, "-01-01"), tp)
        tp <- ifelse(grepl("^\\d{4}-\\d{2}$", tp), paste0(tp, "-01"), tp)
        as.Date(tp)
      },
      OBS_VALUE = as.numeric(OBS_VALUE)
    )
}


# == Schemas ===================================================================

.schema_bis <- tibble(
  date = as.Date(character()), value = numeric(),
  ref_area = character(), title = character()
)


