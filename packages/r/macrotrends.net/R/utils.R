#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom xml2 read_html xml_find_all xml_text
#' @keywords internal
NULL

# macrotrends-net.R
# Self-contained Macrotrends financial data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tibble
# Auth: none required (HTML scraping)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: fundamental financial data for US public companies


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.mt_base <- "https://www.macrotrends.net/stocks/charts"

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

# -- Scraping engine -----------------------------------------------------------

.mt_fetch <- function(ticker, slug, metric) {
  # Macrotrends URL: /stocks/charts/{TICKER}/{slug}/{metric}
  url <- sprintf("%s/%s/%s/%s", .mt_base, toupper(ticker), tolower(slug), metric)

  tmp <- tempfile(fileext = ".html")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  doc <- xml2::read_html(tmp)
  tables <- xml2::xml_find_all(doc, ".//table")
  if (length(tables) == 0) return(list(annual = tibble(), quarterly = tibble()))

  parse_table <- function(tbl) {
    rows <- xml2::xml_find_all(tbl, ".//tr")
    if (length(rows) < 2) return(tibble())

    cells <- lapply(rows, function(r) {
      xml2::xml_text(xml2::xml_find_all(r, ".//td|.//th"), trim = TRUE)
    })

    # Keep only rows with 2+ cells (skip title/header rows with 1 cell)
    data <- Filter(function(r) length(r) >= 2, cells)
    if (length(data) == 0) return(tibble())

    mat <- do.call(rbind, lapply(data, function(r) r[1:2]))

    df <- tibble(
      date_raw = mat[, 1],
      value    = mat[, 2]
    )
    # Clean: remove $ signs, commas, % signs
    df$value <- gsub("[$,%]", "", df$value)
    df$value <- as.numeric(df$value)
    # Parse dates: try YYYY-MM-DD first, then year-only
    df$date <- tryCatch(as.Date(df$date_raw), error = function(e) rep(as.Date(NA), nrow(df)))
    year_only <- is.na(df$date) & grepl("^\\d{4}$", df$date_raw)
    if (any(year_only))
      df$date[year_only] <- as.Date(paste0(df$date_raw[year_only], "-12-31"))
    df <- df[!is.na(df$date), ]
    df |> select(date, value)
  }

  annual <- if (length(tables) >= 1) parse_table(tables[[1]]) else tibble()
  quarterly <- if (length(tables) >= 2) parse_table(tables[[2]]) else tibble()

  list(annual = annual, quarterly = quarterly)
}


# == Schemas ===================================================================

.schema_metric <- tibble(
  date = as.Date(character()), value = numeric(),
  ticker = character(), metric = character(), period = character()
)


