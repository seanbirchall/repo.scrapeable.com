#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# treasury-gov.R
# Self-contained US Treasury Fiscal Data API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://fiscaldata.treasury.gov/api-documentation/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.treas_base <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service"

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

# -- Fetch + type casting ------------------------------------------------------

.treas_get <- function(endpoint, params = list(), max_results = NULL) {
  # Default sort by record_date desc
  if (is.null(params[["sort"]])) params[["sort"]] <- "-record_date"

  # Pagination
  page_size <- min(max_results %||% 10000, 10000)
  params[["page[size]"]] <- page_size
  params[["page[number]"]] <- 1

  all_data <- list()
  total <- NULL

  repeat {
    query <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(.treas_base, "/", endpoint, "?", query)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    if (is.null(total)) {
      total <- raw$meta$`total-count`
      if (!is.null(total) && total > page_size)
        message(sprintf("Fetching %d records...", min(total, max_results %||% total)))
    }

    df <- as_tibble(raw$data)
    if (nrow(df) == 0) break
    all_data[[length(all_data) + 1]] <- df

    # Check if we have enough
    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break

    total_pages <- raw$meta$`total-pages`
    if (is.null(total_pages) || params[["page[number]"]] >= total_pages) break
    params[["page[number]"]] <- params[["page[number]"]] + 1
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Auto-type using meta datatypes
  types <- raw$meta$dataTypes
  if (!is.null(types)) {
    for (col in names(types)) {
      if (!col %in% names(result)) next
      t <- types[[col]]
      if (t == "DATE") result[[col]] <- as.Date(result[[col]])
      else if (t %in% c("NUMBER", "CURRENCY", "PERCENTAGE"))
        result[[col]] <- suppressWarnings(as.numeric(result[[col]]))
    }
  }
  result
}


# == Schemas ===================================================================

.schema_debt <- tibble(
  record_date = as.Date(character()), debt_held_public_amt = numeric(),
  intragov_hold_amt = numeric(), tot_pub_debt_out_amt = numeric()
)

.schema_rates <- tibble(
  record_date = as.Date(character()), security_desc = character(),
  avg_interest_rate_amt = numeric()
)

.schema_exchange <- tibble(
  record_date = as.Date(character()), country_currency_desc = character(),
  exchange_rate = numeric()
)


