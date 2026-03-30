#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_retry
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# fec-gov.R
# Self-contained Federal Election Commission API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. DEMO_KEY works for testing (low rate limit).
#   Get a key at https://api.data.gov/signup/
# Rate limits: DEMO_KEY = 30 req/hour. Real key = 1000 req/hour.
# Docs: https://api.open.fec.gov/developers/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.fec_base <- "https://api.open.fec.gov/v1"

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

# -- Fetch helpers -------------------------------------------------------------

.fec_get <- function(endpoint, params = list(), api_key = "DEMO_KEY",
                     max_results = NULL) {
  params$api_key <- api_key
  params$per_page <- min(max_results %||% 100, 100)
  params$page <- 1

  all_results <- list()
  repeat {
    query <- paste(names(params),
                   vapply(params, function(v) utils::URLencode(as.character(v), reserved = FALSE),
                          character(1)),
                   sep = "=", collapse = "&")
    url <- paste0(.fec_base, "/", endpoint, "?", query)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_retry(max_tries = 2, backoff = function(...) 10) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    results <- raw$results
    if (is.null(results) || length(results) == 0) break
    df <- as_tibble(results)
    all_results[[length(all_results) + 1]] <- df

    n_so_far <- sum(vapply(all_results, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break

    pages <- raw$pagination$pages %||% 1
    if (params$page >= pages) break
    params$page <- params$page + 1
  }

  if (length(all_results) == 0) return(tibble())
  result <- bind_rows(all_results)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Auto-type date columns (defensive — only pure YYYY-MM-DD strings)
  for (col in names(result)) {
    if (grepl("date", col) && is.character(result[[col]])) {
      vals <- result[[col]]
      is_datelike <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals) & !is.na(vals)
      if (sum(is_datelike) > length(vals) * 0.5) {
        result[[col]] <- suppressWarnings(as.Date(substr(vals, 1, 10)))
      }
    }
  }
  result
}


# == Schemas ===================================================================

.schema_candidates <- tibble(
  candidate_id = character(), name = character(), party = character(),
  party_full = character(), state = character(), office = character(),
  office_full = character(), district = character(),
  incumbent_challenge = character(), election_years = list()
)

.schema_committees <- tibble(
  committee_id = character(), name = character(), designation = character(),
  committee_type = character(), party = character(), state = character(),
  treasurer_name = character()
)

.schema_totals <- tibble(
  candidate_id = character(), name = character(), party = character(),
  office = character(), state = character(), election_year = integer(),
  receipts = numeric(), disbursements = numeric(),
  cash_on_hand_end_period = numeric()
)


