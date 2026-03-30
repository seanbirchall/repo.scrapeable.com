#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_body_json
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# usaspending-gov.R
# Self-contained USASpending.gov API v2 client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Rate limits: undocumented, be reasonable
# Docs: https://api.usaspending.gov/docs/endpoints


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.usas_base <- "https://api.usaspending.gov/api/v2"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# POST with JSON body — many USASpending endpoints are POST
.post_json <- function(url, body) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

# -- Pagination for POST endpoints --------------------------------------------

.usas_fetch_all <- function(url, body, results_key = "results",
                            max_pages = 50, page_size = 100) {
  body$limit <- page_size
  body$page <- 1
  all_results <- list()

  for (i in seq_len(max_pages)) {
    raw <- .post_json(url, body)
    results <- raw[[results_key]]
    if (is.null(results) || length(results) == 0) break

    df <- if (is.data.frame(results)) results else as_tibble(results)
    all_results[[i]] <- df

    # Check if more pages
    has_next <- raw$page_metadata$hasNext %||% FALSE
    if (!has_next) break
    body$page <- body$page + 1

    if (i %% 10 == 0) message(sprintf("  ...fetched %d pages", i))
  }

  if (length(all_results) == 0) return(tibble())
  bind_rows(all_results)
}


# == Schemas ===================================================================

.schema_agencies <- tibble(
  agency_id = integer(), toptier_code = character(), agency_name = character(),
  active_fy = character(), active_fq = character(),
  budget_authority = numeric(), obligated_amount = numeric(),
  outlay_amount = numeric()
)

.schema_awards <- tibble(
  award_id = character(), generated_internal_id = character(),
  type = character(), type_description = character(),
  description = character(), piid = character(), fain = character(),
  uri = character(), total_obligation = numeric(),
  date_signed = as.Date(character()), start_date = as.Date(character()),
  end_date = as.Date(character()),
  awarding_agency_name = character(), funding_agency_name = character(),
  recipient_name = character(), recipient_uei = character(),
  place_of_performance_state = character()
)

.schema_spending_time <- tibble(
  time_period = character(), aggregated_amount = numeric()
)

.schema_spending_geo <- tibble(
  shape_code = character(), display_name = character(),
  aggregated_amount = numeric(), population = integer(),
  per_capita = numeric()
)

.schema_recipients <- tibble(
  name = character(), uei = character(), duns = character(),
  recipient_id = character(), recipient_level = character(),
  amount = numeric()
)

.schema_states <- tibble(
  fips = character(), code = character(), name = character(),
  type = character(), amount = numeric(), count = integer()
)


