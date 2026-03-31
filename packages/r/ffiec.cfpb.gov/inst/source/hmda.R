# ffiec-cfpb-gov.R
# Self-contained HMDA (Home Mortgage Disclosure Act) data browser client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be courteous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hmda_base <- "https://ffiec.cfpb.gov/v2/data-browser-api"

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

# == Schemas ===================================================================

.schema_aggregations <- tibble(
  count = integer(), sum = numeric(), actions_taken = character(),
  loan_types = character(), purposes = character(),
  races = character(), ethnicities = character()
)

.schema_filers <- tibble(
  lei = character(), name = character(), period = integer()
)

# == Public functions ==========================================================

#' Fetch HMDA nationwide aggregations
#'
#' Returns aggregated HMDA data from the CFPB Data Browser API.
#' Filters may be combined. All filter values should be character strings.
#'
#' @param years Integer year (e.g. 2022). Required.
#' @param actions_taken Action taken code: "1" (originated), "2" (approved),
#'   "3" (denied), "4" (withdrawn), "5" (incomplete), "6" (purchased),
#'   "7" (preapproval denied), "8" (preapproval approved)
#' @param loan_types Loan type: "1" (conventional), "2" (FHA), "3" (VA), "4" (USDA)
#' @param purposes Purpose: "1" (purchase), "2" (improvement), "31" (refinance),
#'   "32" (cash-out refi), "4" (other), "5" (not applicable)
#' @param races Race code: "1" (Native), "2" (Asian), "3" (Black),
#'   "4" (Pacific), "5" (White), "6" (not available), "7" (not applicable)
#' @param ethnicities Ethnicity: "1" (Hispanic), "2" (Not Hispanic),
#'   "3" (not available), "4" (not applicable)
#' @return tibble: count (integer), sum (numeric), plus filter columns
hmda_aggregations <- function(years, actions_taken = NULL, loan_types = NULL,
                              purposes = NULL, races = NULL,
                              ethnicities = NULL) {
  params <- list(years = years, actions_taken = actions_taken,
                 loan_types = loan_types, purposes = purposes,
                 races = races, ethnicities = ethnicities)
  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/view/nationwide/aggregations?%s", .hmda_base, query)

  raw <- .fetch_json(url)
  aggs <- raw$aggregations
  if (is.null(aggs) || length(aggs) == 0) return(.schema_aggregations[0, names(aggs)])

  result <- as_tibble(aggs)
  if ("count" %in% names(result)) result$count <- as.integer(result$count)
  if ("sum" %in% names(result)) result$sum <- as.numeric(result$sum)
  result
}

#' Fetch HMDA filer institutions for a given year
#'
#' Returns a list of institutions that filed HMDA data.
#'
#' @param years Integer year (e.g. 2022)
#' @return tibble: lei (character), name (character), period (integer)
hmda_filers <- function(years) {
  url <- sprintf("%s/view/filers?years=%s", .hmda_base, years)
  raw <- .fetch_json(url)
  filers <- raw$institutions
  if (is.null(filers) || length(filers) == 0) return(.schema_filers)

  as_tibble(filers) |>
    transmute(
      lei = as.character(lei),
      name = as.character(name),
      period = as.integer(period)
    )
}

#' HMDA Data Browser context for LLM use
#'
#' Prints package overview, auth info, and function signatures.
#' @return Invisible string with context info
hmda_context <- function() {
  header <- c(
    "# ffiec.cfpb.gov - HMDA Data Browser API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: not documented",
    "#",
    "# Common action_taken codes: 1=originated, 3=denied, 5=incomplete",
    "# Common loan_types: 1=conventional, 2=FHA, 3=VA, 4=USDA",
    "# Available years: 2018-2023"
  )
  .build_context("ffiec.cfpb.gov", header_lines = header)
}
