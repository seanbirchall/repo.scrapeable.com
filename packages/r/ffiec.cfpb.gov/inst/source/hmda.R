


# ffiec-cfpb-gov.R
# Self-contained HMDA (Home Mortgage Disclosure Act) data browser client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hmda_base <- "https://ffiec.cfpb.gov/v2/data-browser-api"
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
#' @export
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
#' @export
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

# == Context ===================================================================

#' Generate LLM-friendly context for ffiec.cfpb.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
hmda_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/ffiec.cfpb.gov.R"
  if (!file.exists(src_file)) {
    cat("# ffiec.cfpb.gov context - source not found\n")
    return(invisible("# ffiec.cfpb.gov context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

