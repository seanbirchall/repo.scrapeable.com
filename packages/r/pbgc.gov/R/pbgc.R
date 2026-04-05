# pbgc.gov.R - Self-contained PBGC (Pension Benefit Guaranty Corporation) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required


`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.pbgc_base <- "https://www.pbgc.gov"

.fetch_excel <- function(url, ext = ".xlsx") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_trusteed <- tibble(
  case_number = character(), sponsor_name = character(), plan_name = character(),
  ein = character(), plan_number = numeric(), city = character(), state = character(),
  termination_date = as.Date(character()), trusteeship_date = as.Date(character()),
  participants = numeric()
)

.schema_multiemployer <- tibble(
  plan_name = character(), admin_city = character(), admin_state = character(),
  sponsor_name = character(), phone = character(), effective_date = character(),
  plan_id = character(), ein = character(), pn = character(),
  participant_count = numeric()
)

# == Public functions ==========================================================

#' List PBGC datasets
#'
#' Returns a catalog of available datasets from the Pension Benefit Guaranty
#' Corporation (PBGC). PBGC insures private-sector defined benefit pension
#' plans and publishes data on plans it has taken over (trusteed) and
#' multiemployer plans it insures.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{id}{Character. Dataset identifier.}
#'   \item{name}{Character. Human-readable dataset name.}
#'   \item{description}{Character. Brief description of the dataset.}
#'   \item{url}{Character. Direct download URL (XLSX format).}
#'   \item{format}{Character. File format (\code{"xlsx"}).}
#' }
#'
#' @examples
#' \dontrun{
#' pbgc_list()
#' }
#'
#' @export
pbgc_list <- function() {
  tibble(
    id = c("trusteed_plans", "multiemployer_plans"),
    name = c("Single-Employer Plans Trusteed by PBGC",
             "Multiemployer Pension Plans"),
    description = c(
      "All single-employer defined benefit pension plans trusteed by PBGC since 1974",
      "Active multiemployer pension plans insured by PBGC"
    ),
    url = c(
      paste0(.pbgc_base, "/sites/default/files/trusteedplans.xlsx"),
      paste0(.pbgc_base, "/sites/default/files/legacy/docs/open/multiemployerlist.xlsx")
    ),
    format = c("xlsx", "xlsx")
  )
}

#' Fetch PBGC trusteed single-employer plans
#'
#' Downloads and parses the complete list of single-employer defined benefit
#' pension plans that PBGC has taken over (trusteed) since 1974. Includes
#' plan sponsor information, EINs, termination dates, and participant counts.
#' Data is sourced from an Excel file on pbgc.gov.
#'
#' @param state Character. Optional two-letter state abbreviation to filter
#'   results (e.g. \code{"PA"}, \code{"NY"}, \code{"OH"}).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{case_number}{Character. PBGC case number.}
#'   \item{sponsor_name}{Character. Plan sponsor company name.}
#'   \item{plan_name}{Character. Pension plan name.}
#'   \item{ein}{Character. Employer Identification Number.}
#'   \item{plan_number}{Numeric. Plan number.}
#'   \item{city}{Character. Sponsor city.}
#'   \item{state}{Character. Two-letter state abbreviation.}
#'   \item{termination_date}{Date. Date the plan was terminated.}
#'   \item{trusteeship_date}{Date. Date PBGC assumed trusteeship.}
#'   \item{participants}{Numeric. Number of plan participants.}
#' }
#'
#' @examples
#' \dontrun{
#' pbgc_trusteed_plans()
#' pbgc_trusteed_plans(state = "PA")
#' }
#'
#' @seealso \code{\link{pbgc_search}} for name-based searching,
#'   \code{\link{pbgc_multiemployer_plans}} for multiemployer plans.
#' @export
pbgc_trusteed_plans <- function(state = NULL) {
  url <- paste0(.pbgc_base, "/sites/default/files/trusteedplans.xlsx")
  f <- .fetch_excel(url)
  raw <- tryCatch(readxl::read_excel(f), error = function(e) return(.schema_trusteed))
  if (nrow(raw) == 0) return(.schema_trusteed)

  out <- tibble(
    case_number = as.character(raw[[1]]),
    sponsor_name = as.character(raw[[2]]),
    plan_name = as.character(raw[[3]]),
    ein = as.character(raw[[4]]),
    plan_number = suppressWarnings(as.numeric(raw[[5]])),
    city = as.character(raw[[6]]),
    state = as.character(raw[[7]]),
    termination_date = suppressWarnings(as.Date(raw[[8]])),
    trusteeship_date = suppressWarnings(as.Date(raw[[9]])),
    participants = suppressWarnings(as.numeric(raw[[10]]))
  )

  if (!is.null(state)) {
    out <- out |> filter(.data$state == !!state)
  }
  out
}

#' Fetch PBGC multiemployer pension plans
#'
#' Downloads and parses the list of active multiemployer pension plans
#' insured by PBGC. These are union-sponsored plans covering workers in
#' multiple companies. Includes plan administrator locations, sponsor
#' information, and participant counts. Data is sourced from an Excel
#' file on pbgc.gov.
#'
#' @param state Character. Optional two-letter state abbreviation to filter
#'   by administrator state (e.g. \code{"NY"}, \code{"CA"}).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{plan_name}{Character. Pension plan name.}
#'   \item{admin_city}{Character. Plan administrator city.}
#'   \item{admin_state}{Character. Plan administrator state.}
#'   \item{sponsor_name}{Character. Plan sponsor name.}
#'   \item{phone}{Character. Contact phone number.}
#'   \item{effective_date}{Character. Plan effective date.}
#'   \item{plan_id}{Character. Plan identifier.}
#'   \item{ein}{Character. Employer Identification Number.}
#'   \item{pn}{Character. Plan number.}
#'   \item{participant_count}{Numeric. Number of plan participants.}
#' }
#'
#' @examples
#' \dontrun{
#' pbgc_multiemployer_plans()
#' pbgc_multiemployer_plans(state = "NY")
#' }
#'
#' @seealso \code{\link{pbgc_trusteed_plans}} for single-employer plans.
#' @export
pbgc_multiemployer_plans <- function(state = NULL) {
  url <- paste0(.pbgc_base, "/sites/default/files/legacy/docs/open/multiemployerlist.xlsx")
  f <- .fetch_excel(url)
  raw <- tryCatch(readxl::read_excel(f), error = function(e) return(.schema_multiemployer))
  if (nrow(raw) == 0) return(.schema_multiemployer)

  out <- tibble(
    plan_name = as.character(raw[[1]]),
    admin_city = as.character(raw[[2]]),
    admin_state = as.character(raw[[3]]),
    sponsor_name = as.character(raw[[4]]),
    phone = as.character(raw[[5]]),
    effective_date = as.character(raw[[6]]),
    plan_id = as.character(raw[[7]]),
    ein = as.character(raw[[8]]),
    pn = as.character(raw[[9]]),
    participant_count = suppressWarnings(as.numeric(raw[[10]]))
  )

  if (!is.null(state)) {
    out <- out |> filter(.data$admin_state == !!state)
  }
  out
}

#' Search PBGC trusteed plans by sponsor or plan name
#'
#' Searches the PBGC trusteed plans list by matching the query string
#' against sponsor names and plan names (case-insensitive partial match).
#' Downloads the full trusteed plans dataset first, then filters locally.
#'
#' @param query Character. Search string to match against \code{sponsor_name}
#'   and \code{plan_name} (e.g. \code{"steel"}, \code{"airlines"}).
#'
#' @return A tibble with the same columns as \code{\link{pbgc_trusteed_plans}},
#'   filtered to matching rows.
#'
#' @examples
#' \dontrun{
#' pbgc_search("steel")
#' pbgc_search("airlines")
#' }
#'
#' @seealso \code{\link{pbgc_trusteed_plans}}
#' @export
pbgc_search <- function(query) {
  plans <- pbgc_trusteed_plans()
  pattern <- tolower(query)
  plans |> filter(
    grepl(pattern, tolower(sponsor_name)) |
    grepl(pattern, tolower(plan_name))
  )
}

#' Get pbgc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
pbgc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pbgc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/pbgc.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "pbgc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# pbgc.gov context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# pbgc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
