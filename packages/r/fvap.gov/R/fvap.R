# fvap.gov.R - Self-contained FVAP (Federal Voting Assistance Program) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required


`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.fvap_base <- "https://www.fvap.gov"

.fetch_excel <- function(url, ext = ".xls") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Dataset metadata ==========================================================

.fvap_datasets <- tibble(
  id = c("military", "overseas_citizens", "vao_military",
         "leo", "federal_civilians", "dos_vao"),
  name = c("Uniformed Services Absentee Voting",
           "Overseas Citizens Absentee Voting",
           "Military Voting Assistance Officers",
           "Local Election Official Data",
           "Federal Employees Overseas Absentee Voting",
           "Department of State Voting Assistance Officers"),
  description = c(
    "2008 post election survey of active duty military on absentee voting",
    "2008 post election survey of American citizens living overseas on absentee voting",
    "2008 post election survey of military voting assistance officers (VAO)",
    "2008 post election survey of Local Election Officials on military/overseas absentee voting",
    "2008 post election survey of Federal employees overseas on absentee voting",
    "2008 post election survey of Dept of State voting assistance officers"
  ),
  url = c(
    paste0(.fvap_base, "/uploads/FVAP/Surveys/military_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/overseas_citizens_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/uvao_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/leo_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/federal_civilians_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/dosvao_data.xls")
  ),
  year = rep(2008L, 6)
)

# == Public functions ==========================================================

#' List available FVAP survey datasets
#'
#' Returns metadata for all available Federal Voting Assistance Program
#' survey datasets. These are 2008 post-election surveys covering military
#' personnel, overseas citizens, voting assistance officers, and local
#' election officials.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Dataset identifier (use with \code{\link{fvap_data}})}
#'     \item{name}{Dataset name}
#'     \item{description}{Brief description of the survey}
#'     \item{url}{Download URL for the Excel file}
#'     \item{year}{Survey year}
#'   }
#' @examples
#' fvap_list()
#' @export
fvap_list <- function() {
  .fvap_datasets
}

#' Fetch an FVAP survey dataset
#'
#' Downloads and parses an FVAP survey Excel file. Each dataset contains
#' individual survey response data with coded variables. Use
#' \code{\link{fvap_sheets}} to explore available sheets and
#' \code{\link{fvap_summary}} for column metadata.
#'
#' @param dataset Character dataset ID from \code{\link{fvap_list}} (e.g.
#'   "military", "overseas_citizens", "leo").
#' @param sheet Sheet name or number to read (default 1).
#' @return A tibble with survey response data. Columns vary by dataset.
#' @examples
#' \dontrun{
#' # Military absentee voting survey
#' fvap_data("military")
#'
#' # Local election officials survey
#' fvap_data("leo")
#' }
#' @export
fvap_data <- function(dataset, sheet = 1) {
  match <- .fvap_datasets |> filter(.data$id == !!dataset)
  if (nrow(match) == 0) {
    stop("Unknown dataset: ", dataset,
         ". Use fvap_list() to see available datasets.", call. = FALSE)
  }
  url <- match$url[1]
  f <- .fetch_excel(url, ext = ".xls")
  tryCatch(
    readxl::read_excel(f, sheet = sheet) |> as_tibble(),
    error = function(e) {
      warning("Could not read dataset: ", e$message, call. = FALSE)
      tibble()
    }
  )
}

#' List sheets in an FVAP survey dataset
#'
#' Downloads the Excel file for the given dataset and returns the names of
#' all available sheets. Useful for exploring multi-sheet workbooks before
#' calling \code{\link{fvap_data}}.
#'
#' @param dataset Character dataset ID from \code{\link{fvap_list}}.
#' @return A character vector of sheet names.
#' @examples
#' \dontrun{
#' fvap_sheets("military")
#' }
#' @export
fvap_sheets <- function(dataset) {
  match <- .fvap_datasets |> filter(.data$id == !!dataset)
  if (nrow(match) == 0) {
    stop("Unknown dataset: ", dataset, call. = FALSE)
  }
  url <- match$url[1]
  f <- .fetch_excel(url, ext = ".xls")
  readxl::excel_sheets(f)
}

#' Summarize an FVAP survey dataset
#'
#' Downloads a dataset and returns a tibble describing each column's name,
#' type, missing value count, and cardinality. Useful for understanding
#' the structure before analysis.
#'
#' @param dataset Character dataset ID from \code{\link{fvap_list}}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{column_name}{Column name from the Excel file}
#'     \item{column_type}{R data type (e.g. "numeric", "character")}
#'     \item{n_missing}{Count of NA values}
#'     \item{n_unique}{Count of distinct non-NA values}
#'   }
#' @examples
#' \dontrun{
#' fvap_summary("military")
#' }
#' @export
fvap_summary <- function(dataset) {
  d <- fvap_data(dataset)
  if (nrow(d) == 0) return(tibble(column_name = character(), column_type = character(),
                                   n_missing = integer(), n_unique = integer()))
  tibble(
    column_name = names(d),
    column_type = vapply(d, function(x) class(x)[1], character(1)),
    n_missing = vapply(d, function(x) sum(is.na(x)), integer(1)),
    n_unique = vapply(d, function(x) length(unique(x[!is.na(x)])), integer(1))
  )
}

#' Get fvap.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fvap_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fvap_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fvap.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fvap.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fvap.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fvap.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
