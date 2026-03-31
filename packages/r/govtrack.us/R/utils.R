#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_user_agent req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# govtrack-us.R
# Self-contained GovTrack congressional data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://www.govtrack.us/api/v2


# == Private utilities =========================================================

.ua <- "R/4.4 httr2/1.0"
.gt_base <- "https://www.govtrack.us/api/v2"

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_user_agent(.ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -- URL builder ---------------------------------------------------------------

.gt_url <- function(endpoint, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  if (length(params) == 0) return(paste0(.gt_base, "/", endpoint))
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  paste0(.gt_base, "/", endpoint, "?", query)
}

# == Schemas ===================================================================

.schema_bills <- tibble(
  id = integer(), bill_type = character(), number = integer(),
  congress = integer(), title = character(),
  current_status = character(), current_status_date = as.Date(character()),
  introduced_date = as.Date(character()), sponsor_name = character(),
  link = character()
)

.schema_members <- tibble(
  person_id = integer(), name = character(), party = character(),
  state = character(), district = integer(), role_type = character(),
  start_date = as.Date(character()), end_date = as.Date(character()),
  website = character()
)

.schema_votes <- tibble(
  id = integer(), congress = integer(), session = character(),
  chamber = character(), number = integer(),
  question = character(), result = character(),
  created = as.POSIXct(character()),
  total_plus = integer(), total_minus = integer(),
  link = character()
)

.schema_committees <- tibble(
  code = character(), name = character(),
  committee_type = character(), url = character(), obsolete = logical()
)
