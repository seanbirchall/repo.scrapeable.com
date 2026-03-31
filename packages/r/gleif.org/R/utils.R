#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# gleif-org.R
# Self-contained GLEIF LEI (Legal Entity Identifier) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.lei_base <- "https://api.gleif.org/api/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_lei <- tibble(
  lei = character(), legal_name = character(), status = character(),
  jurisdiction = character(), country = character(), city = character(),
  address = character(), category = character(),
  registration_date = as.Date(character()), last_update = as.Date(character())
)

# == Helper: parse lei-record ==================================================

.parse_lei_record <- function(rec) {
  a <- rec$attributes
  ent <- a$entity %||% list()
  reg <- a$registration %||% list()
  addr <- ent$legalAddress %||% list()
  tibble(
    lei               = as.character(a$lei %||% NA),
    legal_name        = as.character((ent$legalName %||% list())$name %||% NA),
    status            = as.character(ent$status %||% NA),
    jurisdiction      = as.character(ent$jurisdiction %||% NA),
    country           = as.character(addr$country %||% NA),
    city              = as.character(addr$city %||% NA),
    address           = as.character(paste((addr$addressLines %||% list()), collapse = ", ")),
    category          = as.character(ent$category %||% NA),
    registration_date = tryCatch(as.Date(reg$initialRegistrationDate %||% NA), error = function(e) NA),
    last_update       = tryCatch(as.Date(reg$lastUpdateDate %||% NA), error = function(e) NA)
  )
}

# == Search ====================================================================
