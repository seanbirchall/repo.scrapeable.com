#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# rdap-org.R
# Self-contained RDAP (Registration Data Access Protocol) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rdap_base <- "https://rdap.org"

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
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- RDAP entity parser -------------------------------------------------------

.parse_entities <- function(entities) {
  if (is.null(entities) || length(entities) == 0) return(tibble(role = character(), name = character()))
  rows <- lapply(entities, function(e) {
    roles <- paste(unlist(e$roles %||% list()), collapse = ", ")
    name <- tryCatch({
      vcard <- e$vcardArray
      if (!is.null(vcard) && length(vcard) >= 2) {
        fn_entry <- Filter(function(x) x[[1]] == "fn", vcard[[2]])
        if (length(fn_entry) > 0) fn_entry[[1]][[4]] else e$handle %||% NA_character_
      } else e$handle %||% NA_character_
    }, error = function(err) e$handle %||% NA_character_)
    tibble(role = roles, name = as.character(name))
  })
  bind_rows(rows)
}

# == Schemas ===================================================================

.schema_domain <- tibble(
  name = character(), handle = character(), status = character(),
  registrar = character(), registration = as.Date(character()),
  expiration = as.Date(character()), last_changed = as.Date(character()),
  nameservers = character()
)

.schema_ip <- tibble(
  handle = character(), name = character(), type = character(),
  start_address = character(), end_address = character(),
  country = character(), status = character()
)

.schema_autnum <- tibble(
  handle = character(), name = character(), type = character(),
  start_autnum = integer(), end_autnum = integer(),
  country = character(), status = character()
)
