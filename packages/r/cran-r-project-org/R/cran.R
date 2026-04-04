# cran-r-project-org.R
# Self-contained CRAN package metadata client via crandb API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.crandb_base <- "https://crandb.r-pkg.org"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_package <- tibble(
  package = character(), version = character(), title = character(),
  description = character(), author = character(), maintainer = character(),
  license = character(), depends = character(), imports = character(),
  date = as.Date(character())
)

.schema_search <- tibble(
  package = character(), version = character(), title = character(),
  description = character(), maintainer = character(), score = numeric()
)

# == Package metadata ==========================================================

#' Get CRAN package metadata
#'
#' Returns metadata for a specific R package from CRAN via the crandb API,
#' including version, title, description, author, dependencies, and
#' publication date.
#'
#' @param name Package name (e.g. "dplyr", "ggplot2", "data.table").
#'   Must be an exact match to the CRAN package name.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{package}{Package name}
#'     \item{version}{Latest CRAN version string}
#'     \item{title}{Short package title}
#'     \item{description}{Full package description}
#'     \item{author}{Author field}
#'     \item{maintainer}{Current maintainer name and email}
#'     \item{license}{License string (e.g. "MIT + file LICENSE")}
#'     \item{depends}{Comma-separated Depends packages}
#'     \item{imports}{Comma-separated Imports packages}
#'     \item{date}{Publication date (Date)}
#'   }
#' @examples
#' cran_package("dplyr")
#' cran_package("ggplot2")
#' @seealso [cran_search()], [cran_context()]
#' @source <https://crandb.r-pkg.org>
cran_package <- function(name) {
  url <- sprintf("%s/%s", .crandb_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("crandb fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_package)

  deps <- if (!is.null(raw$Depends) && is.list(raw$Depends)) paste(names(raw$Depends), collapse = ", ") else as.character(raw$Depends %||% NA_character_)
  imps <- if (!is.null(raw$Imports) && is.list(raw$Imports)) paste(names(raw$Imports), collapse = ", ") else as.character(raw$Imports %||% NA_character_)
  pub_date <- tryCatch(as.Date(raw$`Date/Publication` %||% raw$date %||% NA_character_), error = function(e) as.Date(NA))

  tibble(
    package     = as.character(raw$Package %||% raw$`_id` %||% NA_character_),
    version     = as.character(raw$Version %||% NA_character_),
    title       = as.character(raw$Title %||% NA_character_),
    description = as.character(raw$Description %||% NA_character_),
    author      = as.character(raw$Author %||% NA_character_),
    maintainer  = as.character(raw$Maintainer %||% NA_character_),
    license     = as.character(raw$License %||% NA_character_),
    depends     = deps,
    imports     = imps,
    date        = pub_date
  )
}

#' Search CRAN packages by keyword
#'
#' Searches through all CRAN package names using case-insensitive matching
#' and fetches metadata for each match. Useful for discovering R packages
#' related to a topic or finding packages by partial name.
#'
#' @param query Search query string (e.g. "dplyr", "time series", "plot").
#'   Matched against package names using case-insensitive grep.
#' @param limit Maximum number of results to return (default 20). Each match
#'   requires a separate API call, so lower values are faster.
#' @return A tibble with columns:
#'   \describe{
#'     \item{package}{Package name}
#'     \item{version}{Latest CRAN version}
#'     \item{title}{Short package title}
#'     \item{description}{Full description}
#'     \item{maintainer}{Package maintainer}
#'     \item{score}{Relevance score (currently NA; reserved for future use)}
#'   }
#' @examples
#' cran_search("dplyr")
#' cran_search("time series", limit = 5)
#' @seealso [cran_package()], [cran_context()]
#' @source <https://crandb.r-pkg.org>
cran_search <- function(query, limit = 20) {
  # Get all package names from crandb
  all_names <- tryCatch(.fetch_json(sprintf("%s/-/pkgnames", .crandb_base)),
                        error = function(e) {
    message("crandb pkgnames failed: ", e$message)
    return(NULL)
  })
  if (is.null(all_names) || length(all_names) == 0) return(.schema_search)

  # Search through names
  matches <- grep(query, all_names, ignore.case = TRUE, value = TRUE)
  if (length(matches) == 0) return(.schema_search)
  matches <- head(matches, limit)

  # Fetch metadata for each match individually from crandb
  rows <- lapply(matches, function(pkg_name) {
    tryCatch({
      p <- jsonlite::fromJSON(.fetch(sprintf("%s/%s", .crandb_base, utils::URLencode(pkg_name))),
                              simplifyVector = FALSE)
      tibble(
        package     = as.character(p$Package %||% pkg_name),
        version     = as.character(p$Version %||% NA_character_),
        title       = as.character(p$Title %||% NA_character_),
        description = as.character(p$Description %||% NA_character_),
        maintainer  = as.character(p$Maintainer %||% NA_character_),
        score       = NA_real_
      )
    }, error = function(e) NULL)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(.schema_search)
  bind_rows(rows)
}

# == Context (LLM injection) ==================================================

#' Get cran-r-project-org client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the CRAN metadata client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' cran_context()
#' @export
cran_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cran_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cran-r-project-org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cran-r-project-org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cran-r-project-org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cran-r-project-org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
