# federallabs.org.R - Federal Laboratory Consortium client
# Self-contained. All public functions return tibbles.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Source: CSV of federal inventions available for licensing


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.flc_csv_url <- "https://federallabs.org/FLC_KenticoXperience_Core/media/Files/Data.Gov%20Uploads/opportunity_1764955984.csv"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.load_flc_data <- function() {
  tmp <- .fetch(.flc_csv_url, ext = ".csv")
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  as_tibble(raw)
}

# == Schemas ===================================================================

.schema_inventions <- tibble(
  id = character(), url = character(), title = character(),
  abstract = character(), source_url = character(),
  related_patents = character(), people = character(),
  organizations = character(), tags = character()
)

# == Public functions ==========================================================

#' List all federal lab inventions
#'
#' Downloads and parses the complete Federal Laboratory Consortium (FLC)
#' inventory of federal inventions available for licensing. Data is sourced
#' from the official FLC CSV file hosted on federallabs.org.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Invention identifier (character)}
#'     \item{url}{FLC listing URL (character)}
#'     \item{title}{Invention title (character)}
#'     \item{abstract}{Description of the invention (character)}
#'     \item{source_url}{Original source URL (character)}
#'     \item{related_patents}{Associated patent numbers (character)}
#'     \item{people}{Inventors and contacts (character)}
#'     \item{organizations}{Originating federal labs/agencies (character)}
#'     \item{tags}{Subject tags (character)}
#'   }
#' @examples
#' flc_inventions()
#' @seealso [flc_search()], [flc_organizations()], [flc_context()]
#' @source <https://federallabs.org>
#' @export
flc_inventions <- function() {
  raw <- tryCatch(.load_flc_data(), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_inventions)

  nms <- names(raw)
  tibble(
    id              = as.character(raw[[nms[1]]] %||% NA),
    url             = as.character(raw[[nms[2]]] %||% NA),
    title           = as.character(raw[[nms[3]]] %||% NA),
    abstract        = as.character(raw[[nms[4]]] %||% NA),
    source_url      = as.character(raw[[nms[5]]] %||% NA),
    related_patents = as.character(raw[[nms[6]]] %||% NA),
    people          = as.character(raw[[nms[7]]] %||% NA),
    organizations   = as.character(raw[[nms[8]]] %||% NA),
    tags            = as.character(raw[[nms[9]]] %||% NA)
  )
}

#' Search federal lab inventions by keyword
#'
#' Searches across title, abstract, organizations, and tags fields of
#' the FLC invention inventory. Performs case-insensitive substring matching.
#'
#' @param query Character string to search for (case-insensitive).
#'   Examples: "solar", "battery", "sensor", "robot".
#' @return A tibble with the same columns as [flc_inventions()]:
#'   id, url, title, abstract, source_url, related_patents, people,
#'   organizations, tags. Only rows matching the query are returned.
#' @examples
#' flc_search("battery")
#' flc_search("sensor")
#' @seealso [flc_inventions()], [flc_organizations()], [flc_context()]
#' @export
flc_search <- function(query) {
  inv <- flc_inventions()
  if (nrow(inv) == 0) return(inv)
  q <- tolower(query)
  matches <- grepl(q, tolower(inv$title), fixed = TRUE) |
    grepl(q, tolower(inv$abstract), fixed = TRUE) |
    grepl(q, tolower(inv$organizations), fixed = TRUE) |
    grepl(q, tolower(inv$tags), fixed = TRUE)
  inv[matches, ]
}

#' List unique organizations in the federal lab inventory
#'
#' Extracts and counts the distinct federal organizations (labs, agencies)
#' that appear in the FLC invention inventory, sorted by number of inventions.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{organization}{Organization name (character)}
#'     \item{n_inventions}{Number of inventions from this organization (integer)}
#'   }
#' @examples
#' flc_organizations()
#' @seealso [flc_inventions()], [flc_search()], [flc_context()]
#' @export
flc_organizations <- function() {
  inv <- flc_inventions()
  if (nrow(inv) == 0) return(tibble(organization = character(), n_inventions = integer()))

  # Organizations can have multiple entries separated by newlines
  orgs <- trimws(unlist(strsplit(inv$organizations, "\n")))
  orgs <- orgs[nchar(orgs) > 0]
  tbl <- as.data.frame(table(orgs), stringsAsFactors = FALSE)
  names(tbl) <- c("organization", "n_inventions")
  as_tibble(tbl) |> arrange(desc(n_inventions))
}

# == Context ===================================================================

#' Get federallabs.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
flc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(flc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/federallabs.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "federallabs.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# federallabs.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# federallabs.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
