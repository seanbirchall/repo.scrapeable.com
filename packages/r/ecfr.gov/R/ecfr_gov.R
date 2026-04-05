# ecfr.gov.R - Self-contained ecfr.gov client



# ecfr-gov.R
# Self-contained eCFR (Electronic Code of Federal Regulations) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://www.ecfr.gov/api


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ecfr_base <- "https://www.ecfr.gov/api"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_titles <- tibble(
  number = integer(), name = character(),
  latest_amended_on = as.Date(character()),
  latest_issue_date = as.Date(character()),
  up_to_date_as_of = as.Date(character()),
  reserved = logical()
)

.schema_search <- tibble(
  title = integer(), part = character(), section = character(),
  heading = character(), excerpt = character(),
  starts_on = as.Date(character()), score = numeric()
)

.schema_structure <- tibble(
  identifier = character(), label = character(),
  label_description = character(), type = character(),
  reserved = logical()
)

.schema_versions <- tibble(
  date = as.Date(character()), identifier = character(),
  name = character(), part = character(), type = character(),
  substantive = logical(), removed = logical()
)

# == Titles ====================================================================

#' List all CFR titles
#'
#' Returns all 50 titles of the Code of Federal Regulations with
#' their latest amendment and issue dates from the eCFR versioner API.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{number}{integer -- CFR title number (1--50)}
#'     \item{name}{character -- title name (e.g. \code{"General Provisions"},
#'       \code{"Protection of Environment"})}
#'     \item{latest_amended_on}{Date -- date of the most recent amendment}
#'     \item{latest_issue_date}{Date -- date of the latest Federal Register issue}
#'     \item{up_to_date_as_of}{Date -- date through which the eCFR reflects changes}
#'     \item{reserved}{logical -- \code{TRUE} if the title is reserved (unused)}
#'   }
#' @examples
#' ecfr_titles()
#' @export
ecfr_titles <- function() {
  url <- paste0(.ecfr_base, "/versioner/v1/titles")
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_titles)

  titles <- raw$titles
  if (is.null(titles) || length(titles) == 0) return(.schema_titles)

  tibble(
    number            = vapply(titles, function(t) as.integer(t$number %||% NA_integer_), integer(1)),
    name              = vapply(titles, function(t) t$name %||% NA_character_, character(1)),
    latest_amended_on = as.Date(vapply(titles, function(t) t$latest_amended_on %||% NA_character_, character(1))),
    latest_issue_date = as.Date(vapply(titles, function(t) t$latest_issue_date %||% NA_character_, character(1))),
    up_to_date_as_of  = as.Date(vapply(titles, function(t) t$up_to_date_as_of %||% NA_character_, character(1))),
    reserved          = vapply(titles, function(t) as.logical(t$reserved %||% FALSE), logical(1))
  )
}


# == Structure =================================================================

#' Get the table of contents for a CFR title
#'
#' Returns the hierarchical structure (chapters, subchapters, parts, and
#' optionally sections) for a CFR title, flattened to a tibble.
#'
#' @param title_number Integer. CFR title number (1--50). For example,
#'   40 = Protection of Environment, 21 = Food and Drugs.
#' @param date Character. Date for the version (\code{"YYYY-MM-DD"}).
#'   Default \code{NULL} uses the latest available date.
#' @param depth Character. Flatten depth: \code{"part"} (default, returns
#'   chapters/subchapters/parts) or \code{"section"} (returns parts and sections).
#' @return A tibble with columns:
#'   \describe{
#'     \item{identifier}{character -- node identifier (e.g. \code{"I"}, \code{"A"}, \code{"1"})}
#'     \item{label}{character -- formatted label with type prefix}
#'     \item{label_description}{character -- short description (e.g. \code{"Environmental Protection Agency"})}
#'     \item{type}{character -- node type (\code{"chapter"}, \code{"subchapter"}, \code{"part"}, \code{"section"})}
#'     \item{reserved}{logical -- \code{TRUE} if the node is reserved}
#'   }
#' @examples
#' ecfr_structure(40)
#' ecfr_structure(21, depth = "section")
#' @export
ecfr_structure <- function(title_number, date = NULL, depth = "part") {
  if (is.null(date)) {
    # Use the latest available date from the titles endpoint
    titles <- ecfr_titles()
    title_row <- titles[titles$number == title_number, ]
    if (nrow(title_row) > 0 && !is.na(title_row$up_to_date_as_of[1])) {
      date <- format(title_row$up_to_date_as_of[1], "%Y-%m-%d")
    } else {
      date <- format(Sys.Date() - 1, "%Y-%m-%d")
    }
  }
  url <- sprintf("%s/versioner/v1/structure/%s/title-%d.json",
                 .ecfr_base, date, title_number)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_structure)

  # Flatten the tree
  nodes <- list()
  .flatten_tree <- function(node, target_types) {
    if (node$type %in% target_types) {
      nodes[[length(nodes) + 1]] <<- list(
        identifier        = node$identifier %||% NA_character_,
        label             = node$label %||% NA_character_,
        label_description = node$label_description %||% NA_character_,
        type              = node$type %||% NA_character_,
        reserved          = as.logical(node$reserved %||% FALSE)
      )
    }
    children <- node$children
    if (!is.null(children)) {
      for (child in children) .flatten_tree(child, target_types)
    }
  }

  target <- if (depth == "section") c("part", "section") else c("chapter", "subchapter", "part")
  .flatten_tree(raw, target)

  if (length(nodes) == 0) return(.schema_structure)

  tibble(
    identifier        = vapply(nodes, `[[`, character(1), "identifier"),
    label             = vapply(nodes, `[[`, character(1), "label"),
    label_description = vapply(nodes, `[[`, character(1), "label_description"),
    type              = vapply(nodes, `[[`, character(1), "type"),
    reserved          = vapply(nodes, `[[`, logical(1), "reserved")
  )
}


# == Search ====================================================================

#' Search the eCFR full text
#'
#' Full-text search across all 50 titles of the Code of Federal Regulations.
#' Returns matching sections with hierarchical headings and text excerpts
#' (HTML tags stripped).
#'
#' @param query Character. Search term (e.g. \code{"clean air"},
#'   \code{"data privacy"}, \code{"endangered species"}).
#' @param per_page Integer. Results per page (default 20, max varies).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{integer -- CFR title number (1--50)}
#'     \item{part}{character -- CFR part number}
#'     \item{section}{character -- section number (e.g. \code{"88.304-94"})}
#'     \item{heading}{character -- hierarchical heading path}
#'     \item{excerpt}{character -- matching text excerpt (HTML stripped)}
#'     \item{starts_on}{Date -- effective date of the section}
#'     \item{score}{numeric -- relevance score (higher = more relevant)}
#'   }
#' @examples
#' ecfr_search("clean air", per_page = 5)
#' ecfr_search("data privacy", per_page = 10, page = 2)
#' @export
ecfr_search <- function(query, per_page = 20, page = 1) {
  url <- sprintf("%s/search/v1/results?query=%s&per_page=%d&page=%d",
                 .ecfr_base, utils::URLencode(query, reserved = TRUE),
                 per_page, page)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_search)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_search)

  tibble(
    title     = vapply(results, function(r) {
      as.integer(r$hierarchy$title %||% NA_integer_)
    }, integer(1)),
    part      = vapply(results, function(r) r$hierarchy$part %||% NA_character_, character(1)),
    section   = vapply(results, function(r) r$hierarchy$section %||% NA_character_, character(1)),
    heading   = vapply(results, function(r) {
      h <- r$headings
      if (is.null(h) || length(h) == 0) NA_character_
      else paste(h, collapse = " > ")
    }, character(1)),
    excerpt   = vapply(results, function(r) {
      ex <- r$full_text_excerpt %||% ""
      gsub("<[^>]+>", "", ex)  # strip HTML tags
    }, character(1)),
    starts_on = as.Date(vapply(results, function(r) r$starts_on %||% NA_character_, character(1))),
    score     = vapply(results, function(r) as.numeric(r$score %||% NA_real_), numeric(1))
  )
}


# == Versions (change history) =================================================

#' Get change history for a CFR title
#'
#' Returns a chronological log of amendments to sections within
#' a CFR title, showing what changed and whether the change was
#' substantive or editorial.
#'
#' @param title_number Integer. CFR title number (1--50).
#' @param since Character or Date. Only show changes on or after this date
#'   (e.g. \code{"2024-01-01"}). Default \code{NULL} returns all.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- date of the amendment}
#'     \item{identifier}{character -- section identifier (e.g. \code{"52.70"})}
#'     \item{name}{character -- section name}
#'     \item{part}{character -- CFR part number}
#'     \item{type}{character -- node type (typically \code{"section"})}
#'     \item{substantive}{logical -- \code{TRUE} if the change was substantive}
#'     \item{removed}{logical -- \code{TRUE} if the section was removed}
#'   }
#' @examples
#' ecfr_versions(40, since = "2024-01-01")
#' @export
ecfr_versions <- function(title_number, since = NULL) {
  url <- sprintf("%s/versioner/v1/versions/title-%d", .ecfr_base, title_number)
  if (!is.null(since)) {
    url <- paste0(url, "?issue_date%5Bgte%5D=", as.character(since))
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_versions)

  versions <- raw$content_versions
  if (is.null(versions) || length(versions) == 0) return(.schema_versions)

  tibble(
    date        = as.Date(vapply(versions, function(v) v$date %||% NA_character_, character(1))),
    identifier  = vapply(versions, function(v) v$identifier %||% NA_character_, character(1)),
    name        = vapply(versions, function(v) v$name %||% NA_character_, character(1)),
    part        = vapply(versions, function(v) v$part %||% NA_character_, character(1)),
    type        = vapply(versions, function(v) v$type %||% NA_character_, character(1)),
    substantive = vapply(versions, function(v) as.logical(v$substantive %||% NA), logical(1)),
    removed     = vapply(versions, function(v) as.logical(v$removed %||% FALSE), logical(1))
  )
}


# == Context ===================================================================

#' Get ecfr.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ecfr_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ecfr_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ecfr.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ecfr.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ecfr.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ecfr.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
