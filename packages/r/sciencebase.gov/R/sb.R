# sciencebase.gov.R - Self-contained USGS ScienceBase catalog client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public catalog)
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sb_base <- "https://www.sciencebase.gov/catalog"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

.collapse_tags <- function(tags_list) {
  if (is.null(tags_list)) return(NA_character_)
  if (is.data.frame(tags_list)) return(paste(tags_list$name, collapse = "; "))
  if (is.list(tags_list)) {
    return(vapply(tags_list, function(x) {
      if (is.data.frame(x)) paste(x$name, collapse = "; ")
      else if (is.list(x)) paste(vapply(x, function(z) z$name %||% "", character(1)), collapse = "; ")
      else NA_character_
    }, character(1)))
  }
  NA_character_
}

# == Schemas ===================================================================

.schema_item <- tibble(
  id = character(), title = character(), summary = character(),
  hasChildren = logical(), dateCreated = as.POSIXct(character()),
  lastUpdated = as.POSIXct(character()), tags = character()
)

.schema_item_detail <- tibble(
  id = character(), title = character(), summary = character(),
  hasChildren = logical(), dateCreated = as.POSIXct(character()),
  lastUpdated = as.POSIXct(character()), tags = character(),
  contacts = character(), files = character()
)

# == Search ====================================================================

#' Search the USGS ScienceBase catalog
#'
#' Searches the USGS ScienceBase data catalog for datasets, publications,
#' and other scientific items by keyword. ScienceBase hosts over 200,000
#' items covering geology, hydrology, ecology, climate, and more.
#'
#' @param q Character. Search query string (e.g. \code{"water quality"},
#'   \code{"salmon habitat"}, \code{"groundwater"}).
#' @param folder_id Character or NULL. ScienceBase item ID of a parent
#'   folder to constrain the search. NULL (default) searches the entire
#'   catalog.
#' @param max Integer. Number of results to return (default 20, max 100).
#' @param offset Integer. Pagination offset (default 0). Use with
#'   \code{max} to page through results.
#' @param fields Character. Comma-separated ScienceBase field names to
#'   return. Defaults to \code{"id,title,summary,hasChildren,provenance,tags"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. ScienceBase item UUID.}
#'     \item{title}{Character. Item title.}
#'     \item{summary}{Character. Short summary text.}
#'     \item{hasChildren}{Logical. Whether the item has child items.}
#'     \item{dateCreated}{POSIXct. Date the item was created (may be NA).}
#'     \item{lastUpdated}{POSIXct. Date the item was last updated (may be NA).}
#'     \item{tags}{Character. Semicolon-separated tag names.}
#'   }
#' @examples
#' sb_search("water quality")
#' sb_search("salmon", max = 5)
#' @export
sb_search <- function(q, folder_id = NULL, max = 20, offset = 0,
                      fields = "id,title,summary,hasChildren,provenance,tags") {
  params <- list(
    format = "json", q = q, max = min(max, 100), offset = offset,
    fields = fields
  )
  if (!is.null(folder_id)) params$parentId <- folder_id

  pairs <- paste0(names(params), "=",
    vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE), character(1)))
  url <- paste0(.sb_base, "/items?", paste(pairs, collapse = "&"))

  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || is.null(res$items) || length(res$items) == 0)
    return(.schema_item)

  items <- res$items
  n <- length(items$id)

  prov <- items$provenance
  dc <- if (!is.null(prov) && "dateCreated" %in% names(prov))
    as.POSIXct(prov$dateCreated, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  else rep(as.POSIXct(NA), n)

  lu <- if (!is.null(prov) && "lastUpdated" %in% names(prov))
    as.POSIXct(prov$lastUpdated, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  else rep(as.POSIXct(NA), n)

  tag_vals <- if ("tags" %in% names(items) && is.list(items$tags)) {
    vapply(items$tags, function(t) {
      if (is.data.frame(t)) paste(t$name, collapse = "; ") else NA_character_
    }, character(1))
  } else rep(NA_character_, n)

  tibble(
    id = as.character(items$id),
    title = as.character(items$title),
    summary = as.character(.safe_col(items, "summary")),
    hasChildren = as.logical(.safe_col(items, "hasChildren", NA)),
    dateCreated = dc,
    lastUpdated = lu,
    tags = tag_vals
  )
}

# == Item Detail ===============================================================

#' Get full details for a ScienceBase item
#'
#' Retrieves metadata for a single ScienceBase item including contacts
#' and attached files. The item ID is obtained from \code{sb_search()}
#' or the ScienceBase website URL.
#'
#' @param item_id Character. ScienceBase item UUID
#'   (e.g. \code{"50dfa373e4b0615c0a238799"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. ScienceBase item UUID.}
#'     \item{title}{Character. Item title.}
#'     \item{summary}{Character. Short summary text.}
#'     \item{hasChildren}{Logical. Whether the item has child items.}
#'     \item{dateCreated}{POSIXct. Date the item was created.}
#'     \item{lastUpdated}{POSIXct. Date the item was last updated.}
#'     \item{tags}{Character. Semicolon-separated tag names.}
#'     \item{contacts}{Character. Semicolon-separated contact names.}
#'     \item{files}{Character. Semicolon-separated attached file names.}
#'   }
#' @examples
#' sb_item("50dfa373e4b0615c0a238799")
#' @export
sb_item <- function(item_id) {
  url <- sprintf("%s/item/%s?format=json&fields=id,title,summary,hasChildren,provenance,tags,contacts,files",
                 .sb_base, item_id)
  d <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(d)) return(.schema_item_detail[0, ])

  prov <- d$provenance
  dc <- if (!is.null(prov$dateCreated))
    as.POSIXct(prov$dateCreated, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  else as.POSIXct(NA)
  lu <- if (!is.null(prov$lastUpdated))
    as.POSIXct(prov$lastUpdated, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  else as.POSIXct(NA)

  tags_str <- if (is.data.frame(d$tags)) paste(d$tags$name, collapse = "; ")
    else NA_character_

  contacts_str <- if (is.data.frame(d$contacts))
    paste(d$contacts$name %||% character(0), collapse = "; ")
  else NA_character_

  files_str <- if (is.data.frame(d$files))
    paste(d$files$name %||% character(0), collapse = "; ")
  else NA_character_

  tibble(
    id = as.character(d$id %||% NA_character_),
    title = as.character(d$title %||% NA_character_),
    summary = as.character(d$summary %||% NA_character_),
    hasChildren = as.logical(d$hasChildren %||% NA),
    dateCreated = dc,
    lastUpdated = lu,
    tags = tags_str,
    contacts = contacts_str,
    files = files_str
  )
}

# == Child Items ===============================================================

#' List child items of a ScienceBase item
#'
#' Returns items nested under a parent ScienceBase item, which is how
#' ScienceBase organizes collections and project hierarchies.
#'
#' @param parent_id Character. ScienceBase item UUID of the parent item.
#' @param max Integer. Number of results to return (default 20, max 100).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. ScienceBase item UUID.}
#'     \item{title}{Character. Item title.}
#'     \item{summary}{Character. Short summary text.}
#'     \item{hasChildren}{Logical. Whether the item has child items.}
#'     \item{dateCreated}{POSIXct. Date the item was created (may be NA).}
#'     \item{lastUpdated}{POSIXct. Date the item was last updated (may be NA).}
#'     \item{tags}{Character. Semicolon-separated tag names.}
#'   }
#' @examples
#' sb_children("4f4e4760e4b07f02db47e13b")
#' @export
sb_children <- function(parent_id, max = 20, offset = 0) {
  url <- sprintf("%s/items?format=json&parentId=%s&max=%d&offset=%d&fields=id,title,summary,hasChildren,provenance,tags",
                 .sb_base, parent_id, min(max, 100), offset)
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || is.null(res$items) || length(res$items) == 0)
    return(.schema_item)

  items <- res$items
  n <- length(items$id)

  prov <- items$provenance
  dc <- if (!is.null(prov) && "dateCreated" %in% names(prov))
    as.POSIXct(prov$dateCreated, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  else rep(as.POSIXct(NA), n)

  lu <- if (!is.null(prov) && "lastUpdated" %in% names(prov))
    as.POSIXct(prov$lastUpdated, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  else rep(as.POSIXct(NA), n)

  tag_vals <- if ("tags" %in% names(items) && is.list(items$tags)) {
    vapply(items$tags, function(t) {
      if (is.data.frame(t)) paste(t$name, collapse = "; ") else NA_character_
    }, character(1))
  } else rep(NA_character_, n)

  tibble(
    id = as.character(items$id),
    title = as.character(items$title),
    summary = as.character(.safe_col(items, "summary")),
    hasChildren = as.logical(.safe_col(items, "hasChildren", NA)),
    dateCreated = dc,
    lastUpdated = lu,
    tags = tag_vals
  )
}

# == Context ===================================================================

#' Get sciencebase.gov client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' sb_context()
#' @export
sb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/sciencebase.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "sciencebase.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# sciencebase.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# sciencebase.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
