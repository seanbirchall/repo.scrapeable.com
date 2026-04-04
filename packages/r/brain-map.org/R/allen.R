# portal.brain-map.org.R - Self-contained portal.brain-map.org client


`%||%` <- function(a, b) if (is.null(a)) b else a

.ua <- "support@scrapeable.com"
.allen_base <- "https://api.brain-map.org/api/v2"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
.fetch_json_df <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_structures <- tibble(
  id = integer(), name = character(), acronym = character(),
  parent_structure_id = integer()
)

.schema_genes <- tibble(
  id = integer(), acronym = character(), name = character(),
  entrez_id = integer()
)

# == Public functions ==========================================================

#' Search Allen Brain Atlas structures
#'
#' Queries the Allen Institute Reference Atlas for brain structures matching
#' a name pattern. Returns anatomical structures with their IDs, acronyms,
#' and parent relationships for building structural hierarchies.
#'
#' @param query Character or \code{NULL}. Search term matched against structure
#'   names using case-insensitive substring matching (e.g. \code{"cortex"},
#'   \code{"hippocampus"}, \code{"thalamus"}). If \code{NULL}, returns
#'   structures in default order.
#' @param num_rows Integer. Maximum number of results to return (default 50).
#' @return A tibble with one row per structure and 4 columns:
#' \describe{
#'   \item{id}{Integer. Unique Allen structure ID (e.g. \code{315}, \code{688}).}
#'   \item{name}{Character. Full structure name (e.g. \code{"Isocortex"}, \code{"Cerebellar cortex"}).}
#'   \item{acronym}{Character. Standard abbreviation (e.g. \code{"CTX"}, \code{"CBX"}).}
#'   \item{parent_structure_id}{Integer. ID of the parent structure in the ontology tree,
#'     or \code{NA} for root structures.}
#' }
#' @examples
#' allen_structures("cortex")
#' allen_structures("hippocampus", num_rows = 10)
#' allen_structures()
#' @export
allen_structures <- function(query = NULL, num_rows = 50) {
  if (!is.null(query)) {
    url <- sprintf("%s/data/Structure/query.json?criteria=[name$il*%s*]&num_rows=%d",
                   .allen_base, URLencode(query, TRUE), num_rows)
  } else {
    url <- sprintf("%s/data/Structure/query.json?num_rows=%d", .allen_base, num_rows)
  }
  raw <- .fetch_json(url)
  msgs <- raw$msg
  if (is.null(msgs) || length(msgs) == 0 || is.character(msgs)) return(.schema_structures)
  tibble(
    id = vapply(msgs, function(x) as.integer(x$id %||% NA), integer(1)),
    name = vapply(msgs, function(x) as.character(x$name %||% NA), character(1)),
    acronym = vapply(msgs, function(x) as.character(x$acronym %||% NA), character(1)),
    parent_structure_id = vapply(msgs, function(x) as.integer(x$parent_structure_id %||% NA), integer(1))
  )
}

#' Search Allen Brain Atlas genes
#'
#' Searches the Allen Institute gene catalog by acronym/symbol using
#' case-insensitive substring matching. Returns gene metadata including
#' NCBI Entrez identifiers for cross-referencing with other databases.
#'
#' @param query Character. Gene symbol or partial name to search for
#'   (e.g. \code{"GABA"}, \code{"BDNF"}, \code{"SLC6"}).
#' @param num_rows Integer. Maximum number of results to return (default 50).
#' @return A tibble with one row per gene and 4 columns:
#' \describe{
#'   \item{id}{Integer. Allen Institute gene ID.}
#'   \item{acronym}{Character. Official gene symbol (e.g. \code{"GABARAP"}, \code{"BDNF"}).}
#'   \item{name}{Character. Full gene name (e.g. \code{"GABA(A) receptor-associated protein"}).}
#'   \item{entrez_id}{Integer. NCBI Entrez Gene ID for cross-referencing.}
#' }
#' @examples
#' allen_genes("GABA")
#' allen_genes("BDNF", num_rows = 5)
#' @export
allen_genes <- function(query, num_rows = 50) {
  url <- sprintf("%s/data/Gene/query.json?criteria=[acronym$il*%s*]&num_rows=%d",
                 .allen_base, URLencode(query, TRUE), num_rows)
  raw <- .fetch_json(url)
  msgs <- raw$msg
  if (is.null(msgs) || length(msgs) == 0 || is.character(msgs)) return(.schema_genes)
  tibble(
    id = vapply(msgs, function(x) as.integer(x$id %||% NA), integer(1)),
    acronym = vapply(msgs, function(x) as.character(x$acronym %||% NA), character(1)),
    name = vapply(msgs, function(x) as.character(x$name %||% NA), character(1)),
    entrez_id = vapply(msgs, function(x) as.integer(x$entrez_id %||% NA), integer(1))
  )
}

#' List Allen Brain Atlas datasets/products
#'
#' Returns available atlas products from the Allen Institute, including
#' the Mouse Brain Atlas, Human Brain Atlas, connectivity studies, and
#' specialized experiment series. Use product IDs to filter other queries.
#'
#' @param num_rows Integer. Maximum number of products to return (default 50).
#' @return A tibble with one row per product and 4 columns:
#' \describe{
#'   \item{id}{Integer. Product ID (e.g. \code{7} for Mouse Connectivity Reference Data).}
#'   \item{name}{Character. Full product name (e.g. \code{"Mouse Connectivity Reference Data"}).}
#'   \item{abbreviation}{Character. Short code (e.g. \code{"ConnRef"}, \code{"HumanASD"}).}
#'   \item{description}{Character. Brief description of the dataset's scope and methods.}
#' }
#' @examples
#' allen_products()
#' allen_products(num_rows = 10)
#' @export
allen_products <- function(num_rows = 50) {
  schema <- tibble(id = integer(), name = character(),
                   abbreviation = character(), description = character())
  url <- sprintf("%s/data/Product/query.json?num_rows=%d", .allen_base, num_rows)
  raw <- .fetch_json(url)
  msgs <- raw$msg
  if (is.null(msgs) || length(msgs) == 0 || is.character(msgs)) return(schema)
  tibble(
    id = vapply(msgs, function(x) as.integer(x$id %||% NA), integer(1)),
    name = vapply(msgs, function(x) as.character(x$name %||% NA), character(1)),
    abbreviation = vapply(msgs, function(x) as.character(x$abbreviation %||% NA), character(1)),
    description = vapply(msgs, function(x) as.character(x$description %||% NA), character(1))
  )
}

# == Context ===================================================================

#' Get brain-map.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
allen_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(allen_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/brain-map.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "brain-map.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# brain-map.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# brain-map.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
