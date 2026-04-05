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
#' Queries the Allen Brain Atlas API for anatomical brain structures.
#' Returns hierarchical structure data from the Allen Reference Atlas
#' used in neuroscience research.
#'
#' @param query Search term for structure name (optional, e.g. "cortex",
#'   "hippocampus", "thalamus"). If NULL, returns default structures.
#' @param num_rows Max results to return (default 50)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Allen Brain Atlas structure ID (integer)}
#'     \item{name}{Full structure name (character)}
#'     \item{acronym}{Standard abbreviation (character)}
#'     \item{parent_structure_id}{ID of parent in hierarchy (integer)}
#'   }
#' @examples
#' allen_structures()
#' allen_structures("hippocampus")
#' @seealso [allen_genes()], [allen_products()], [allen_context()]
#' @source <https://api.brain-map.org/api/v2>
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
#' Searches for genes in the Allen Brain Atlas gene expression database.
#' Matches on gene symbol/acronym prefix.
#'
#' @param query Gene name or symbol to search for (e.g. "GAD", "SLC",
#'   "GFAP", "RBFOX3")
#' @param num_rows Max results to return (default 50)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Allen Brain Atlas gene ID (integer)}
#'     \item{acronym}{Gene symbol (character)}
#'     \item{name}{Full gene name (character)}
#'     \item{entrez_id}{NCBI Entrez Gene ID (integer)}
#'   }
#' @examples
#' allen_genes("GAD")
#' allen_genes("GFAP")
#' @seealso [allen_structures()], [allen_products()], [allen_context()]
#' @source <https://api.brain-map.org/api/v2>
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
#' Returns the available atlas products from the Allen Institute, such as
#' the Human Brain Atlas, Mouse Brain Atlas, and Allen Cell Types Database.
#'
#' @param num_rows Max results to return (default 50)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Product ID (integer)}
#'     \item{name}{Product name (character)}
#'     \item{abbreviation}{Short abbreviation (character)}
#'     \item{description}{Product description (character)}
#'   }
#' @examples
#' allen_products()
#' @seealso [allen_structures()], [allen_genes()], [allen_context()]
#' @source <https://api.brain-map.org/api/v2>
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

#' Get portal.brain-map.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/portal.brain-map.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "portal.brain-map.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# portal.brain-map.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# portal.brain-map.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
