# api.catalogueoflife.org.R - Self-contained api.catalogueoflife.org client



# api-catalogueoflife-org.R
# Self-contained Catalogue of Life API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.col_base <- "https://api.catalogueoflife.org"
.col_dataset <- 3  # COL working project (most comprehensive)
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_search <- tibble(
  id = character(), name = character(), authorship = character(),
  rank = character(), status = character(), family = character(),
  order = character(), class = character(), phylum = character(),
  kingdom = character()
)

.schema_taxon <- tibble(
  id = character(), name = character(), authorship = character(),
  rank = character(), status = character(), extinct = logical(),
  parent_id = character(), family = character(),
  order = character(), class = character(), phylum = character(),
  kingdom = character()
)



# == Search ====================================================================

#' Search the Catalogue of Life for taxa
#'
#' Searches name usages in the Catalogue of Life (COL) working project
#' by scientific name. Returns matching taxa with their full Linnaean
#' classification from kingdom to family.
#'
#' @param query Character. Scientific name or partial name to search for
#'   (e.g. \code{"Panthera"}, \code{"Quercus"}, \code{"Drosophila"},
#'   \code{"Homo sapiens"}).
#' @param limit Integer. Maximum results to return (default 50, max 1000).
#' @param dataset_key Integer. COL dataset key (default \code{3}, the COL
#'   working project, which is the most comprehensive).
#' @return A tibble with one row per taxon and 10 columns:
#' \describe{
#'   \item{id}{Character. COL taxon ID (e.g. \code{"HsuBgskqlr9aAWFh95pB4"}).
#'     Use with \code{col_taxon()} for details.}
#'   \item{name}{Character. Scientific name (e.g. \code{"Panthera pardus"}).}
#'   \item{authorship}{Character. Taxonomic authority (e.g. \code{"(Linnaeus, 1758)"}).}
#'   \item{rank}{Character. Taxonomic rank: \code{"kingdom"}, \code{"phylum"},
#'     \code{"class"}, \code{"order"}, \code{"family"}, \code{"genus"},
#'     \code{"species"}, \code{"subspecies"}, etc.}
#'   \item{status}{Character. Taxonomic status: \code{"accepted"} or \code{"synonym"}.}
#'   \item{family}{Character. Family name (e.g. \code{"Felidae"}).}
#'   \item{order}{Character. Order name (e.g. \code{"Carnivora"}).}
#'   \item{class}{Character. Class name (e.g. \code{"Mammalia"}).}
#'   \item{phylum}{Character. Phylum name (e.g. \code{"Chordata"}).}
#'   \item{kingdom}{Character. Kingdom name (e.g. \code{"Animalia"}).}
#' }
#' @examples
#' col_search("Panthera")
#' col_search("Quercus", limit = 10)
#' @export
col_search <- function(query, limit = 50, dataset_key = 3) {
  url <- sprintf("%s/dataset/%s/nameusage/search?q=%s&limit=%d",
                 .col_base, dataset_key, utils::URLencode(query), limit)
  raw <- .fetch_json(url)
  results <- raw$result
  if (is.null(results) || length(results) == 0) return(.schema_search)

  rows <- lapply(seq_len(nrow(results)), function(i) {
    r <- results[i, ]
    cls <- r$classification
    cls_list <- if (is.data.frame(cls)) cls else if (is.list(cls)) cls[[1]] else NULL
    .get_rank <- function(rank_name) {
      if (is.null(cls_list) || !is.data.frame(cls_list)) return(NA_character_)
      idx <- which(cls_list$rank == rank_name)
      if (length(idx) > 0) cls_list$name[idx[1]] else NA_character_
    }
    tibble(
      id         = as.character(r$id %||% NA),
      name       = as.character(r$usage$name$scientificName %||% r$usage$label %||% NA),
      authorship = as.character(r$usage$name$authorship %||% NA),
      rank       = as.character(r$usage$name$rank %||% NA),
      status     = as.character(r$usage$status %||% NA),
      family     = .get_rank("family"),
      order      = .get_rank("order"),
      class      = .get_rank("class"),
      phylum     = .get_rank("phylum"),
      kingdom    = .get_rank("kingdom")
    )
  })
  bind_rows(rows)
}


# == Taxon detail ==============================================================

#' Fetch a single taxon from the Catalogue of Life
#'
#' Returns detailed information for a specific taxon by its COL ID,
#' including extinction status and parent taxon ID for building
#' taxonomic trees.
#'
#' @param id Character. COL taxon ID obtained from \code{col_search()}
#'   results (e.g. \code{"HsuBgskqlr9aAWFh95pB4"}).
#' @param dataset_key Integer. COL dataset key (default \code{3}, the
#'   working project).
#' @return A tibble with 1 row and 12 columns:
#' \describe{
#'   \item{id}{Character. COL taxon ID.}
#'   \item{name}{Character. Scientific name.}
#'   \item{authorship}{Character. Taxonomic authority.}
#'   \item{rank}{Character. Taxonomic rank (e.g. \code{"genus"}, \code{"species"}).}
#'   \item{status}{Character. Taxonomic status (\code{"accepted"} or \code{"synonym"}).}
#'   \item{extinct}{Logical. Whether the taxon is extinct (\code{TRUE}/\code{FALSE}).}
#'   \item{parent_id}{Character. COL ID of the parent taxon.}
#'   \item{family}{Character. Family name, or \code{NA} if above family rank.}
#'   \item{order}{Character. Order name, or \code{NA}.}
#'   \item{class}{Character. Class name, or \code{NA}.}
#'   \item{phylum}{Character. Phylum name, or \code{NA}.}
#'   \item{kingdom}{Character. Kingdom name, or \code{NA}.}
#' }
#' @examples
#' # First search, then get details:
#' # results <- col_search("Panthera")
#' # col_taxon(results$id[1])
#' @export
col_taxon <- function(id, dataset_key = 3) {
  url <- sprintf("%s/dataset/%s/taxon/%s", .col_base, dataset_key, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_taxon)

  cls <- raw$classification
  .get_rank <- function(rank_name) {
    if (is.null(cls) || !is.data.frame(cls)) return(NA_character_)
    idx <- which(cls$rank == rank_name)
    if (length(idx) > 0) cls$name[idx[1]] else NA_character_
  }

  tibble(
    id         = as.character(raw$id %||% NA),
    name       = as.character(raw$name$scientificName %||% raw$label %||% NA),
    authorship = as.character(raw$name$authorship %||% NA),
    rank       = as.character(raw$name$rank %||% NA),
    status     = as.character(raw$status %||% NA),
    extinct    = as.logical(raw$extinct %||% NA),
    parent_id  = as.character(raw$parentId %||% NA),
    family     = .get_rank("family"),
    order      = .get_rank("order"),
    class      = .get_rank("class"),
    phylum     = .get_rank("phylum"),
    kingdom    = .get_rank("kingdom")
  )
}


# == Context ===================================================================

#' Get catalogueoflife.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
col_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(col_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/catalogueoflife.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "catalogueoflife.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# catalogueoflife.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# catalogueoflife.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
