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
#' Searches scientific name usages across the Catalogue of Life (COL),
#' the most comprehensive taxonomic database. Returns matching taxa with
#' their full Linnaean classification. Use this for species discovery,
#' then \code{col_taxon()} to retrieve detailed information for a specific
#' taxon. Results may include accepted names and synonyms across all
#' kingdoms (Animalia, Plantae, Fungi, etc.).
#'
#' @param query Character. Scientific name to search for, e.g.
#'   \code{"Panthera"} (genus), \code{"Quercus robur"} (species),
#'   \code{"Felidae"} (family). Partial matches are supported.
#' @param limit Integer. Maximum results to return, default \code{50},
#'   maximum \code{1000}.
#' @param dataset_key Integer. COL dataset key. Default \code{3} (the COL
#'   working project, which is the most comprehensive). Other values select
#'   specific source datasets.
#' @return A tibble with one row per matching name usage:
#'   \describe{
#'     \item{id}{\code{character} -- COL taxon ID (alphanumeric, use with \code{col_taxon()})}
#'     \item{name}{\code{character} -- Scientific name (e.g. "Panthera leo")}
#'     \item{authorship}{\code{character} -- Taxonomic authority (e.g. "(Linnaeus, 1758)")}
#'     \item{rank}{\code{character} -- Taxonomic rank ("kingdom", "phylum", "class", "order", "family", "genus", "species", "subspecies")}
#'     \item{status}{\code{character} -- Name status ("accepted", "synonym", "provisionally accepted")}
#'     \item{family}{\code{character} -- Family name from classification (e.g. "Felidae")}
#'     \item{order}{\code{character} -- Order name (e.g. "Carnivora")}
#'     \item{class}{\code{character} -- Class name (e.g. "Mammalia")}
#'     \item{phylum}{\code{character} -- Phylum name (e.g. "Chordata")}
#'     \item{kingdom}{\code{character} -- Kingdom name (e.g. "Animalia", "Plantae")}
#'   }
#' @examples
#' \dontrun{
#' # Search for big cats
#' col_search("Panthera", limit = 10)
#'
#' # Find oak species
#' col_search("Quercus") |> dplyr::filter(rank == "species", status == "accepted")
#'
#' # Search for a specific species
#' col_search("Homo sapiens", limit = 5)
#' }
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

#' Fetch detailed information for a single taxon
#'
#' Returns comprehensive metadata for one taxon identified by its COL ID.
#' Includes extinction status, parent taxon ID for tree traversal, and
#' full Linnaean classification. Use \code{col_search()} first to find
#' taxon IDs, then this function to drill into details.
#'
#' @param id Character. COL taxon ID, an alphanumeric string obtained from
#'   \code{col_search()$id} (e.g. \code{"HsuBgskqlr9aA"}).
#' @param dataset_key Integer. COL dataset key, default \code{3} (working
#'   project). Must match the dataset used in \code{col_search()}.
#' @return A single-row tibble:
#'   \describe{
#'     \item{id}{\code{character} -- COL taxon ID}
#'     \item{name}{\code{character} -- Scientific name}
#'     \item{authorship}{\code{character} -- Taxonomic authority}
#'     \item{rank}{\code{character} -- Taxonomic rank (e.g. "genus", "species")}
#'     \item{status}{\code{character} -- Name status ("accepted", "synonym")}
#'     \item{extinct}{\code{logical} -- TRUE if the taxon is extinct}
#'     \item{parent_id}{\code{character} -- ID of the parent taxon (for traversing the tree)}
#'     \item{family}{\code{character} -- Family classification (may be NA for higher ranks)}
#'     \item{order}{\code{character} -- Order classification}
#'     \item{class}{\code{character} -- Class classification}
#'     \item{phylum}{\code{character} -- Phylum classification}
#'     \item{kingdom}{\code{character} -- Kingdom classification}
#'   }
#' @examples
#' \dontrun{
#' # Look up a taxon found via search
#' results <- col_search("Panthera leo")
#' col_taxon(results$id[1])
#'
#' # Check if a taxon is extinct
#' col_taxon("HsuBgskqlr9aA")$extinct
#' }
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

#' Get api.catalogueoflife.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.catalogueoflife.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.catalogueoflife.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.catalogueoflife.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.catalogueoflife.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
