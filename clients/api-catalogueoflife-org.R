# api-catalogueoflife-org.R
# Self-contained Catalogue of Life API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.col_base <- "https://api.catalogueoflife.org"
.col_dataset <- 3  # COL working project (most comprehensive)

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
#' Searches name usages in the COL working project by scientific name.
#' Returns matching taxa with their classification.
#'
#' @param query Search term (scientific name, e.g. "Panthera", "Quercus")
#' @param limit Maximum results to return (default 50, max 1000)
#' @param dataset_key COL dataset key (default 3, the working project)
#' @return tibble: id, name, authorship, rank, status, family, order,
#'   class, phylum, kingdom
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
#' Returns detailed information for a specific taxon by its COL ID.
#'
#' @param id COL taxon ID (from col_search results)
#' @param dataset_key COL dataset key (default 3, the working project)
#' @return tibble: id, name, authorship, rank, status, extinct, parent_id,
#'   family, order, class, phylum, kingdom
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

#' Show Catalogue of Life package context for LLM integration
#'
#' Prints a summary of all public functions, their signatures, and
#' roxygen documentation. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
col_context <- function() {
  header <- c(
    "# api.catalogueoflife.org - Catalogue of Life API Client",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Dataset 3 is the COL working project (most comprehensive).",
    "# Common ranks: kingdom, phylum, class, order, family, genus, species",
    "# Status values: accepted, synonym, ambiguous synonym, misapplied"
  )
  .build_context("api.catalogueoflife.org", header_lines = header)
}
