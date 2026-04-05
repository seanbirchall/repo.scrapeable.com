# data.rcsb.org.R - Self-contained data.rcsb.org client



# data-rcsb-org.R
# Self-contained RCSB Protein Data Bank client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pdb_base <- "https://data.rcsb.org/rest/v1"
.pdb_search_base <- "https://search.rcsb.org/rcsbsearch/v2/query"

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

.schema_entry <- tibble(
  pdb_id = character(), title = character(), method = character(),
  resolution = numeric(), deposit_date = as.Date(character()),
  release_date = as.Date(character()), organism = character(),
  authors = character(), citation_title = character()
)

.schema_search <- tibble(
  pdb_id = character(), score = numeric()
)

# == Public functions ==========================================================


#' Get PDB entry metadata
#'
#' Retrieve detailed metadata for a macromolecular structure deposited
#' in the RCSB Protein Data Bank, including experimental method,
#' resolution, deposition dates, authors, and primary citation.
#'
#' @details
#' The PDB ID is a four-character alphanumeric code (case-insensitive).
#' Classic examples include \code{"4HHB"} (hemoglobin), \code{"1BNA"}
#' (B-DNA), and \code{"6LU7"} (SARS-CoV-2 main protease). Resolution
#' is reported in angstroms for diffraction methods; it may be \code{NA}
#' for NMR or cryo-EM structures without a reported value.
#'
#' @param pdb_id Character. Four-character PDB identifier.
#' @return A single-row tibble with columns:
#' \describe{
#'   \item{pdb_id}{Character. PDB identifier (upper-case).}
#'   \item{title}{Character. Structure title.}
#'   \item{method}{Character. Experimental method (e.g. \code{"X-RAY DIFFRACTION"}).}
#'   \item{resolution}{Numeric. Resolution in angstroms, or \code{NA}.}
#'   \item{deposit_date}{Date. Deposition date.}
#'   \item{release_date}{Date. Initial release date.}
#'   \item{organism}{Character. Source organism (may be \code{NA}).}
#'   \item{authors}{Character. Semicolon-separated author list.}
#'   \item{citation_title}{Character. Title of the primary citation.}
#' }
#' @export
#' @seealso \code{\link{pdb_search}}, \code{\link{pdb_polymer}},
#'   \code{\link{pdb_search_enriched}}
#' @examples
#' pdb_entry("4HHB")
#' pdb_entry("6LU7")
pdb_entry <- function(pdb_id) {
  pdb_id <- toupper(pdb_id)
  url <- sprintf("%s/core/entry/%s", .pdb_base, pdb_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_entry)

  struct <- raw$struct %||% list()
  exptl <- raw$exptl
  method <- if (!is.null(exptl)) {
    if (is.data.frame(exptl)) exptl$method[1] else if (is.list(exptl)) exptl[[1]]$method else NA_character_
  } else NA_character_

  refine <- raw$refine
  resolution <- if (!is.null(refine) && is.data.frame(refine)) {
    res_col <- intersect(c("ls_dres_high", "ls_d_res_high"), names(refine))
    if (length(res_col) > 0) suppressWarnings(as.numeric(refine[[res_col[1]]][1]))
    else NA_real_
  } else NA_real_

  rcsb <- raw$rcsb_accession_info %||% list()

  authors_str <- tryCatch({
    au <- raw$audit_author
    if (!is.null(au) && is.data.frame(au)) paste(au$name, collapse = "; ")
    else NA_character_
  }, error = function(e) NA_character_)

  citation <- raw$rcsb_primary_citation %||% list()

  tibble(
    pdb_id = pdb_id,
    title = as.character(struct$title %||% NA_character_),
    method = as.character(method),
    resolution = resolution,
    deposit_date = tryCatch(as.Date(rcsb$deposit_date), error = function(e) as.Date(NA)),
    release_date = tryCatch(as.Date(rcsb$initial_release_date), error = function(e) as.Date(NA)),
    organism = NA_character_,
    authors = as.character(authors_str),
    citation_title = as.character(citation$title %||% NA_character_)
  )
}

#' Search PDB structures by keyword
#'
#' Full-text search of the RCSB Protein Data Bank. Returns PDB IDs
#' ranked by relevance score.
#'
#' @details
#' Uses the RCSB Search API v2 with the \code{full_text} service.
#' The search covers titles, abstracts, author names, organism names,
#' and other metadata fields. For richer results including titles and
#' methods, use \code{\link{pdb_search_enriched}} instead.
#'
#' @param query Character. Search text (e.g. \code{"insulin"},
#'   \code{"CRISPR"}, \code{"G-protein coupled receptor"}).
#' @param rows Integer. Number of results (default 10).
#' @return A tibble with columns:
#' \describe{
#'   \item{pdb_id}{Character. Four-character PDB identifier.}
#'   \item{score}{Numeric. Relevance score (higher = more relevant).}
#' }
#' @export
#' @seealso \code{\link{pdb_entry}}, \code{\link{pdb_search_enriched}}
#' @examples
#' pdb_search("insulin", rows = 5)
pdb_search <- function(query, rows = 10) {
  search_body <- list(
    query = list(
      type = "terminal",
      service = "full_text",
      parameters = list(value = query)
    ),
    return_type = "entry",
    request_options = list(
      paginate = list(start = 0, rows = as.integer(rows))
    )
  )

  json_body <- jsonlite::toJSON(search_body, auto_unbox = TRUE)

  tmp <- tempfile(fileext = ".json")
  httr2::request(.pdb_search_base) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_raw(json_body, type = "application/json") |>
    httr2::req_perform(path = tmp)

  raw <- jsonlite::fromJSON(tmp)
  results <- raw$result_set
  if (is.null(results) || length(results) == 0) return(.schema_search)
  if (!is.data.frame(results)) return(.schema_search)

  as_tibble(results) |>
    transmute(
      pdb_id = as.character(identifier),
      score = as.numeric(score)
    )
}

#' Get polymer entity information for a PDB entry
#'
#' Retrieve information about each polymer entity (protein, DNA, or RNA
#' chain) in a PDB structure, including sequence length, organism, and
#' chain identifiers.
#'
#' @details
#' Each polymer entity represents a distinct macromolecular sequence.
#' A homodimer has one entity mapped to two chain IDs. The function
#' iterates entity IDs 1--20, stopping when no more are found, so it
#' works for structures with up to 20 unique polymer entities.
#'
#' @param pdb_id Character. Four-character PDB identifier (e.g. \code{"4HHB"}).
#' @return A tibble with columns:
#' \describe{
#'   \item{pdb_id}{Character. PDB identifier.}
#'   \item{entity_id}{Character. Entity number within the structure.}
#'   \item{type}{Character. Polymer type (e.g. \code{"polypeptide(L)"}).}
#'   \item{description}{Character. Entity description.}
#'   \item{organism}{Character. Source organism scientific name.}
#'   \item{sequence_length}{Integer. Length of the canonical sequence.}
#'   \item{chain_ids}{Character. Comma-separated chain identifiers.}
#' }
#' @export
#' @seealso \code{\link{pdb_entry}}, \code{\link{pdb_search}}
#' @examples
#' pdb_polymer("4HHB")
pdb_polymer <- function(pdb_id) {
  pdb_id <- toupper(pdb_id)
  url <- sprintf("%s/core/entry/%s", .pdb_base, pdb_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(pdb_id = character(), entity_id = character(),
                                   type = character(), description = character()))

  # Get polymer entities
  entity_url <- sprintf("https://data.rcsb.org/rest/v1/core/polymer_entity/%s/1", pdb_id)
  # Try to get all entities
  entities <- list()
  for (i in 1:20) {
    eurl <- sprintf("https://data.rcsb.org/rest/v1/core/polymer_entity/%s/%d", pdb_id, i)
    ent <- tryCatch(.fetch_json(eurl), error = function(e) NULL)
    if (is.null(ent)) break
    entities[[i]] <- ent
  }

  if (length(entities) == 0) return(tibble(pdb_id = character(), entity_id = character(),
                                            type = character(), description = character()))

  rows <- lapply(seq_along(entities), function(i) {
    e <- entities[[i]]
    ep <- e$entity_poly %||% list()
    src <- e$rcsb_entity_source_organism
    org <- if (!is.null(src) && length(src) > 0) {
      if (is.data.frame(src)) src$ncbi_scientific_name[1]
      else src[[1]]$ncbi_scientific_name
    } else NA_character_

    tibble(
      pdb_id = pdb_id,
      entity_id = as.character(i),
      type = as.character(ep$type %||% e$rcsb_polymer_entity$type %||% NA_character_),
      description = as.character(e$rcsb_polymer_entity$pdbx_description %||% NA_character_),
      organism = as.character(org %||% NA_character_),
      sequence_length = as.integer(nchar(ep$pdbx_seq_one_letter_code_can %||% "") %||% NA_integer_),
      chain_ids = as.character(paste(ep$pdbx_strand_id %||% "", collapse = ","))
    )
  })
  bind_rows(rows)
}

#' Search PDB with enriched metadata
#'
#' Convenience wrapper that searches the PDB and then fetches full entry
#' metadata for each hit, returning a single combined tibble.
#'
#' @details
#' This function calls \code{\link{pdb_search}} followed by
#' \code{\link{pdb_entry}} for each result. Because it makes \code{rows + 1}
#' API calls, keep \code{rows} small (5--10) to avoid rate-limiting.
#' The result includes all columns from \code{pdb_entry} plus the
#' relevance \code{score} from the search.
#'
#' @param query Character. Search text (e.g. \code{"insulin"}, \code{"CRISPR"}).
#' @param rows Integer. Number of results (default 5).
#' @return A tibble with columns: \code{pdb_id}, \code{score}, \code{title},
#'   \code{method}, \code{resolution}, and all other \code{pdb_entry} columns.
#' @export
#' @seealso \code{\link{pdb_search}}, \code{\link{pdb_entry}}
#' @examples
#' pdb_search_enriched("hemoglobin", rows = 3)
pdb_search_enriched <- function(query, rows = 5) {
  search_results <- pdb_search(query, rows = rows)
  if (nrow(search_results) == 0) return(tibble(pdb_id = character(), score = numeric(),
                                                 title = character(), method = character(),
                                                 resolution = numeric()))

  entries <- lapply(search_results$pdb_id, function(id) {
    entry <- tryCatch(pdb_entry(id), error = function(e) NULL)
    if (is.null(entry) || nrow(entry) == 0) return(NULL)
    entry |> mutate(score = search_results$score[search_results$pdb_id == id])
  })
  entries <- entries[!vapply(entries, is.null, logical(1))]
  if (length(entries) == 0) return(tibble(pdb_id = character(), score = numeric(),
                                           title = character()))
  bind_rows(entries) |> select(pdb_id, score, title, method, resolution, everything())
}

# == Context ===================================================================

#' Get rcsb.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
pdb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pdb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/rcsb.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "rcsb.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# rcsb.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# rcsb.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
