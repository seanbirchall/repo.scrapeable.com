#' Get PDB entry metadata
#'
#' Returns metadata for a specific PDB entry including structure title,
#' experimental method, resolution, dates, and citation.
#'
#' @param pdb_id PDB ID (4-character, e.g., "4HHB" for hemoglobin)
#' @return tibble: pdb_id, title, method, resolution, deposit_date,
#'   release_date, organism, authors, citation_title
#' @export
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

#' Search PDB structures
#'
#' Searches the RCSB PDB using full-text search.
#'
#' @param query Search query string (e.g., "insulin", "CRISPR")
#' @param rows Number of results. Default 10.
#' @return tibble: pdb_id, score
#' @export
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

#' Print PDB context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
pdb_context <- function() {
  .build_context(
    pkg_name = "data.rcsb.org",
    header_lines = c(
      "# Package: data.rcsb.org",
      "# RCSB Protein Data Bank REST API",
      "# Auth: none",
      "# Rate limits: none documented",
      "#",
      "# 200K+ macromolecular structures (X-ray, cryo-EM, NMR)",
      "# PDB IDs are 4-character codes: 4HHB (hemoglobin),",
      "#   1BNA (DNA), 6LU7 (SARS-CoV-2 main protease)"
    )
  )
}
