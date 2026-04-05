# nih.gov.R
# Self-contained NIH client.
# Covers: NIH RePORTER (grants), NCBI (Gene/PubMed), PubChem (compounds),
# and RxNav/RxNorm (drug information).
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.nih_base <- "https://api.reporter.nih.gov/v2"
.ncbi_base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
.pubchem_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
.rx_base <- "https://rxnav.nlm.nih.gov/REST"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
.fetch_json_nested <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)


# == Schemas ===================================================================

.schema_projects <- tibble(
  project_num = character(), title = character(), pi_name = character(),
  org_name = character(), fiscal_year = integer(),
  award_amount = numeric(), agency = character()
)

.schema_pubs <- tibble(
  pmid = character(), title = character(), journal = character(),
  pub_year = integer(), project_num = character()
)

.schema_search <- tibble(id = character(), count = integer())

.schema_summary <- tibble(uid = character(), name = character(), description = character())

.schema_compound <- tibble(
  cid = integer(), iupac_name = character(), molecular_formula = character(),
  molecular_weight = numeric(), inchi = character(),
  canonical_smiles = character()
)

.schema_properties <- tibble(
  cid = integer(), property = character(), value = character()
)

.schema_synonyms <- tibble(
  cid = integer(), synonym = character()
)

.schema_concepts <- tibble(
  rxcui = character(), name = character(), tty = character(),
  synonym = character()
)

.schema_rxcui <- tibble(
  rxcui = character()
)

.schema_suggestions <- tibble(
  suggestion = character()
)

.schema_classes <- tibble(
  class_id = character(), class_name = character(), class_type = character(),
  rxcui = character(), drug_name = character()
)




#' Search NIH-funded research projects
#'
#' Searches the NIH RePORTER database for funded research projects.
#' Multiple filter criteria can be combined. The NIH funds over $40 billion
#' annually in biomedical research.
#'
#' @param text Character or NULL. Full-text search across project title and
#'   abstract.
#' @param pi_names Character vector or NULL. Principal investigator last names.
#' @param org_names Character vector or NULL. Organization/institution names.
#' @param fiscal_years Integer vector or NULL. Fiscal years to filter
#'   (e.g. \code{c(2023, 2024)}).
#' @param agencies Character vector or NULL. NIH institute codes
#'   (e.g. \code{"NCI"}, \code{"NIAID"}, \code{"NHLBI"}).
#' @param limit Integer. Max results (default 50, max 500).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{project_num}{Character. NIH project number.}
#'     \item{title}{Character. Project title.}
#'     \item{pi_name}{Character. Semicolon-separated PI names.}
#'     \item{org_name}{Character. Grantee organization.}
#'     \item{fiscal_year}{Integer. Fiscal year.}
#'     \item{award_amount}{Numeric. Award amount in USD.}
#'     \item{agency}{Character. Funding NIH institute abbreviation.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nih_projects(text = "CRISPR", limit = 10)
#' nih_projects(agencies = "NCI", fiscal_years = 2024)
#' }
nih_projects <- function(text = NULL, pi_names = NULL, org_names = NULL,
                         fiscal_years = NULL, agencies = NULL,
                         limit = 50, offset = 0) {
  criteria <- list()
  if (!is.null(text))         criteria$advanced_text_search <- list(operator = "and", search_field = "projecttitle,terms", search_text = text)
  if (!is.null(pi_names))     criteria$pi_names <- lapply(pi_names, function(n) list(last_name = n, any_name = n))
  if (!is.null(org_names))    criteria$org_names <- as.list(org_names)
  if (!is.null(fiscal_years)) criteria$fiscal_years <- as.list(as.integer(fiscal_years))
  if (!is.null(agencies))     criteria$agencies <- as.list(agencies)

  body <- list(criteria = criteria, limit = limit, offset = offset)

  resp <- httr2::request(paste0(.nih_base, "/projects/search")) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_projects)

  tibble(
    project_num  = vapply(results, function(r) r$project_num %||% NA_character_, character(1)),
    title        = vapply(results, function(r) r$project_title %||% NA_character_, character(1)),
    pi_name      = vapply(results, function(r) {
      pis <- r$principal_investigators
      if (is.null(pis) || length(pis) == 0) NA_character_
      else paste(vapply(pis, function(p) p$full_name %||% "", character(1)), collapse = "; ")
    }, character(1)),
    org_name     = vapply(results, function(r) r$organization$org_name %||% NA_character_, character(1)),
    fiscal_year  = vapply(results, function(r) as.integer(r$fiscal_year %||% NA_integer_), integer(1)),
    award_amount = vapply(results, function(r) as.numeric(r$award_amount %||% NA_real_), numeric(1)),
    agency       = vapply(results, function(r) r$agency_ic_fundings[[1]]$abbreviation %||% r$agency_code %||% NA_character_, character(1))
  )
}


#' Search publications linked to NIH grants
#'
#' Searches the NIH RePORTER publications database for peer-reviewed
#' articles linked to NIH-funded research projects.
#'
#' @param text Character or NULL. Search text for title/abstract.
#' @param project_nums Character vector or NULL. NIH project numbers to filter by.
#' @param limit Integer. Max results (default 50).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{pmid}{Character. PubMed ID.}
#'     \item{title}{Character. Article title.}
#'     \item{journal}{Character. Journal name.}
#'     \item{pub_year}{Integer. Publication year.}
#'     \item{project_num}{Character. Associated NIH project number.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nih_publications(text = "CRISPR")
#' }
nih_publications <- function(text = NULL, project_nums = NULL,
                             limit = 50, offset = 0) {
  criteria <- list()
  if (!is.null(text))         criteria$advanced_text_search <- list(operator = "and", search_field = "title,abstract", search_text = text)
  if (!is.null(project_nums)) criteria$project_nums <- as.list(project_nums)

  body <- list(criteria = criteria, limit = limit, offset = offset)

  resp <- httr2::request(paste0(.nih_base, "/publications/search")) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pubs)

  tibble(
    pmid        = vapply(results, function(r) as.character(r$pmid %||% NA_character_), character(1)),
    title       = vapply(results, function(r) r$title %||% NA_character_, character(1)),
    journal     = vapply(results, function(r) r$journal %||% NA_character_, character(1)),
    pub_year    = vapply(results, function(r) as.integer(r$pub_year %||% NA_integer_), integer(1)),
    project_num = vapply(results, function(r) {
      cnums <- r$coreproject
      if (is.null(cnums)) NA_character_ else as.character(cnums)
    }, character(1))
  )
}


#' Get detailed information about a specific NIH project
#'
#' Retrieves full metadata for a single NIH-funded project including
#' the abstract, PI information, and project dates.
#'
#' @param project_num Character. NIH project number
#'   (e.g. \code{"5R01CA123456-05"}).
#' @return A tibble with one row and columns: project_num, title,
#'   abstract, pi_name, org_name, fiscal_year, award_amount,
#'   start_date, end_date.
#' @export
#' @examples
#' \dontrun{
#' nih_project_detail("5R01CA279801-04")
#' }
nih_project_detail <- function(project_num) {
  schema <- tibble(project_num = character(), title = character(),
                   abstract = character(), pi_name = character(),
                   org_name = character(), fiscal_year = integer(),
                   award_amount = numeric(), start_date = character(),
                   end_date = character())

  body <- list(
    criteria = list(project_nums = list(project_num)),
    limit = 1, offset = 0
  )

  resp <- tryCatch({
    httr2::request(paste0(.nih_base, "/projects/search")) |>
      httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_perform()
  }, error = function(e) NULL)
  if (is.null(resp)) return(schema)

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(schema)

  r <- results[[1]]
  tibble(
    project_num = as.character(r$project_num %||% NA_character_),
    title = as.character(r$project_title %||% NA_character_),
    abstract = as.character(r$abstract_text %||% NA_character_),
    pi_name = {
      pis <- r$principal_investigators
      if (is.null(pis) || length(pis) == 0) NA_character_
      else paste(vapply(pis, function(p) p$full_name %||% "", character(1)), collapse = "; ")
    },
    org_name = as.character(r$organization$org_name %||% NA_character_),
    fiscal_year = as.integer(r$fiscal_year %||% NA_integer_),
    award_amount = as.numeric(r$award_amount %||% NA_real_),
    start_date = as.character(r$project_start_date %||% NA_character_),
    end_date = as.character(r$project_end_date %||% NA_character_)
  )
}


#' Get NIH spending by institute/center
#'
#' Returns project counts by NIH institute/center for a fiscal year.
#' Queries the 20 major NIH institutes to provide a spending overview.
#'
#' @param fiscal_year Integer. Fiscal year (default current year).
#' @param limit Integer. Max results (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{agency}{Character. NIH institute abbreviation.}
#'     \item{total_funding}{Numeric. Currently NA (API limitation).}
#'     \item{project_count}{Integer. Number of funded projects.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nih_spending(2024)
#' }
nih_spending <- function(fiscal_year = as.integer(format(Sys.Date(), "%Y")), limit = 100) {
  schema <- tibble(agency = character(), total_funding = numeric(), project_count = integer())

  # Search for all projects in the fiscal year, grouped by agency
  agencies <- c("NCI", "NIAID", "NHLBI", "NIGMS", "NINDS", "NIDDK", "NIA",
                "NIMH", "NHGRI", "NEI", "NIBIB", "NIDCR", "NIEHS", "NIDCD",
                "NINR", "NICHD", "NIAAA", "NIDA", "NCATS", "NCCIH")

  rows <- lapply(agencies, function(ag) {
    tryCatch({
      body <- list(
        criteria = list(
          fiscal_years = list(as.integer(fiscal_year)),
          agencies = list(ag)
        ),
        limit = 1, offset = 0
      )
      resp <- httr2::request(paste0(.nih_base, "/projects/search")) |>
        httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
        httr2::req_body_json(body) |>
        httr2::req_perform()

      tmp <- tempfile(fileext = ".json")
      writeLines(httr2::resp_body_string(resp), tmp)
      raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

      meta <- raw$meta
      total <- meta$total %||% 0
      funding <- if (total > 0 && length(raw$results) > 0) {
        as.numeric(meta$total %||% 0)
      } else 0

      tibble(agency = ag, total_funding = NA_real_, project_count = as.integer(total))
    }, error = function(e) NULL)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(schema)
  bind_rows(rows) |> arrange(desc(project_count))
}



#' Search NCBI databases
#'
#' Searches any NCBI Entrez database (gene, pubmed, protein, nucleotide, etc.)
#' and returns matching record IDs. Use \code{\link{ncbi_summary}} to fetch
#' details for the returned IDs.
#'
#' @param db Character. Database name: \code{"gene"}, \code{"pubmed"},
#'   \code{"protein"}, \code{"nucleotide"}, etc.
#' @param term Character. Search term (e.g. \code{"BRCA1"},
#'   \code{"cancer AND 2024[pdat]"}).
#' @param retmax Integer. Max results (default 20, max 10000).
#' @param api_key Character or NULL. Optional NCBI API key
#'   (raises rate limit from 3/sec to 10/sec).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. NCBI record identifier.}
#'     \item{count}{Integer. Total number of matches in the database.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ncbi_search("gene", "BRCA1")
#' ncbi_search("pubmed", "CRISPR AND 2024[pdat]", retmax = 50)
#' }
ncbi_search <- function(db, term, retmax = 20, api_key = NULL) {
  url <- sprintf("%s/esearch.fcgi?db=%s&term=%s&retmode=json&retmax=%d",
                 .ncbi_base, db, utils::URLencode(term, reserved = TRUE), retmax)
  if (!is.null(api_key)) url <- paste0(url, "&api_key=", api_key)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("NCBI error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_search)
  result <- raw$esearchresult
  ids <- result$idlist
  if (is.null(ids) || length(ids) == 0) return(tibble(id = character(), count = as.integer(result$count %||% 0L)))
  tibble(id = as.character(ids), count = as.integer(result$count %||% length(ids)))
}


#' Get summaries for NCBI IDs
#'
#' Fetches document summaries from NCBI for a set of record IDs,
#' typically obtained from \code{\link{ncbi_search}}.
#'
#' @param db Character. Database name (must match the database used in search).
#' @param ids Character vector. NCBI record IDs from \code{\link{ncbi_search}}.
#' @param api_key Character or NULL. Optional NCBI API key.
#' @return A tibble with columns: uid, name, description.
#'   Content varies by database (gene returns gene symbols, pubmed returns
#'   article titles, etc.).
#' @export
ncbi_summary <- function(db, ids, api_key = NULL) {
  id_str <- paste(ids, collapse = ",")
  url <- sprintf("%s/esummary.fcgi?db=%s&id=%s&retmode=json", .ncbi_base, db, id_str)
  if (!is.null(api_key)) url <- paste0(url, "&api_key=", api_key)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("NCBI error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_summary)
  result <- raw$result
  uids <- result$uids
  if (is.null(uids) || length(uids) == 0) return(.schema_summary)
  entries <- lapply(uids, function(uid) {
    r <- result[[uid]]
    if (is.null(r)) return(NULL)
    tibble(uid = as.character(uid),
           name = as.character(r$name %||% r$title %||% NA_character_),
           description = as.character(r$description %||% r$fulljournalname %||% NA_character_))
  })
  bind_rows(entries)
}


#' Search PubMed articles
#'
#' Convenience wrapper that searches PubMed and returns article summaries
#' in a single call. Combines \code{\link{ncbi_search}} and
#' \code{\link{ncbi_summary}}.
#'
#' @param query Character. PubMed search query
#'   (e.g. \code{"CRISPR AND 2024[pdat]"}).
#' @param retmax Integer. Max results (default 20).
#' @param api_key Character or NULL. Optional NCBI API key.
#' @return A tibble with columns: uid (PubMed ID), name (article title),
#'   description (journal name).
#' @export
#' @examples
#' \dontrun{
#' ncbi_pubmed("CRISPR")
#' }
ncbi_pubmed <- function(query, retmax = 20, api_key = NULL) {
  ids <- ncbi_search("pubmed", query, retmax = retmax, api_key = api_key)
  if (nrow(ids) == 0) return(.schema_summary)
  ncbi_summary("pubmed", ids$id, api_key = api_key)
}


#' Search NCBI genes
#'
#' Convenience wrapper that searches the NCBI Gene database and returns
#' gene summaries. Combines \code{\link{ncbi_search}} and
#' \code{\link{ncbi_summary}}.
#'
#' @param query Character. Gene name or symbol (e.g. \code{"BRCA1"},
#'   \code{"TP53"}).
#' @param retmax Integer. Max results (default 10).
#' @param api_key Character or NULL. Optional NCBI API key.
#' @return A tibble with columns: uid (Gene ID), name (gene symbol),
#'   description (gene description).
#' @export
#' @examples
#' \dontrun{
#' ncbi_genes("BRCA1")
#' ncbi_genes("TP53")
#' }
ncbi_genes <- function(query, retmax = 10, api_key = NULL) {
  ids <- ncbi_search("gene", query, retmax = retmax, api_key = api_key)
  if (nrow(ids) == 0) return(.schema_summary)
  ncbi_summary("gene", ids$id, api_key = api_key)
}



#' Look up a compound by name in PubChem
#'
#' Returns key chemical properties for a compound identified by common name.
#' Uses the PubChem REST (PUG) API.
#'
#' @param name Character. Compound name (e.g. \code{"aspirin"},
#'   \code{"caffeine"}, \code{"glucose"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{cid}{Integer. PubChem Compound ID.}
#'     \item{iupac_name}{Character. IUPAC systematic name.}
#'     \item{molecular_formula}{Character. Molecular formula.}
#'     \item{molecular_weight}{Numeric. Molecular weight in g/mol.}
#'     \item{inchi}{Character. InChI identifier.}
#'     \item{canonical_smiles}{Character. Canonical SMILES notation.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' pubchem_compound("aspirin")
#' pubchem_compound("caffeine")
#' }
pubchem_compound <- function(name) {
  url <- sprintf(
    "%s/compound/name/%s/property/IUPACName,MolecularFormula,MolecularWeight,InChI,CanonicalSMILES/JSON",
    .pubchem_base, utils::URLencode(name, reserved = TRUE)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_compound)

  props <- raw$PropertyTable$Properties
  if (is.null(props) || nrow(props) == 0) return(.schema_compound)

  nms <- names(props)
  as_tibble(props) |>
    transmute(
      cid = as.integer(CID),
      iupac_name = as.character(if ("IUPACName" %in% nms) IUPACName else NA_character_),
      molecular_formula = as.character(if ("MolecularFormula" %in% nms) MolecularFormula else NA_character_),
      molecular_weight = as.numeric(if ("MolecularWeight" %in% nms) MolecularWeight else NA_real_),
      inchi = as.character(if ("InChI" %in% nms) InChI else NA_character_),
      canonical_smiles = as.character(if ("CanonicalSMILES" %in% nms) CanonicalSMILES else NA_character_)
    )
}


#' Get specific properties for a compound
#'
#' Returns computed molecular properties from PubChem in long (tidy) format.
#'
#' @param name Character. Compound name (e.g. \code{"aspirin"}).
#' @param properties Character. Comma-separated property names. Default
#'   includes MolecularFormula, MolecularWeight, XLogP, ExactMass,
#'   MonoisotopicMass, TPSA, Complexity, HBondDonorCount,
#'   HBondAcceptorCount, and RotatableBondCount.
#' @return A tibble with columns: cid (integer), property (character),
#'   value (character, in long format).
#' @export
#' @examples
#' \dontrun{
#' pubchem_properties("aspirin")
#' pubchem_properties("caffeine", "MolecularWeight,XLogP")
#' }
pubchem_properties <- function(name,
                               properties = "MolecularFormula,MolecularWeight,XLogP,ExactMass,MonoisotopicMass,TPSA,Complexity,HBondDonorCount,HBondAcceptorCount,RotatableBondCount") {
  url <- sprintf(
    "%s/compound/name/%s/property/%s/JSON",
    .pubchem_base, utils::URLencode(name, reserved = TRUE), properties
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_properties)

  props <- raw$PropertyTable$Properties
  if (is.null(props) || nrow(props) == 0) return(.schema_properties)

  # Pivot to long format
  cid <- props$CID[1]
  prop_names <- setdiff(names(props), "CID")
  rows <- lapply(prop_names, function(p) {
    tibble(
      cid = as.integer(cid),
      property = p,
      value = as.character(props[[p]][1])
    )
  })
  bind_rows(rows)
}


#' Get synonyms for a compound
#'
#' Returns all known names and synonyms for a compound from PubChem.
#'
#' @param name Character. Compound name (e.g. \code{"aspirin"}).
#' @return A tibble with columns: cid (integer), synonym (character).
#' @export
#' @examples
#' \dontrun{
#' pubchem_synonyms("aspirin")
#' }
pubchem_synonyms <- function(name) {
  url <- sprintf(
    "%s/compound/name/%s/synonyms/JSON",
    .pubchem_base, utils::URLencode(name, reserved = TRUE)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_synonyms)

  info <- raw$InformationList$Information
  if (is.null(info) || length(info) == 0) return(.schema_synonyms)

  cid <- info[[1]]$CID %||% info$CID[1]
  syns <- info[[1]]$Synonym %||% info$Synonym[[1]]
  if (is.null(syns)) return(.schema_synonyms)

  tibble(
    cid = as.integer(cid),
    synonym = as.character(syns)
  )
}


#' Search PubChem compounds by text query
#'
#' Searches PubChem for compounds matching a name or text query and
#' returns basic chemical properties for each match.
#'
#' @param query Character. Search text (e.g. \code{"aspirin"},
#'   \code{"anti-inflammatory"}).
#' @param max_results Integer. Maximum number of results (default 10).
#' @return A tibble with columns: cid (integer), iupac_name (character),
#'   molecular_formula (character), molecular_weight (numeric).
#' @export
#' @examples
#' \dontrun{
#' pubchem_search("aspirin")
#' }
pubchem_search <- function(query, max_results = 10) {
  # Step 1: get CIDs from autocomplete/search
  url <- sprintf(
    "%s/compound/name/%s/cids/JSON",
    .pubchem_base, utils::URLencode(query, reserved = TRUE)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_compound[, c("cid", "iupac_name", "molecular_formula", "molecular_weight")])

  cids <- raw$IdentifierList$CID
  if (is.null(cids) || length(cids) == 0) return(.schema_compound[, c("cid", "iupac_name", "molecular_formula", "molecular_weight")])
  cids <- head(cids, max_results)

  # Step 2: get properties for those CIDs
  cid_str <- paste(cids, collapse = ",")
  prop_url <- sprintf(
    "%s/compound/cid/%s/property/IUPACName,MolecularFormula,MolecularWeight/JSON",
    .pubchem_base, cid_str
  )
  prop_raw <- tryCatch(.fetch_json(prop_url), error = function(e) NULL)
  if (is.null(prop_raw)) return(tibble(cid = as.integer(cids), iupac_name = NA_character_,
                                        molecular_formula = NA_character_, molecular_weight = NA_real_))

  props <- prop_raw$PropertyTable$Properties
  if (is.null(props) || nrow(props) == 0) return(tibble(cid = as.integer(cids)))

  nms <- names(props)
  as_tibble(props) |>
    transmute(
      cid = as.integer(CID),
      iupac_name = as.character(if ("IUPACName" %in% nms) IUPACName else NA_character_),
      molecular_formula = as.character(if ("MolecularFormula" %in% nms) MolecularFormula else NA_character_),
      molecular_weight = as.numeric(if ("MolecularWeight" %in% nms) MolecularWeight else NA_real_)
    )
}


#' Get bioactivity/assay data for a compound
#'
#' Returns bioassay results from the PubChem BioAssay database for a
#' given compound. Shows which biological targets a compound has been
#' tested against and the activity outcomes.
#'
#' @param cid Integer. PubChem Compound ID (obtain from
#'   \code{\link{pubchem_compound}} or \code{\link{pubchem_search}}).
#' @param max_results Integer. Maximum assays to return (default 20).
#' @return A tibble with columns: aid (integer), assay_name (character),
#'   activity_outcome (character), target_name (character).
#' @export
#' @examples
#' \dontrun{
#' pubchem_assays(2244)
#' }
pubchem_assays <- function(cid, max_results = 20) {
  schema <- tibble(aid = integer(), assay_name = character(),
                   activity_outcome = character(), target_name = character())

  # Get assay IDs linked to this compound
  url <- sprintf("%s/compound/cid/%d/assaysummary/JSON", .pubchem_base, as.integer(cid))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)

  tbl <- raw$Table
  if (is.null(tbl)) return(schema)

  cols <- tbl$Columns$Column
  rows <- tbl$Row
  if (is.null(rows) || length(rows) == 0) return(schema)

  # Parse tabular response
  col_names <- if (is.data.frame(cols)) cols$Heading else vapply(cols, function(c) c$Heading %||% "", character(1))

  aid_idx <- which(col_names == "AID")[1]
  name_idx <- which(col_names == "Assay Name")[1]
  outcome_idx <- which(col_names == "Activity Outcome")[1]
  target_idx <- which(col_names == "Target Name")[1]

  parsed <- lapply(head(rows, max_results), function(row) {
    cells <- row$Cell
    get_cell <- function(idx) {
      if (is.na(idx) || idx > length(cells)) return(NA_character_)
      cells[[idx]] %||% NA_character_
    }
    tibble(
      aid = suppressWarnings(as.integer(get_cell(aid_idx))),
      assay_name = as.character(get_cell(name_idx)),
      activity_outcome = as.character(get_cell(outcome_idx)),
      target_name = as.character(get_cell(target_idx))
    )
  })
  bind_rows(parsed)
}


#' Look up RxCUI by drug name
#'
#' Returns the RxNorm Concept Unique Identifier (RxCUI) for a drug name.
#' RxCUI is the standard identifier used across RxNorm APIs.
#'
#' @param name Character. Drug name (e.g. \code{"aspirin"},
#'   \code{"metformin"}).
#' @return A tibble with column: rxcui (character).
#' @export
#' @examples
#' \dontrun{
#' rxnorm_rxcui("aspirin")
#' }
rxnorm_rxcui <- function(name) {
  url <- sprintf("%s/rxcui.json?name=%s", .rx_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_rxcui)

  ids <- raw$idGroup$rxnormId
  if (is.null(ids) || length(ids) == 0) return(.schema_rxcui)
  tibble(rxcui = as.character(ids))
}



#' Search drugs by name
#'
#' Returns all RxNorm concepts matching a drug name, including brand names,
#' clinical drugs, and dose forms grouped by term type (tty).
#'
#' @param name Character. Drug name (e.g. \code{"metformin"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{rxcui}{Character. RxNorm concept ID.}
#'     \item{name}{Character. Full drug concept name.}
#'     \item{tty}{Character. Term type (e.g. "SBD", "SCD", "BN").}
#'     \item{synonym}{Character. Synonym or brand name.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' rxnorm_drugs("metformin")
#' }
rxnorm_drugs <- function(name) {
  url <- sprintf("%s/drugs.json?name=%s", .rx_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_concepts)

  groups <- raw$drugGroup$conceptGroup
  if (is.null(groups) || length(groups) == 0) return(.schema_concepts)

  # groups may be a data.frame (simplifyVector=TRUE); iterate rows not columns
  if (is.data.frame(groups)) {
    results <- lapply(seq_len(nrow(groups)), function(i) {
      props <- groups$conceptProperties[[i]]
      if (is.null(props) || length(props) == 0) return(NULL)
      if (is.data.frame(props)) {
        tibble(
          rxcui   = as.character(props$rxcui %||% NA_character_),
          name    = as.character(props$name %||% NA_character_),
          tty     = as.character(props$tty %||% NA_character_),
          synonym = as.character(props$synonym %||% NA_character_)
        )
      } else {
        tibble(
          rxcui   = vapply(props, function(p) p$rxcui %||% NA_character_, character(1)),
          name    = vapply(props, function(p) p$name %||% NA_character_, character(1)),
          tty     = vapply(props, function(p) p$tty %||% NA_character_, character(1)),
          synonym = vapply(props, function(p) p$synonym %||% NA_character_, character(1))
        )
      }
    })
  } else {
    results <- lapply(groups, function(g) {
      props <- g$conceptProperties
      if (is.null(props) || length(props) == 0) return(NULL)
      tibble(
        rxcui   = vapply(props, function(p) p$rxcui %||% NA_character_, character(1)),
        name    = vapply(props, function(p) p$name %||% NA_character_, character(1)),
        tty     = vapply(props, function(p) p$tty %||% NA_character_, character(1)),
        synonym = vapply(props, function(p) p$synonym %||% NA_character_, character(1))
      )
    })
  }
  bind_rows(results)
}



#' Get drug properties by RxCUI
#'
#' Returns basic properties for a single RxNorm concept.
#'
#' @param rxcui Character. RxNorm concept ID (obtain from
#'   \code{\link{rxnorm_rxcui}} or \code{\link{rxnorm_drugs}}).
#' @return A tibble with one row: rxcui, name, tty, synonym.
#' @export
rxnorm_properties <- function(rxcui) {
  url <- sprintf("%s/rxcui/%s/properties.json", .rx_base, rxcui)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw) || is.null(raw$properties)) return(.schema_concepts)

  p <- raw$properties
  tibble(
    rxcui   = p$rxcui %||% NA_character_,
    name    = p$name %||% NA_character_,
    tty     = p$tty %||% NA_character_,
    synonym = p$synonym %||% NA_character_
  )
}



#' Get all related concepts for a drug
#'
#' Returns all related RxNorm concepts including brand names, clinical
#' drugs, dose forms, and ingredients. Optionally filter by term type.
#'
#' @param rxcui Character. RxNorm concept ID.
#' @param tty Character vector or NULL. Term type filter
#'   (e.g. \code{"BN"} for brand names, \code{"SCD"} for semantic
#'   clinical drugs, \code{"IN"} for ingredients). Can be a vector.
#' @return A tibble with columns: rxcui, name, tty, synonym.
#' @export
#' @examples
#' \dontrun{
#' rxnorm_related("161", tty = "BN")
#' }
rxnorm_related <- function(rxcui, tty = NULL) {
  if (!is.null(tty)) {
    tty_str <- paste(tty, collapse = "+")
    url <- sprintf("%s/rxcui/%s/related.json?tty=%s", .rx_base, rxcui, tty_str)
  } else {
    url <- sprintf("%s/rxcui/%s/allrelated.json", .rx_base, rxcui)
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_concepts)

  groups <- raw$allRelatedGroup$conceptGroup %||% raw$relatedGroup$conceptGroup
  if (is.null(groups) || length(groups) == 0) return(.schema_concepts)

  results <- lapply(groups, function(g) {
    props <- g$conceptProperties
    if (is.null(props) || length(props) == 0) return(NULL)
    tibble(
      rxcui   = vapply(props, function(p) p$rxcui %||% NA_character_, character(1)),
      name    = vapply(props, function(p) p$name %||% NA_character_, character(1)),
      tty     = vapply(props, function(p) p$tty %||% NA_character_, character(1)),
      synonym = vapply(props, function(p) p$synonym %||% NA_character_, character(1))
    )
  })
  bind_rows(results)
}



#' Get spelling suggestions for a drug name
#'
#' Returns spelling suggestions from RxNorm for potentially misspelled
#' drug names.
#'
#' @param name Character. Potentially misspelled drug name.
#' @return A tibble with column: suggestion (character).
#' @export
#' @examples
#' \dontrun{
#' rxnorm_spelling("acetaminophen")
#' }
rxnorm_spelling <- function(name) {
  url <- sprintf("%s/spellingsuggestions.json?name=%s", .rx_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_suggestions)

  suggestions <- raw$suggestionGroup$suggestionList$suggestion
  if (is.null(suggestions) || length(suggestions) == 0) return(.schema_suggestions)
  tibble(suggestion = as.character(suggestions))
}



#' Get drug classes for an RxCUI
#'
#' Returns therapeutic classification codes (ATC, VA, MeSH, or MED-RT)
#' for a drug concept.
#'
#' @param rxcui Character. RxNorm concept ID.
#' @param source Character. Classification source: \code{"ATC"},
#'   \code{"VA"}, \code{"MESH"}, or \code{"MEDRT"}. Default \code{"ATC"}.
#' @return A tibble with columns: class_id, class_name, class_type,
#'   rxcui, drug_name.
#' @export
#' @examples
#' \dontrun{
#' rxnorm_class("161", source = "ATC")
#' }
rxnorm_class <- function(rxcui, source = "ATC") {
  url <- sprintf("%s/rxclass/class/byRxcui.json?rxcui=%s&relaSource=%s",
                 .rx_base, rxcui, source)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_classes)

  items <- raw$rxclassDrugInfoList$rxclassDrugInfo
  if (is.null(items) || length(items) == 0) return(.schema_classes)

  tibble(
    class_id   = vapply(items, function(i) i$rxclassMinConceptItem$classId %||% NA_character_, character(1)),
    class_name = vapply(items, function(i) i$rxclassMinConceptItem$className %||% NA_character_, character(1)),
    class_type = vapply(items, function(i) i$rxclassMinConceptItem$classType %||% NA_character_, character(1)),
    rxcui      = vapply(items, function(i) i$minConcept$rxcui %||% NA_character_, character(1)),
    drug_name  = vapply(items, function(i) i$minConcept$name %||% NA_character_, character(1))
  )
}


# == Context ===================================================================

#' Get nih.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nih_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nih_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nih.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nih.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nih.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nih.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
