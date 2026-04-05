# proteinatlas.org.R - Self-contained proteinatlas.org client



# proteinatlas-org.R
# Self-contained Human Protein Atlas client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)
# Note: API returns gzip-compressed JSON; httr2 handles decompression.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hpa_base <- "https://www.proteinatlas.org/api"

`%||%` <- function(a, b) if (is.null(a)) b else a
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Accept-Encoding` = "gzip") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_hpa_json <- function(url) {
  tmp <- .fetch(url)
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  # Check for gzip magic number
  if (length(raw) >= 2 && raw[1] == as.raw(0x1f) && raw[2] == as.raw(0x8b)) {
    txt <- memDecompress(raw, type = "gzip")
    jsonlite::fromJSON(rawToChar(txt))
  } else {
    jsonlite::fromJSON(rawToChar(raw))
  }
}

# == Schemas ===================================================================

.schema_search <- tibble(
  gene = character(), ensembl = character(), uniprot = character()
)

.schema_gene <- tibble(
  gene = character(), ensembl = character(), uniprot = character(),
  gene_description = character(), chromosome = character(),
  protein_class = character()
)

# == Public functions ==========================================================

#' Search the Human Protein Atlas by gene name
#'
#' Queries the Human Protein Atlas download API for genes matching the
#' given search term. Returns basic identifiers by default; request
#' additional columns to include tissue expression, subcellular
#' localization, or protein class annotations.
#'
#' @param query Character. Gene name, symbol, or keyword (e.g.
#'   \code{"BRCA1"}, \code{"TP53"}, \code{"kinase"}).
#' @param columns Character. Comma-separated column codes to include in
#'   the result. Default \code{"g,eg,up"} returns Gene, Ensembl ID, and
#'   UniProt ID. Additional codes:
#'   \describe{
#'     \item{\code{"gd"}}{Gene description}
#'     \item{\code{"chr"}}{Chromosome}
#'     \item{\code{"pc"}}{Protein class}
#'     \item{\code{"t"}}{Tissue expression summary}
#'     \item{\code{"sc"}}{Subcellular location}
#'   }
#'
#' @return A tibble with one row per matching gene. Column names are
#'   lowercased versions of the API response fields (e.g. \code{gene},
#'   \code{ensembl}, \code{uniprot}). List-valued columns (like
#'   multi-entry UniProt IDs) are collapsed with semicolons.
#'
#' @examples
#' hpa_search("TP53")
#' hpa_search("BRCA", columns = "g,eg,up,gd,chr")
#'
#' @seealso \code{\link{hpa_gene}}, \code{\link{hpa_gene_detail}}
#' @export
hpa_search <- function(query, columns = "g,eg,up") {
  url <- sprintf("%s/search_download.php?search=%s&format=json&columns=%s",
                 .hpa_base, utils::URLencode(query), columns)
  raw <- tryCatch(.fetch_hpa_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_search)

  df <- as_tibble(raw)
  # Normalize column names to lowercase
  names(df) <- tolower(names(df))

  # Flatten list columns (e.g. uniprot can be a list)
  for (col in names(df)) {
    if (is.list(df[[col]])) {
      df[[col]] <- vapply(df[[col]], function(x) paste(x, collapse = ";"), character(1))
    }
  }

  df
}

#' Fetch Human Protein Atlas gene summary by Ensembl ID
#'
#' Retrieves a structured summary for a single gene using the Human
#' Protein Atlas search/download endpoint. Returns gene symbol,
#' Ensembl ID, UniProt accession, description, chromosome, and protein
#' class in a single-row tibble.
#'
#' @param ensembl_id Character. Ensembl gene ID (e.g.
#'   \code{"ENSG00000012048"} for BRCA1, \code{"ENSG00000141510"} for TP53).
#' @param columns Character. Comma-separated column codes (default
#'   \code{"g,eg,up,gd,chr,pc"} for gene, Ensembl, UniProt, description,
#'   chromosome, and protein class).
#'
#' @return A tibble (typically one row) with columns corresponding to the
#'   requested column codes. Default columns: \code{gene}, \code{ensembl},
#'   \code{uniprot}, \code{gene description}, \code{chromosome},
#'   \code{protein class}.
#'
#' @examples
#' hpa_gene("ENSG00000141510")   # TP53
#'
#' @seealso \code{\link{hpa_search}}, \code{\link{hpa_gene_detail}}
#' @export
hpa_gene <- function(ensembl_id, columns = "g,eg,up,gd,chr,pc") {
  url <- sprintf("%s/search_download.php?search=%s&format=json&columns=%s",
                 .hpa_base, ensembl_id, columns)
  raw <- tryCatch(.fetch_hpa_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_gene)

  df <- as_tibble(raw)
  names(df) <- tolower(names(df))

  for (col in names(df)) {
    if (is.list(df[[col]])) {
      df[[col]] <- vapply(df[[col]], function(x) paste(x, collapse = ";"), character(1))
    }
  }

  df
}

#' Fetch rich gene annotation from the Human Protein Atlas
#'
#' Uses the per-gene JSON endpoint (\code{proteinatlas.org/<ENSG>.json})
#' to retrieve deep annotation for a single gene, including RNA tissue
#' specificity, subcellular localization, and disease involvement.
#' This endpoint returns far more detail than the search/download API.
#'
#' @param ensembl_id Character. Ensembl gene ID (e.g.
#'   \code{"ENSG00000141510"} for TP53).
#'
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{gene}{Character. Gene symbol.}
#'     \item{ensembl}{Character. Ensembl gene ID.}
#'     \item{description}{Character. Gene description.}
#'     \item{chromosome}{Character. Chromosome number or name.}
#'     \item{protein_class}{Character. Semicolon-separated protein class
#'       annotations (e.g. \code{"Cancer-related genes; Transcription factors"}).}
#'     \item{rna_tissue_specificity}{Character. Tissue specificity classification
#'       (e.g. \code{"Low tissue specificity"}).}
#'     \item{subcellular_location}{Character. Main subcellular location(s).}
#'     \item{disease_involvement}{Character. Known disease associations.}
#'   }
#'
#' @details The API may return gzip-compressed JSON; decompression is
#'   handled automatically.
#'
#' @examples
#' hpa_gene_detail("ENSG00000141510")   # TP53
#' hpa_gene_detail("ENSG00000012048")   # BRCA1
#'
#' @seealso \code{\link{hpa_search}}, \code{\link{hpa_gene}}
#' @export
hpa_gene_detail <- function(ensembl_id) {
  schema <- tibble(
    gene = character(), ensembl = character(), description = character(),
    chromosome = character(), protein_class = character(),
    rna_tissue_specificity = character(), subcellular_location = character(),
    disease_involvement = character()
  )
  url <- sprintf("https://www.proteinatlas.org/%s.json", ensembl_id)
  raw <- tryCatch(.fetch_hpa_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)

  .safe <- function(x) if (is.null(x)) NA_character_ else if (is.list(x) || length(x) > 1) paste(x, collapse = "; ") else as.character(x)

  tibble(
    gene = .safe(raw[["Gene"]]),
    ensembl = .safe(raw[["Ensembl"]]),
    description = .safe(raw[["Gene description"]]),
    chromosome = .safe(raw[["Chromosome"]]),
    protein_class = .safe(raw[["Protein class"]]),
    rna_tissue_specificity = .safe(raw[["RNA tissue specificity"]]),
    subcellular_location = .safe(raw[["Subcellular main location"]]),
    disease_involvement = .safe(raw[["Disease involvement"]])
  )
}

# == Context ===================================================================

#' Get proteinatlas.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
proteinatlas_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(proteinatlas_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/proteinatlas.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "proteinatlas.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# proteinatlas.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# proteinatlas.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
