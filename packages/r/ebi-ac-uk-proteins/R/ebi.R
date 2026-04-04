# ebi-ac-uk-proteins.R
# Self-contained UniProt Proteins API client (via EBI).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ebi_base <- "https://www.ebi.ac.uk/proteins/api"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_protein <- tibble(
  accession = character(), id = character(), gene = character(),
  organism = character(), protein_name = character(),
  existence = character(), sequence_length = integer()
)

# == Public functions ==========================================================

#' Fetch a single UniProt protein entry by accession
#'
#' Retrieves detailed protein metadata from the EBI Proteins API for a
#' single UniProt accession, including gene name, organism, recommended
#' protein name, evidence level, and sequence length.
#'
#' @param accession UniProt accession ID (e.g. "P12345", "P38398", "Q9Y6K9").
#'   These are stable identifiers from the UniProt Knowledgebase.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{accession}{UniProt accession (e.g. "P12345")}
#'     \item{id}{UniProt entry name (e.g. "AATM_RABIT")}
#'     \item{gene}{Primary gene name}
#'     \item{organism}{Species scientific name}
#'     \item{protein_name}{Recommended full protein name}
#'     \item{existence}{Protein existence evidence level}
#'     \item{sequence_length}{Amino acid sequence length (integer)}
#'   }
#' @examples
#' ebi_protein("P12345")
#' ebi_protein("P38398")
#' @seealso [ebi_search()], [ebi_context()]
#' @source <https://www.ebi.ac.uk/proteins/api>
#' @export
ebi_protein <- function(accession) {
  url <- sprintf("%s/proteins/%s", .ebi_base, accession)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_protein)

  tibble(
    accession       = as.character(raw$accession %||% NA_character_),
    id              = as.character(raw$id %||% NA_character_),
    gene            = tryCatch(as.character(raw$gene[[1]]$name$value), error = function(e) NA_character_),
    organism        = tryCatch(as.character(raw$organism$names[[1]]$value), error = function(e) NA_character_),
    protein_name    = tryCatch(as.character(raw$protein$recommendedName$fullName$value), error = function(e) NA_character_),
    existence       = as.character(raw$proteinExistence %||% NA_character_),
    sequence_length = as.integer(raw$sequence$length %||% NA_integer_)
  )
}

#' Search UniProt proteins
#'
#' Searches the EBI Proteins API by gene name and returns matching protein
#' entries. Supports pagination via \code{offset} and \code{size} parameters.
#'
#' @param query Search query string -- typically a gene name (e.g. "BRCA1",
#'   "TP53", "insulin"). Searches the gene name field.
#' @param size Number of results per page (default 10, max 100).
#' @param offset Starting offset for pagination (default 0). Use to fetch
#'   subsequent pages of results.
#' @return A tibble with the same columns as \code{\link{ebi_protein}}:
#'   accession, id, gene, organism, protein_name, existence, sequence_length.
#' @examples
#' ebi_search("BRCA1", size = 5)
#' ebi_search("insulin", size = 10, offset = 0)
#' @seealso [ebi_protein()], [ebi_context()]
#' @source <https://www.ebi.ac.uk/proteins/api>
#' @export
ebi_search <- function(query, size = 10, offset = 0) {
  url <- sprintf("%s/proteins?offset=%d&size=%d&gene=%s",
                 .ebi_base, offset, size, utils::URLencode(query))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_protein)

  results <- if (is.list(raw) && !is.null(raw[[1]]$accession)) raw else list(raw)

  tibble(
    accession       = vapply(results, function(x) x$accession %||% NA_character_, character(1)),
    id              = vapply(results, function(x) x$id %||% NA_character_, character(1)),
    gene            = vapply(results, function(x)
      tryCatch(as.character(x$gene[[1]]$name$value), error = function(e) NA_character_), character(1)),
    organism        = vapply(results, function(x)
      tryCatch(as.character(x$organism$names[[1]]$value), error = function(e) NA_character_), character(1)),
    protein_name    = vapply(results, function(x) {
      v <- tryCatch(x$protein$recommendedName$fullName$value, error = function(e) NULL)
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
    }, character(1)),
    existence       = vapply(results, function(x) x$proteinExistence %||% NA_character_, character(1)),
    sequence_length = vapply(results, function(x) as.integer(x$sequence$length %||% NA_integer_), integer(1))
  )
}

#' Get ebi-ac-uk-proteins client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the EBI Proteins API client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' ebi_context()
#' @export
ebi_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ebi_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ebi-ac-uk-proteins.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ebi-ac-uk-proteins")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ebi-ac-uk-proteins context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ebi-ac-uk-proteins", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
