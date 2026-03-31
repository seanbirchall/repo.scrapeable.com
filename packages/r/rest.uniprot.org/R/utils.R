#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# rest-uniprot-org.R
# Self-contained UniProt protein database API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented (be respectful)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.uniprot_base <- "https://rest.uniprot.org"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
    j <- fi - 1; rox_start <- fi
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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_search <- tibble(
  accession = character(), id = character(), protein_name = character(),
  gene_name = character(), organism = character(), length = integer(),
  annotation_score = numeric(), entry_type = character()
)

.schema_entry <- tibble(
  accession = character(), id = character(), protein_name = character(),
  gene_name = character(), organism = character(), taxon_id = integer(),
  length = integer(), sequence = character(), annotation_score = numeric(),
  entry_type = character()
)

# == Parsers ===================================================================

.parse_search_result <- function(item) {
  protein_name <- tryCatch(
    item$proteinDescription$recommendedName$fullName$value %||% NA_character_,
    error = function(e) NA_character_
  )
  gene_name <- tryCatch({
    genes <- item$genes
    if (!is.null(genes) && length(genes) > 0) {
      genes[[1]]$geneName$value %||% NA_character_
    } else NA_character_
  }, error = function(e) NA_character_)
  organism <- tryCatch(
    item$organism$scientificName %||% NA_character_,
    error = function(e) NA_character_
  )
  seq_len <- tryCatch(
    as.integer(item$sequence$length %||% NA),
    error = function(e) NA_integer_
  )

  tibble(
    accession        = as.character(item$primaryAccession %||% NA),
    id               = as.character(item$uniProtkbId %||% NA),
    protein_name     = as.character(protein_name),
    gene_name        = as.character(gene_name),
    organism         = as.character(organism),
    length           = seq_len,
    annotation_score = as.numeric(item$annotationScore %||% NA),
    entry_type       = as.character(item$entryType %||% NA)
  )
}

