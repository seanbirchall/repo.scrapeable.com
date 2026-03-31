# proteinatlas-org.R
# Self-contained Human Protein Atlas client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)
# Note: API returns gzip-compressed JSON; httr2 handles decompression.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hpa_base <- "https://www.proteinatlas.org/api"

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

#' Search Human Protein Atlas by gene name
#'
#' @param query Gene name or search term (e.g. "BRCA1", "TP53")
#' @param columns Comma-separated column codes to include. Default "g,eg,up"
#'   (Gene, Ensembl, Uniprot). Other codes: "gd" (gene description),
#'   "chr" (chromosome), "pc" (protein class), "t" (tissue), "sc" (subcellular)
#' @return tibble: gene, ensembl, uniprot (plus any extra columns requested)
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

#' Fetch Human Protein Atlas gene detail by Ensembl ID
#'
#' @param ensembl_id Ensembl gene ID (e.g. "ENSG00000012048")
#' @param columns Column codes (default "g,eg,up,gd,chr,pc")
#' @return tibble: gene, ensembl, uniprot, gene_description, chromosome,
#'   protein_class
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

#' Show Human Protein Atlas package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
hpa_context <- function() {
  .build_context(
    pkg_name = "proteinatlas.org",
    header_lines = c(
      "# proteinatlas.org",
      "# Human Protein Atlas API Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "#",
      "# Column codes: g (Gene), eg (Ensembl), up (Uniprot),",
      "#   gd (Gene description), chr (Chromosome), pc (Protein class),",
      "#   t (Tissue expression), sc (Subcellular location)"
    )
  )
}
