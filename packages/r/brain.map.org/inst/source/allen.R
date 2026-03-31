# brain-map-org.R
# Self-contained Allen Brain Map API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.allen_base <- "https://api.brain-map.org/api/v2"

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
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_genes <- tibble(
  id = integer(), acronym = character(), name = character(),
  entrez_id = integer(), chromosome = character(),
  organism_id = integer()
)

.schema_structures <- tibble(
  id = integer(), name = character(), acronym = character(),
  atlas_id = integer(), parent_structure_id = integer(),
  depth = integer(), color_hex_triplet = character()
)

.schema_datasets <- tibble(
  id = integer(), name = character(), product_id = integer(),
  specimen_id = integer(), plane_of_section = character()
)

# == Genes =====================================================================

#' Search Allen Brain Atlas genes
#'
#' Query the Allen Brain Atlas gene database by acronym or name.
#'
#' @param acronym Gene acronym (e.g. "BRCA1", "TP53", "EGFR").
#'   Case-insensitive.
#' @param name Gene name substring to search (alternative to acronym)
#' @param organism_id Organism ID: 1 = human, 2 = mouse (default 1)
#' @param limit Max results (default 50)
#' @return tibble: id, acronym, name, entrez_id, chromosome, organism_id
allen_genes <- function(acronym = NULL, name = NULL, organism_id = 1,
                        limit = 50) {
  criteria <- sprintf("model::Gene,rma::criteria,[organism_id$eq%d]", organism_id)
  if (!is.null(acronym)) {
    criteria <- paste0(criteria, sprintf(",[acronym$il'*%s*']", acronym))
  } else if (!is.null(name)) {
    criteria <- paste0(criteria, sprintf(",[name$il'*%s*']", name))
  }
  url <- sprintf(
    "%s/data/query.json?criteria=%s&num_rows=%d",
    .allen_base, utils::URLencode(criteria), as.integer(limit)
  )
  raw <- .fetch_json(url)
  msg <- raw$msg
  if (is.null(msg) || length(msg) == 0) return(.schema_genes)
  if (is.data.frame(msg) && nrow(msg) == 0) return(.schema_genes)

  as_tibble(msg) |>
    transmute(
      id = as.integer(id),
      acronym = as.character(acronym),
      name = as.character(name),
      entrez_id = as.integer(entrez_id %||% NA),
      chromosome = as.character(chromosome_id %||% NA),
      organism_id = as.integer(organism_id)
    )
}

# == Structures ================================================================

#' Search Allen Brain Atlas structures
#'
#' Query brain structures (regions) from the Allen Brain Atlas ontology.
#'
#' @param name Structure name or substring (e.g. "hippocampus", "cortex")
#' @param acronym Structure acronym (e.g. "CA1", "VISp")
#' @param atlas_id Atlas ID (default 1 = adult mouse brain)
#' @param limit Max results (default 50)
#' @return tibble: id, name, acronym, atlas_id, parent_structure_id, depth,
#'   color_hex_triplet
allen_structures <- function(name = NULL, acronym = NULL, atlas_id = NULL,
                             limit = 50) {
  criteria <- "model::Structure"
  filters <- character()
  if (!is.null(name)) filters <- c(filters, sprintf("[name$il'*%s*']", name))
  if (!is.null(acronym)) filters <- c(filters, sprintf("[acronym$il'*%s*']", acronym))
  if (!is.null(atlas_id)) filters <- c(filters, sprintf("[atlas_id$eq%d]", as.integer(atlas_id)))
  if (length(filters) > 0) {
    criteria <- paste0(criteria, ",rma::criteria,", paste(filters, collapse = ","))
  }
  url <- sprintf(
    "%s/data/query.json?criteria=%s&num_rows=%d",
    .allen_base, utils::URLencode(criteria), as.integer(limit)
  )
  raw <- .fetch_json(url)
  msg <- raw$msg
  if (is.null(msg) || length(msg) == 0) return(.schema_structures)
  if (is.data.frame(msg) && nrow(msg) == 0) return(.schema_structures)

  as_tibble(msg) |>
    transmute(
      id = as.integer(id),
      name = as.character(name),
      acronym = as.character(acronym),
      atlas_id = as.integer(if ("atlas_id" %in% names(msg)) atlas_id else NA),
      parent_structure_id = as.integer(if ("parent_structure_id" %in% names(msg)) parent_structure_id else NA),
      depth = as.integer(if ("depth" %in% names(msg)) depth else NA),
      color_hex_triplet = as.character(if ("color_hex_triplet" %in% names(msg)) color_hex_triplet else NA)
    )
}

# == Datasets ==================================================================

#' Search Allen Brain Atlas datasets
#'
#' Query datasets (section data sets) from the Allen Brain Atlas.
#'
#' @param gene_acronym Filter by gene acronym (e.g. "Gad1")
#' @param product_id Product: 1 = Mouse Brain ISH, 2 = Human Brain ISH
#' @param limit Max results (default 50)
#' @return tibble: id, name, product_id, specimen_id, plane_of_section
allen_datasets <- function(gene_acronym = NULL, product_id = NULL, limit = 50) {
  criteria <- "model::SectionDataSet"
  filters <- character()
  if (!is.null(gene_acronym)) {
    filters <- c(filters, sprintf("genes[acronym$il'%s']", gene_acronym))
  }
  if (!is.null(product_id)) {
    filters <- c(filters, sprintf("[product_id$eq%d]", as.integer(product_id)))
  }
  if (length(filters) > 0) {
    criteria <- paste0(criteria, ",rma::criteria,", paste(filters, collapse = ","))
  }
  url <- sprintf(
    "%s/data/query.json?criteria=%s&num_rows=%d",
    .allen_base, utils::URLencode(criteria), as.integer(limit)
  )
  raw <- .fetch_json(url)
  msg <- raw$msg
  if (is.null(msg) || length(msg) == 0) return(.schema_datasets)
  if (is.data.frame(msg) && nrow(msg) == 0) return(.schema_datasets)

  planes <- c("coronal", "sagittal")
  as_tibble(msg) |>
    transmute(
      id = as.integer(id),
      name = as.character(if ("name" %in% names(msg)) name else NA),
      product_id = as.integer(if ("product_id" %in% names(msg)) product_id else NA),
      specimen_id = as.integer(if ("specimen_id" %in% names(msg)) specimen_id else NA),
      plane_of_section = as.character(
        if ("plane_of_section_id" %in% names(msg))
          ifelse(plane_of_section_id %in% 1:2, planes[plane_of_section_id], as.character(plane_of_section_id))
        else NA
      )
    )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the brain.map.org package
#'
#' @return Character string (invisibly), also printed
allen_context <- function() {
  .build_context("brain.map.org", header_lines = c(
    "# brain.map.org - Allen Brain Atlas API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://api.brain-map.org/api/v2",
    "# All functions return tibbles with typed columns.",
    "# Organisms: 1 = human, 2 = mouse"
  ))
}
