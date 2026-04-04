



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://api.brain-map.org/api/v2"

#' Search Allen Brain Atlas structures
#' @param query Search term
#' @return tibble of brain structures
#' @export
allen_structures <- function(query = NULL) {
  url <- paste0(.base, "/data/Structure/query.json?criteria=products[id$eq1]&num_rows=50")
  if (!is.null(query)) url <- paste0(.base, "/data/Structure/query.json?criteria=[name$il*", URLencode(query, TRUE), "*]&num_rows=50")
  raw <- .fetch_json(url)
  msgs <- raw$msg
  if (length(msgs) == 0) return(tibble::tibble(id = integer(), name = character(), acronym = character()))
  tibble::tibble(
    id = vapply(msgs, function(x) x$id %||% NA_integer_, integer(1)),
    name = vapply(msgs, function(x) x$name %||% NA_character_, character(1)),
    acronym = vapply(msgs, function(x) x$acronym %||% NA_character_, character(1)),
    parent_structure_id = vapply(msgs, function(x) x$parent_structure_id %||% NA_integer_, integer(1))
  )
}

#' Search Allen Brain Atlas genes
#' @param query Gene name or symbol
#' @return tibble of genes
#' @export
allen_genes <- function(query) {
  url <- sprintf("%s/data/Gene/query.json?criteria=[acronym$il*%s*]&num_rows=50", .base, URLencode(query, TRUE))
  raw <- .fetch_json(url)
  msgs <- raw$msg
  if (length(msgs) == 0) return(tibble::tibble(id = integer(), acronym = character(), name = character()))
  tibble::tibble(
    id = vapply(msgs, function(x) x$id %||% NA_integer_, integer(1)),
    acronym = vapply(msgs, function(x) x$acronym %||% NA_character_, character(1)),
    name = vapply(msgs, function(x) x$name %||% NA_character_, character(1)),
    entrez_id = vapply(msgs, function(x) x$entrez_id %||% NA_integer_, integer(1))
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for portal.brain-map.org
#'
#' @return Character string with full function signatures and bodies
#' @export
allen_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/portal.brain-map.org.R"
  if (!file.exists(src_file)) {
    cat("# portal.brain-map.org context - source not found\n")
    return(invisible("# portal.brain-map.org context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

