# federallabs.org.R - Federal Laboratory Consortium client
# Self-contained. All public functions return tibbles.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Source: CSV of federal inventions available for licensing

library(httr2)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.flc_csv_url <- "https://federallabs.org/FLC_KenticoXperience_Core/media/Files/Data.Gov%20Uploads/opportunity_1764955984.csv"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.load_flc_data <- function() {
  tmp <- .fetch(.flc_csv_url, ext = ".csv")
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  as_tibble(raw)
}

# == Schemas ===================================================================

.schema_inventions <- tibble(
  id = character(), url = character(), title = character(),
  abstract = character(), source_url = character(),
  related_patents = character(), people = character(),
  organizations = character(), tags = character()
)

# == Public functions ==========================================================

#' List all federal lab inventions
#'
#' Returns all federal inventions available for licensing from the
#' Federal Laboratory Consortium.
#'
#' @return tibble: id, url, title, abstract, source_url, related_patents,
#'   people, organizations, tags
#' @export
flc_inventions <- function() {
  raw <- tryCatch(.load_flc_data(), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_inventions)

  nms <- names(raw)
  tibble(
    id              = as.character(raw[[nms[1]]] %||% NA),
    url             = as.character(raw[[nms[2]]] %||% NA),
    title           = as.character(raw[[nms[3]]] %||% NA),
    abstract        = as.character(raw[[nms[4]]] %||% NA),
    source_url      = as.character(raw[[nms[5]]] %||% NA),
    related_patents = as.character(raw[[nms[6]]] %||% NA),
    people          = as.character(raw[[nms[7]]] %||% NA),
    organizations   = as.character(raw[[nms[8]]] %||% NA),
    tags            = as.character(raw[[nms[9]]] %||% NA)
  )
}

#' Search federal lab inventions by keyword
#'
#' Searches title, abstract, organizations, and tags fields.
#'
#' @param query Character string to search for (case-insensitive)
#' @return tibble: same columns as flc_inventions()
#' @export
flc_search <- function(query) {
  inv <- flc_inventions()
  if (nrow(inv) == 0) return(inv)
  q <- tolower(query)
  matches <- grepl(q, tolower(inv$title), fixed = TRUE) |
    grepl(q, tolower(inv$abstract), fixed = TRUE) |
    grepl(q, tolower(inv$organizations), fixed = TRUE) |
    grepl(q, tolower(inv$tags), fixed = TRUE)
  inv[matches, ]
}

#' List unique organizations in the federal lab inventory
#'
#' @return tibble: organization, n_inventions
#' @export
flc_organizations <- function() {
  inv <- flc_inventions()
  if (nrow(inv) == 0) return(tibble(organization = character(), n_inventions = integer()))

  # Organizations can have multiple entries separated by newlines
  orgs <- trimws(unlist(strsplit(inv$organizations, "\n")))
  orgs <- orgs[nchar(orgs) > 0]
  tbl <- as.data.frame(table(orgs), stringsAsFactors = FALSE)
  names(tbl) <- c("organization", "n_inventions")
  as_tibble(tbl) |> arrange(desc(n_inventions))
}

# == Context ===================================================================

#' Generate LLM-friendly context for federallabs.org
#'
#' @return Character string with full function signatures and bodies
#' @export
flc_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/federallabs.org.R"
  if (!file.exists(src_file)) {
    cat("# federallabs.org context - source not found\n")
    return(invisible("# federallabs.org context - source not found"))
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
