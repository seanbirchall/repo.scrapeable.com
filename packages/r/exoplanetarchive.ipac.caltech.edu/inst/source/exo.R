



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://exoplanetarchive.ipac.caltech.edu/TAP/sync"

#' Query confirmed exoplanets
#' @param where SQL WHERE clause (optional)
#' @param top Number of results (default 50)
#' @return tibble of exoplanets
#' @export
exo_planets <- function(where = NULL, top = 50L) {
  query <- sprintf("SELECT TOP %d pl_name,hostname,disc_year,pl_bmasse,pl_rade,pl_orbper,sy_dist FROM ps WHERE default_flag=1", top)
  if (!is.null(where)) query <- sprintf("SELECT TOP %d pl_name,hostname,disc_year,pl_bmasse,pl_rade,pl_orbper,sy_dist FROM ps WHERE default_flag=1 AND %s", top, where)
  url <- sprintf("%s?query=%s&format=json", .base, URLencode(query, TRUE))
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(tibble::tibble(pl_name = character(), hostname = character()))
  tibble::tibble(
    pl_name = vapply(raw, function(x) x$pl_name %||% NA_character_, character(1)),
    hostname = vapply(raw, function(x) x$hostname %||% NA_character_, character(1)),
    disc_year = vapply(raw, function(x) x$disc_year %||% NA_integer_, integer(1)),
    mass_earth = vapply(raw, function(x) as.numeric(x$pl_bmasse %||% NA), numeric(1)),
    radius_earth = vapply(raw, function(x) as.numeric(x$pl_rade %||% NA), numeric(1)),
    orbital_period = vapply(raw, function(x) as.numeric(x$pl_orbper %||% NA), numeric(1)),
    distance_pc = vapply(raw, function(x) as.numeric(x$sy_dist %||% NA), numeric(1))
  )
}

#' Generate LLM context for exoplanetarchive.ipac.caltech.edu
#' @return Character string
#' @export
exo_context <- function() { .build_context("exoplanetarchive.ipac.caltech.edu") }


# == Context ===================================================================

#' Generate LLM-friendly context for exoplanetarchive.ipac.caltech.edu
#'
#' @return Character string with full function signatures and bodies
#' @export
exoplanetarchive_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/exoplanetarchive.ipac.caltech.edu.R"
  if (!file.exists(src_file)) {
    cat("# exoplanetarchive.ipac.caltech.edu context - source not found\n")
    return(invisible("# exoplanetarchive.ipac.caltech.edu context - source not found"))
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

