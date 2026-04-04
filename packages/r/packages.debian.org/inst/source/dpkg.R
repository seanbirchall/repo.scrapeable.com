



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://sources.debian.org/api"

#' Search Debian packages
#' @param query Package name search
#' @return tibble of packages
#' @export
dpkg_search <- function(query) {
  url <- sprintf("%s/search/%s/", .base, URLencode(query, TRUE))
  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble::tibble(package = character()))
  if (is.list(results) && !is.null(results$exact)) {
    pkgs <- c(if (!is.null(results$exact)) list(results$exact) else list(),
             if (!is.null(results$other)) results$other else list())
  } else {
    pkgs <- results
  }
  tibble::tibble(
    package = vapply(pkgs, function(x) {
      if (is.character(x)) x else if (is.list(x) && !is.null(x$name)) x$name else NA_character_
    }, character(1))
  )
}

#' Get Debian package info
#' @param package Package name
#' @return tibble with package versions
#' @export
dpkg_info <- function(package) {
  url <- sprintf("%s/src/%s/", .base, URLencode(package, TRUE))
  raw <- .fetch_json(url)
  versions <- raw$versions
  if (length(versions) == 0) return(tibble::tibble(version = character(), suites = character()))
  tibble::tibble(
    version = vapply(versions, function(x) x$version %||% NA_character_, character(1)),
    suites = vapply(versions, function(x) paste(unlist(x$suites), collapse = ", "), character(1))
  )
}

#' Generate LLM context for packages.debian.org
#' @return Character string
#' @export
dpkg_context <- function() { .build_context("packages.debian.org") }


# == Context ===================================================================

#' Generate LLM-friendly context for packages.debian.org
#'
#' @return Character string with full function signatures and bodies
#' @export
packages_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/packages.debian.org.R"
  if (!file.exists(src_file)) {
    cat("# packages.debian.org context - source not found\n")
    return(invisible("# packages.debian.org context - source not found"))
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

