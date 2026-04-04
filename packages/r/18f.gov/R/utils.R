#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# 18f.gov.R
# Self-contained 18F / GSA government API catalog client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.apis_url <- "https://raw.githubusercontent.com/18F/API-All-the-X/master/_data/individual_apis.yml"
.hubs_url <- "https://raw.githubusercontent.com/18F/API-All-the-X/master/_data/developer_hubs.yml"

`%||%` <- function(x, y) if (is.null(x)) y else x

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

.fetch_text <- function(url) {
  tmp <- tempfile(fileext = ".txt")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  readLines(tmp, warn = FALSE)
}

.parse_apis_yaml <- function(lines) {
  orgs <- character()
  api_names <- character()
  api_urls <- character()

  current_org <- NA_character_
  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]
    if (grepl("^- name:\\s*", line)) {
      current_org <- trimws(sub("^- name:\\s*", "", line))
      current_org <- gsub("^\"|\"$", "", current_org)
    }
    if (grepl("^  - name:\\s*", line)) {
      api_name <- trimws(sub("^  - name:\\s*", "", line))
      api_name <- gsub("^\"|\"$", "", api_name)
      api_url <- NA_character_
      if (i + 1 <= n && grepl("^    url:\\s*", lines[i + 1])) {
        api_url <- trimws(sub("^    url:\\s*", "", lines[i + 1]))
        api_url <- gsub("^\"|\"$", "", api_url)
        i <- i + 1L
      }
      orgs <- c(orgs, current_org)
      api_names <- c(api_names, api_name)
      api_urls <- c(api_urls, api_url)
    }
    i <- i + 1L
  }

  tibble::tibble(
    organization = as.character(orgs),
    api_name     = as.character(api_names),
    api_url      = as.character(api_urls)
  )
}

.parse_hubs_yaml <- function(lines) {
  hub_names <- character()
  hub_urls  <- character()

  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]
    if (nzchar(trimws(line)) && !grepl("^\\s*#", line)) {
      if (grepl("^[A-Za-z].*:\\s*$", line)) {
        hub_name <- sub(":\\s*$", "", line)
        hub_url <- NA_character_
        j <- i + 1L
        while (j <= n && !nzchar(trimws(lines[j]))) j <- j + 1L
        if (j <= n && grepl("^\\s+url:\\s*", lines[j])) {
          hub_url <- trimws(sub("^\\s+url:\\s*", "", lines[j]))
          hub_url <- gsub("^\"|\"$", "", hub_url)
        }
        hub_names <- c(hub_names, hub_name)
        hub_urls  <- c(hub_urls, hub_url)
      }
    }
    i <- i + 1L
  }

  tibble::tibble(
    organization = as.character(hub_names),
    hub_url      = as.character(hub_urls)
  )
}
