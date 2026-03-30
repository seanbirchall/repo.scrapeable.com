#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# fda-gov.R
# Self-contained FDA openFDA API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: optional API key (query param api_key). Without: 240 req/min, 120K/day.
#   With: 240 req/min, 120K/day (same limits, key prevents throttling).
#   Register at https://open.fda.gov/apis/authentication/
# Docs: https://open.fda.gov/apis/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.fda_base <- "https://api.fda.gov"

# -- Context generator ---------------------------------------------------------

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
  cat(out, "\n"); invisible(out)
}

# -- Core fetch engine ---------------------------------------------------------

.fda_get <- function(endpoint, search = NULL, count = NULL, sort = NULL,
                     limit = 100, skip = 0, api_key = NULL) {
  params <- list()
  if (!is.null(search))  params$search <- search
  if (!is.null(count))   params$count <- count
  if (!is.null(sort))    params$sort <- sort
  if (!is.null(api_key)) params$api_key <- api_key
  params$limit <- min(limit, 1000)
  if (skip > 0) params$skip <- skip

  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.fda_base, "/", endpoint, "?", query)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# Paginated fetch returning tibble
.fda_fetch_all <- function(endpoint, search = NULL, sort = NULL,
                           api_key = NULL, max_results = 100) {
  all_results <- list()
  skip <- 0
  page_size <- min(max_results, 100)

  repeat {
    raw <- .fda_get(endpoint, search = search, sort = sort,
                    limit = page_size, skip = skip, api_key = api_key)
    results <- raw$results
    if (is.null(results) || length(results) == 0) break

    all_results <- c(all_results, results)
    skip <- skip + length(results)

    if (skip >= (raw$meta$results$total %||% 0)) break
    if (length(all_results) >= max_results) break
    if (length(results) < page_size) break
  }

  if (length(all_results) == 0) return(tibble())
  if (length(all_results) > max_results) all_results <- all_results[seq_len(max_results)]

  # Flatten list-of-lists to tibble
  bind_rows(lapply(all_results, function(r) {
    flat <- lapply(r, function(v) {
      if (is.null(v)) NA_character_
      else if (is.list(v) || length(v) > 1) paste(v, collapse = "; ")
      else as.character(v)
    })
    as_tibble(flat)
  }))
}


