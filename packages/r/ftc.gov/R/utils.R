#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# ftc.gov.R
# Self-contained Federal Trade Commission API client.
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. Register at https://api.ftc.gov
# Docs: https://api.ftc.gov

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ftc_base <- "https://api.ftc.gov/v0"

.ftc_get <- function(endpoint, api_key = NULL, page_size = 50,
                     page_offset = 0, sort = NULL, filter = NULL) {
  key <- api_key %||% Sys.getenv("FTC_API_KEY", "DEMO_KEY")
  url <- paste0(.ftc_base, "/", endpoint)

  params <- list(
    api_key = key,
    `page[limit]` = min(page_size, 50),
    `page[offset]` = page_offset
  )
  if (!is.null(sort)) params[["sort[sort-field][path]"]] <- sort

  req <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30)

  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.ftc_parse_hsr <- function(data_list) {
  if (length(data_list) == 0) return(tibble::tibble())
  bind_rows(lapply(data_list, function(rec) {
    a <- rec$attributes
    tibble::tibble(
      id               = rec$id %||% NA_character_,
      title            = a$title %||% NA_character_,
      transaction_number = a$`transaction-number` %||% NA_character_,
      acquiring_party  = a$`acquiring-party` %||% NA_character_,
      acquired_party   = a$`acquired-party` %||% NA_character_,
      acquired_entities = paste(a$`acquired-entities` %||% list(), collapse = "; "),
      date             = as.Date(a$date %||% NA_character_),
      created          = a$created %||% NA_character_,
      updated          = a$updated %||% NA_character_
    )
  }))
}

.ftc_fetch_all <- function(endpoint, api_key = NULL, max_results = 100,
                           sort = NULL) {
  all_data <- list()
  offset <- 0
  page_size <- min(max_results, 50)

  repeat {
    raw <- .ftc_get(endpoint, api_key = api_key, page_size = page_size,
                    page_offset = offset, sort = sort)
    recs <- raw$data
    if (is.null(recs) || length(recs) == 0) break

    all_data <- c(all_data, recs)
    offset <- offset + length(recs)

    total <- raw$meta$count %||% 0
    if (offset >= total) break
    if (length(all_data) >= max_results) break
    if (length(recs) < page_size) break
    Sys.sleep(0.5)
  }

  if (length(all_data) > max_results) all_data <- all_data[seq_len(max_results)]
  all_data
}

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
