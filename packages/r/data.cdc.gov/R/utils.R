#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# cdc-gov.R
# Self-contained CDC Open Data (Socrata SODA) client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: optional app token (reduces throttling). Register at data.cdc.gov.
# Rate limits: Without token: throttled. With token: higher limits.
# Docs: https://dev.socrata.com/docs/queries/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cdc_base <- "https://data.cdc.gov"

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

# -- SODA query engine ---------------------------------------------------------

.cdc_get <- function(dataset_id, where = NULL, select = NULL, group = NULL,
                     order = NULL, limit = 1000, offset = 0, token = NULL,
                     max_results = NULL) {
  all_data <- list()
  current_offset <- offset
  page_size <- min(limit, 50000)

  repeat {
    params <- list(`$limit` = page_size, `$offset` = current_offset)
    if (!is.null(where))  params[["$where"]] <- where
    if (!is.null(select)) params[["$select"]] <- select
    if (!is.null(group))  params[["$group"]] <- group
    if (!is.null(order))  params[["$order"]] <- order

    query <- paste(names(params),
                   vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE),
                          character(1)),
                   sep = "=", collapse = "&")
    url <- paste0(.cdc_base, "/resource/", dataset_id, ".json?", query)
    if (!is.null(token)) url <- paste0(url, "&$$app_token=", token)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    if (is.null(raw) || length(raw) == 0 || nrow(raw) == 0) break
    all_data[[length(all_data) + 1]] <- as_tibble(raw)

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break
    if (nrow(raw) < page_size) break
    current_offset <- current_offset + page_size
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)
  result
}



utils::globalVariables(c("head","type"))
