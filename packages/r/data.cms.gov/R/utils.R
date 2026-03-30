#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# cms-gov.R
# Self-contained CMS (Centers for Medicare & Medicaid Services) data client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://data.cms.gov/provider-data/api


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cms_base <- "https://data.cms.gov/provider-data/api/1"

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

# -- CMS datastore query engine ------------------------------------------------

.cms_query <- function(dataset_id, conditions = list(), limit = 1000,
                       offset = 0, max_results = NULL) {
  all_data <- list()
  current_offset <- offset
  page_size <- min(limit, 1000)
  total <- NULL

  repeat {
    # Build URL with conditions
    params <- sprintf("limit=%d&offset=%d", page_size, current_offset)
    for (i in seq_along(conditions)) {
      c <- conditions[[i]]
      params <- paste0(params,
        sprintf("&conditions[%d][property]=%s&conditions[%d][value]=%s&conditions[%d][operator]=%%3D",
                i - 1, utils::URLencode(c$property),
                i - 1, utils::URLencode(c$value),
                i - 1))
    }

    url <- sprintf("%s/datastore/query/%s/0?%s", .cms_base, dataset_id, params)
    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

    if (is.null(total)) {
      total <- raw$count
      if (total > page_size)
        message(sprintf("Found %d records, fetching...", min(total, max_results %||% total)))
    }

    results <- raw$results
    if (is.null(results) || length(results) == 0) break

    # Get column names from schema
    schema_id <- names(raw$schema)[1]
    col_names <- names(raw$schema[[schema_id]]$fields)

    # Parse array-of-arrays into tibble
    df <- bind_rows(lapply(results, function(r) {
      vals <- vapply(r, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
      if (length(vals) == length(col_names)) names(vals) <- col_names
      as.list(vals)
    }))
    all_data[[length(all_data) + 1]] <- as_tibble(df)

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break
    if (n_so_far >= total) break
    current_offset <- current_offset + page_size
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Auto-type numeric-looking columns (exclude IDs, zip codes, phone numbers)
  skip_cols <- grep("id$|_id|zip|phone|fips|code$|number$|footnote", names(result), value = TRUE)
  type_cols <- setdiff(names(result), c(skip_cols, "state", "address", "citytown",
                                         "facility_name", "hospital_name"))
  for (col in type_cols) {
    vals <- result[[col]]
    if (!is.character(vals)) next
    nums <- suppressWarnings(as.numeric(vals))
    if (sum(!is.na(nums)) >= sum(vals != "" & !is.na(vals)) * 0.8)
      result[[col]] <- nums
  }
  result
}


