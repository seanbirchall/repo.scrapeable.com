#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# fcc-gov.R
# Self-contained FCC Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at opendata.fcc.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fcc_base <- "https://opendata.fcc.gov"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# -- SODA query builder --------------------------------------------------------

._soda_query <- function(view_id, where = NULL, select = NULL, group = NULL,
                         order = NULL, q = NULL, limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .fcc_base, view_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("FCC SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

# -- Paginator -----------------------------------------------------------------

._soda_fetch_all <- function(view_id, where = NULL, select = NULL, group = NULL,
                             order = NULL, q = NULL, max_rows = 10000,
                             page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(view_id, where = where, select = select,
                          group = group, order = order, q = q,
                          limit = min(page_size, max_rows - offset),
                          offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), type = character(),
  description = character(), updated_at = character()
)

.schema_complaints <- tibble(
  id = character(), ticket_created = character(), issue_type = character(),
  method = character(), issue = character(), city = character(),
  state = character(), zip = character()
)

.schema_broadband <- tibble(
  provider_id = character(), providername = character(), stateabbr = character(),
  blockcode = character(), techcode = character(), maxaddown = character(),
  maxadup = character(), consumer = character()
)

.schema_pirate_radio <- tibble(
  case_id_number = character(), enforcement_target = character(),
  state_or_territory = character(), type_of_enforcement_action = character(),
  issued_date = character(), penalty_amount = character(),
  frequency = character()
)

# == Known view IDs ============================================================

.fcc_views <- list(
  complaints         = "3xyp-aqkj",
  pirate_radio       = "xqgr-24et",
  csric              = "qb45-rw2t",
  broadband_dec2020  = "hicn-aujz",
  broadband_dec2019  = "whue-6pnt",
  broadband_jun2021  = "jdr4-3q4p",
  broadband_jun2020  = "4kuc-phrr",
  psap_registry      = "dpq5-ta9j",
  earth_stations     = "acbv-jbb4",
  eas_test_firms     = "nubx-v54a",
  eas_grantees       = "3b3k-34jp",
  geography_lookup   = "v5vt-e7vw",
  provider_lookup    = "awrw-t4m8",
  area_dec2020       = "ymd4-xaiz",
  pra_collections    = "mjr7-dxdj",
  uls_3650           = "euz5-46g2",
  intermediate_provider = "a6ec-cry4",
  linked_stations    = "2ah7-n9rk"
)
