#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_retry req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# somervillema.gov - City of Somerville Open Data (Socrata SODA 2.0) client
# 46 datasets (27 queryable Socrata views)
# No auth required. Rate limits: ~1000 req/hour without app token.
#
# Dependencies: httr2, jsonlite, dplyr, tibble

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.somer_base <- "https://data.somervillema.gov"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

._soda_query <- function(view_id, where = NULL, select = NULL,
                         order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query_str <- paste(
    names(params),
    utils::URLencode(as.character(params), reserved = TRUE),
    sep = "=", collapse = "&"
  )
  url <- sprintf("%s/resource/%s.json?%s", .somer_base, view_id, query_str)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

._soda_all <- function(view_id, where = NULL, select = NULL,
                       order = NULL, q = NULL,
                       page_size = 5000, max_rows = 50000) {
  collected <- list()
  offset <- 0
  repeat {
    lim <- min(page_size, max_rows - offset)
    if (lim <= 0) break
    chunk <- ._soda_query(view_id, where = where, select = select,
                          order = order, q = q,
                          limit = lim, offset = offset)
    if (nrow(chunk) == 0) break
    collected[[length(collected) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < lim) break
  }
  if (length(collected) == 0) return(tibble::tibble())
  dplyr::bind_rows(collected)
}

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

# == Schemas ===================================================================

.schema_catalog <- tibble::tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(),
  updated_at = as.POSIXct(character()), view_count = integer()
)

.schema_cad <- tibble::tibble(
  incnum = character(), day_and_month = character(), year = integer(),
  police_shift = character(), inctype = character(), incdesc = character(),
  ward = character()
)

.schema_crimes <- tibble::tibble(
  incnum = character(), day_and_month = character(), year = integer(),
  police_shift = character(), offense = character(), offensetype = character(),
  category = character(), ward = character()
)

.schema_311 <- tibble::tibble(
  id = character(), classification = character(), category = character(),
  type = character(), most_recent_status = character(),
  date_created = as.POSIXct(character())
)

.schema_permits <- tibble::tibble(
  id = character(), type = character(), status = character(),
  address = character(), work = character(),
  application_date = as.POSIXct(character()),
  issue_date = as.POSIXct(character()), amount = numeric()
)

.schema_citations <- tibble::tibble(
  citationnum = character(), dtissued = as.POSIXct(character()),
  address = character(), chgdesc = character(), warning = character(),
  ward = character()
)
