#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_error
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.fdic_base <- "https://banks.data.fdic.gov/api"

# Core fetch engine
.fdic_get <- function(endpoint, filters = NULL, fields = NULL, sort_by = NULL,
                      sort_order = "DESC", limit = 100, offset = 0) {
  params <- list()
  if (!is.null(filters) && nchar(filters) > 0) params$filters <- filters
  if (!is.null(fields))  params$fields <- fields
  if (!is.null(sort_by)) params$sort_by <- sort_by
  params$sort_order <- sort_order
  params$limit <- min(limit, 10000)
  if (offset > 0) params$offset <- offset

  query <- paste(names(params),
                 sapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE)),
                 sep = "=", collapse = "&")
  url <- paste0(.fdic_base, "/", endpoint, "?", query)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  if (!is.null(raw$error)) {
    stop("FDIC API error: ", raw$error$message %||% "unknown", call. = FALSE)
  }
  raw
}

# Parse FDIC response data into tibble
.fdic_parse <- function(raw) {
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(tibble::tibble())
  dplyr::bind_rows(lapply(data, function(item) {
    d <- item$data
    flat <- lapply(d, function(v) {
      if (is.null(v)) NA_character_ else as.character(v)
    })
    tibble::as_tibble(flat)
  }))
}

# Paginated fetch
.fdic_fetch_all <- function(endpoint, filters = NULL, fields = NULL,
                            sort_by = NULL, sort_order = "DESC",
                            max_results = 1000) {
  all_rows <- list()
  offset <- 0
  page_size <- min(max_results, 10000)

  repeat {
    raw <- .fdic_get(endpoint, filters = filters, fields = fields,
                     sort_by = sort_by, sort_order = sort_order,
                     limit = page_size, offset = offset)
    total <- raw$meta$total %||% 0
    batch <- .fdic_parse(raw)
    if (nrow(batch) == 0) break

    all_rows[[length(all_rows) + 1]] <- batch
    offset <- offset + nrow(batch)

    if (offset >= total) break
    if (offset >= max_results) break
  }

  if (length(all_rows) == 0) return(tibble::tibble())
  result <- dplyr::bind_rows(all_rows)
  if (nrow(result) > max_results) result <- result[seq_len(max_results), ]
  result
}

.inst_fields <- paste0(
  "CERT,NAME,CITY,STNAME,STALP,ZIP,ASSET,DEP,NETINC,REPDTE,",
  "ACTIVE,CHARTER,SPECGRP,ESTYMD,INSDATE,NAMEHCR,WEBADDR,RISESSION"
)

.fin_fields <- paste0(
  "CERT,REPDTE,ASSET,DEP,NETINC,EQ,LNLSNET,SC,INTINC,EINTEXP,ROA,ROE,",
  "NITEFAAV,ELNATR,NCLNLS,NUMEMP"
)

.fail_fields <- paste0(
  "CERT,NAME,CITY,PSTALP,FAILDATE,FAILYR,SAVR,RESTYPE,RESTYPE1,",
  "COST,QBFASSET,QBFDEP"
)
