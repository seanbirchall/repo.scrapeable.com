#' @import dplyr
#' @importFrom tibble tibble as_tibble tribble
#' @importFrom httr2 request req_headers req_url_query req_options req_error req_perform resp_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils head
#' @keywords internal
NULL

# transportation.gov.R
# Self-contained data.transportation.gov (US DOT) Socrata client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public data). Optional app token reduces throttling.
# Base: https://data.transportation.gov (Socrata SODA, follows redirects)
# Docs: https://dev.socrata.com/docs/queries/

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.dot_base <- "https://data.transportation.gov"

# -- SODA query engine ---------------------------------------------------------

.dot_soda <- function(dataset_id, where = NULL, select = NULL, group = NULL,
                      order = NULL, q = NULL, limit = 1000, offset = 0,
                      token = NULL, max_results = NULL) {
  all_data <- list()
  current_offset <- offset
  page_size <- min(limit, 50000)

  repeat {
    base_url <- paste0(.dot_base, "/resource/", dataset_id, ".json")
    req <- httr2::request(base_url) |>
      httr2::req_url_query(`$limit` = page_size, `$offset` = current_offset)
    if (!is.null(where))  req <- req |> httr2::req_url_query(`$where` = where)
    if (!is.null(select)) req <- req |> httr2::req_url_query(`$select` = select)
    if (!is.null(group))  req <- req |> httr2::req_url_query(`$group` = group)
    if (!is.null(order))  req <- req |> httr2::req_url_query(`$order` = order)
    if (!is.null(q))      req <- req |> httr2::req_url_query(`$q` = q)
    if (!is.null(token))  req <- req |> httr2::req_url_query(`$$app_token` = token)

    tmp <- tempfile(fileext = ".json")
    resp <- tryCatch(
      req |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_options(followlocation = TRUE) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform(path = tmp),
      error = function(e) NULL
    )
    if (is.null(resp)) break
    if (httr2::resp_status(resp) >= 400) break
    raw <- tryCatch(jsonlite::fromJSON(tmp), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) break

    all_data[[length(all_data) + 1]] <- as_tibble(raw)

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break
    if (is.data.frame(raw) && nrow(raw) < page_size) break
    current_offset <- current_offset + page_size
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)
  result
}
