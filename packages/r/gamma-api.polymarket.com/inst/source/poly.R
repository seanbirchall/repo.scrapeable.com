



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://gamma-api.polymarket.com"

#' Get Polymarket markets
#' @param limit Number of markets (default 25)
#' @param active Show only active markets (default TRUE)
#' @return tibble of markets
#' @export
poly_markets <- function(limit = 25L, active = TRUE) {
  url <- sprintf("%s/markets?limit=%d&active=%s", .base, limit, tolower(as.character(active)))
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(tibble::tibble(id = character(), question = character()))
  tibble::tibble(
    id = vapply(raw, function(x) x$id %||% NA_character_, character(1)),
    question = vapply(raw, function(x) x$question %||% NA_character_, character(1)),
    end_date = vapply(raw, function(x) x$endDate %||% NA_character_, character(1)),
    volume = vapply(raw, function(x) as.numeric(x$volume %||% NA), numeric(1)),
    active = vapply(raw, function(x) x$active %||% NA, logical(1))
  )
}

#' Get details for a specific Polymarket market
#' @param market_id Market ID or slug
#' @return tibble with market details
#' @export
poly_market <- function(market_id) {
  url <- sprintf("%s/markets/%s", .base, market_id)
  raw <- .fetch_json(url)
  tibble::tibble(
    id = raw$id %||% NA_character_,
    question = raw$question %||% NA_character_,
    description = raw$description %||% NA_character_,
    end_date = raw$endDate %||% NA_character_,
    volume = as.numeric(raw$volume %||% NA),
    active = raw$active %||% NA
  )
}

#' Generate LLM context for gamma-api.polymarket.com
#' @return Character string
#' @export
poly_context <- function() {
  .build_context("gamma-api.polymarket.com")
}


# == Context ===================================================================

#' Generate LLM-friendly context for gamma-api.polymarket.com
#'
#' @return Character string with full function signatures and bodies
gamma-api_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/gamma-api.polymarket.com.R"
  if (!file.exists(src_file)) {
    cat("# gamma-api.polymarket.com context - source not found\n")
    return(invisible("# gamma-api.polymarket.com context - source not found"))
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

