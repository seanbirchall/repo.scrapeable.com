# norges-bank.no.R - Self-contained norges-bank.no client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

.ua <- "support@scrapeable.com"
.nb_base <- "https://data.norges-bank.no/api/data"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

#' Get Norges Bank exchange rates (SDMX-JSON)
#'
#' Retrieves recent exchange rates against the Norwegian krone (NOK) from
#' the Norges Bank SDMX-JSON data service. Returns the last 100 observations
#' for a given currency.
#'
#' @param currency ISO 4217 currency code (e.g. "USD", "EUR", "GBP").
#' @param frequency Frequency code: "B" for business daily (default),
#'   "M" for monthly average.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Observation date (character, YYYY-MM-DD format)}
#'     \item{value}{Exchange rate: NOK per 1 unit of foreign currency}
#'     \item{currency}{ISO currency code}
#'   }
#' @examples
#' nb_exchange_rates("USD")
#' nb_exchange_rates("EUR", frequency = "M")
#' @seealso [nb_policy_rate()], [nb_multi_rates()], [nb_context()]
#' @source <https://data.norges-bank.no>
nb_exchange_rates <- function(currency = "USD", frequency = "B") {
  url <- sprintf("%s/EXR/%s.%s.NOK.SP?format=sdmx-json&lastNObservations=100",
                 .nb_base, frequency, currency)
  raw <- .fetch_json(url)
  ds <- raw$data$dataSets
  if (length(ds) == 0 || length(ds[[1]]$series) == 0) return(tibble::tibble(date = character(), value = numeric()))
  obs <- ds[[1]]$series[[1]]$observations
  if (length(obs) == 0) return(tibble::tibble(date = character(), value = numeric()))
  dims <- raw$data$structure$dimensions$observation
  time_vals <- dims[[1]]$values
  tibble::tibble(
    date = vapply(seq_along(obs), function(i) {
      idx <- as.integer(names(obs)[i]) + 1
      if (idx <= length(time_vals)) time_vals[[idx]]$id else NA_character_
    }, character(1)),
    value = vapply(obs, function(x) as.numeric(x[[1]]), numeric(1)),
    currency = currency
  )
}

#' Get Norges Bank key policy rate
#'
#' Retrieves the most recent key policy rate (styringsrenten) observations
#' from the Norges Bank SDMX-JSON interest rate dataset. Returns the last
#' 50 observations.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Observation date (character, YYYY-MM-DD format)}
#'     \item{rate}{Key policy rate as a percentage}
#'   }
#' @examples
#' \dontrun{
#' nb_policy_rate()
#' }
#' @seealso [nb_exchange_rates()], [nb_multi_rates()], [nb_context()]
#' @source <https://data.norges-bank.no>
nb_policy_rate <- function() {
  url <- sprintf("%s/IR/B..KPRA?format=sdmx-json&lastNObservations=50", .nb_base)
  raw <- .fetch_json(url)
  ds <- raw$data$dataSets
  if (length(ds) == 0 || length(ds[[1]]$series) == 0) return(tibble::tibble(date = character(), rate = numeric()))
  obs <- ds[[1]]$series[[1]]$observations
  dims <- raw$data$structure$dimensions$observation
  time_vals <- dims[[1]]$values
  tibble::tibble(
    date = vapply(seq_along(obs), function(i) {
      idx <- as.integer(names(obs)[i]) + 1
      if (idx <= length(time_vals)) time_vals[[idx]]$id else NA_character_
    }, character(1)),
    rate = vapply(obs, function(x) as.numeric(x[[1]]), numeric(1))
  )
}

#' Get multiple Norges Bank exchange rates
#'
#' Convenience function that fetches recent exchange rates for multiple
#' currencies in a single call. Combines results from individual
#' \code{\link{nb_exchange_rates}} calls into one tibble.
#'
#' @param currencies Character vector of ISO 4217 currency codes
#'   (default: \code{c("USD", "EUR", "GBP")}).
#' @param n Last N observations per currency (default 20). Currently
#'   passed through to the underlying API which returns the last 100.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Observation date (character)}
#'     \item{value}{Exchange rate: NOK per 1 unit of foreign currency}
#'     \item{currency}{ISO currency code}
#'   }
#' @examples
#' nb_multi_rates(c("USD", "EUR"), n = 10)
#' @seealso [nb_exchange_rates()], [nb_policy_rate()], [nb_context()]
#' @source <https://data.norges-bank.no>
nb_multi_rates <- function(currencies = c("USD", "EUR", "GBP"), n = 20) {
  schema <- tibble(date = character(), value = numeric(), currency = character())
  results <- lapply(currencies, function(cur) {
    tryCatch(nb_exchange_rates(cur, frequency = "B"), error = function(e) schema)
  })
  bind_rows(results)
}

# == Context ===================================================================

#' Get norges-bank.no client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the Norges Bank SDMX-JSON client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' nb_context()
#' @export
nb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/norges-bank.no.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "norges-bank.no")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# norges-bank.no context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# norges-bank.no", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
