# ripe.net.R - Self-contained ripe.net client



# ripe-net.R
# Self-contained RIPE Stat API client for network data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ripe_base <- "https://stat.ripe.net/data"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_prefixes <- tibble(
  prefix = character(), timelines = character()
)

.schema_country_resources <- tibble(
  resource = character(), type = character(), status = character()
)

.schema_whois <- tibble(
  key = character(), value = character()
)

.schema_geoloc <- tibble(
  prefix = character(), latitude = numeric(), longitude = numeric(),
  city = character(), country = character()
)


# == Announced Prefixes ========================================================

#' Get announced IP prefixes for an ASN
#'
#' Queries the RIPE Stat API for all IP prefixes currently announced via BGP
#' by a given Autonomous System Number. Useful for mapping the address space
#' an organization originates.
#'
#' @param resource Character. ASN with or without the "AS" prefix
#'   (e.g. \code{"AS3333"}, \code{"3333"}), or an IP prefix
#'   (e.g. \code{"193.0.0.0/21"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{prefix}{Character. Announced IP prefix in CIDR notation (e.g. \code{"193.0.0.0/21"}).}
#'     \item{timelines}{Character. Semicolon-separated time ranges showing when the prefix was visible (e.g. \code{"2026-03-20T00:00:00-2026-04-03T00:00:00"}).}
#'   }
#' @examples
#' ripe_prefixes("AS3333")
#' ripe_prefixes("3333")
#' @export
ripe_prefixes <- function(resource) {
  url <- sprintf("%s/announced-prefixes/data.json?resource=%s&sourceapp=%s",
                 .ripe_base, utils::URLencode(resource), .ua)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RIPE prefixes failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_prefixes)

  prefixes <- raw$data$prefixes
  if (is.null(prefixes) || length(prefixes) == 0) return(.schema_prefixes)

  tibble(
    prefix    = as.character(prefixes$prefix %||% NA_character_),
    timelines = vapply(prefixes$timelines, function(t) {
      if (is.null(t) || length(t) == 0) return(NA_character_)
      paste(sprintf("%s-%s", t$starttime %||% "?", t$endtime %||% "?"), collapse = "; ")
    }, character(1))
  )
}

# == Country Resources =========================================================

#' Get internet number resources allocated to a country
#'
#' Returns all ASNs and IP prefixes (IPv4 and IPv6) registered to a given
#' country in the RIPE database.
#'
#' @param country Character. ISO 3166-1 alpha-2 country code
#'   (e.g. \code{"NL"}, \code{"US"}, \code{"DE"}, \code{"GB"}).
#'   Case-insensitive.
#' @return A tibble with columns:
#'   \describe{
#'     \item{resource}{Character. The ASN number or IP prefix (e.g. \code{"1101"}, \code{"193.0.0.0/21"}).}
#'     \item{type}{Character. Resource type: \code{"asn"}, \code{"ipv4"}, or \code{"ipv6"}.}
#'     \item{status}{Character. Always \code{"active"}.}
#'   }
#' @examples
#' ripe_country_resources("NL")
#' ripe_country_resources("DE")
#' @export
ripe_country_resources <- function(country) {
  url <- sprintf("%s/country-resource-list/data.json?resource=%s&sourceapp=%s",
                 .ripe_base, utils::URLencode(toupper(country)), .ua)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RIPE country resources failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_country_resources)

  resources <- raw$data$resources
  if (is.null(resources)) return(.schema_country_resources)

  rows <- list()
  for (rtype in names(resources)) {
    vals <- resources[[rtype]]
    if (length(vals) > 0) {
      rows[[length(rows) + 1]] <- tibble(
        resource = as.character(vals),
        type     = rtype,
        status   = "active"
      )
    }
  }
  if (length(rows) == 0) return(.schema_country_resources)
  bind_rows(rows)
}

# == WHOIS =====================================================================

#' WHOIS lookup via RIPE Stat
#'
#' Retrieves WHOIS registration records for an IP address, prefix, or ASN
#' from the RIPE database. Returns key-value pairs from all matching
#' WHOIS record groups (e.g. aut-num, inetnum, organisation).
#'
#' @param resource Character. An IP address (e.g. \code{"193.0.6.139"}),
#'   IP prefix (e.g. \code{"193.0.0.0/21"}), or ASN
#'   (e.g. \code{"AS3333"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{Character. WHOIS field name (e.g. \code{"aut-num"}, \code{"as-name"}, \code{"descr"}, \code{"org"}, \code{"admin-c"}).}
#'     \item{value}{Character. Corresponding field value.}
#'   }
#' @examples
#' ripe_whois("AS3333")
#' ripe_whois("193.0.6.139")
#' @export
ripe_whois <- function(resource) {
  url <- sprintf("%s/whois/data.json?resource=%s&sourceapp=%s",
                 .ripe_base, utils::URLencode(resource), .ua)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RIPE whois failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_whois)

  records <- raw$data$records
  if (is.null(records) || length(records) == 0) return(.schema_whois)

  rows <- list()
  for (rec_group in records) {
    if (is.data.frame(rec_group)) {
      rows[[length(rows) + 1]] <- tibble(
        key   = as.character(rec_group$key %||% NA_character_),
        value = as.character(rec_group$value %||% NA_character_)
      )
    }
  }
  if (length(rows) == 0) return(.schema_whois)
  bind_rows(rows)
}

# == Context ===================================================================

#' Get ripe.net client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' ripe_context()
#' @export
ripe_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ripe_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ripe.net.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ripe.net")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ripe.net context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ripe.net", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
