# gstatic.com.R - Self-contained gstatic.com client



# gstatic-com.R
# Self-contained Google Cloud IP Ranges client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none (static JSON file)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gcp_url <- "https://www.gstatic.com/ipranges/cloud.json"

`%||%` <- function(a, b) if (is.null(a)) b else a
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_ranges <- tibble(
  ip_prefix = character(), service = character(), scope = character()
)

# == Public functions ==========================================================

#' Fetch Google Cloud IP address ranges
#'
#' Downloads and parses the official Google Cloud IP ranges JSON file.
#' Returns IPv4 and IPv6 CIDR prefixes for Google Cloud services,
#' optionally filtered by service name and/or scope (region).
#'
#' @param service Optional filter by service name (e.g. "Google Cloud").
#'   Case-insensitive.
#' @param scope Optional filter by scope/region (e.g. "us-east1", "europe-west1")
#' @return A tibble with columns:
#'   \describe{
#'     \item{ip_prefix}{IPv4 or IPv6 CIDR block (character)}
#'     \item{service}{Google Cloud service name (character)}
#'     \item{scope}{Region/scope identifier (character)}
#'   }
#' @examples
#' gcpip_ranges()
#' gcpip_ranges(scope = "us-east1")
#' @seealso [gcpip_services()], [gcpip_regions()], [gstatic_context()]
#' @source <https://www.gstatic.com/ipranges/cloud.json>
#' @export
gcpip_ranges <- function(service = NULL, scope = NULL) {
  raw <- .fetch_json(.gcp_url)
  prefixes <- raw$prefixes
  if (is.null(prefixes) || length(prefixes) == 0) return(.schema_ranges)

  df <- as_tibble(prefixes)
  # Combine ipv4Prefix and ipv6Prefix into ip_prefix
  if ("ipv4Prefix" %in% names(df) && "ipv6Prefix" %in% names(df)) {
    df <- df |> mutate(ip_prefix = ifelse(is.na(ipv4Prefix), ipv6Prefix, ipv4Prefix))
  } else if ("ipv4Prefix" %in% names(df)) {
    df <- df |> mutate(ip_prefix = ipv4Prefix)
  } else if ("ipv6Prefix" %in% names(df)) {
    df <- df |> mutate(ip_prefix = ipv6Prefix)
  } else {
    return(.schema_ranges)
  }

  result <- df |> select(ip_prefix, service, scope)

  if (!is.null(service)) {
    result <- result |> filter(toupper(service) == toupper(!!service))
  }
  if (!is.null(scope)) {
    result <- result |> filter(scope == !!scope)
  }

  result
}

#' List unique GCP services with IP range counts
#'
#' Summarizes the Google Cloud IP ranges by service, showing how many
#' IPv4 and IPv6 prefixes each service has.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{service}{Google Cloud service name (character)}
#'     \item{ipv4_count}{Number of IPv4 CIDR blocks (integer)}
#'     \item{ipv6_count}{Number of IPv6 CIDR blocks (integer)}
#'     \item{total}{Total prefix count (integer)}
#'   }
#' @examples
#' gcpip_services()
#' @seealso [gcpip_ranges()], [gcpip_regions()], [gstatic_context()]
#' @export
gcpip_services <- function() {
  schema <- tibble(service = character(), ipv4_count = integer(),
                   ipv6_count = integer(), total = integer())
  raw <- tryCatch(.fetch_json(.gcp_url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$prefixes)) return(schema)

  df <- as_tibble(raw$prefixes)
  if (nrow(df) == 0) return(schema)

  df |>
    mutate(has_v4 = !is.na(ipv4Prefix), has_v6 = !is.na(ipv6Prefix)) |>
    group_by(service) |>
    summarise(
      ipv4_count = sum(has_v4),
      ipv6_count = sum(has_v6),
      total = n(),
      .groups = "drop"
    ) |>
    arrange(desc(total))
}

#' List unique GCP regions/scopes with IP range counts
#'
#' Summarizes the Google Cloud IP ranges by region/scope, showing the
#' number of prefixes allocated to each region.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{scope}{Region/scope identifier, e.g. "us-east1" (character)}
#'     \item{count}{Number of IP prefix entries (integer)}
#'   }
#' @examples
#' gcpip_regions()
#' @seealso [gcpip_ranges()], [gcpip_services()], [gstatic_context()]
#' @export
gcpip_regions <- function() {
  schema <- tibble(scope = character(), count = integer())
  raw <- tryCatch(.fetch_json(.gcp_url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$prefixes)) return(schema)

  df <- as_tibble(raw$prefixes)
  if (nrow(df) == 0) return(schema)

  df |>
    group_by(scope) |>
    summarise(count = n(), .groups = "drop") |>
    arrange(desc(count))
}

# == Context ===================================================================

#' Get gstatic.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gstatic_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gstatic_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gstatic.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gstatic.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gstatic.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gstatic.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
