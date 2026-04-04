# ip-ranges.amazonaws.com.R - Self-contained ip-ranges.amazonaws.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# ip-ranges-amazonaws-com.R
# Self-contained AWS IP Ranges API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.aws_ip_url <- "https://ip-ranges.amazonaws.com/ip-ranges.json"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch_aws_ip <- function() {
  tmp <- tempfile(fileext = ".json")
  httr2::request(.aws_ip_url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# == Schemas ===================================================================

.schema_ranges <- tibble(
  ip_prefix = character(), region = character(),
  service = character(), network_border_group = character()
)

# == Public functions ==========================================================

#' Get AWS IPv4 IP ranges
#'
#' Fetches all AWS IPv4 CIDR prefixes from the public ip-ranges.json endpoint
#' with optional filtering by service or region. Typically returns ~9000+
#' prefixes across all services and regions.
#'
#' @param service Character or NULL. Filter by AWS service name (exact match).
#'   Use \code{awsip_services()} to see available values. Common values:
#'   \code{"S3"}, \code{"EC2"}, \code{"CLOUDFRONT"}, \code{"AMAZON"},
#'   \code{"API_GATEWAY"}, \code{"ROUTE53_RESOLVER"}, \code{"GLOBALACCELERATOR"}.
#'   Default \code{NULL} returns all services.
#' @param region Character or NULL. Filter by region pattern (partial match via
#'   \code{grepl}). Use \code{awsip_regions()} to see available values. Examples:
#'   \code{"us-east-1"} (exact), \code{"eu-"} (all EU regions),
#'   \code{"ap-"} (all Asia-Pacific). Default \code{NULL} returns all regions.
#' @return A tibble with columns:
#'   \describe{
#'     \item{ip_prefix}{Character. IPv4 CIDR block (e.g. "18.34.0.0/19").}
#'     \item{region}{Character. AWS region (e.g. "us-east-1", "eu-west-2").}
#'     \item{service}{Character. AWS service name (e.g. "S3", "EC2").}
#'     \item{network_border_group}{Character. Network border group, usually
#'       same as region.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # All S3 ranges in us-east-1
#' awsip_ranges(service = "S3", region = "us-east-1")
#'
#' # All CloudFront ranges globally
#' awsip_ranges(service = "CLOUDFRONT")
#'
#' # All ranges in EU regions
#' awsip_ranges(region = "eu-")
#' }
awsip_ranges <- function(service = NULL, region = NULL) {
  raw <- tryCatch(.fetch_aws_ip(), error = function(e) NULL)
  if (is.null(raw)) return(.schema_ranges)

  prefixes <- raw$prefixes
  if (length(prefixes) == 0) return(.schema_ranges)

  tbl <- tibble(
    ip_prefix = vapply(prefixes, function(x) x$ip_prefix %||% NA_character_, character(1)),
    region = vapply(prefixes, function(x) x$region %||% NA_character_, character(1)),
    service = vapply(prefixes, function(x) x$service %||% NA_character_, character(1)),
    network_border_group = vapply(prefixes, function(x) x$network_border_group %||% NA_character_, character(1))
  )
  if (!is.null(service)) tbl <- tbl[tbl$service == service, ]
  if (!is.null(region)) tbl <- tbl[grepl(region, tbl$region), ]
  tbl
}

#' Get AWS IPv6 IP ranges
#'
#' Fetches all AWS IPv6 CIDR prefixes from the public ip-ranges.json endpoint
#' with optional filtering by service or region. Typically returns ~5000+
#' prefixes.
#'
#' @param service Character or NULL. Filter by AWS service name (exact match).
#'   Same values as \code{awsip_ranges()}: \code{"S3"}, \code{"EC2"},
#'   \code{"CLOUDFRONT"}, etc. Default \code{NULL} returns all.
#' @param region Character or NULL. Filter by region pattern (partial match).
#'   Examples: \code{"us-east-1"}, \code{"eu-"}, \code{"ap-"}.
#'   Default \code{NULL} returns all.
#' @return A tibble with columns:
#'   \describe{
#'     \item{ipv6_prefix}{Character. IPv6 CIDR block
#'       (e.g. "2a05:d06a:c000::/40").}
#'     \item{region}{Character. AWS region.}
#'     \item{service}{Character. AWS service name.}
#'     \item{network_border_group}{Character. Network border group.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' awsip_ipv6_ranges(service = "EC2", region = "eu-west")
#' awsip_ipv6_ranges(service = "S3")
#' }
awsip_ipv6_ranges <- function(service = NULL, region = NULL) {
  raw <- tryCatch(.fetch_aws_ip(), error = function(e) NULL)
  if (is.null(raw)) return(tibble(ipv6_prefix = character(), region = character(),
                                   service = character(), network_border_group = character()))

  prefixes <- raw$ipv6_prefixes
  if (length(prefixes) == 0) return(tibble(ipv6_prefix = character(), region = character(),
                                            service = character(), network_border_group = character()))

  tbl <- tibble(
    ipv6_prefix = vapply(prefixes, function(x) x$ipv6_prefix %||% NA_character_, character(1)),
    region = vapply(prefixes, function(x) x$region %||% NA_character_, character(1)),
    service = vapply(prefixes, function(x) x$service %||% NA_character_, character(1)),
    network_border_group = vapply(prefixes, function(x) x$network_border_group %||% NA_character_, character(1))
  )
  if (!is.null(service)) tbl <- tbl[tbl$service == service, ]
  if (!is.null(region)) tbl <- tbl[grepl(region, tbl$region), ]
  tbl
}

#' List all AWS services in IP ranges
#'
#' Summarizes the AWS ip-ranges.json data by service, showing the number of
#' IPv4 and IPv6 CIDR prefixes allocated to each. Typically returns ~26
#' services sorted by IPv4 count descending. Useful for discovering valid
#' service names for \code{awsip_ranges()} and \code{awsip_ipv6_ranges()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{service}{Character. AWS service name (e.g. "AMAZON", "EC2", "S3",
#'       "CLOUDFRONT", "API_GATEWAY", "ROUTE53_RESOLVER").}
#'     \item{ipv4_count}{Integer. Number of IPv4 CIDR prefixes.}
#'     \item{ipv6_count}{Integer. Number of IPv6 CIDR prefixes.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' awsip_services()
#' }
awsip_services <- function() {
  raw <- tryCatch(.fetch_aws_ip(), error = function(e) NULL)
  if (is.null(raw)) return(tibble(service = character(), ipv4_count = integer(), ipv6_count = integer()))

  v4 <- vapply(raw$prefixes, function(x) x$service %||% NA_character_, character(1))
  v6 <- vapply(raw$ipv6_prefixes, function(x) x$service %||% NA_character_, character(1))

  v4_counts <- as.data.frame(table(v4), stringsAsFactors = FALSE)
  names(v4_counts) <- c("service", "ipv4_count")
  v6_counts <- as.data.frame(table(v6), stringsAsFactors = FALSE)
  names(v6_counts) <- c("service", "ipv6_count")

  result <- merge(v4_counts, v6_counts, by = "service", all = TRUE)
  result$ipv4_count[is.na(result$ipv4_count)] <- 0L
  result$ipv6_count[is.na(result$ipv6_count)] <- 0L
  as_tibble(result) |>
    mutate(ipv4_count = as.integer(ipv4_count), ipv6_count = as.integer(ipv6_count)) |>
    arrange(desc(ipv4_count))
}

#' List all AWS regions in IP ranges
#'
#' Summarizes the AWS ip-ranges.json data by region, showing the number of
#' IPv4 and IPv6 CIDR prefixes in each. Typically returns ~42 regions sorted
#' by IPv4 count descending. Useful for discovering valid region patterns for
#' \code{awsip_ranges()} and \code{awsip_ipv6_ranges()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{region}{Character. AWS region (e.g. "us-east-1", "eu-central-1",
#'       "ap-southeast-1", "GLOBAL").}
#'     \item{ipv4_count}{Integer. Number of IPv4 CIDR prefixes.}
#'     \item{ipv6_count}{Integer. Number of IPv6 CIDR prefixes.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' awsip_regions()
#' }
awsip_regions <- function() {
  raw <- tryCatch(.fetch_aws_ip(), error = function(e) NULL)
  if (is.null(raw)) return(tibble(region = character(), ipv4_count = integer(), ipv6_count = integer()))

  v4 <- vapply(raw$prefixes, function(x) x$region %||% NA_character_, character(1))
  v6 <- vapply(raw$ipv6_prefixes, function(x) x$region %||% NA_character_, character(1))

  v4_counts <- as.data.frame(table(v4), stringsAsFactors = FALSE)
  names(v4_counts) <- c("region", "ipv4_count")
  v6_counts <- as.data.frame(table(v6), stringsAsFactors = FALSE)
  names(v6_counts) <- c("region", "ipv6_count")

  result <- merge(v4_counts, v6_counts, by = "region", all = TRUE)
  result$ipv4_count[is.na(result$ipv4_count)] <- 0L
  result$ipv6_count[is.na(result$ipv6_count)] <- 0L
  as_tibble(result) |>
    mutate(ipv4_count = as.integer(ipv4_count), ipv6_count = as.integer(ipv6_count)) |>
    arrange(desc(ipv4_count))
}

# == Context ===================================================================

#' Get amazonaws.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
awsip_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(awsip_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/amazonaws.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "amazonaws.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# amazonaws.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# amazonaws.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
