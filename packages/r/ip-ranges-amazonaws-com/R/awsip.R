# ip-ranges-amazonaws-com.R
# Self-contained AWS IP Ranges client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none (static JSON file)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.aws_url <- "https://ip-ranges.amazonaws.com/ip-ranges.json"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
  ip_prefix = character(), region = character(), service = character(),
  network_border_group = character()
)

# == Public functions ==========================================================

#' Fetch AWS IP address ranges
#'
#' Downloads and parses the official AWS IP address ranges JSON file. Returns
#' IPv4 CIDR prefixes for AWS services, optionally filtered by service name
#' and/or region. Useful for firewall rules, network security analysis, and
#' infrastructure planning.
#'
#' @param service Optional AWS service name filter (e.g. "S3", "EC2",
#'   "CLOUDFRONT", "AMAZON", "ROUTE53"). Case-insensitive. When NULL (default),
#'   returns all services.
#' @param region Optional AWS region filter (e.g. "us-east-1", "eu-west-1",
#'   "ap-southeast-2"). Matches against both the region and
#'   network_border_group fields. When NULL (default), returns all regions.
#' @return A tibble with columns:
#'   \describe{
#'     \item{ip_prefix}{IPv4 CIDR block (e.g. "54.231.0.0/16")}
#'     \item{region}{AWS region (e.g. "us-east-1")}
#'     \item{service}{AWS service name (e.g. "S3", "EC2")}
#'     \item{network_border_group}{Network border group (usually same as region)}
#'   }
#' @examples
#' awsip_ranges(service = "S3", region = "us-east-1")
#' awsip_ranges(service = "CLOUDFRONT")
#' @seealso [awsip_context()]
#' @source <https://ip-ranges.amazonaws.com/ip-ranges.json>
#' @export
awsip_ranges <- function(service = NULL, region = NULL) {
  raw <- .fetch_json(.aws_url)
  prefixes <- raw$prefixes
  if (is.null(prefixes) || nrow(prefixes) == 0) return(.schema_ranges)

  result <- as_tibble(prefixes) |>
    select(ip_prefix, region, service, network_border_group)

  if (!is.null(service)) {
    result <- result |> filter(toupper(service) == toupper(!!service))
  }
  if (!is.null(region)) {
    result <- result |> filter(region == !!region | network_border_group == !!region)
  }

  result
}

#' Get ip-ranges-amazonaws-com client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the AWS IP ranges client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' awsip_context()
#' @export
awsip_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(awsip_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ip-ranges-amazonaws-com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ip-ranges-amazonaws-com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ip-ranges-amazonaws-com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ip-ranges-amazonaws-com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
