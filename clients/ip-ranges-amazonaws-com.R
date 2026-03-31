# ip-ranges-amazonaws-com.R
# Self-contained AWS IP Ranges client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none (static JSON file)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
#' Returns the current list of AWS IP address ranges, optionally filtered
#' by service and/or region.
#'
#' @param service Optional filter by AWS service name (e.g. "S3", "EC2",
#'   "CLOUDFRONT", "AMAZON"). Case-insensitive.
#' @param region Optional filter by AWS region (e.g. "us-east-1", "eu-west-1").
#'   Matches region and network_border_group.
#' @return tibble: ip_prefix, region, service, network_border_group
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

#' Show AWS IP Ranges package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
awsip_context <- function() {
  .build_context(
    pkg_name = "ip.ranges.amazonaws.com",
    header_lines = c(
      "# ip.ranges.amazonaws.com",
      "# AWS IP Address Ranges Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Source: https://ip-ranges.amazonaws.com/ip-ranges.json",
      "#",
      "# Common services: AMAZON, S3, EC2, CLOUDFRONT, ROUTE53,",
      "#   API_GATEWAY, DYNAMODB, GLOBALACCELERATOR"
    )
  )
}
