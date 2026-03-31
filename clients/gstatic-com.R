# gstatic-com.R
# Self-contained Google Cloud IP Ranges client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none (static JSON file)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gcp_url <- "https://www.gstatic.com/ipranges/cloud.json"

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
  ip_prefix = character(), service = character(), scope = character()
)

# == Public functions ==========================================================

#' Fetch Google Cloud IP address ranges
#'
#' Returns the current list of Google Cloud IP address ranges, optionally
#' filtered by service and/or scope (region).
#'
#' @param service Optional filter by service name (e.g. "Google Cloud").
#'   Case-insensitive.
#' @param scope Optional filter by scope/region (e.g. "us-east1", "europe-west1")
#' @return tibble: ip_prefix, service, scope
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

#' Show Google Cloud IP Ranges package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
gcpip_context <- function() {
  .build_context(
    pkg_name = "gstatic.com",
    header_lines = c(
      "# gstatic.com",
      "# Google Cloud IP Address Ranges Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Source: https://www.gstatic.com/ipranges/cloud.json"
    )
  )
}
