# 18f.gov.R - Self-contained 18F / GSA government API catalog client
#
# Data source: 18F API-All-the-X project (GitHub)
# Two datasets:
#   - Individual government APIs (org -> list of APIs with name + url)
#   - Government developer hubs (org -> url)
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.apis_url <- "https://raw.githubusercontent.com/18F/API-All-the-X/master/_data/individual_apis.yml"
.hubs_url <- "https://raw.githubusercontent.com/18F/API-All-the-X/master/_data/developer_hubs.yml"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch_text <- function(url) {

  tmp <- tempfile(fileext = ".txt")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  readLines(tmp, warn = FALSE)
}

.parse_apis_yaml <- function(lines) {
  # Structure:
  # - name: Org Name
  #   apis:
  #   - name: "API Name"
  #     url: "https://..."
  orgs <- character()
  api_names <- character()
  api_urls <- character()

  current_org <- NA_character_
  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]
    # Match org-level "- name: ..."
    if (grepl("^- name:\\s*", line)) {
      current_org <- trimws(sub("^- name:\\s*", "", line))
      current_org <- gsub("^\"|\"$", "", current_org)
    }
    # Match api-level "  - name: ..."
    if (grepl("^  - name:\\s*", line)) {
      api_name <- trimws(sub("^  - name:\\s*", "", line))
      api_name <- gsub("^\"|\"$", "", api_name)
      api_url <- NA_character_
      # Next line should be url
      if (i + 1 <= n && grepl("^    url:\\s*", lines[i + 1])) {
        api_url <- trimws(sub("^    url:\\s*", "", lines[i + 1]))
        api_url <- gsub("^\"|\"$", "", api_url)
        i <- i + 1L
      }
      orgs <- c(orgs, current_org)
      api_names <- c(api_names, api_name)
      api_urls <- c(api_urls, api_url)
    }
    i <- i + 1L
  }

  tibble(
    organization = as.character(orgs),
    api_name     = as.character(api_names),
    api_url      = as.character(api_urls)
  )
}

.parse_hubs_yaml <- function(lines) {
  # Structure:
  # Org Name:
  #   url: http://...
  hub_names <- character()
  hub_urls  <- character()

  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]
    # Skip blank lines and comments
    if (nzchar(trimws(line)) && !grepl("^\\s*#", line)) {
      # Match "Org Name:" at start of line (not indented)
      if (grepl("^[A-Za-z].*:\\s*$", line)) {
        hub_name <- sub(":\\s*$", "", line)
        hub_url <- NA_character_
        # Next non-blank line should be "  url: ..."
        j <- i + 1L
        while (j <= n && !nzchar(trimws(lines[j]))) j <- j + 1L
        if (j <= n && grepl("^\\s+url:\\s*", lines[j])) {
          hub_url <- trimws(sub("^\\s+url:\\s*", "", lines[j]))
          hub_url <- gsub("^\"|\"$", "", hub_url)
        }
        hub_names <- c(hub_names, hub_name)
        hub_urls  <- c(hub_urls, hub_url)
      }
    }
    i <- i + 1L
  }

  tibble(
    organization = as.character(hub_names),
    hub_url      = as.character(hub_urls)
  )
}

# == Schemas ===================================================================

.schema_apis <- tibble(
  organization = character(),
  api_name     = character(),
  api_url      = character()
)

.schema_hubs <- tibble(
  organization = character(),
  hub_url      = character()
)

.schema_catalog <- tibble(
  name         = character(),
  type         = character(),
  url          = character(),
  organization = character()
)

# == Public functions ==========================================================

#' List all US government APIs from the 18F catalog
#'
#' Fetches and parses the individual_apis dataset from 18F's API-All-the-X
#' project on GitHub. Returns a tibble with one row per API endpoint published
#' by US federal agencies, including the sponsoring organization, API name,
#' and documentation URL. Typically returns ~500 entries spanning dozens of
#' agencies (NASA, USGS, HHS, etc.).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{organization}{Character. Federal agency or sub-agency name
#'       (e.g. "National Aeronautics and Space Administration", "White House").}
#'     \item{api_name}{Character. Name of the API
#'       (e.g. "Astronomy Picture of the Day API", "Policy Snapshots").}
#'     \item{api_url}{Character. URL to the API documentation or endpoint.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' apis <- gsa_apis()
#' apis
#' # Filter to NASA APIs
#' apis[grepl("NASA|Aeronautics", apis$organization), ]
#' }
gsa_apis <- function() {
  lines <- tryCatch(.fetch_text(.apis_url), error = function(e) {
    warning("Failed to fetch APIs data: ", conditionMessage(e))
    return(character())
  })
  if (length(lines) == 0) return(.schema_apis)
  .parse_apis_yaml(lines)
}

#' List all US government developer hubs from the 18F catalog
#'
#' Fetches and parses the developer_hubs dataset from 18F's API-All-the-X
#' project on GitHub. Developer hubs are agency landing pages that aggregate
#' API documentation, SDKs, and data resources. Typically returns ~109 entries.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{organization}{Character. Federal agency or sub-agency name
#'       (e.g. "Department of Agriculture", "Census Bureau").}
#'     \item{hub_url}{Character. URL to the agency's developer hub page.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hubs <- gsa_hubs()
#' hubs
#' }
gsa_hubs <- function() {
  lines <- tryCatch(.fetch_text(.hubs_url), error = function(e) {
    warning("Failed to fetch hubs data: ", conditionMessage(e))
    return(character())
  })
  if (length(lines) == 0) return(.schema_hubs)
  .parse_hubs_yaml(lines)
}

#' Search across government APIs and developer hubs
#'
#' Performs a case-insensitive keyword search across the combined catalog of
#' APIs and developer hubs. Matches on name, organization, and URL fields.
#' Searches the full merged catalog returned by \code{gsa_list()}.
#'
#' @param query Character string to search for. Case-insensitive grep pattern.
#'   Examples: \code{"NASA"}, \code{"health"}, \code{"census"}, \code{"weather"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. API name or organization name for hubs.}
#'     \item{type}{Character. Either \code{"api"} or \code{"hub"}.}
#'     \item{url}{Character. URL to the API endpoint or developer hub.}
#'     \item{organization}{Character. Sponsoring federal agency.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' gsa_search("NASA")
#' gsa_search("health")
#' gsa_search("census")
#' }
gsa_search <- function(query) {
  stopifnot(is.character(query), length(query) == 1, nzchar(query))
  catalog <- gsa_list()
  if (nrow(catalog) == 0) return(.schema_catalog)

  pattern <- query
  matches <- grepl(pattern, catalog$name, ignore.case = TRUE) |
    grepl(pattern, catalog$organization, ignore.case = TRUE) |
    grepl(pattern, catalog$url, ignore.case = TRUE)
  catalog[matches, , drop = FALSE]
}

#' Combined catalog of all government APIs and developer hubs
#'
#' Merges both datasets (from \code{gsa_apis()} and \code{gsa_hubs()}) into a
#' single tibble with a \code{type} column distinguishing between APIs and
#' developer hubs. Typically returns ~609 rows (500 APIs + 109 hubs).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. API name (for type "api") or organization name
#'       (for type "hub").}
#'     \item{type}{Character. Either \code{"api"} or \code{"hub"}.}
#'     \item{url}{Character. URL to the resource.}
#'     \item{organization}{Character. Sponsoring federal agency.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' catalog <- gsa_list()
#' catalog
#' # Count by type
#' table(catalog$type)
#' }
gsa_list <- function() {
  apis <- gsa_apis()
  hubs <- gsa_hubs()

  api_rows <- if (nrow(apis) > 0) {
    tibble(
      name         = apis$api_name,
      type         = "api",
      url          = apis$api_url,
      organization = apis$organization
    )
  } else {
    .schema_catalog[0, ]
  }

  hub_rows <- if (nrow(hubs) > 0) {
    tibble(
      name         = hubs$organization,
      type         = "hub",
      url          = hubs$hub_url,
      organization = hubs$organization
    )
  } else {
    .schema_catalog[0, ]
  }

  bind_rows(api_rows, hub_rows)
}

#' Get 18f.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gsa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gsa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/18f.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "18f.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# 18f.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# 18f.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
