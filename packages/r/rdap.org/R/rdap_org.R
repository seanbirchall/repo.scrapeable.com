# rdap.org.R - Self-contained rdap.org client



# rdap-org.R
# Self-contained RDAP (Registration Data Access Protocol) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rdap_base <- "https://rdap.org"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- RDAP entity parser -------------------------------------------------------

.parse_entities <- function(entities) {
  if (is.null(entities) || length(entities) == 0) return(tibble(role = character(), name = character()))
  rows <- lapply(entities, function(e) {
    roles <- paste(unlist(e$roles %||% list()), collapse = ", ")
    name <- tryCatch({
      vcard <- e$vcardArray
      if (!is.null(vcard) && length(vcard) >= 2) {
        fn_entry <- Filter(function(x) x[[1]] == "fn", vcard[[2]])
        if (length(fn_entry) > 0) fn_entry[[1]][[4]] else e$handle %||% NA_character_
      } else e$handle %||% NA_character_
    }, error = function(err) e$handle %||% NA_character_)
    tibble(role = roles, name = as.character(name))
  })
  bind_rows(rows)
}

# == Schemas ===================================================================

.schema_domain <- tibble(
  name = character(), handle = character(), status = character(),
  registrar = character(), registration = as.Date(character()),
  expiration = as.Date(character()), last_changed = as.Date(character()),
  nameservers = character()
)

.schema_ip <- tibble(
  handle = character(), name = character(), type = character(),
  start_address = character(), end_address = character(),
  country = character(), status = character()
)

.schema_autnum <- tibble(
  handle = character(), name = character(), type = character(),
  start_autnum = integer(), end_autnum = integer(),
  country = character(), status = character()
)


# == Domain Lookup =============================================================

#' Look up domain registration data via RDAP
#'
#' Queries the Registration Data Access Protocol (RDAP) for WHOIS-style
#' registration information about an internet domain name. Returns the
#' registrar, registration and expiration dates, current status flags,
#' and authoritative nameservers. RDAP is the IETF-standard successor
#' to the legacy WHOIS protocol.
#'
#' @param domain Fully qualified domain name (e.g. \code{"example.com"},
#'   \code{"google.com"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{name}{Canonical (LDH) domain name (character).}
#'     \item{handle}{Registry handle / ID (character).}
#'     \item{status}{Comma-separated EPP status codes (character).}
#'     \item{registrar}{Sponsoring registrar name (character).}
#'     \item{registration}{Domain registration date (Date).}
#'     \item{expiration}{Domain expiration date (Date).}
#'     \item{last_changed}{Date of last registrar update (Date).}
#'     \item{nameservers}{Comma-separated authoritative nameservers (character).}
#'   }
#' @export
#' @seealso \code{\link{rdap_ip}}, \code{\link{rdap_autnum}}
#' @examples
#' \dontrun{
#' rdap_domain("google.com")
#' rdap_domain("example.org")
#' }
rdap_domain <- function(domain) {
  url <- sprintf("%s/domain/%s", .rdap_base, utils::URLencode(domain))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RDAP domain lookup failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_domain)

  statuses <- paste(unlist(raw$status %||% list()), collapse = ", ")
  ns_list <- if (!is.null(raw$nameservers)) {
    paste(vapply(raw$nameservers, function(n) n$ldhName %||% "", character(1)), collapse = ", ")
  } else NA_character_

  events <- raw$events %||% list()
  get_event <- function(action) {
    for (e in events) {
      if (identical(e$eventAction, action)) return(tryCatch(as.Date(e$eventDate), error = function(err) as.Date(NA)))
    }
    as.Date(NA)
  }

  registrar <- tryCatch({
    ents <- raw$entities %||% list()
    reg_ent <- Filter(function(e) "registrar" %in% unlist(e$roles %||% list()), ents)
    if (length(reg_ent) > 0) {
      vc <- reg_ent[[1]]$vcardArray
      if (!is.null(vc) && length(vc) >= 2) {
        fn_entry <- Filter(function(x) x[[1]] == "fn", vc[[2]])
        if (length(fn_entry) > 0) fn_entry[[1]][[4]] else reg_ent[[1]]$handle %||% NA_character_
      } else reg_ent[[1]]$handle %||% NA_character_
    } else NA_character_
  }, error = function(e) NA_character_)

  tibble(
    name         = as.character(raw$ldhName %||% NA_character_),
    handle       = as.character(raw$handle %||% NA_character_),
    status       = statuses,
    registrar    = as.character(registrar),
    registration = get_event("registration"),
    expiration   = get_event("expiration"),
    last_changed = get_event("last changed"),
    nameservers  = as.character(ns_list)
  )
}

# == IP Lookup =================================================================

#' Look up IP address or network block via RDAP
#'
#' Queries RDAP for registration information about an IPv4 or IPv6 address,
#' or a CIDR network block. Returns the owning network name, address range,
#' allocation type, and country of the Regional Internet Registry (RIR)
#' assignment.
#'
#' @param ip IPv4 or IPv6 address, or a CIDR block (e.g. \code{"8.8.8.8"},
#'   \code{"2001:4860::"}, \code{"192.168.1.0/24"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{handle}{Network handle at the RIR (character).}
#'     \item{name}{Network name (character).}
#'     \item{type}{Allocation type, e.g. \code{"DIRECT ALLOCATION"} (character).}
#'     \item{start_address}{First address in the block (character).}
#'     \item{end_address}{Last address in the block (character).}
#'     \item{country}{ISO country code of the allocation (character).}
#'     \item{status}{Comma-separated status values (character).}
#'   }
#' @export
#' @seealso \code{\link{rdap_domain}}, \code{\link{rdap_autnum}}
#' @examples
#' \dontrun{
#' rdap_ip("8.8.8.8")
#' rdap_ip("2001:4860:4860::8888")
#' }
rdap_ip <- function(ip) {
  url <- sprintf("%s/ip/%s", .rdap_base, utils::URLencode(ip))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RDAP IP lookup failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_ip)

  tibble(
    handle        = as.character(raw$handle %||% NA_character_),
    name          = as.character(raw$name %||% NA_character_),
    type          = as.character(raw$type %||% NA_character_),
    start_address = as.character(raw$startAddress %||% NA_character_),
    end_address   = as.character(raw$endAddress %||% NA_character_),
    country       = as.character(raw$country %||% NA_character_),
    status        = paste(unlist(raw$status %||% list()), collapse = ", ")
  )
}

# == ASN Lookup ================================================================

#' Look up an Autonomous System Number (ASN) via RDAP
#'
#' Queries RDAP for registration information about a BGP Autonomous System
#' Number. Returns the AS handle, network name, and the numeric range
#' assigned to the organisation.
#'
#' @param asn Autonomous System Number as integer or character string
#'   (e.g. \code{15169} or \code{"15169"} for Google).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{handle}{AS handle at the RIR, e.g. \code{"AS15169"} (character).}
#'     \item{name}{Organisation name (character).}
#'     \item{type}{Allocation type (character).}
#'     \item{start_autnum}{First ASN in the allocated block (integer).}
#'     \item{end_autnum}{Last ASN in the allocated block (integer).}
#'     \item{country}{ISO country code (character).}
#'     \item{status}{Comma-separated status values (character).}
#'   }
#' @export
#' @seealso \code{\link{rdap_domain}}, \code{\link{rdap_ip}}
#' @examples
#' \dontrun{
#' rdap_autnum(15169)   # Google
#' rdap_autnum("13335") # Cloudflare
#' }
rdap_autnum <- function(asn) {
  url <- sprintf("%s/autnum/%s", .rdap_base, as.character(asn))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RDAP autnum lookup failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_autnum)

  tibble(
    handle       = as.character(raw$handle %||% NA_character_),
    name         = as.character(raw$name %||% NA_character_),
    type         = as.character(raw$type %||% NA_character_),
    start_autnum = as.integer(raw$startAutnum %||% NA_integer_),
    end_autnum   = as.integer(raw$endAutnum %||% NA_integer_),
    country      = as.character(raw$country %||% NA_character_),
    status       = paste(unlist(raw$status %||% list()), collapse = ", ")
  )
}

# == Context ===================================================================

#' Get rdap.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
rdap_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(rdap_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/rdap.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "rdap.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# rdap.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# rdap.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
