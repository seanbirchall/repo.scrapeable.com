# rdap-org.R
# Self-contained RDAP (Registration Data Access Protocol) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rdap_base <- "https://rdap.org"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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

#' RDAP domain lookup
#'
#' Queries RDAP for domain registration information.
#'
#' @param domain Domain name (e.g. "example.com", "google.com")
#' @return tibble: name, handle, status, registrar, registration,
#'   expiration, last_changed, nameservers
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

#' RDAP IP address/network lookup
#'
#' @param ip IP address or CIDR block (e.g. "8.8.8.8", "192.168.1.0/24")
#' @return tibble: handle, name, type, start_address, end_address,
#'   country, status
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

#' RDAP autonomous system number lookup
#'
#' @param asn Autonomous System Number (e.g. 15169 or "15169")
#' @return tibble: handle, name, type, start_autnum, end_autnum,
#'   country, status
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

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the rdap.org package
#'
#' @return Character string (invisibly), also printed
rdap_context <- function() {
  .build_context("rdap.org", header_lines = c(
    "# rdap.org - RDAP (Registration Data Access Protocol) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Endpoints: domain, IP, and ASN lookups",
    "# All functions return tibbles with typed columns."
  ))
}
