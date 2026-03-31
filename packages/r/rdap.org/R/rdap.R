
# == Domain Lookup =============================================================

#' RDAP domain lookup
#'
#' Queries RDAP for domain registration information.
#'
#' @param domain Domain name (e.g. "example.com", "google.com")
#' @return tibble: name, handle, status, registrar, registration,
#'   expiration, last_changed, nameservers
#' @export
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
#' @export
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
#' @export
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
#' @export
rdap_context <- function() {
  .build_context("rdap.org", header_lines = c(
    "# rdap.org - RDAP (Registration Data Access Protocol) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Endpoints: domain, IP, and ASN lookups",
    "# All functions return tibbles with typed columns."
  ))
}
