# == Data access ===============================================================

#' List available Arkansas GIS feature services
#'
#' @return tibble: key, service_path
#' @export
ark_services <- function() {
  tibble(
    key          = names(.ark_services),
    service_path = as.character(unlist(.ark_services))
  )
}

#' List layers in an Arkansas GIS feature service
#'
#' @param service Service key from ark_services() or full path
#' @return tibble: id, name, type
#' @export
ark_layers <- function(service) {
  path <- if (service %in% names(.ark_services)) .ark_services[[service]] else service
  url <- sprintf("%s/%s?f=json", .ark_base, path)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(id = integer(), name = character(), type = character()))
  layers <- raw$layers
  if (is.null(layers) || !is.data.frame(layers)) return(tibble(id = integer(), name = character(), type = character()))
  tibble(
    id   = as.integer(layers$id),
    name = as.character(layers$name),
    type = as.character(layers$type %||% NA_character_)
  )
}

#' List known Arkansas GIS datasets with friendly names
#'
#' @return tibble: key, service, layer_id, name
#' @export
ark_datasets <- function() {
  tibble(
    key      = names(.ark_layers),
    service  = vapply(.ark_layers, function(x) x$svc, character(1)),
    layer_id = vapply(.ark_layers, function(x) as.integer(x$layer), integer(1)),
    name     = vapply(.ark_layers, function(x) x$name, character(1))
  )
}

#' Query an Arkansas GIS layer
#'
#' @param dataset Dataset key from ark_datasets(), or a full service URL
#' @param where SQL WHERE clause (default "1=1" for all)
#' @param out_fields Comma-separated field names (default "*")
#' @param limit Max records (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble with feature attributes
#' @export
ark_query <- function(dataset, where = "1=1", out_fields = "*",
                      limit = 1000, offset = 0) {
  if (dataset %in% names(.ark_layers)) {
    info <- .ark_layers[[dataset]]
    svc_path <- .ark_services[[info$svc]]
    svc_url <- sprintf("%s/%s/%d", .ark_base, svc_path, info$layer)
  } else {
    svc_url <- dataset
  }
  .arcgis_query(svc_url, where = where, out_fields = out_fields,
                limit = limit, offset = offset)
}

#' Get Arkansas Senate district boundaries
#'
#' @param limit Max records (default 100)
#' @return tibble with senate district attributes
#' @export
ark_senate_districts <- function(limit = 100) {
  ark_query("senate_districts", limit = limit)
}

#' Get Arkansas hospital/health facility locations
#'
#' @param limit Max records (default 1000)
#' @return tibble with hospital attributes
#' @export
ark_hospitals <- function(limit = 1000) {
  ark_query("hospitals", limit = limit)
}

# == Context ===================================================================

#' Generate LLM-friendly context for arkansas.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
ark_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/arkansas.gov.R"
  if (!file.exists(src_file)) { cat("# arkansas.gov context - source not found\n"); return(invisible(NULL)) }
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    blocks[[length(blocks) + 1]] <- c(rox, lines[fi:end_line], "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
