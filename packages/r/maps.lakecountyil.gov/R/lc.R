# == Data access ===============================================================

#' List available Lake County ArcGIS service folders
#'
#' @return tibble: folder
#' @export
lc_folders <- function() {
  url <- sprintf("%s?f=json", .lc_arcgis)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$folders)) return(tibble(folder = character()))
  tibble(folder = as.character(raw$folders))
}

#' List layers in a Lake County ArcGIS service
#'
#' @param folder Folder name (e.g. "Health", "DOT")
#' @param service Service name (default: same as folder)
#' @param type Service type (default "MapServer")
#' @return tibble: id, name, type
#' @export
lc_layers <- function(folder, service = folder, type = "MapServer") {
  url <- sprintf("%s/%s/%s/%s?f=json", .lc_arcgis, folder, service, type)
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

#' Query a Lake County ArcGIS layer
#'
#' @param folder Folder name (e.g. "Health")
#' @param service Service name
#' @param type Service type (default "MapServer")
#' @param layer_id Layer ID number
#' @param where SQL WHERE clause (default "1=1" for all)
#' @param out_fields Comma-separated field names (default "*")
#' @param limit Max records (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble with layer attributes
#' @export
lc_query <- function(folder, service = folder, type = "MapServer",
                     layer_id = 0, where = "1=1", out_fields = "*",
                     limit = 1000, offset = 0) {
  svc_url <- sprintf("%s/%s/%s/%s/%d", .lc_arcgis, folder, service, type, layer_id)
  .arcgis_query(svc_url, where = where, out_fields = out_fields,
                limit = limit, offset = offset)
}

#' Search Lake County Hub datasets
#'
#' @param q Search query
#' @return tibble: id, type, title, description, url
#' @export
lc_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .lc_hub, utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(tibble())
  d <- raw$data
  tibble(
    id          = as.character(d$id),
    type        = as.character(d$type %||% NA_character_),
    title       = as.character(d$attributes$name %||% NA_character_),
    description = as.character(d$attributes$searchDescription %||% NA_character_),
    url         = as.character(d$attributes$url %||% NA_character_)
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for lakecountyil.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
lc_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/lakecountyil.gov.R"
  if (!file.exists(src_file)) { cat("# lakecountyil.gov context - source not found\n"); return(invisible(NULL)) }
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
