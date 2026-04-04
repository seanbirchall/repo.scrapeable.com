



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://re.jrc.ec.europa.eu/api/v5_3"

#' Get monthly PV energy output estimates
#' @param lat Latitude
#' @param lon Longitude
#' @param peakpower Peak power of PV system in kW (default 1)
#' @return tibble of monthly estimates
#' @export
pvgis_monthly <- function(lat, lon, peakpower = 1) {
  url <- sprintf("%s/PVcalc?lat=%f&lon=%f&peakpower=%f&loss=14&outputformat=json", .base, lat, lon, peakpower)
  raw <- .fetch_json(url)
  months <- raw$outputs$monthly$fixed
  if (length(months) == 0) return(tibble::tibble(month = integer(), E_m = numeric()))
  tibble::tibble(
    month = vapply(months, function(x) x$month %||% NA_integer_, integer(1)),
    E_m = vapply(months, function(x) as.numeric(x$E_m %||% NA), numeric(1)),
    H_m = vapply(months, function(x) as.numeric(x$H_m %||% NA), numeric(1)),
    SD_m = vapply(months, function(x) as.numeric(x$SD_m %||% NA), numeric(1))
  )
}

#' Get solar radiation data
#' @param lat Latitude
#' @param lon Longitude
#' @return tibble of monthly solar radiation
#' @export
pvgis_radiation <- function(lat, lon) {
  url <- sprintf("%s/MRcalc?lat=%f&lon=%f&outputformat=json", .base, lat, lon)
  raw <- .fetch_json(url)
  months <- raw$outputs$monthly
  if (length(months) == 0) return(tibble::tibble(month = integer(), H_sun = numeric()))
  tibble::tibble(
    month = vapply(months, function(x) x$month %||% NA_integer_, integer(1)),
    H_sun = vapply(months, function(x) as.numeric(x$H_sun %||% NA), numeric(1))
  )
}

#' Generate LLM context for joint-research-centre.ec.europa.eu
#' @return Character string
#' @export
pvgis_context <- function() { .build_context("joint-research-centre.ec.europa.eu") }


# == Context ===================================================================

#' Generate LLM-friendly context for joint-research-centre.ec.europa.eu
#'
#' @return Character string with full function signatures and bodies
joint-research-centre_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/joint-research-centre.ec.europa.eu.R"
  if (!file.exists(src_file)) {
    cat("# joint-research-centre.ec.europa.eu context - source not found\n")
    return(invisible("# joint-research-centre.ec.europa.eu context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

