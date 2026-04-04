#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cpsc_base <- "https://www.saferproducts.gov/RestWebServices"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.cpsc_recall_url <- function(start_date = NULL, end_date = NULL, keyword = NULL) {
  params <- list(format = "json")
  if (!is.null(start_date)) params$RecallDateStart <- format(as.Date(start_date), "%Y-%m-%d")
  if (!is.null(end_date)) params$RecallDateEnd <- format(as.Date(end_date), "%Y-%m-%d")
  if (!is.null(keyword)) params$RecallTitle <- keyword
  query <- paste(names(params), vapply(params, as.character, character(1)), sep = "=", collapse = "&")
  paste0(.cpsc_base, "/Recall?", query)
}

.parse_recalls <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(.schema_recalls)
  if (!is.data.frame(raw)) return(.schema_recalls)

  products <- vapply(seq_len(nrow(raw)), function(i) {
    p <- raw$Products[[i]]
    if (is.null(p) || (is.data.frame(p) && nrow(p) == 0)) return(NA_character_)
    if (is.data.frame(p)) paste(p$Name, collapse = "; ") else NA_character_
  }, character(1))

  injuries <- vapply(seq_len(nrow(raw)), function(i) {
    inj <- raw$Injuries[[i]]
    if (is.null(inj) || (is.data.frame(inj) && nrow(inj) == 0)) return(NA_character_)
    if (is.data.frame(inj)) paste(inj$Name, collapse = "; ") else NA_character_
  }, character(1))

  mfr_countries <- vapply(seq_len(nrow(raw)), function(i) {
    mc <- raw$ManufacturerCountries[[i]]
    if (is.null(mc) || (is.data.frame(mc) && nrow(mc) == 0)) return(NA_character_)
    if (is.data.frame(mc)) paste(mc$Country, collapse = "; ") else NA_character_
  }, character(1))

  tibble(
    recall_id = as.integer(raw$RecallID),
    recall_number = as.character(raw$RecallNumber),
    recall_date = as.Date(raw$RecallDate),
    title = as.character(raw$Title),
    description = as.character(raw$Description),
    products = products,
    injuries = injuries,
    manufacturer_countries = mfr_countries,
    consumer_contact = as.character(raw$ConsumerContact),
    url = as.character(raw$URL),
    last_published = as.Date(raw$LastPublishDate)
  )
}

# == Schemas ===================================================================

.schema_recalls <- tibble(
  recall_id = integer(), recall_number = character(), recall_date = as.Date(character()),
  title = character(), description = character(), products = character(),
  injuries = character(), manufacturer_countries = character(),
  consumer_contact = character(), url = character(),
  last_published = as.Date(character())
)

`%||%` <- function(x, y) if (is.null(x)) y else x

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
