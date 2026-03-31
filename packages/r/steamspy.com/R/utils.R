#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# steamspy-com.R
# Self-contained SteamSpy API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: 4 requests per second


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.steam_base <- "https://steamspy.com/api.php"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- Parser for named-list (top100/genre) responses ---------------------------

.parse_app_list <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(.schema_app)
  rows <- lapply(raw, function(x) {
    tibble(
      appid          = as.integer(x$appid %||% NA),
      name           = as.character(x$name %||% NA),
      developer      = as.character(x$developer %||% NA),
      publisher      = as.character(x$publisher %||% NA),
      positive       = as.integer(x$positive %||% NA),
      negative       = as.integer(x$negative %||% NA),
      owners         = as.character(x$owners %||% NA),
      average_forever = as.integer(x$average_forever %||% NA),
      average_2weeks = as.integer(x$average_2weeks %||% NA),
      price          = as.integer(x$price %||% NA),
      initialprice   = as.integer(x$initialprice %||% NA),
      discount       = as.integer(x$discount %||% NA),
      ccu            = as.integer(x$ccu %||% NA)
    )
  })
  bind_rows(rows)
}

# == Schemas ===================================================================

.schema_app <- tibble(
  appid = integer(), name = character(), developer = character(),
  publisher = character(), positive = integer(), negative = integer(),
  owners = character(), average_forever = integer(),
  average_2weeks = integer(), price = integer(),
  initialprice = integer(), discount = integer(), ccu = integer()
)

.schema_app_detail <- tibble(
  appid = integer(), name = character(), developer = character(),
  publisher = character(), positive = integer(), negative = integer(),
  owners = character(), average_forever = integer(),
  average_2weeks = integer(), median_forever = integer(),
  median_2weeks = integer(), price = integer(), initialprice = integer(),
  discount = integer(), ccu = integer(), genre = character(),
  languages = character()
)

