#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# commerce.gov.R
# Self-contained Department of Commerce open data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.base_catalog    <- "https://www.commerce.gov/sites/default/files/data.json"
.base_governance <- "https://www.commerce.gov/sites/default/files/2018-11/governanceboards.json"
.base_savings    <- "https://www.commerce.gov/sites/default/files/costsavings.json"
.base_leadership <- "https://www.commerce.gov/sites/default/files/bureaudirectory.json"

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
  cat(out, "\n")
  invisible(out)
}

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) return(paste(unlist(x), collapse = "; "))
  as.character(x)[1]
}

.safe_date <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(as.Date(NA))
  d <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (!is.na(d)) return(d)
  tryCatch(as.Date(x, format = "%m/%d/%Y"), error = function(e) as.Date(NA))
}

.extract_publisher <- function(pub) {
  if (is.null(pub)) return(NA_character_)
  .safe_chr(pub$name)
}

.extract_distribution_urls <- function(dist) {
  if (is.null(dist) || length(dist) == 0) return(NA_character_)
  urls <- vapply(dist, function(d) {
    .safe_chr(d$downloadURL %||% d$accessURL)
  }, character(1))
  paste(urls[!is.na(urls)], collapse = "; ")
}

.extract_distribution_formats <- function(dist) {
  if (is.null(dist) || length(dist) == 0) return(NA_character_)
  fmts <- vapply(dist, function(d) .safe_chr(d$format %||% d$mediaType), character(1))
  paste(unique(fmts[!is.na(fmts)]), collapse = "; ")
}

# == Schemas ===================================================================

.schema_catalog <- tibble::tibble(
  title       = character(),
  description = character(),
  publisher   = character(),
  keywords    = character(),
  modified    = as.Date(character()),
  identifier  = character(),
  access_level = character(),
  format      = character(),
  access_url  = character(),
  distribution_urls = character(),
  landing_page = character(),
  bureau_code = character(),
  license     = character(),
  temporal    = character(),
  spatial     = character()
)

.schema_governance <- tibble::tibble(
  board_name   = character(),
  bureau_code  = character(),
  program_code = character(),
  description  = character()
)

.schema_savings <- tibble::tibble(
  strategy_id    = integer(),
  strategy_title = character(),
  decision_date  = as.Date(character()),
  omb_initiative = character(),
  amount_type    = character(),
  fy2012 = numeric(), fy2013 = numeric(), fy2014 = numeric(),
  fy2015 = numeric(), fy2016 = numeric(), fy2017 = numeric(),
  fy2018 = numeric(), fy2019 = numeric(), fy2020 = numeric()
)

.schema_leadership <- tibble::tibble(
  bureau_code     = character(),
  first_name      = character(),
  last_name       = character(),
  employment_type = character(),
  appointment     = character(),
  responsibilities = character(),
  rating_official  = character(),
  review_official  = character(),
  key_bureau_cio   = logical()
)

# == Internal parsers ==========================================================

.parse_catalog <- function(raw) {
  datasets <- raw$dataset
  if (is.null(datasets) || length(datasets) == 0) return(.schema_catalog)

  rows <- lapply(datasets, function(d) {
    tibble::tibble(
      title       = .safe_chr(d$title),
      description = .safe_chr(d$description),
      publisher   = .extract_publisher(d$publisher),
      keywords    = .safe_chr(d$keyword),
      modified    = .safe_date(.safe_chr(d$modified)),
      identifier  = .safe_chr(d$identifier),
      access_level = .safe_chr(d$accessLevel),
      format      = .safe_chr(d$format) %||% .extract_distribution_formats(d$distribution),
      access_url  = .safe_chr(d$accessURL),
      distribution_urls = .extract_distribution_urls(d$distribution),
      landing_page = .safe_chr(d$landingPage),
      bureau_code = .safe_chr(d$bureauCode),
      license     = .safe_chr(d$license),
      temporal    = .safe_chr(d$temporal),
      spatial     = .safe_chr(d$spatial)
    )
  })
  dplyr::bind_rows(rows)
}

.parse_governance <- function(raw) {
  boards <- raw$boards
  if (is.null(boards) || length(boards) == 0) return(.schema_governance)

  rows <- lapply(boards, function(b) {
    tibble::tibble(
      board_name   = .safe_chr(b$governanceBoardName),
      bureau_code  = .safe_chr(b$bureauCode),
      program_code = .safe_chr(b$programCodeFPI),
      description  = .safe_chr(b$cioInvolvementDescription)
    )
  })
  dplyr::bind_rows(rows)
}

.parse_savings <- function(raw) {
  strategies <- raw$strategies
  if (is.null(strategies) || length(strategies) == 0) return(.schema_savings)

  rows <- lapply(strategies, function(s) {
    .fy <- function(year) {
      fy <- s[[paste0("fy", year)]]
      if (is.null(fy)) return(NA_real_)
      as.numeric(fy$amount %||% NA)
    }
    tibble::tibble(
      strategy_id    = as.integer(s$strategyId %||% NA),
      strategy_title = .safe_chr(s$strategyTitle),
      decision_date  = .safe_date(.safe_chr(s$decisionDate)),
      omb_initiative = .safe_chr(s$ombInitiative),
      amount_type    = .safe_chr(s$amountType),
      fy2012 = .fy(2012), fy2013 = .fy(2013), fy2014 = .fy(2014),
      fy2015 = .fy(2015), fy2016 = .fy(2016), fy2017 = .fy(2017),
      fy2018 = .fy(2018), fy2019 = .fy(2019), fy2020 = .fy(2020)
    )
  })
  dplyr::bind_rows(rows)
}

.parse_leadership <- function(raw) {
  leaders <- raw$leaders
  if (is.null(leaders) || length(leaders) == 0) return(.schema_leadership)

  rows <- lapply(leaders, function(l) {
    tibble::tibble(
      bureau_code     = .safe_chr(l$bureauCode),
      first_name      = .safe_chr(l$firstName),
      last_name       = .safe_chr(l$lastName),
      employment_type = .safe_chr(l$employmentType),
      appointment     = .safe_chr(l$typeOfAppointment),
      responsibilities = .safe_chr(l$otherResponsibilities),
      rating_official  = .safe_chr(l$evaluationRatingOfficialTitle),
      review_official  = .safe_chr(l$evaluationReviewingOfficialTitle),
      key_bureau_cio   = identical(tolower(.safe_chr(l$keyBureauCIO)), "yes")
    )
  })
  dplyr::bind_rows(rows)
}
