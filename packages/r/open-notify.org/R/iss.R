# open-notify.org.R - Self-contained open-notify.org client



.ua <- "support@scrapeable.com"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


#' Get the current position of the International Space Station
#'
#' Queries the Open Notify API for the real-time latitude and longitude
#' of the ISS. The position is updated roughly every second on the server
#' side.
#'
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{latitude}{Current latitude in decimal degrees (numeric).}
#'     \item{longitude}{Current longitude in decimal degrees (numeric).}
#'     \item{timestamp}{UTC timestamp of the position reading (POSIXct).}
#'   }
#' @export
#' @seealso \code{\link{iss_astronauts}}, \code{\link{iss_craft_summary}}
#' @examples
#' \dontrun{
#' iss_position()
#' }
iss_position <- function() {
  raw <- .fetch_json("http://api.open-notify.org/iss-now.json")
  tibble::tibble(
    latitude = as.numeric(raw$iss_position$latitude),
    longitude = as.numeric(raw$iss_position$longitude),
    timestamp = as.POSIXct(raw$timestamp, origin = "1970-01-01")
  )
}

#' List astronauts currently in space
#'
#' Returns every person currently aboard a spacecraft, including both
#' the International Space Station and other crewed vehicles such as
#' China's Tiangong station. Data comes from the Open Notify
#' \code{astros.json} endpoint.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Astronaut / cosmonaut / taikonaut full name (character).}
#'     \item{craft}{Name of the spacecraft they are aboard (character).}
#'   }
#' @export
#' @seealso \code{\link{iss_craft_summary}}, \code{\link{iss_position}}
#' @examples
#' \dontrun{
#' iss_astronauts()
#' }
iss_astronauts <- function() {
  raw <- .fetch_json("http://api.open-notify.org/astros.json")
  people <- raw$people
  if (length(people) == 0) return(tibble::tibble(name = character(), craft = character()))
  tibble::tibble(
    name = vapply(people, function(x) x$name %||% NA_character_, character(1)),
    craft = vapply(people, function(x) x$craft %||% NA_character_, character(1))
  )
}

#' Summarise astronauts by spacecraft
#'
#' Convenience wrapper around \code{\link{iss_astronauts}} that groups
#' the current crew by spacecraft and returns the number of people
#' aboard each vehicle.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{craft}{Spacecraft name (character).}
#'     \item{count}{Number of crew members aboard (integer).}
#'   }
#' @export
#' @seealso \code{\link{iss_astronauts}}, \code{\link{iss_position}}
#' @examples
#' \dontrun{
#' iss_craft_summary()
#' }
iss_craft_summary <- function() {
  schema <- tibble(craft = character(), count = integer())
  raw <- tryCatch(.fetch_json("http://api.open-notify.org/astros.json"), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$people) || length(raw$people) == 0) return(schema)

  crafts <- vapply(raw$people, function(x) x$craft %||% NA_character_, character(1))
  tbl <- table(crafts)
  tibble(craft = names(tbl), count = as.integer(tbl))
}

# == Context ===================================================================

#' Get open-notify.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
iss_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(iss_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/open-notify.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "open-notify.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# open-notify.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# open-notify.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
