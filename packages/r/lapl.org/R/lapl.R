# lapl.org.R - Self-contained LAPL (Los Angeles Public Library) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none required


`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.fetch_xml <- function(url) {
  tmp <- tempfile(fileext = ".xml")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  xml2::read_xml(tmp)
}

.parse_rss_items <- function(xml_doc) {
  items <- xml2::xml_find_all(xml_doc, "//item")
  if (length(items) == 0) return(tibble(title = character(), link = character(),
                                         author = character(), genre = character(),
                                         pub_date = character(), cover_url = character()))

  title <- xml2::xml_text(xml2::xml_find_first(items, "title"))
  link <- xml2::xml_text(xml2::xml_find_first(items, "link"))
  pub_date <- xml2::xml_text(xml2::xml_find_first(items, "pubDate"))
  desc_raw <- xml2::xml_text(xml2::xml_find_first(items, "description"))

  # Parse author and genre from description HTML: "Author<br />\nGenre"
  author <- vapply(desc_raw, function(d) {
    parts <- strsplit(d, "<br\\s*/?>")[[1]]
    # Author is typically the second-to-last text segment
    texts <- trimws(gsub("<[^>]+>", "", parts))
    texts <- texts[nchar(texts) > 0]
    if (length(texts) >= 2) texts[length(texts) - 1] else NA_character_
  }, character(1), USE.NAMES = FALSE)

  genre <- vapply(desc_raw, function(d) {
    parts <- strsplit(d, "<br\\s*/?>")[[1]]
    texts <- trimws(gsub("<[^>]+>", "", parts))
    texts <- texts[nchar(texts) > 0]
    if (length(texts) >= 1) texts[length(texts)] else NA_character_
  }, character(1), USE.NAMES = FALSE)

  # Extract cover image URL from description
  cover_url <- vapply(desc_raw, function(d) {
    m <- regmatches(d, regexpr('src="([^"]+)"', d))
    if (length(m) > 0) gsub('src="([^"]+)"', "\\1", m[1]) else NA_character_
  }, character(1), USE.NAMES = FALSE)

  tibble(
    title = trimws(title),
    link = link,
    author = author,
    genre = genre,
    pub_date = pub_date,
    cover_url = cover_url
  )
}

# == Feed definitions ==========================================================

.lapl_feeds <- tibble(
  id = c("new_scifi", "new_romance", "new_chicklit", "new_mystery",
         "new_historical", "new_largeprint", "new_ya", "new_urban", "new_western",
         "hot_children", "hot_fiction", "hot_nonfiction", "hot_ya"),
  name = c("New Science Fiction", "New Romance Fiction", "New Chick Lit",
           "New Mystery Fiction", "New Historical Fiction", "New Large Print Books",
           "New Young Adult", "New Urban Fiction", "New Westerns",
           "Hot Children's", "Hot Fiction", "Hot Non-Fiction", "Hot YA"),
  url = c(
    "https://www.lapl.org/new-science-fiction-to-lapl.xml",
    "https://www.lapl.org/new-romance-fiction-to-lapl.xml",
    "https://www.lapl.org/new-chick-lit-to-lapl.xml",
    "https://www.lapl.org/new-mystery-fiction-to-lapl.xml",
    "https://www.lapl.org/new-historical-fiction-to-lapl.xml",
    "https://cms.lapl.org/new-large-type-books-to-lapl.xml",
    "https://cms.lapl.org/new-ya-fiction-to-lapl.xml",
    "https://cms.lapl.org/new-urban-fiction-to-lapl.xml",
    "https://cms.lapl.org/new-western-fiction-to-lapl.xml",
    "https://www.lapl.org/hot-titles-children.xml",
    "https://www.lapl.org/hot-titles-fiction.xml",
    "https://www.lapl.org/hot-titles-nonfiction.xml",
    "https://www.lapl.org/hot-titles-ya.xml"
  ),
  category = c(rep("new_titles", 9), rep("hot_titles", 4))
)

# == Public functions ==========================================================

#' List available LAPL book feeds
#'
#' Returns the catalog of RSS book feeds published by the Los Angeles
#' Public Library, covering new arrivals and popular ("hot") titles across
#' genres such as science fiction, romance, mystery, young adult, and more.
#'
#' @param category Optional filter. One of \code{"new_titles"} (new arrivals)
#'   or \code{"hot_titles"} (popular/trending). \code{NULL} (default) returns
#'   all feeds.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Feed identifier used by other \code{lapl_*}
#'       functions (e.g. \code{"new_scifi"}, \code{"hot_fiction"}).}
#'     \item{name}{Character. Human-readable feed name
#'       (e.g. \code{"New Science Fiction"}).}
#'     \item{url}{Character. RSS feed URL.}
#'     \item{category}{Character. \code{"new_titles"} or \code{"hot_titles"}.}
#'   }
#' @examples
#' lapl_list()
#' lapl_list(category = "hot_titles")
lapl_list <- function(category = NULL) {
  out <- .lapl_feeds
  if (!is.null(category)) {
    out <- out |> filter(.data$category == !!category)
  }
  out
}

#' Fetch titles from an LAPL book feed
#'
#' Retrieves and parses the RSS feed for a single LAPL book list,
#' returning structured metadata for every title in the feed.
#'
#' @param feed_id Character. A feed identifier as shown by
#'   \code{\link{lapl_list}}, e.g. \code{"new_scifi"}, \code{"hot_fiction"},
#'   \code{"new_mystery"}, \code{"hot_ya"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Book title.}
#'     \item{link}{Character. URL to the LAPL catalog page for the book.}
#'     \item{author}{Character. Author name (parsed from feed description).}
#'     \item{genre}{Character. Genre label (parsed from feed description).}
#'     \item{pub_date}{Character. Publication date string from the RSS feed.}
#'     \item{cover_url}{Character. URL to the book cover image, or \code{NA}.}
#'   }
#' @examples
#' lapl_titles("hot_fiction")
#' lapl_titles("new_scifi")
lapl_titles <- function(feed_id) {
  match <- .lapl_feeds |> filter(.data$id == !!feed_id)
  if (nrow(match) == 0) {
    stop("Unknown feed: ", feed_id,
         ". Use lapl_list() to see available feeds.", call. = FALSE)
  }
  xml_doc <- tryCatch(.fetch_xml(match$url[1]),
                       error = function(e) {
                         warning("Could not fetch feed: ", e$message, call. = FALSE)
                         return(NULL)
                       })
  if (is.null(xml_doc)) {
    return(tibble(title = character(), link = character(), author = character(),
                  genre = character(), pub_date = character(), cover_url = character()))
  }
  .parse_rss_items(xml_doc)
}

#' Fetch new titles across all genres
#'
#' Iterates over every \code{"new_titles"} feed (science fiction, romance,
#' mystery, historical fiction, large print, young adult, urban fiction,
#' westerns, and chick lit) and returns a combined tibble of all new arrivals.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{feed_id}{Character. Source feed identifier
#'       (e.g. \code{"new_scifi"}).}
#'     \item{title}{Character. Book title.}
#'     \item{link}{Character. LAPL catalog URL.}
#'     \item{author}{Character. Author name.}
#'     \item{genre}{Character. Genre label.}
#'     \item{pub_date}{Character. Publication date string.}
#'     \item{cover_url}{Character. Cover image URL, or \code{NA}.}
#'   }
#' @examples
#' lapl_new_titles()
lapl_new_titles <- function() {
  feeds <- .lapl_feeds |> filter(.data$category == "new_titles")
  results <- lapply(seq_len(nrow(feeds)), function(i) {
    tryCatch({
      items <- lapl_titles(feeds$id[i])
      if (nrow(items) > 0) items$feed_id <- feeds$id[i]
      items
    }, error = function(e) NULL)
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(tibble(feed_id = character(), title = character(), link = character(),
                  author = character(), genre = character(), pub_date = character(),
                  cover_url = character()))
  }
  bind_rows(results) |> select(feed_id, everything())
}

#' Fetch hot/popular titles across all categories
#'
#' Iterates over every \code{"hot_titles"} feed (children's, fiction,
#' non-fiction, and young adult) and returns a combined tibble of the
#' most popular titles at the Los Angeles Public Library.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{feed_id}{Character. Source feed identifier
#'       (e.g. \code{"hot_fiction"}).}
#'     \item{title}{Character. Book title.}
#'     \item{link}{Character. LAPL catalog URL.}
#'     \item{author}{Character. Author name.}
#'     \item{genre}{Character. Genre label.}
#'     \item{pub_date}{Character. Publication date string.}
#'     \item{cover_url}{Character. Cover image URL, or \code{NA}.}
#'   }
#' @examples
#' lapl_hot_titles()
lapl_hot_titles <- function() {
  feeds <- .lapl_feeds |> filter(.data$category == "hot_titles")
  results <- lapply(seq_len(nrow(feeds)), function(i) {
    tryCatch({
      items <- lapl_titles(feeds$id[i])
      if (nrow(items) > 0) items$feed_id <- feeds$id[i]
      items
    }, error = function(e) NULL)
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(tibble(feed_id = character(), title = character(), link = character(),
                  author = character(), genre = character(), pub_date = character(),
                  cover_url = character()))
  }
  bind_rows(results) |> select(feed_id, everything())
}

#' Search LAPL titles by keyword
#'
#' Performs a case-insensitive keyword search across title and author
#' fields in one or more LAPL book feeds. When no specific feeds are
#' given, searches all new-arrival and hot-title feeds.
#'
#' @param query Character. Search string matched case-insensitively
#'   against title and author fields (e.g. \code{"Atwood"},
#'   \code{"dragon"}).
#' @param feeds Character vector of feed identifiers to search, or
#'   \code{NULL} (default) to search all feeds. Valid values are those
#'   returned by \code{\link{lapl_list}}, e.g. \code{c("hot_fiction",
#'   "new_scifi")}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{feed_id}{Character. Source feed identifier.}
#'     \item{title}{Character. Book title.}
#'     \item{link}{Character. LAPL catalog URL.}
#'     \item{author}{Character. Author name.}
#'     \item{genre}{Character. Genre label.}
#'     \item{pub_date}{Character. Publication date string.}
#'     \item{cover_url}{Character. Cover image URL, or \code{NA}.}
#'   }
#' @examples
#' lapl_search("mystery")
#' lapl_search("King", feeds = c("hot_fiction", "new_scifi"))
lapl_search <- function(query, feeds = NULL) {
  if (is.null(feeds)) {
    all_items <- bind_rows(lapl_new_titles(), lapl_hot_titles())
  } else {
    results <- lapply(feeds, function(fid) {
      tryCatch({
        items <- lapl_titles(fid)
        if (nrow(items) > 0) items$feed_id <- fid
        items
      }, error = function(e) NULL)
    })
    results <- results[!vapply(results, is.null, logical(1))]
    all_items <- bind_rows(results)
  }
  pattern <- tolower(query)
  all_items |> filter(
    grepl(pattern, tolower(title)) |
    grepl(pattern, tolower(author))
  )
}

#' Get lapl.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
lapl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(lapl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/lapl.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "lapl.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# lapl.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# lapl.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
