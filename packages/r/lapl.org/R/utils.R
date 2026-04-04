#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

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

