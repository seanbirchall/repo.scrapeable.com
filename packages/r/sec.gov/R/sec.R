# == Entity resolution =========================================================

#' @export
sec_entities <- function() {
  if (!is.null(.sec_cache$entities)) return(.sec_cache$entities)
  raw <- .fetch_json("https://www.sec.gov/files/company_tickers.json")
  df <- bind_rows(lapply(raw, as_tibble)) |>
    setNames(c("cik", "ticker", "name")) |>
    mutate(cik = as.integer(cik), ticker = as.character(ticker), name = as.character(name))
  .sec_cache$entities <- df
  df
}


#' @export
sec_resolve <- function(cik = NULL, ticker = NULL, name = NULL) {
  df <- sec_entities()
  if (!is.null(cik))    df <- df |> filter(.data$cik == as.integer(!!cik))
  if (!is.null(ticker)) df <- df |> filter(toupper(.data$ticker) == toupper(!!ticker))
  if (!is.null(name))   df <- df |> filter(grepl(!!name, .data$name, ignore.case = TRUE))
  df
}

#' @export
sec_search <- function(query) {
  empty <- .schema_search
  url <- paste0("https://efts.sec.gov/LATEST/search-index?keysTyped=",
                utils::URLencode(query))
  raw <- .fetch_json(url)

  if (is.data.frame(raw)) return(as_tibble(raw) |> rename_with(tolower))
  hits <- raw$hits
  if (is.null(hits)) return(empty)
  if (is.data.frame(hits)) return(as_tibble(hits) |> rename_with(tolower))
  inner <- hits$hits
  if (!is.data.frame(inner)) return(empty)

  tibble(
    cik  = inner$`_id`,
    name = if (!is.null(inner$`_source`)) inner$`_source`$entity else NA_character_
  )
}


# == Company data ==============================================================

#' @export
sec_filings <- function(cik, form = NULL, all = FALSE) {
  raw <- .fetch_json(paste0("https://data.sec.gov/submissions/CIK",
                            .pad_cik(cik), ".json"))
  df <- as_tibble(raw$filings$recent)

  if (all && is.data.frame(raw$filings$files) && nrow(raw$filings$files) > 0) {
    older <- lapply(raw$filings$files$name, function(fname) {
      tryCatch(as_tibble(.fetch_json(paste0("https://data.sec.gov/submissions/", fname))),
               error = function(e) NULL)
    })
    df <- bind_rows(c(list(df), Filter(Negate(is.null), older)))
  }

  if (!is.null(form)) df <- df |> filter(toupper(.data$form) == toupper(.env$form))
  df |>
    rename_with(~ gsub("[. ]+", "_", tolower(.x))) |>
    mutate(
      cik = as.integer(!!cik), entity = !!raw$name,
      across(any_of(c("filingdate", "reportdate")), as.Date),
      across(any_of(c("size", "isxbrl", "isinlinexbrl")), as.integer)
    )
}

#' @export
sec_facts <- function(cik) {
  raw <- .fetch_json(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",
                            .pad_cik(cik), ".json"))
  rows <- list()
  for (taxonomy in names(raw$facts)) {
    for (tag_name in names(raw$facts[[taxonomy]])) {
      tag_data <- raw$facts[[taxonomy]][[tag_name]]
      for (unit_name in names(tag_data$units)) {
        obs <- tag_data$units[[unit_name]]
        if (is.null(obs) || length(obs) == 0) next
        rows[[length(rows) + 1]] <- as_tibble(obs) |>
          mutate(taxonomy = taxonomy, tag = tag_name,
                 label = tag_data$label %||% NA_character_,
                 description = tag_data$description %||% NA_character_,
                 unit = unit_name)
      }
    }
  }
  df <- bind_rows(rows)
  if (nrow(df) == 0) return(.schema_facts)
  df |> mutate(
    cik = as.integer(raw$cik), entity = raw$entityName,
    val = suppressWarnings(as.numeric(val)),
    fy = suppressWarnings(as.integer(fy)),
    across(any_of(c("end", "filed", "start")), as.Date)
  )
}

#' @export
sec_concepts <- function(cik, taxonomy, tag) {
  raw <- .fetch_json(paste0("https://data.sec.gov/api/xbrl/companyconcept/CIK",
                            .pad_cik(cik), "/", taxonomy, "/", tag, ".json"))
  df <- bind_rows(lapply(names(raw$units), function(u) {
    as_tibble(raw$units[[u]]) |> mutate(unit = u)
  }))
  if (nrow(df) == 0) return(.schema_concepts)
  df |> mutate(
    cik = as.integer(raw$cik), entity = raw$entityName,
    taxonomy = raw$taxonomy, tag = raw$tag,
    label = raw$label %||% NA_character_,
    description = raw$description %||% NA_character_,
    val = suppressWarnings(as.numeric(val)),
    fy = suppressWarnings(as.integer(fy)),
    across(any_of(c("end", "filed", "start")), as.Date)
  )
}

#' @export
sec_tags <- function(taxonomy = "us-gaap") {
  key <- paste0("tags_", if (is.null(taxonomy)) "all" else taxonomy)
  if (!is.null(.sec_cache[[key]])) return(.sec_cache[[key]])

  facts <- sec_facts(320193)
  if (nrow(facts) == 0) return(.schema_tags)

  if (!is.null(taxonomy)) facts <- facts |> filter(.data$taxonomy == !!taxonomy)
  df <- facts |> distinct(taxonomy, tag, label, description)
  .sec_cache[[key]] <- df
  df
}


# == Cross-company =============================================================

#' @export
sec_frames <- function(taxonomy, tag, unit, period) {
  raw <- .fetch_json(paste0("https://data.sec.gov/api/xbrl/frames/",
                            taxonomy, "/", tag, "/", unit, "/", period, ".json"))
  df <- as_tibble(raw$data)
  if (nrow(df) == 0) return(.schema_frames)
  df |> mutate(
    taxonomy = raw$taxonomy, tag = raw$tag, unit = raw$uom,
    period = raw$ccp, label = raw$label %||% NA_character_,
    description = raw$description %||% NA_character_,
    val = suppressWarnings(as.numeric(val)),
    cik = as.integer(cik),
    across(any_of("end"), as.Date)
  )
}


# == Bulk datasets =============================================================

#' @export
sec_filings_bulk <- function() {
  zip <- .fetch("https://www.sec.gov/Archives/edgar/daily-index/bulkdata/submissions.zip", ".zip")
  dir <- file.path(tempdir(), "sec_submissions")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  utils::unzip(zip, exdir = dir, overwrite = TRUE)

  main <- list.files(dir, pattern = "^CIK\\d+\\.json$", full.names = TRUE)
  message(sprintf("Parsing %d company submissions...", length(main)))

  results <- lapply(main, function(f) {
    tryCatch({
      raw <- jsonlite::fromJSON(f)
      recent <- raw$filings$recent
      if (is.null(recent) || length(recent$accessionNumber) == 0) return(NULL)
      as_tibble(recent) |>
        mutate(cik = as.integer(raw$cik), entity_name = raw$name,
               sic = raw$sic, sic_description = raw$sicDescription)
    }, error = function(e) NULL)
  })
  bind_rows(results) |> rename_with(~ gsub("[. ]+", "_", tolower(.x)))
}

#' @export
sec_facts_bulk <- function() {
  zip <- .fetch("https://www.sec.gov/Archives/edgar/daily-index/xbrl/companyfacts.zip", ".zip")
  dir <- file.path(tempdir(), "sec_companyfacts")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  utils::unzip(zip, exdir = dir, overwrite = TRUE)

  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)
  message(sprintf("Indexing %d company facts files...", length(files)))

  results <- lapply(files, function(f) {
    tryCatch({
      raw <- jsonlite::fromJSON(f, simplifyVector = FALSE)
      taxonomies <- names(raw$facts)
      n_tags <- sum(vapply(taxonomies, function(t) length(raw$facts[[t]]), integer(1)))
      tibble(cik = as.integer(raw$cik), entity = raw$entityName,
             taxonomies = paste(taxonomies, collapse = ","),
             n_tags = n_tags, path = f)
    }, error = function(e) NULL)
  })
  bind_rows(results)
}

#' @export
sec_insiders_bulk <- function(year = NULL, quarter = NULL) {
  links <- .scrape_zip_links(
    "https://www.sec.gov/data-research/sec-markets-data/insider-transactions-data-sets")
  if (nrow(links) == 0) return(tibble())

  if (!is.null(year))    links <- links |> filter(grepl(as.character(year), href))
  if (!is.null(quarter)) links <- links |> filter(grepl(paste0("q", quarter), href, ignore.case = TRUE))
  if (nrow(links) == 0) return(tibble())
  message(sprintf("Downloading %d insider transaction file(s)...", nrow(links)))

  n_total <- nrow(links)
  results <- lapply(seq_along(links$url), function(i) {
    message(sprintf("[%d/%d] %s", i, n_total, links$href[i]))
    tryCatch(.parse_insiders_zip(.fetch(links$url[i], ".zip")),
             error = function(e) { message("  Failed: ", e$message); NULL })
  })
  bind_rows(results)
}

#' @export
sec_13fs_bulk <- function(n = NULL, year = NULL) {
  links <- .scrape_zip_links(
    "https://www.sec.gov/data-research/sec-markets-data/form-13f-data-sets")
  if (nrow(links) == 0) return(tibble())
  if (!is.null(year)) links <- links |> filter(grepl(as.character(year), href))
  if (!is.null(n)) links <- head(links, n)
  if (nrow(links) == 0) return(tibble())
  message(sprintf("Downloading %d 13F file(s)...", nrow(links)))

  n_total <- nrow(links)
  results <- lapply(seq_along(links$url), function(i) {
    message(sprintf("[%d/%d] %s", i, n_total, links$href[i]))
    tryCatch(.parse_13f_zip(.fetch(links$url[i], ".zip")),
             error = function(e) { message("  Failed: ", e$message); NULL })
  })
  bind_rows(results)
}

#' @export
sec_ftds_bulk <- function(n = NULL, year = NULL, month = NULL) {
  links <- .scrape_zip_links(
    "https://www.sec.gov/data-research/sec-markets-data/fails-deliver-data")
  if (nrow(links) == 0) return(tibble())
  if (!is.null(year) && !is.null(month))
    links <- links |> filter(grepl(paste0(year, sprintf("%02d", as.integer(month))), href))
  else if (!is.null(year))
    links <- links |> filter(grepl(as.character(year), href))
  if (!is.null(n)) links <- head(links, n)
  if (nrow(links) == 0) return(tibble())
  message(sprintf("Downloading %d FTD file(s)...", nrow(links)))

  n_total <- nrow(links)
  results <- lapply(seq_along(links$url), function(i) {
    message(sprintf("[%d/%d] %s", i, n_total, links$href[i]))
    tryCatch(.parse_ftd_zip(.fetch(links$url[i], ".zip")),
             error = function(e) { message("  Failed: ", e$message); NULL })
  })
  bind_rows(results)
}


# == sec_filing: S3 object (the foundation) ====================================

#' @export
sec_filing <- function(accession) {
  if (inherits(accession, "sec_filing")) return(accession)
  docs <- .filing_docs(accession) |> mutate(accession = accession)

  # Extract metadata from -index.htm
  meta <- tryCatch({
    base <- .filing_url(accession)
    doc <- .fetch_html(paste0(base, accession, "-index.htm"))

    company_node <- xml2::xml_find_first(doc,
      ".//span[@class='companyName']|.//div[@class='companyName']")
    company_raw <- if (!is.na(company_node)) .clean_text(xml2::xml_text(company_node)) else NA
    company_name <- sub("\\s*\\(?(Filer|Issuer|Reporting).*$", "", company_raw)

    form_node <- xml2::xml_find_first(doc, ".//*[@id='formName']")
    form_raw <- if (!is.na(form_node)) .clean_text(xml2::xml_text(form_node)) else ""
    form_type <- sub("^Form\\s+", "", form_raw)
    form_type <- sub("\\s+-\\s+.*", "", form_type)

    content_node <- xml2::xml_find_first(doc, ".//div[@class='formContent']")
    content_text <- if (!is.na(content_node)) .clean_text(xml2::xml_text(content_node)) else ""
    extract <- function(label) {
      m <- regmatches(content_text, regexpr(paste0(label, "\\s+([0-9-]+)"), content_text))
      if (length(m) > 0) sub(paste0(label, "\\s+"), "", m) else NA_character_
    }

    list(entity = company_name,
         form   = if (nchar(form_type) > 0) form_type else NA,
         filed  = extract("Filing Date"),
         period = extract("Period of Report"),
         accepted = extract("Accepted"))
  }, error = function(e) list(entity = NA, form = NA, filed = NA, period = NA, accepted = NA))

  if (is.na(meta$form)) {
    primary_row <- docs |> filter(category == "primary")
    if (nrow(primary_row) > 0 && !is.na(primary_row$description[1]))
      meta$form <- .clean_text(primary_row$description[1])
  }

  attr(docs, "accession")  <- accession
  attr(docs, "cik")        <- as.integer(sub("-.*", "", accession))
  attr(docs, "entity")     <- meta$entity
  attr(docs, "form")       <- meta$form
  attr(docs, "filed")      <- meta$filed
  attr(docs, "period")     <- meta$period
  attr(docs, "accepted")   <- meta$accepted
  attr(docs, "source_url") <- .filing_url(accession)
  class(docs) <- c("sec_filing", class(docs))
  docs
}

#' @export
print.sec_filing <- function(x, ...) {
  accn   <- attr(x, "accession")
  cik    <- attr(x, "cik")
  entity <- attr(x, "entity")
  form   <- attr(x, "form")
  filed  <- attr(x, "filed")
  period <- attr(x, "period")
  cats   <- table(x$category)

  cat(strrep("\u2500", 70), "\n")
  if (!is.na(entity)) cat(entity, "\n")
  cat(sprintf("CIK: %d | Form: %s | Filed: %s | Period: %s\n",
              cik, if (is.na(form)) "?" else form,
              if (is.na(filed)) "?" else filed,
              if (is.na(period)) "?" else period))
  cat(sprintf("Accession: %s\n", accn))
  cat(strrep("\u2500", 70), "\n")

  cat(sprintf("\n%d documents:\n", nrow(x)))
  cat_order <- c("primary", names(sort(cats[names(cats) != "primary"], decreasing = TRUE)))
  cat_order <- intersect(cat_order, names(cats))

  for (cat_name in cat_order) {
    rows <- x |> filter(category == cat_name)
    n <- nrow(rows)
    detail <- ""
    if (cat_name == "primary") {
      detail <- paste0("  ", rows$name[1])
    } else if (cat_name == "exhibit") {
      descs <- rows$description[!is.na(rows$description)]
      if (length(descs) > 0) detail <- paste0("  ", paste(descs, collapse = ", "))
    } else if (cat_name == "xbrl_report") {
      stmts <- rows$report_short[!is.na(rows$report_short)]
      if (length(stmts) > 0) detail <- paste0("  ", paste(head(stmts, 5), collapse = ", "))
      if (length(stmts) > 5) detail <- paste0(detail, ", ...")
    } else if (cat_name == "excel") {
      detail <- paste0("  ", rows$name[1])
    }
    cat(sprintf("  %-18s %3d%s\n", cat_name, n, detail))
  }

  if (!is.na(form)) {
    parsers <- sec_parsers(form)
    if (nrow(parsers) > 0)
      cat(sprintf("\nParsers: %s\n", paste(parsers$fn, collapse = ", ")))
  }

  # Show the tibble data
  cat("\n")
  NextMethod()
  invisible(x)
}

.as_filing <- function(x) {
  if (inherits(x, "sec_filing")) x else sec_filing(x)
}

.tag_result <- function(df, filing, parser) {
  df <- as_tibble(df)
  attr(df, "accession")  <- attr(filing, "accession")
  attr(df, "cik")        <- attr(filing, "cik")
  attr(df, "entity")     <- attr(filing, "entity")
  attr(df, "form")       <- attr(filing, "form")
  attr(df, "filed")      <- attr(filing, "filed")
  attr(df, "period")     <- attr(filing, "period")
  attr(df, "parser")     <- parser
  attr(df, "source_url") <- attr(filing, "source_url")
  df
}


# == Filing parsers: XBRL =====================================================

#' @export
sec_filing_xbrl <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  xbrl_doc <- .find_xbrl(filing)
  if (is.null(xbrl_doc))
    stop("No XBRL document found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(xbrl_doc$url)
  facts <- xml2::xml_find_all(doc, "//*[@contextRef]")
  if (length(facts) == 0)
    return(.schema_xbrl)

  df <- tibble(
    tag         = xml2::xml_name(facts),
    value       = xml2::xml_text(facts, trim = TRUE),
    context_ref = xml2::xml_attr(facts, "contextRef"),
    unit_ref    = xml2::xml_attr(facts, "unitRef"),
    decimals    = xml2::xml_attr(facts, "decimals"),
    accession   = accession
  )
  .tag_result(df, filing, "xbrl")
}

#' @export
sec_filing_xbrl_segment <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  xbrl_doc <- .find_xbrl(filing)
  if (is.null(xbrl_doc))
    stop("No XBRL document found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(xbrl_doc$url)

  # Parse contexts with segments
  contexts <- xml2::xml_find_all(doc, "//*[local-name()='context']")
  ctx_list <- lapply(contexts, function(ctx) {
    id <- xml2::xml_attr(ctx, "id")
    instant <- xml2::xml_text(xml2::xml_find_first(ctx, ".//*[local-name()='instant']"))
    start   <- xml2::xml_text(xml2::xml_find_first(ctx, ".//*[local-name()='startDate']"))
    end     <- xml2::xml_text(xml2::xml_find_first(ctx, ".//*[local-name()='endDate']"))
    members <- xml2::xml_find_all(ctx, ".//*[local-name()='explicitMember']")
    if (length(members) == 0) return(NULL)
    tibble(
      context_id = id,
      dimension  = gsub("Axis$", "", gsub(".*:", "", xml2::xml_attr(members, "dimension"))),
      segment    = gsub("Member$", "", gsub(".*:", "", xml2::xml_text(members))),
      start_date = if (is.na(start)) instant else start,
      end_date   = if (is.na(end)) instant else end
    )
  })
  seg_df <- bind_rows(ctx_list)
  if (nrow(seg_df) == 0)
    return(.tag_result(.schema_xbrl_segment, filing, "xbrl_segment"))

  # Join with facts
  facts <- xml2::xml_find_all(doc, "//*[@contextRef]")
  if (length(facts) > 0) {
    fact_df <- tibble(
      context_id = xml2::xml_attr(facts, "contextRef"),
      xbrl_tag   = xml2::xml_name(facts),
      value      = xml2::xml_text(facts, trim = TRUE)
    )
    seg_df <- seg_df |> left_join(fact_df, by = "context_id", relationship = "many-to-many")
  } else {
    seg_df <- seg_df |> mutate(xbrl_tag = NA_character_, value = NA_character_)
  }

  seg_df |> mutate(accession = accession) |> .tag_result(filing, "xbrl_segment")
}

#' @export
sec_filing_xbrl_ownership <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")

  xml_doc <- filing |> filter(category %in% c("xbrl_instance", "xml"),
                              grepl("\\.xml$", name, ignore.case = TRUE))
  if (nrow(xml_doc) == 0)
    stop("No XML document found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(xml_doc$url[1])
  tree <- xml2::as_list(doc)
  root <- tree[[1]]

  owner <- root[["reportingOwner"]]
  owner_cik  <- unlist(owner[["reportingOwnerId"]][["rptOwnerCik"]])
  owner_name <- unlist(owner[["reportingOwnerId"]][["rptOwnerName"]])
  issuer_cik    <- unlist(root[["issuer"]][["issuerCik"]])
  issuer_ticker <- unlist(root[["issuer"]][["issuerTradingSymbol"]])

  parse_tx <- function(table_node, type) {
    if (is.null(table_node)) return(NULL)
    txs <- table_node[grepl("Transaction$", names(table_node))]
    lapply(txs, function(tx) {
      get_val <- function(...) {
        path <- list(...)
        node <- tx
        for (p in path) { node <- node[[p]]; if (is.null(node)) return(NA_character_) }
        val <- unlist(node)
        if (is.null(val)) NA_character_ else val[1]
      }
      tibble(
        type            = type,
        security_title  = get_val("securityTitle", "value"),
        tx_date         = get_val("transactionDate", "value"),
        tx_code         = get_val("transactionCoding", "transactionCode"),
        shares          = get_val("transactionAmounts", "transactionShares", "value"),
        price_per_share = get_val("transactionAmounts", "transactionPricePerShare", "value"),
        acquired_disposed = get_val("transactionAmounts", "transactionAcquiredDisposedCode", "value"),
        shares_after    = get_val("postTransactionAmounts", "sharesOwnedFollowingTransaction", "value"),
        ownership       = get_val("ownershipNature", "directOrIndirectOwnership", "value")
      )
    })
  }

  df <- bind_rows(
    bind_rows(parse_tx(root[["nonDerivativeTable"]], "non_derivative")),
    bind_rows(parse_tx(root[["derivativeTable"]], "derivative"))
  )
  if (nrow(df) == 0)
    return(.tag_result(.schema_xbrl_ownership, filing, "xbrl_ownership"))

  df |>
    mutate(
      owner_cik = owner_cik, owner_name = owner_name,
      issuer_cik = issuer_cik, issuer_ticker = issuer_ticker,
      across(any_of(c("shares", "price_per_share", "shares_after")),
             ~ suppressWarnings(as.numeric(.x))),
      across(any_of("tx_date"), as.Date),
      accession = accession
    ) |>
    .tag_result(filing, "xbrl_ownership")
}

#' @export
sec_filing_xbrl_holding <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")

  xml_docs <- filing |> filter(category %in% c("xml", "xbrl_instance"))
  info_doc <- xml_docs |> filter(grepl("infotable|information", name, ignore.case = TRUE))
  if (nrow(info_doc) == 0) info_doc <- xml_docs |> filter(grepl("\\.xml$", name, ignore.case = TRUE))
  if (nrow(info_doc) == 0)
    stop("No information table XML found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(info_doc$url[1])
  entries <- xml2::xml_find_all(doc, "//*[local-name()='infoTable']")
  if (length(entries) == 0)
    return(.tag_result(.schema_xbrl_holding, filing, "xbrl_holding"))

  tibble(
    name_of_issuer = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='nameOfIssuer']")),
    title_of_class = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='titleOfClass']")),
    cusip          = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='cusip']")),
    value          = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='value']")))),
    shares         = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='sshPrnamt']")))),
    shares_type    = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='sshPrnamtType']")),
    put_call       = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='putCall']")),
    investment_discretion = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='investmentDiscretion']")),
    voting_sole    = suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='Sole']")))),
    voting_shared  = suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='Shared']")))),
    voting_none    = suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='None']")))),
    accession      = accession
  ) |> .tag_result(filing, "xbrl_holding")
}


# == Filing parsers: table =====================================================

#' @export
sec_filing_table <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  primary <- .find_primary(filing)
  if (is.null(primary))
    stop("No HTML document found in filing ", accession, call. = FALSE)

  doc <- .fetch_html(primary$url)
  tbls <- tryCatch(.html_tables(doc), error = function(e) list())

  results <- lapply(seq_along(tbls), function(i) {
    df <- tbls[[i]]
    if (nrow(df) == 0) return(NULL)
    nr <- nrow(df); nc <- ncol(df)
    tibble(
      table_index = i,
      row_index   = rep(seq_len(nr), each = nc),
      col_index   = rep(seq_len(nc), nr),
      name        = rep(names(df), nr),
      value       = .clean_text(c(t(as.matrix(df)))),
      accession   = accession
    )
  })
  bind_rows(results) |> .tag_result(filing, "table")
}

#' @export
sec_filing_table_subsidiary <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")

  ex21 <- filing |> filter(grepl("EX-21|exhibit.*21", description, ignore.case = TRUE))
  if (nrow(ex21) == 0)
    ex21 <- filing |> filter(grepl("ex.?21|exhibit.?21", name, ignore.case = TRUE))
  if (nrow(ex21) == 0)
    stop("No Exhibit 21 found in filing ", accession, call. = FALSE)

  doc <- .fetch_html(ex21$url[1])
  tbls <- tryCatch(.html_tables(doc), error = function(e) list())

  if (length(tbls) > 0) {
    sizes <- vapply(tbls, nrow, integer(1))
    best <- as_tibble(tbls[[which.max(sizes)]], .name_repair = "unique")
    best <- best |> filter(rowSums(is.na(pick(everything())) | pick(everything()) == "") < ncol(best))
    return(best |> mutate(accession = accession, source = "table") |>
             .tag_result(filing, "table_subsidiary"))
  }

  nodes <- xml2::xml_find_all(doc, ".//p|.//font|.//span|.//div")
  lines <- .clean_text(xml2::xml_text(nodes))
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0) {
    body <- xml2::xml_find_first(doc, ".//body")
    if (!is.na(body)) lines <- .clean_text(unlist(strsplit(xml2::xml_text(body), "\n")))
    lines <- lines[nchar(lines) > 0]
  }
  if (length(lines) == 0)
    return(.tag_result(tibble(value = character(), accession = character(),
                              source = character()), filing, "table_subsidiary"))


  tibble(value = lines, accession = accession, source = "text") |>
    .tag_result(filing, "table_subsidiary")
}


# == Filing parsers: text ======================================================

#' @export
sec_filing_text <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  primary <- .find_primary(filing)
  if (is.null(primary))
    stop("No HTML document found in filing ", accession, call. = FALSE)

  doc <- .fetch_html(primary$url)
  nodes <- xml2::xml_find_all(doc, ".//p|.//h1|.//h2|.//h3|.//h4|.//h5|.//h6|.//li|.//td")

  lines <- if (length(nodes) > 0) {
    xml2::xml_text(nodes)
  } else {
    body <- xml2::xml_find_first(doc, ".//body")
    if (is.na(body)) return(.schema_text)
    unlist(strsplit(xml2::xml_text(body), "\n"))
  }
  lines <- .clean_text(lines)
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0)
    return(.schema_text)

  tibble(line = seq_along(lines), text = lines, accession = accession) |>
    .tag_result(filing, "text")
}


# == Filing parsers: xlsx ======================================================

#' @export
sec_filing_xlsx <- function(x) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' required for sec_filing_xlsx", call. = FALSE)

  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  xlsx_doc <- .find_xlsx(filing)
  if (is.null(xlsx_doc))
    stop("No Excel report found in filing ", accession, call. = FALSE)

  ext <- if (grepl("\\.xls$", xlsx_doc$name)) ".xls" else ".xlsx"
  path <- .fetch(xlsx_doc$url, ext)
  sheet_names <- readxl::excel_sheets(path)

  results <- lapply(seq_along(sheet_names), function(i) {
    raw <- as_tibble(readxl::read_excel(path, sheet = sheet_names[i],
                                         col_names = FALSE, .name_repair = "unique"))
    if (nrow(raw) == 0) return(NULL)
    mat <- as.matrix(raw)
    mat <- ifelse(is.na(mat) | trimws(mat) == "", NA_character_, as.character(mat))
    idx <- which(!is.na(mat), arr.ind = TRUE)
    if (nrow(idx) == 0) return(NULL)
    row_labels <- as.character(raw[[1]])
    tibble(
      sheet_name = sheet_names[i], sheet_position = i,
      row_index = idx[, "row"], col_index = idx[, "col"],
      name = ifelse(!is.na(row_labels[idx[, "row"]]), row_labels[idx[, "row"]], ""),
      value = mat[idx], accession = accession
    )
  })
  bind_rows(results) |> .tag_result(filing, "xlsx")
}


# == Filing parsers: html ======================================================

#' @export
sec_filing_html <- function(x, dest = NULL) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  primary <- .find_primary(filing)
  if (is.null(primary))
    stop("No HTML document found in filing ", accession, call. = FALSE)

  if (is.null(dest)) dest <- tempfile(fileext = ".html")
  httr2::request(primary$url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = dest)

  tibble(accession = accession, document = primary$name,
         path = dest, bytes = file.size(dest)) |>
    .tag_result(filing, "html")
}


# == Parser discovery ==========================================================

#' @export
sec_parsers <- function(form = NULL) {
  all_parsers <- tibble(
    parser = c("xbrl", "xbrl_segment", "xbrl_ownership", "xbrl_holding",
               "table", "table_subsidiary", "text", "xlsx", "html"),
    forms  = c("10-K 10-Q 20-F 40-F 4 3 13F-HR N-PORT",
               "10-K 10-Q 20-F 40-F",
               "4 3",
               "13F-HR",
               "10-K 10-Q 20-F 40-F 8-K 4 13F-HR DEF-14A S-1",
               "10-K 20-F 40-F",
               "10-K 10-Q 20-F 40-F 8-K 4 13F-HR DEF-14A S-1",
               "10-K 10-Q 20-F 40-F",
               "10-K 10-Q 20-F 40-F 8-K 4 13F-HR DEF-14A S-1"),
    description = c(
      "Raw XBRL tagged facts from instance document",
      "Dimensional/segment breakdowns from XBRL contexts",
      "Form 3/4/5 insider ownership transactions",
      "13F-HR institutional holdings from information table",
      "All HTML tables from primary document",
      "Exhibit 21 subsidiary list (10-K/20-F/40-F)",
      "Plain text extraction from primary document",
      "Excel report sheets (Financial_Report.xlsx)",
      "Download primary HTML document to disk"),
    fn = c("sec_filing_xbrl", "sec_filing_xbrl_segment",
           "sec_filing_xbrl_ownership", "sec_filing_xbrl_holding",
           "sec_filing_table", "sec_filing_table_subsidiary",
           "sec_filing_text", "sec_filing_xlsx", "sec_filing_html")
  )
  if (is.null(form)) return(all_parsers)

  pattern <- paste0("\\b", toupper(form), "\\b")
  all_parsers |> filter(grepl(pattern, toupper(forms)))
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the sec.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
sec_context <- function() {
  .build_context("sec.gov", header_lines = c(
    "# sec.gov - SEC EDGAR Client for R",
    "# Dependencies: httr2, xml2, jsonlite, dplyr, tidyr, tibble",
    "# Auth: User-Agent header (set to support@scrapeable.com)",
    "# All functions return tibbles with typed columns."
  ))
}
