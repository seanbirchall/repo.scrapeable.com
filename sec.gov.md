# Resolution / lookup
# https://www.sec.gov/files/company_tickers.json
sec_entities()
# returns data.frame with cik (cik_str), ticker (ticker), name (title) columns

# Resolve helpers (local lookups against cached sec_entities, no HTTP)
# local lookup against cached sec_entities(), no HTTP
sec_resolve(cik = NULL, ticker = NULL, name = NULL)
# returns tibble with cik, ticker, name columns

# Search
# https://efts.sec.gov/LATEST/search-index?keysTyped=aapl
sec_search(query)
# returns a bunch of data but we want to return cik (_id), and name (entity)

# Collections by CIK
# https://data.sec.gov/submissions/CIK0000320193.json
sec_filings(cik, form = NULL)                # form = "10-K", "13F-HR", etc.

# https://data.sec.gov/api/xbrl/companyfacts/CIK0000320193.json
sec_facts(cik)

# https://data.sec.gov/api/xbrl/companyconcept/CIK0000320193/us-gaap/AccountsPayableCurrent.json
sec_concepts(cik, taxonomy, tag)

# Bulk / cross-company
# https://data.sec.gov/api/xbrl/frames/us-gaap/AccountsPayableCurrent/USD/CY2019Q1I.json
sec_frames(taxonomy, tag, unit, period)

# Bulk datasets
# https://www.sec.gov/Archives/edgar/daily-index/bulkdata/submissions.zip
sec_filings_bulk()

# https://www.sec.gov/Archives/edgar/daily-index/xbrl/companyfacts.zip
sec_facts_bulk()

sec_tags(taxonomy = "us-gaap") taxonomy optional if not parse all taxonomies
# GET data.sec.gov/api/xbrl/companyfacts/CIK0000320193.json
# extract names(facts$`us-gaap`) → distinct tag vector
# cached per session, single reasonable-sized payload

# scrape html table from https://www.sec.gov/data-research/sec-markets-data/insider-transactions-data-sets
# grab data from example URL https://www.sec.gov/files/structureddata/data/insider-transactions-data-sets/2025q4_form345.zip 
sec_insiders_bulk(year = NULL, quarter = NULL)

# we should scrape the html table from here to get URLS https://www.sec.gov/data-research/sec-markets-data/form-13f-data-sets
# example URL looks like https://www.sec.gov/files/structureddata/data/form-13f-data-sets/01dec2025-28feb2026_form13f.zip
sec_13fs_bulk()

# same thing scrapea html table from here to get URLS https://www.sec.gov/data-research/sec-markets-data/fails-deliver-data
# example URL looks like https://www.sec.gov/files/data/fails-deliver-data/cnsfails202602b.zip
sec_ftds_bulk()

# Filing document (the engine) S3 methods
sec_filing(accession_number, parser = "meta") default meta data (all links and meta data)
# return meta data from sec_filings + all links to various filings
sec_filing_meta(accession_number)
# parse xbrl only certain forms have xbrl
# this covers sec_xbrl and all the segment stuff we will just parse it all in one
# form 4 and 1f also have xbrl
sec_filing_xbrl(accession_number)
# all forms have text parse text from the doc
sec_filing_text(accession_number)
# many forms have html tables parse all html tables
# this covers asreported and html tables from other filings
# form 4 has html tables too and 13f
sec_filing_table(accession_number)
# only certain forms have xlsx return all sheets
# mainly for 10k 10q 40f...so on they have report.xlsx or report.xls in older ones
sec_filing_xlsx(accession_number)
# save the filing as html
# should work for most
sec_filing_html(accession_number)

# Parser discovery
sec_parsers(form)                            # → which parsers are valid for a given form