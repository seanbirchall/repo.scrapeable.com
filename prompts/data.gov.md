# data.gov — R Client Factory

## Mission
Build self-contained R API clients from the data.gov resource catalog stored in PostgreSQL. The `client_context` table has one row per domain with three fields: `r_package` (domain name), `context` (a text blob with every known dataset and endpoint for that domain), and `status`. Process one row at a time. Each client becomes an R package and a webR package. **Never stop. Never hang. Never prompt for input. If blocked, log it and move to the next row.**

---

## API Keys

These are data.gov API keys (shared across all federal APIs via api.data.gov). Rotate between them to avoid rate limits. Pass as `api_key` parameter in clients that need it, or as `X-Api-Key` header.

```
PgMWLYcPjn2ddhFc95mOH01QpyCWznEMzO44flYY
5FBLV04H2FYQjo37PR4cQHv2Bxk43cmlzg9YPZkB
6Jc28rwzaU7qTBpGH4RZJYwip8bcYrRKIaqiveji
```

When building clients that hit federal APIs requiring a key, use these for probing and testing. In the client code itself, still use `api_key = NULL` as the default parameter with a `stop()` message — users provide their own key at call time.

---

## Database

### Table: `client_context`
```
r_package   TEXT PRIMARY KEY   -- the domain / R package name (e.g. "cdc.gov")
context     TEXT               -- everything known about this domain (datasets, endpoints, formats, tags, descriptions)
status      TEXT               -- processing state
```

### Status values
| Value | Meaning |
|---|---|
| `todo` | Not yet processed |
| `done` | Client built, packaged, tested |
| `extended` | Existing client was found and extended with new endpoints |
| `skipped_auth` | Requires credentials not available |
| `skipped_paywall` | Paid-only access, no free tier |
| `skipped_unreachable` | All endpoints dead after probing |
| `skipped_no_data` | Context blob has no usable endpoints after probing all of them |
| `skipped_exists` | Client already exists and context adds nothing new |
| `failed_check` | R CMD check failed after one fix attempt |
| `failed_other` | Other failure |

**This pass targets**: rows where `status = 'todo'`.

After finishing each domain, update the row:
```sql
UPDATE client_context SET status = 'done' WHERE r_package = '{domain}';
```

Append a one-line summary to `logs/datagov.log`:
```
2026-04-02 14:22:01 | cdc.gov | done | 8 functions | Socrata SODA client, 823 views
2026-04-02 14:22:03 | neh.gov | done | 4 functions | XML download client
2026-04-02 14:44:02 | someplace.edu | skipped_no_data | only landing pages
```

---

## Reference Implementations
- **Complex API template**: `packages/r/sec.gov/` (S3 objects, multiple parsers, rich print methods, file inventory patterns)
- **Simple API template**: `packages/r/fred.stlouisfed.org/` (JSON API with key, typed schemas, search/browse/fetch pattern)
- **Conventions doc**: `prompts/repo.md` (naming, deps, patterns, schemas)

---

## Reading the Context Blob

Each row's `context` field is a structured text blob. Example:

```
# cdc.gov
# Organizations: U.S. Department of Health & Human Services
# Subdomains: data.cdc.gov, wonder.cdc.gov
# Datasets: 913
# Unique endpoints: 967
# Socrata views: 823 (SODA API — use view ID to query)

## Weekly Gasoline Product Supplied
Description of the dataset...
Tags: gasoline; fhwa; weekly
Update frequency: R/P1W
Docs: https://www.cdc.gov/something
  [JSON] https://data.cdc.gov/api/views/sz5x-j2c3/rows.json?accessType=DOWNLOAD
    schema: https://data.cdc.gov/api/views/sz5x-j2c3/columns.json

## Another Dataset
...
  [CSV] https://example.gov/data/file.csv
  [ZIP] https://example.gov/data/archive.zip
```

Key fields in the header:
- **Socrata views** — if present, this is a Socrata portal. Build a SODA query client.
- **Unique endpoints** — total number of distinct URLs. Gives you scope.
- **Datasets** — number of datasets. Maps roughly to how many functions you might need.

Per dataset:
- **`[FORMAT] url`** — the actual endpoint. **Trust these URLs.** They came from the CKAN catalog.
- **`schema: url`** — data dictionary or column definition URL, if available.
- **Tags** — use for `_search()` and `_context()` functions.
- **Update frequency** — document in roxygen. `R/P1W` = weekly, `R/P1M` = monthly, `R/P1Y` = yearly.
- **Docs** — landing page for human-readable documentation.

---

## Using Agents

You may use agents to parallelize work where it makes sense:
- **Research agents** — to probe endpoints, inspect response structure, discover column names and types
- **Build agents** — to work on multiple independent domains simultaneously
- **Test agents** — to run tests while you move to the next domain

Use your judgment on when parallelism helps vs adds complexity. Single-threaded is fine for most domains.

---

## Process Per Row

### 0. Check Status and Existing Code
Read `client_context` for the next `todo` row.

**Before writing anything**, check if a client already exists:
- Search `clients/` for `{domain}.R`
- Search `packages/r/` for a `{domain}/` directory

If a client exists:
1. Read the existing client code
2. Compare against the context blob — are there endpoints not yet covered?
3. If yes — extend the client with new functions, set `status = 'extended'`
4. If no — set `status = 'skipped_exists'`, move on

### 1. Analyze the Context Blob and Design the Client

Read the `context` field. Every client needs two things:

1. **Discovery** — how does a user find what's available? This could be a search endpoint, a catalog listing, an HTML index page to scrape, or a hardcoded inventory derived from the context blob.
2. **Parse** — how does a user get the actual data back as a tibble? For each type of data, there should be a specific function that tells the user exactly what they're getting.

And where it makes sense:

3. **Recursive depth** — drill down as far as the data goes. `sec.gov` is the gold standard: `sec_filings(cik)` → `sec_filing(accession)` → `sec_filing_xbrl(filing)` → individual fact tables. If a domain has datasets that contain sub-resources, parse them.

#### Function naming — be specific, never vague

Every function name must tell the user exactly what data they're getting. Study how `sec.gov` does it:

```
sec_entities()                   # discovery: all known entities
sec_search(query)                # discovery: search by keyword
sec_filings(cik)                 # parse: filings for an entity
sec_facts(cik)                   # parse: XBRL facts for an entity
sec_filing(accession)            # deeper: one specific filing (S3 object)
sec_filing_xbrl(filing)          # deepest: parse XBRL from that filing
sec_filing_table(filing)         # deepest: parse HTML tables from that filing
sec_filings_bulk()               # bulk: full filings index download
sec_facts_bulk()                 # bulk: full facts download
sec_insiders_bulk(year, quarter) # bulk: insider transactions by period
```

**Be specific about what data a function returns.** The name should tell the user what they're getting:
- `cms_providers()` not `cms_get()`
- `cdc_wastewater(pathogen)` not `cdc_download(view_id)`
- `dot_crossings()` not `dot_data()`
- `neh_grants()` not `neh_fetch()`

**Listing and search are first-class features.** LLMs iterate fast and need to explore what's available. Every domain should have robust discovery:
- `{prefix}_list()` — list all available datasets/resources as a tibble. This is the catalog. An LLM calls this first to see what exists.
- `{prefix}_search(query)` — keyword search across titles, tags, descriptions.

These are distinct from data-fetching functions. `cdc_list()` returns a tibble of available datasets with their view IDs, titles, tags. `cdc_wastewater()` returns actual wastewater data. Both are useful — one for discovery, one for data.

Use `_list` not `_datasets` — it's shorter and unambiguous. If the domain only has one type of thing, `{prefix}_list()` is fine. If it has multiple resource types, be specific: `sec_filings(cik)` lists filings, `sec_tags()` lists XBRL tags.

**Bulk operations** use `{prefix}_{thing}_bulk()`:
- `cdc_surveillance_bulk()` — download all surveillance datasets
- `dot_crashes_bulk(year)` — download crash data by year

#### Socrata portals
For Socrata domains, every view ID in the context blob is a queryable dataset. The SODA API is uniform but the **function names should reflect the data**, not the API:

```r
# Good — names describe the data
cdc_wastewater(pathogen = "SARS-CoV-2")
cdc_mortality(cause = "COVID-19", state = "NY")
cdc_surveillance(disease = "influenza")
cdc_search(query)           # discovery across all views
cdc_view(view_id, ...)      # escape hatch for any view by ID

# Bad — generic, tells you nothing
cdc_get(view_id)
cdc_datasets()
cdc_download(id)
```

Group related Socrata views into named functions where patterns emerge (e.g., all wastewater datasets → one function with a `pathogen` parameter). Use a generic `_view(view_id)` as a fallback for the long tail.

#### HTML pages and geo services — don't skip them

HTML pages often contain discovery tables, file indexes, or embedded data. Scrape them with `xml2::read_html()` and parse useful tables/links into tibbles.

ArcGIS/WMS/WFS endpoints return real data — features, geometries, attributes. These can be queried and returned as tibbles with `sf`-compatible columns. Don't skip geo services just because they're spatial.

#### How to classify

| What you see in the context | What to build |
|---|---|
| Socrata views (`/api/views/`) | Named functions per data category + generic `_view()` fallback |
| JSON REST API (`/api/` + JSON) | Standard REST functions per endpoint, per `repo.md` |
| CSV/ZIP/XLS downloads | Named download functions per data type + `_bulk()` variants |
| HTML pages | Scrape for discovery (file lists, data tables, indexes) |
| ArcGIS REST / WMS / WFS | Query layers, return feature attributes as tibbles |
| Mixed | All of the above — build the complete client |

### 2. Probe Every Endpoint
Hit every endpoint listed in the context blob. No guesswork. For each URL:

- Fetch a small sample (`?$limit=3` for Socrata, `?limit=3` for REST, or HEAD + first few KB for downloads)
- Record: HTTP status, Content-Type, column names, column types, pagination pattern
- Flag dead links (4xx/5xx), auth-required (401/403), redirects (3xx)
- Note rate limit headers if present

This is the source of truth for what the client will wrap. Every function you write must correspond to a probed, working endpoint.

If every endpoint is dead → `skipped_unreachable`, move on.
If auth required and no key available → write the client with `api_key = NULL` default, `skipped_auth`.

### 3. Research (When Needed)
If the probed endpoints show a non-obvious structure (custom pagination, nested JSON, SDMX, etc.), use a research agent to:
- Check the domain's API documentation (the `Docs:` URLs in the context blob)
- Search for existing R/Python wrappers for ideas (but never use them as dependencies)
- Identify auth patterns, rate limits, query parameters

Don't research if it's a straightforward Socrata or CSV download domain.

### 4. Write the Client (`clients/{domain}.R`)

**Dependencies** (same everywhere):
```
httr2, jsonlite, dplyr, tibble
```
Edge cases only: `xml2` (HTML/XML parsing), `readxl` (Excel), `tidyr` (reshaping)

**All HTTP requests MUST go through httr2.** Never use `read.csv(url)`, `download.file()`, `url()`, `readLines(url)`, or any base R function that reads directly from a URL. Fetch with httr2 → temp file → read locally.

**Naming**: `{prefix}_{noun}()` or `{prefix}_{noun}_{modifier}()`
- Prefix = short domain identifier (e.g., `cdc_`, `cms_`, `dot_`)
- Every public function starts with the same prefix

**Returns**: Every public function returns a tibble. No exceptions.

**Schemas**: Define `.schema_{name}` tibbles with correct column types. Use for all empty returns.

**Types**: Cast at parse time. Dates → `as.Date()`, timestamps → `as.POSIXct()`, numbers → `as.numeric()`, integers → `as.integer()`.

#### SODA query helper (private, for Socrata domains)
Every Socrata client needs a private `._soda_query()` that the named public functions call:
```r
._soda_query <- function(view_id, where = NULL, limit = 1000, offset = 0) {
  base <- sprintf("https://{subdomain}/resource/%s.json", view_id)
  req <- httr2::request(base) |>
    httr2::req_url_query(`$limit` = limit, `$offset` = offset)
  if (!is.null(where)) req <- req |> httr2::req_url_query(`$where` = where)
  ...
}
```

Public functions call this with the right view ID and type the result:
```r
cdc_wastewater <- function(pathogen = NULL, limit = 1000) {
  # Map pathogen to view ID
  views <- list("SARS-CoV-2" = "j9g8-acpt", "RSV" = "45cq-cw4i", ...)
  view_id <- if (!is.null(pathogen)) views[[pathogen]] else views[[1]]
  df <- ._soda_query(view_id, limit = limit)
  df |> mutate(date = as.Date(date), value = as.numeric(value), ...)
}
```

#### All Domains
- Use dataset descriptions and tags from the context blob for `_context()` output
- Use `update_frequency` to document refresh cadence in roxygen
- Use `schema:` URLs to discover column names/types when available
- The `_context()` function must include the full roxygen + signature for every public function (not the body)
- Every public function returns a typed tibble — define `.schema_{name}` for each
- Drill down recursively where the data has depth (filing → sub-documents, dataset → sub-tables)

### 5. Test Live (`tests/test-{domain}.R`)
- Call every public function with real data
- Print `head(result, 10)` for each
- Save output to `tests/{domain}-output.txt`
- If auth required → note `SKIPPED: requires API key` and continue to packaging

### 6. Package for R (`packages/r/{package.name}/`)
- Create: DESCRIPTION, LICENSE (MIT), NAMESPACE, R/utils.R, R/{prefix}.R, inst/source/{prefix}.R, man/
- Split: private utilities → `R/utils.R`, public functions → `R/{prefix}.R`
- Add `#' @export` to all public functions
- Run `roxygen2::roxygenise()` to generate man pages
- `R CMD build` → tarball
- `R CMD check --no-manual --no-examples --no-tests` → must pass (NOTE ok, WARNING/ERROR not ok)
- If WARNING/ERROR: attempt one fix → if still failing → `failed_check`, move on
- `R CMD INSTALL` → verify it loads

### 7. Package for webR (`packages/webr/`)
```bash
docker run --rm \
  -v "packages/r:/input" \
  -v "packages/webr:/output" \
  ghcr.io/r-wasm/webr:main \
  bash -c '
    R --vanilla -e "install.packages(c(\"httr2\",\"jsonlite\",\"dplyr\",\"tibble\"), repos=\"https://cloud.r-project.org\")"
    mkdir -p /tmp/lib
    R CMD INSTALL /input/{pkg}_0.1.0.tar.gz --library=/tmp/lib
    R_VER=$(R --version | head -1 | grep -oP "\d+\.\d+")
    cp /input/{pkg}_0.1.0.tar.gz /output/src/contrib/
    cd /tmp/lib && tar czf /output/bin/emscripten/contrib/${R_VER}/{pkg}_0.1.0.tgz {pkg}
    cd /output/src/contrib && R --vanilla -e "tools::write_PACKAGES(\".\", type=\"source\")"
    cd /output/bin/emscripten/contrib/${R_VER} && R --vanilla -e "tools::write_PACKAGES(\".\", type=\"mac.binary\")"
  '
```
If Docker fails → note in log, set `status = 'done'`, move on.

### 8. Update Database and Move On
```sql
UPDATE client_context SET status = 'done' WHERE r_package = '{domain}';
```
Append to `logs/datagov.log`. Move immediately to next `todo` row.

---

## Failure Modes — Handle These Without Stopping

### API Key / OAuth Required
1. Write the client with `api_key = NULL` default and clear `stop()` message
2. Skip live testing — note `SKIPPED: requires API key`
3. Package normally — a correct client with auth params is still valuable
4. Set `status = 'skipped_auth'`

### All Endpoints Dead
1. Probe 3-5 endpoints, all return 4xx/5xx
2. Set `status = 'skipped_unreachable'`
3. Move on

### Context Blob Too Large for Context Window
Some domains (census.gov, noaa.gov) have enormous context blobs. If the blob won't fit:
1. Process it in batches — read and probe a chunk of endpoints at a time
2. Use agents to parallelize probing across chunks
3. Build the client incrementally as you discover working endpoints
4. Every endpoint must still be hit — no shortcuts on large domains

### R CMD check Failure
1. Attempt **one fix**
2. If still failing → `status = 'failed_check'`, log error, move on

### Docker / webR Failure
1. Set `status = 'done'` (R package is good), note webR failure
2. Move on — never block on webR

### General Rule
**Never hang. Never prompt for input. Always update the database and move to the next row.**

---

## Anti-patterns (do NOT do these)
- Skip probing endpoints — every URL must be hit and validated before writing client code
- Build a client for a domain that already exists without checking for extension opportunities
- Use 3rd-party APIs or aggregators — the context blob has the primary source URLs
- Hardcode API keys or require them at package load time
- Return bare `tibble()` without column types — always use a schema
- Use `read.csv(url)` or any base R URL reader — all HTTP goes through httr2
- Use `%>%` — always `|>`
- Use xts, zoo, data.table, or any non-tidyverse data structure
- Share code between client files — each is 100% self-contained
- Skip the `_context()` function
- Guess at API behavior — probe first
- Auto-type character columns to numeric by heuristic
- Stop and wait for input under any circumstance
- Retry a failing step more than once
- Spend more than 5 minutes on any single blocked domain

---

## Begin

Connect to the database. Query:
```sql
SELECT r_package, context FROM client_context WHERE status = 'todo' LIMIT 1;
```
Process it according to the instructions above. When done, update the row and move to the next. Continue until all rows are processed or you are stopped.
