# R Client Factory — Scout Pass

## Mission
Build self-contained R API clients from a CSV manifest (`apis.csv`). Process one row at a time. Each client becomes an R package and a webR package. **Never stop. Never hang. Never prompt for input. If blocked, log it and move to the next row.**

---

## CSV Schema
```
index,domain,prefix,auth,base_url,complexity,scout,repair,deepen,polish,quality,notes
```

| Column | Values |
|---|---|
| `scout` | `todo` \| `done` \| `skipped_paywall` \| `skipped_auth` \| `skipped_unreachable` \| `skipped_exists` \| `failed_check` \| `failed_other` |
| `repair` | `todo` \| `done` \| *(empty — not applicable)* |
| `deepen` | `todo` \| `done` \| *(empty — not applicable)* |
| `polish` | `todo` \| `done` \| *(empty — not applicable)* |
| `quality` | `none` \| `shallow` \| `good` \| `comprehensive` \| *(empty)* |

**This pass targets**: rows where `scout == "todo"`.

**Before processing any row**: check `scout`. If it is anything other than `todo` — skip it and move to the next row.

After finishing each row, write back to `apis.csv`:
- `scout` → `done` (or `skipped_*` / `failed_*`)
- `repair`, `deepen`, `polish` → `todo` (only if scout = `done`)
- `quality` → initial assessment (`none` / `shallow` / `good` / `comprehensive`)
- `notes` → update with anything useful discovered

Append a one-line summary to `logs/run.log`:
```
2026-01-15 03:42:11 | sec.gov | scout | done | 8 functions | quality=shallow
2026-01-15 03:44:02 | someapi.com | scout | skipped_paywall | requires subscription
```

---

## Reference Implementations
- **Complex API template**: `packages/r/sec.gov/` (S3 objects, multiple parsers, rich print methods, file inventory patterns)
- **Simple API template**: `packages/r/fred.stlouisfed.org/` (JSON API with key, typed schemas, search/browse/fetch pattern)
- **Conventions doc**: `repo.md` (naming, deps, patterns, schemas)

---

## Failure Modes — Handle These Without Stopping

### API Key / OAuth Required
If an API requires credentials not available in the environment:
1. Write the client with `api_key = NULL` as the default parameter
2. Skip live testing — note `SKIPPED: requires API key` in the test output file
3. Build and package normally — a correct client with auth params is still valuable
4. Set `scout = skipped_auth`, leave `repair`/`deepen`/`polish` empty

### Paywalled / Commercial API
If probing returns 402, 401, or docs indicate paid-only access with no free tier:
1. Spend no more than **5 minutes** confirming it is paywalled
2. Write a stub client where `.fetch` raises: `stop("API requires paid subscription. Visit {base_url} to subscribe.")`
3. Set `scout = skipped_paywall`, add note, leave `repair`/`deepen`/`polish` empty
4. Move on immediately

### API Unreachable
If base URL returns 5xx, connection refused, or times out after **2 attempts**:
1. Set `scout = skipped_unreachable`, leave `repair`/`deepen`/`polish` empty
2. Move to next row immediately

### R CMD check WARNING or ERROR
1. Attempt **one fix**
2. If still failing → set `scout = failed_check`, log error summary, move on

### Docker / webR Failure
If the Docker build step fails for any reason:
1. Set `scout = done` (R package is good), note webR failure in `notes`
2. Log the Docker error to `logs/run.log`
3. Move on — never block on webR

### General Rule
**Never hang. Never prompt for input. Always write back to `apis.csv` and move to the next row.**

---

## Proven Patterns

### Research-First Pattern
Before writing any code for a new client, do deep research:
1. **Check for duplicates** — search `clients/` and `packages/r/` for existing code covering this domain
2. **Read existing legacy code** — check `R/` directory for old scrape_* files that cover this API
3. **Research the API thoroughly** — use web search and the official docs (ALWAYS primary source, never 3rd-party wrappers). Identify every endpoint, response format, auth, rate limits, pagination, enums
4. **Classify** — complex (SEC-like: S3 objects, multiple parsers, file inventories) or simple (FRED-like: JSON with params)
5. **Then iterate** — write client → test live → fix → test again → package

### Primary Sources Only
Always hit the original data source directly. Never go through 3rd-party APIs, aggregators, or proxy services. If a 3rd-party wrapper exists (e.g., tidycensus wraps census.gov), study it for ideas but build against the primary API.

### Self-Contained Files
Each client file must work standalone with zero imports from other client files. Define `%||%`, `.fetch`, `.fetch_json`, schemas — everything — locally in each file. When this becomes a package, the file IS the package.

### API Key Pattern
```r
#' @param api_key API key for authentication. Required for direct API access.
#'   Register at {registration_url} for a free key.
fred_series <- function(series_id, api_key = NULL) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://fred.stlouisfed.org/docs/api/api_key.html"
  )
  url <- paste0(.base_url, "/series?series_id=", series_id, "&api_key=", api_key)
  .fetch_json(url)
}
```
Never hardcode keys. Never require a key at package load time — only at call time.

### S3 Object Pattern (Complex APIs Only)
When an API has a "container" concept (SEC filing, Census dataset) where one lookup yields many sub-resources:
1. Create an S3 constructor that fetches the inventory/metadata
2. Attach metadata as attributes (cik, form, entity, etc.)
3. Write a `print.*` method showing a rich summary + the tibble data via `NextMethod()`
4. Sub-parsers accept the S3 object OR a raw identifier string via `.as_*()` helper
5. Tag results with `.tag_result()` to carry provenance attributes through

### Schema-First Pattern
Define empty tibble schemas with correct column types BEFORE writing any function:
```r
.schema_studies <- tibble(
  nct_id = character(), title = character(), status = character(),
  start_date = as.Date(character()), enrollment = integer()
)
```
Use these for all empty returns. Never return bare `tibble()`.

### Pagination Pattern
For APIs with cursor/offset pagination, implement a private `._fetch_all()` that loops internally and returns the combined result. The public function handles pagination transparently — the user gets one tibble back.

### Context Function Pattern
Every client MUST have `{prefix}_context()`. This is the single function an LLM calls to understand the entire package. It must output:

1. **Package header**: name, deps, auth method, rate limits
2. **Common values/codes**: domain-specific reference values (status enums, popular codes, etc.)
3. **For each public function**: the FULL roxygen comment block (all `#'` lines) + the function signature line — but NOT the function body. Then a note: `Run \`fn_name\` to view source or \`?fn_name\` for help.`

---

## Process Per Row

### 0. Check scout Column
Read `apis.csv`. If `scout != "todo"` — skip and move to the next row.

### 1. Research the API
- Read any existing code in `clients/` and `R/` directories for this domain
- Identify: base URL, auth method, rate limits, response format, available endpoints
- Classify: complex or simple
- If unreachable after 2 attempts → set `scout = skipped_unreachable`, move on

### 2. Write the Client (`clients/{domain}.R`)

**Dependencies** (same everywhere):
```
httr2, jsonlite, dplyr, tibble
```
Edge cases only: `xml2` (HTML/XML parsing), `readxl` (Excel), `tidyr` (reshaping)

**All HTTP requests MUST go through httr2.** The environment routes traffic through a proxy that only supports libcurl-based packages (httr2, xml2). Never use `read.csv(url)`, `read.table(url)`, `download.file()`, `url()`, `readLines(url)`, or any other base R function that reads directly from a URL — these bypass libcurl and will fail with CORS or connection errors. Even for plain CSV or Excel files at a remote URL: fetch the raw bytes with httr2 first, write to a temp file, then read locally.

**Naming**: `{prefix}_{noun}()` or `{prefix}_{noun}_{modifier}()`
- Prefix = short domain identifier (e.g., `yf_`, `fred_`, `sec_`, `census_`)
- Every public function name starts with the same prefix

**Returns**: Every public function returns a tibble. No exceptions.

**Schemas**: Define `.schema_{name}` tibbles with correct column types. Use for all empty returns. Never return bare `tibble()`.

**Types**: Cast at parse time. Dates → `as.Date()`, timestamps → `as.POSIXct()`, numbers → `as.numeric()`, integers → `as.integer()`.

**Auth**: If `auth = api_key` in `apis.csv`, use `api_key = NULL` default with a clear `stop()` message pointing to the registration URL.

**Fetch helpers** (private, per file):
```r
.fetch <- function(url, ext = ".json") { ... }
.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
```

**Context function**: Required. Must document auth requirements and registration URL.

**No shared code between clients**. 100% self-contained.

### 3. Test Live (`tests/test-{domain}.R`)
- Write a test that calls every public function with real data
- Print `head(result, 10)` for each
- Save output to `tests/{domain}-output.txt`
- If auth required and no key available → note `SKIPPED: requires API key` and continue to packaging

### 4. Package for R (`packages/r/{package.name}/`)
- Create: DESCRIPTION, LICENSE (MIT), NAMESPACE, R/utils.R, R/{prefix}.R, inst/source/{prefix}.R, man/
- Split: private utilities → `R/utils.R`, public functions → `R/{prefix}.R`
- Add `#' @export` to all public functions
- Run `roxygen2::roxygenise()` to generate man pages
- `R CMD build` → tarball in `packages/r/`
- `R CMD check --no-manual --no-examples --no-tests` → must pass (NOTE ok, WARNING/ERROR not ok)
- If WARNING/ERROR: attempt one fix → if still failing → set `scout = failed_check`, move on
- `R CMD INSTALL` → verify it loads

### 5. Package for webR (`packages/webr/`)
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
If Docker fails → note in `notes`, set `scout = done`, move on.

### 6. Update CSV and Move On
- `scout` → `done` (or `skipped_*` / `failed_*`)
- `repair`, `deepen`, `polish` → `todo` (only if `scout = done`)
- `quality` → honest assessment of what was built
- `notes` → anything useful for future passes
- Append one line to `logs/run.log`
- Move immediately to next row

---

## Package Naming Convention
Use the API's domain name: `sec.gov`, `census.gov`, `fred.stlouisfed.org`, `finance.yahoo.com`, `dol.gov`

---

## Anti-patterns (do NOT do these)
- Auto-type character columns to numeric by heuristic
- Return bare `data.frame()` or `tibble()` without column types — always use a schema
- Use `%>%` (always `|>`)
- Use xts, zoo, data.table, or any non-tidyverse data structure
- Share code between client files
- Skip the context function
- Skip the research step
- Hardcode API keys in source
- Require API keys at package load time (only at call time)
- Use `library()` calls in package R/ files
- Use 3rd-party APIs or aggregators — always primary source
- Create a client that already exists
- Guess at API behavior
- Use `read.csv(url)`, `read.table(url)`, `download.file()`, `url()`, `readLines(url)`, or any base R function that reads directly from a URL — all requests MUST go through httr2 (fetch with httr2 → temp file → read locally)
- **Stop and wait for input under any circumstance**
- **Retry a failing step more than once**
- **Spend more than 5 minutes on any single blocked API**

---

## CSV Format (`apis.csv`)
```
index,domain,prefix,auth,base_url,complexity,scout,repair,deepen,polish,quality,notes
1,sec.gov,sec,ua_header,https://www.sec.gov,complex,done,todo,todo,todo,shallow,S3 filing object
2,fred.stlouisfed.org,fred,api_key,https://api.stlouisfed.org/fred,simple,todo,,,,, 
```

---

## Lessons Learned

### API Archetypes — recognize early, code faster

| Archetype | Examples | Pattern |
|---|---|---|
| **JSON REST** | FRED, FDA, FEC, ClinicalTrials | `GET /endpoint?param=value` → JSON object/array |
| **SDMX** | ECB, BIS | `GET /data/{dataflow}/{key}?format=csv` → CSV with TIME_PERIOD/OBS_VALUE |
| **Socrata SODA** | CDC | `GET /resource/{id}.json?$where=...&$limit=...` → JSON array |
| **Socrata-like** | CMS | Custom datastore query with conditions array |
| **CSV download** | Stooq, Fed Board, DOL | Direct file download → `read.csv()` |
| **HTML scraping** | Macrotrends, Stock Analysis | Fetch HTML → `xml2::read_html()` → parse tables |
| **Cookie/auth flow** | Yahoo Finance | Get cookie → get crumb → use both in requests |
| **POST-heavy** | USASpending, FINRA | POST JSON body with filters → paginated results |

### Probing sequence — always do this first
1. Hit the base URL with a simple GET → check status code and content type
2. Try `?limit=3` or `?per_page=3` → inspect JSON structure
3. Check for `/docs`, `/api-docs`, or `/swagger`
4. Test pagination: `page/per_page`, `limit/offset`, `pageToken`, or `skip`?
5. Test filtering: query params? POST body? SDMX key path?
6. Check date formats: ISO 8601? Unix timestamps? Custom?

### Common gotchas
- **Heredoc escapes**: Write `.R` files instead of heredocs for anything with regex
- **`as.Date()` errors**: Always `tryCatch()` not `suppressWarnings()` for date parsing
- **Column name case**: Don't assume — check the actual response
- **Rate limit sharing**: `DEMO_KEY` from api.data.gov is shared across all federal APIs
- **`fromJSON(simplifyVector = FALSE)`**: Use for inconsistent arrays or mixed types
- **Empty body responses**: Guard `resp_body_string()` with `tryCatch()`
- **Binance geoblocking**: Default to `api.binance.us`, offer `base = "global"` as opt-in
- **Volume column optional**: Always handle missing columns with `if ("Volume" %in% names(df))`

### Package build shortcuts
- **Split marker pattern**: Section header splits client file into `R/utils.R` and `R/{prefix}.R`
- **@export injection loop**: See reference implementation for the pattern
- **NAMESPACE by hand**: roxygen2 sometimes generates garbage — write manually for reliability
- **webR Docker deps**: Install only what the package needs — fewer deps = faster build

### Tempo optimization
- Probe the API FIRST with a 15-line R script before writing any client code
- Write the full client in one shot
- Test all endpoints in one script
- Package skeleton is a template — generate it, don't hand-craft it

---

## Begin

Read `apis.csv` now. Find the first row where `scout = "todo"`. Process it according to the instructions above. When done, move to the next `scout = "todo"` row. Continue until all rows are processed or you are stopped.
