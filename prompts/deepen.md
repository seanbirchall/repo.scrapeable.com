# R Client Factory — Deepen Pass

## Mission
Expand shallow but working R API clients from a CSV manifest (`apis.csv`). Process one row at a time. **Read the existing client thoroughly before adding anything.** The client works — your job is to make it comprehensive. **Never stop. Never hang. Never prompt for input. If blocked, log it and move to the next row.**

---

## CSV Schema
```
index,domain,prefix,auth,base_url,complexity,scout,repair,deepen,polish,quality,notes
```

| Column | Values |
|---|---|
| `scout` | `done` \| `skipped_*` \| `failed_*` |
| `repair` | `todo` \| `done` \| *(empty)* |
| `deepen` | `todo` \| `done` \| *(empty — not applicable)* |
| `polish` | `todo` \| `done` \| *(empty)* |
| `quality` | `none` \| `shallow` \| `good` \| `comprehensive` \| *(empty)* |

**This pass targets**: rows where `repair == "done" & deepen == "todo"`.

**Before processing any row**: if `deepen != "todo"` — skip and move to the next row.

After finishing each row, write back to `apis.csv`:
- `deepen` → `done` (or keep `todo` if expansion was blocked)
- `quality` → updated assessment
- `notes` → what was added, what gaps remain

Append a one-line summary to `logs/run.log`:
```
2026-01-15 06:45:11 | fred.stlouisfed.org | deepen | done | added 6 functions (releases tags sources categories) | quality=comprehensive
2026-01-15 06:52:03 | otherapi.com | deepen | done | added pagination and 3 filter params | quality=good
```

---

## Reference Implementations
- **Complex API template**: `packages/r/sec.gov/`
- **Simple API template**: `packages/r/fred.stlouisfed.org/`
- **Conventions doc**: `repo.md`

---

## Deepen Strategy — Inventory Before Expanding

### Step 0: Inventory What Exists
Before writing anything:
1. Read `clients/{domain}.R` — list every existing public function
2. Read `tests/{domain}-output.txt` — what actually produced data?
3. Read the official API docs — what endpoints exist that the client doesn't cover?
4. Build a gap list: endpoints in the docs not covered by the client

Prioritize the gap list:
- **High value**: large datasets, search/filter, time series, bulk endpoints
- **Medium value**: reference/lookup, metadata
- **Low value**: admin, write, obscure edge-case paths

### What "Comprehensive" Looks Like
- Every major data-returning endpoint family covered
- Pagination where the API supports it (user gets one tibble, not page 1)
- Key filter/query parameters surfaced as function arguments
- Date range filtering where applicable
- All common enums/codes documented in schemas or context function
- `{prefix}_context()` accurately lists all functions with full roxygen

### Expansion Patterns

**Adding an endpoint family**:
```r
fred_releases <- function(limit = 1000L, offset = 0L, api_key = NULL) { ... }
fred_release <- function(release_id, api_key = NULL) { ... }
fred_release_series <- function(release_id, api_key = NULL) { ... }
```

**Adding pagination to an existing function**:
```r
.fred_fetch_all <- function(url, api_key) {
  results <- list()
  offset <- 0L
  repeat {
    page <- .fetch_json(paste0(url, "&offset=", offset, "&limit=1000&api_key=", api_key))
    if (length(page$seriess) == 0) break
    results <- c(results, list(page$seriess))
    if (nrow(page$seriess) < 1000) break
    offset <- offset + 1000L
  }
  dplyr::bind_rows(results)
}
```

**Adding filter parameters**:
```r
fred_series_search <- function(
  query,
  search_type = c("full_text", "series_id"),
  limit = 1000L,
  filter_variable = NULL,
  filter_value = NULL,
  api_key = NULL
) { ... }
```

---

## Failure Modes — Handle These Without Stopping

### New Endpoint Fails Live Testing
1. Check docs — URL may differ from expected
2. One probe attempt to find correct path
3. If still failing: comment the function out with `# TODO: endpoint unreachable`, note in `notes`
4. Move on — don't block on one endpoint

### R CMD check Regression
1. One fix attempt
2. If still failing → revert new additions, keep `deepen = todo`, log
3. Move on

### General Rule
**Never hang. Never prompt for input. Always update csv and move on.**

---

## Proven Patterns

### Self-Contained Files
Everything defined locally — `%||%`, `.fetch`, `.fetch_json`, schemas, private paginators.

### All Requests Through httr2
The environment routes traffic through a proxy that only supports libcurl-based packages (httr2, xml2). Never use `read.csv(url)`, `read.table(url)`, `download.file()`, `url()`, `readLines(url)`, or any base R function that reads directly from a URL. Even for plain CSV or Excel files: fetch with httr2 → write to temp file → read locally.

### Schema-First Pattern
Define schema for every new endpoint before writing the function:
```r
.schema_releases <- tibble(
  id = integer(), name = character(), press_release = logical(),
  link = character(), notes = character()
)
```

### Pagination Pattern
`._fetch_all()` loops internally. Public function returns one combined tibble. User never sees pagination.

### Context Function Pattern
Update `{prefix}_context()` to include every new function added this pass. Full roxygen block + signature for each.

### Common Gotchas
- **`as.Date()` errors**: Always `tryCatch()`
- **`fromJSON(simplifyVector = FALSE)`**: Use for inconsistent arrays
- **New endpoints may have different response shapes** — always inspect before writing schema
- **Rate limits matter more at depth** — add `Sys.sleep(0.1)` inside pagination loops if needed

---

## Process Per Row

### 0. Check deepen Column
Read `apis.csv`. If `deepen != "todo"` — skip and move to the next row.

### 1. Inventory Existing Client
- Read `clients/{domain}.R` — list all public functions
- Read `tests/{domain}-output.txt` — confirm what's working
- Note current function count and coverage

### 2. Research API Gaps
- Hit official docs (primary source only)
- List all endpoint families
- Cross-reference against existing functions
- Prioritize: high-value gaps first

### 3. Expand `clients/{domain}.R`
For each gap endpoint (highest value first):
1. Define schema tibble
2. Write function with full roxygen
3. Handle auth, pagination, error cases
4. Update `{prefix}_context()` to include the new function

### 4. Test Live (`tests/test-{domain}.R`)
- Add test calls for every new function
- Run full test suite (old + new)
- Save output to `tests/{domain}-output.txt`
- Re-assess quality:
  - `shallow` → only if severely limited by the API itself
  - `good` → most endpoint families covered
  - `comprehensive` → full coverage, pagination, filters working

### 5. Re-package for R
- Rebuild from expanded client
- `R CMD build` → `R CMD check --no-manual --no-examples --no-tests`
- One fix if failing, then move on
- `R CMD INSTALL`

### 6. Re-package for webR
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
If Docker fails → note it, set `deepen = done`, move on.

### 7. Update CSV and Move On
- `deepen` → `done` (or keep `todo` if expansion was blocked/reverted)
- `quality` → updated assessment
- `notes` → what was added, any gaps noticed for future passes
- Append to `logs/run.log`
- Move immediately to next row

---

## Anti-patterns (do NOT do these)
- Rewrite a working client from scratch — extend it
- Add endpoints without adding schemas
- Leave `{prefix}_context()` out of date after adding functions
- Return bare `data.frame()` or `tibble()` without column types
- Use `%>%` (always `|>`)
- Use xts, zoo, data.table
- Share code between client files
- Hardcode API keys
- Use 3rd-party APIs — always primary source
- Use `read.csv(url)`, `read.table(url)`, `download.file()`, `url()`, `readLines(url)`, or any base R function that reads directly from a URL — all requests MUST go through httr2
- **Stop and wait for input**
- **Retry a failing step more than once**
- **Spend more than 10 minutes on any single client**

---

## Begin

Read `apis.csv` now. Find the first row where `repair = "done"` and `deepen = "todo"`. Inventory the existing client, find the gaps, expand it, re-package. Update the CSV and move to the next. Continue until no `deepen = "todo"` rows remain or you are stopped.
