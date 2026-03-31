# R Client Factory — Repair Pass

## Mission
Fix broken R API clients from a CSV manifest (`apis.csv`). Process one row at a time. **Read the existing broken client first — understand what failed before touching anything.** Never rebuild from scratch unless the existing code is unsalvageable. **Never stop. Never hang. Never prompt for input. If blocked, log it and move to the next row.**

---

## This Pass Targets
Process only rows where `needs_pass = repair`. Skip everything else.

Typical repair targets:
- `failed_check` — R CMD check returned WARNING or ERROR
- `failed_other` — something else went wrong during scout pass
- `status` is `done` or `done_no_webr` but `quality = none` — client exists but produces no usable output

---

## CSV Columns (full schema)
```
domain,prefix,auth,base_url,notes,status,quality,last_phase,last_run,needs_pass
```

| Column | Values |
|---|---|
| `status` | `done` \| `done_no_webr` \| `skipped_paywall` \| `skipped_auth` \| `skipped_unreachable` \| `skipped_exists` \| `failed_check` \| `failed_other` |
| `quality` | `none` \| `shallow` \| `good` \| `comprehensive` |
| `last_phase` | `scout` \| `repair` \| `deepen` \| `polish` |
| `last_run` | ISO date e.g. `2026-01-15` |
| `needs_pass` | `repair` \| `deepen` \| `polish` \| `done` \| *(empty)* |

After finishing each row, update `status`, `quality`, `last_phase`, `last_run`, and `needs_pass`.

Log format (append to `logs/run.log`):
```
2026-01-15 05:12:03 | someapi.com | repair | done | fixed NAMESPACE export error | quality=shallow needs_pass=deepen
```

---

## Reference Implementations
- **Complex API template**: `packages/r/sec.gov/`
- **Simple API template**: `packages/r/fred.stlouisfed.org/`
- **Conventions doc**: `repo.md`

---

## Repair Strategy — Read Before Touching

### Step 0: Diagnose First
Before writing a single line:
1. Read `clients/{domain}.R` — what is there?
2. Read `logs/run.log` — what error was logged during scout?
3. Read `tests/{domain}-output.txt` if it exists — what actually ran?
4. Read the existing package in `packages/r/{domain}/` — what did check complain about?

Only after diagnosis: decide whether to patch or rewrite.

**Patch if**: logic is sound, error is mechanical (bad NAMESPACE, missing export, type coercion, date parsing crash)
**Rewrite if**: the client file is empty, produces all NULLs, or fundamentally misunderstood the API

### Common Repair Targets

| Error Type | Fix |
|---|---|
| `no visible binding for global variable` | Add `utils::globalVariables()` at top of R file |
| `NAMESPACE export not found` | Fix function name mismatch between file and NAMESPACE |
| Missing `@export` | Add to all public functions, re-roxygenise |
| `as.Date()` crash | Wrap in `tryCatch()`, return `NA_Date_` on failure |
| `fromJSON` type error | Add `simplifyVector = FALSE`, handle list output |
| Empty tibble returns | Add schema tibble, guard with `if (length(result) == 0)` |
| HTTP 404/400 in tests | Re-probe the endpoint — URL may have changed since scout |
| roxygen2 bad NAMESPACE | Write NAMESPACE by hand |

---

## Failure Modes — Handle These Without Stopping

### Still Broken After One Fix
1. Update `status = failed_check` (or keep existing failed status)
2. Set `quality = none`, `needs_pass = repair` (leave for next repair pass)
3. Log what was tried
4. Move on

### API Unreachable Now
If the API is now down or returning 5xx:
1. Keep existing status, update `needs_pass = repair`
2. Log `unreachable on repair attempt`
3. Move on

### R CMD check WARNING or ERROR
1. Attempt **one fix**
2. If still failing → keep `failed_check`, log, move on

### Docker / webR Failure
If webR build fails, don't block:
1. Mark `done_no_webr` if R package is otherwise good
2. Log Docker error
3. Move on

### General Rule
**Never hang. Never prompt for input. Always update csv and move on.**

---

## Proven Patterns

### Self-Contained Files
Each client file must work standalone with zero imports from other client files. Define `%||%`, `.fetch`, `.fetch_json`, schemas — everything — locally.

### API Key Pattern
```r
fred_series <- function(series_id, api_key = NULL) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://fred.stlouisfed.org/docs/api/api_key.html"
  )
  ...
}
```

### Schema-First Pattern
```r
.schema_studies <- tibble(
  nct_id = character(), title = character(), status = character(),
  start_date = as.Date(character()), enrollment = integer()
)
```

### Context Function Pattern
Every client MUST have `{prefix}_context()`. Check that it exists and outputs correctly. If missing, add it.

### Common Gotchas
- **`as.Date()` errors**: Always `tryCatch()` not `suppressWarnings()`
- **`fromJSON(simplifyVector = FALSE)`**: Use for inconsistent arrays or mixed types
- **Empty body responses**: Guard `resp_body_string()` with `tryCatch()`
- **NAMESPACE by hand**: roxygen2 sometimes generates garbage — write manually for reliability
- **Column name case**: Don't assume — check the actual response

---

## Process Per Row

### 0. Check needs_pass
Read `apis.csv`. If `needs_pass != repair` — skip and move to the next row.

### 1. Diagnose
- Read existing `clients/{domain}.R`
- Read `logs/run.log` entry for this domain
- Read `tests/{domain}-output.txt`
- Identify exact failure mode (see Repair Strategy above)

### 2. Probe API (if needed)
Only re-probe if the existing client fundamentally misunderstood the API or endpoints have changed:
1. Hit base URL → check status and content type
2. Try one live endpoint → inspect response structure
3. Check for `/docs` or `/swagger` changes

### 3. Patch or Rewrite `clients/{domain}.R`
- **Patch**: surgical fix for the diagnosed error
- **Rewrite**: only if client is empty or structurally broken
- Verify `{prefix}_context()` exists and is correct
- Verify all schemas defined
- Verify all public functions return tibbles

### 4. Test Live (`tests/test-{domain}.R`)
- Run tests against every public function
- Save output to `tests/{domain}-output.txt`
- Assess quality honestly:
  - `none` — nothing returns real data
  - `shallow` — 1-3 functions work, limited coverage
  - `good` — most endpoints work, reasonable coverage
  - `comprehensive` — full endpoint coverage, edge cases handled

### 5. Re-package for R (`packages/r/{package.name}/`)
- Rebuild package from fixed client
- `R CMD build` → `R CMD check --no-manual --no-examples --no-tests`
- Must pass (NOTE ok, WARNING/ERROR not ok)
- One fix attempt if failing, then move on
- `R CMD INSTALL` → verify it loads

### 6. Re-package for webR (only if R package passed)
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
If Docker fails → `done_no_webr`, log, move on.

### 7. Update CSV and Move On
Update these columns:
- `status` → `done` or `done_no_webr` (if fixed) or keep `failed_*` (if still broken)
- `quality` → `none` | `shallow` | `good` | `comprehensive`
- `last_phase` → `repair`
- `last_run` → today's date
- `needs_pass` → `deepen` (if quality is shallow/none but working), `polish` (if good), `done` (if comprehensive), `repair` (if still broken)

Append to `logs/run.log`.
Move immediately to next row.

---

## Anti-patterns (do NOT do these)
- Rebuild a working-but-shallow client from scratch (that's deepen's job)
- Return bare `data.frame()` or `tibble()` without column types
- Use `%>%` (always `|>`)
- Use xts, zoo, data.table
- Share code between client files
- Skip the context function
- Hardcode API keys
- Require API keys at package load time
- Use `library()` calls in package R/ files
- Use 3rd-party APIs — always primary source
- **Stop and wait for input**
- **Retry a failing step more than once**
- **Spend more than 5 minutes on any single blocked API**

---

## Begin

Read `apis.csv` now. Find the first row where `needs_pass = repair`. Diagnose it, fix it, update the CSV, move to the next. Continue until no `repair` rows remain or you are stopped.
