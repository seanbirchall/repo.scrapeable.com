# R Client Factory — Repair Pass

## Mission
Fix broken R API clients from a CSV manifest (`apis.csv`). Process one row at a time. **Read the existing broken client first — understand what failed before touching anything.** Never rebuild from scratch unless the existing code is unsalvageable. **Never stop. Never hang. Never prompt for input. If blocked, log it and move to the next row.**

---

## CSV Schema
```
index,domain,prefix,auth,base_url,complexity,scout,repair,deepen,polish,quality,notes
```

| Column | Values |
|---|---|
| `scout` | `done` \| `skipped_*` \| `failed_*` |
| `repair` | `todo` \| `done` \| *(empty — not applicable)* |
| `deepen` | `todo` \| `done` \| *(empty)* |
| `polish` | `todo` \| `done` \| *(empty)* |
| `quality` | `none` \| `shallow` \| `good` \| `comprehensive` \| *(empty)* |

**This pass targets**: rows where `scout == "done" & repair == "todo"`.

**Before processing any row**: if `repair != "todo"` — skip and move to the next row.

After finishing each row, write back to `apis.csv`:
- `repair` → `done` (or keep `todo` if still broken)
- `quality` → updated assessment
- `notes` → what was found and fixed

Append a one-line summary to `logs/run.log`:
```
2026-01-15 05:12:03 | someapi.com | repair | done | fixed NAMESPACE export error | quality=shallow
2026-01-15 05:14:44 | otherapi.com | repair | todo | still failing R CMD check after fix attempt
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
4. Check `packages/r/{domain}/` — what did R CMD check complain about?

Only after diagnosis: decide whether to patch or rewrite.

**Patch if**: logic is sound, error is mechanical (bad NAMESPACE, missing export, type coercion, date parsing crash)
**Rewrite if**: the client file is empty, produces all NULLs, or fundamentally misunderstood the API

### Missing Tests
If `tests/test-{domain}.R` does not exist or `tests/{domain}-output.txt` is empty:
1. Write the test file from scratch — call every public function with real data
2. Run it and save output to `tests/{domain}-output.txt`
3. Use the output to assess quality honestly before setting `repair = done`

### Common Repair Targets

| Error Type | Fix |
|---|---|
| `no visible binding for global variable` | Add `utils::globalVariables()` at top of R file |
| `NAMESPACE export not found` | Fix function name mismatch between file and NAMESPACE |
| Missing `@export` | Add to all public functions, re-roxygenise |
| `as.Date()` crash | Wrap in `tryCatch()`, return `NA` on failure |
| `fromJSON` type error | Add `simplifyVector = FALSE`, handle list output |
| Empty tibble returns | Add schema tibble, guard with `if (length(result) == 0)` |
| HTTP 404/400 in tests | Re-probe the endpoint — URL may have changed since scout |
| roxygen2 bad NAMESPACE | Write NAMESPACE by hand |
| Missing tests entirely | Write `tests/test-{domain}.R` from scratch |

---

## Failure Modes — Handle These Without Stopping

### Still Broken After One Fix
1. Keep `repair = todo` (leaves it in the queue for next repair run)
2. Update `quality = none`
3. Log what was tried
4. Move on

### API Unreachable Now
1. Keep `repair = todo`, add note `unreachable on repair attempt`
2. Move on

### R CMD check WARNING or ERROR
1. Attempt **one fix**
2. If still failing → keep `repair = todo`, log, move on

### Docker / webR Failure
If webR build fails but R package is good:
1. Note webR failure in `notes`
2. Set `repair = done` — don't block on webR
3. Move on

### General Rule
**Never hang. Never prompt for input. Always update csv and move on.**

---

## Proven Patterns

### Self-Contained Files
Each client file must work standalone. Everything defined locally — `%||%`, `.fetch`, `.fetch_json`, schemas, private paginators.

### All Requests Through httr2
The environment routes traffic through a proxy that only supports libcurl-based packages (httr2, xml2). Never use `read.csv(url)`, `read.table(url)`, `download.file()`, `url()`, `readLines(url)`, or any base R function that reads directly from a URL. Even for plain CSV or Excel files: fetch with httr2 → write to temp file → read locally.

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
.schema_releases <- tibble(
  id = integer(), name = character(), press_release = logical(),
  link = character(), notes = character()
)
```

### Context Function Pattern
Every client MUST have `{prefix}_context()`. If missing, add it. Must list every public function with full roxygen block + signature.

### Common Gotchas
- **`as.Date()` errors**: Always `tryCatch()` not `suppressWarnings()`
- **`fromJSON(simplifyVector = FALSE)`**: Use for inconsistent arrays or mixed types
- **Empty body responses**: Guard `resp_body_string()` with `tryCatch()`
- **NAMESPACE by hand**: roxygen2 sometimes generates garbage — write manually for reliability
- **Column name case**: Don't assume — check the actual response

---

## Process Per Row

### 0. Check repair Column
Read `apis.csv`. If `repair != "todo"` — skip and move to the next row.

### 1. Diagnose
- Read existing `clients/{domain}.R`
- Read `logs/run.log` entry for this domain
- Read `tests/{domain}-output.txt` (or note it's missing)
- Identify exact failure mode

### 2. Probe API (only if needed)
Only re-probe if the client fundamentally misunderstood the API or endpoints have changed. Otherwise go straight to the fix.

### 3. Patch or Rewrite `clients/{domain}.R`
- Surgical patch for mechanical errors
- Rewrite only if empty or structurally broken
- Write missing tests if they don't exist
- Verify `{prefix}_context()` exists and is current
- Verify all schemas defined, all public functions return tibbles

### 4. Test Live (`tests/test-{domain}.R`)
- Run every public function with real data
- Save output to `tests/{domain}-output.txt`
- Assess quality honestly:
  - `none` — nothing returns real data
  - `shallow` — 1-3 functions work, limited coverage
  - `good` — most endpoints work, reasonable coverage
  - `comprehensive` — full coverage, pagination, edge cases handled

### 5. Re-package for R
- Rebuild from fixed client
- `R CMD build` → `R CMD check --no-manual --no-examples --no-tests`
- One fix attempt if failing, then move on
- `R CMD INSTALL`

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
If Docker fails → note it, set `repair = done`, move on.

### 7. Update CSV and Move On
- `repair` → `done` (if fixed) or keep `todo` (if still broken)
- `quality` → updated assessment
- `notes` → what was found, what was fixed, anything for future passes
- Append to `logs/run.log`
- Move immediately to next row

---

## Anti-patterns (do NOT do these)
- Rebuild a working-but-shallow client from scratch (that's deepen's job)
- Return bare `data.frame()` or `tibble()` without column types
- Use `%>%` (always `|>`)
- Use xts, zoo, data.table
- Share code between client files
- Skip the context function
- Hardcode API keys
- Use `library()` calls in package R/ files
- Use 3rd-party APIs — always primary source
- Use `read.csv(url)`, `read.table(url)`, `download.file()`, `url()`, `readLines(url)`, or any base R function that reads directly from a URL — all requests MUST go through httr2
- **Stop and wait for input**
- **Retry a failing step more than once**
- **Spend more than 5 minutes on any single blocked API**

---

## Begin

Read `apis.csv` now. Find the first row where `scout = "done"` and `repair = "todo"`. Diagnose it, fix it, write missing tests, update the CSV, move to the next. Continue until no `repair = "todo"` rows remain or you are stopped.
