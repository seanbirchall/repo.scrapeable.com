# R Client Factory — Polish Pass

## Mission
Polish good R API clients to comprehensive quality from a CSV manifest (`apis.csv`). Process one row at a time. **This pass is about correctness, clarity, and usability — not adding more endpoints.** Focus on documentation, edge cases, error messages, and making the context function excellent. **Never stop. Never hang. Never prompt for input. If blocked, log it and move to the next row.**

---

## This Pass Targets
Process only rows where `needs_pass = polish`. Skip everything else.

Typical polish targets:
- `quality = good` — client is functionally complete but docs are thin, error messages are vague, or context function is incomplete
- `quality = comprehensive` but `last_phase != polish` — never had a dedicated polish pass

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

After finishing each row, update `quality`, `last_phase`, `last_run`, and `needs_pass`.

Log format (append to `logs/run.log`):
```
2026-01-15 08:23:44 | fred.stlouisfed.org | polish | done | improved roxygen, added enum docs, context function complete | quality=comprehensive needs_pass=done
```

---

## Reference Implementations
- **Complex API template**: `packages/r/sec.gov/`
- **Simple API template**: `packages/r/fred.stlouisfed.org/`
- **Conventions doc**: `repo.md`

---

## Polish Checklist

Work through this checklist for every client. It is ordered — do not skip to later items before earlier ones are done.

### 1. Context Function (`{prefix}_context()`)
The context function is the most important thing to get right. An LLM using this package will call it first.

**Check:**
- [ ] Lists every public function (no orphans)
- [ ] For each function: FULL roxygen block + signature line (no body)
- [ ] Auth method clearly stated at the top
- [ ] Registration URL for API keys included
- [ ] Rate limits documented if known
- [ ] At least 5 example values for key enums/codes (e.g., common series IDs for FRED, popular ticker symbols for Yahoo Finance)
- [ ] Common workflows described: "To get X, call `{prefix}_y()` then `{prefix}_z()`"

**If any of the above are missing: fix them.**

### 2. Roxygen Documentation (all public functions)
For every public function:
- [ ] `@title` — one line, verb phrase ("Fetch series observations")
- [ ] `@description` — 1-2 sentences on what it does and when to use it
- [ ] `@param` — every parameter documented, including type and valid values/range
- [ ] `@return` — describe the tibble: column names, types, what they mean
- [ ] `@examples` — at least one `\dontrun{}` example with realistic values

### 3. Error Messages
- [ ] Auth errors point to the registration URL
- [ ] Bad parameter values give actionable messages: `stop("series_id must be a non-empty string like 'GDP' or 'UNRATE'")`
- [ ] HTTP errors include the status code and URL
- [ ] Empty result sets return the schema tibble (never NULL, never error)

### 4. Edge Case Hardening
Common gaps to check and fix:
- **Missing columns**: `if ("col" %in% names(df))` before accessing optional fields
- **Type coercion crashes**: `tryCatch()` around all `as.Date()`, `as.numeric()`, `as.integer()`
- **NULL/NA propagation**: functions that receive NULL input should give clear errors, not cryptic downstream failures
- **Empty API response**: returns schema tibble, not `list()` or `NULL`
- **Single-row responses**: `fromJSON` sometimes returns a named vector instead of a data frame — guard with `as.data.frame()` or `tibble::as_tibble()`

### 5. Schemas
- [ ] Every public function has a corresponding `.schema_*` tibble
- [ ] All column types are correct (no character where date is expected)
- [ ] Schema is used for all empty returns

### 6. Function Signatures
- [ ] Sensible defaults for all optional parameters
- [ ] `limit` parameters default to a reasonable number (not 10000 — think about rate limits)
- [ ] `api_key = NULL` pattern for all auth functions
- [ ] No positional-only arguments for anything other than the primary identifier

---

## Failure Modes — Handle These Without Stopping

### R CMD check Regression from Polish Changes
If polishing breaks check:
1. One fix attempt
2. If still failing → revert polish changes, mark `needs_pass = polish`, log what happened
3. Move on

### General Rule
**Never hang. Never prompt for input. Always update csv and move on.**

---

## Process Per Row

### 0. Check needs_pass
Read `apis.csv`. If `needs_pass != polish` — skip and move to the next row.

### 1. Read the Whole Client
Read `clients/{domain}.R` top to bottom. Build a mental model:
- What does each function do?
- What's missing from the roxygen?
- Which error messages are vague?
- Is the context function complete and accurate?

### 2. Work the Polish Checklist
Go through the checklist above in order. Make all fixes in `clients/{domain}.R`.

Do NOT add new endpoint functions in this pass — that's deepen's job. If you notice a missing endpoint, note it in the CSV `notes` column and set `needs_pass = deepen` after this pass.

### 3. Run the Test Suite
- Run `tests/test-{domain}.R` — everything that passed before should still pass
- Save output to `tests/{domain}-output.txt`
- If something broke: one fix, then continue

### 4. Re-package for R
- Rebuild from polished client
- `roxygen2::roxygenise()` — verify all man pages generated correctly
- `R CMD build` → `R CMD check --no-manual --no-examples --no-tests`
- One fix if failing, then move on
- `R CMD INSTALL`

### 5. Re-package for webR
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

### 6. Update CSV and Move On
Update these columns:
- `status` → `done` or `done_no_webr`
- `quality` → `comprehensive` (if polish checklist complete), `good` (if some items couldn't be completed)
- `last_phase` → `polish`
- `last_run` → today's date
- `needs_pass` → `done` (if comprehensive), `deepen` (if gaps noticed during polish), `polish` (if couldn't complete checklist)
- `notes` → note any missing endpoints discovered for future deepen pass

Append to `logs/run.log`.
Move immediately to next row.

---

## Anti-patterns (do NOT do these)
- Add new endpoint functions (that's deepen's job — note it and move on)
- Touch logic that's working — polish is documentation and error handling, not refactoring
- Return bare `data.frame()` or `tibble()` without column types
- Use `%>%` (always `|>`)
- Use xts, zoo, data.table
- Share code between client files
- Leave `{prefix}_context()` incomplete or out of date
- Hardcode API keys
- Use `library()` calls in package R/ files
- **Stop and wait for input**
- **Retry a failing step more than once**
- **Spend more than 15 minutes on any single client**

---

## Begin

Read `apis.csv` now. Find the first row where `needs_pass = polish`. Work the polish checklist, re-package, update the CSV, move to the next. Continue until no `polish` rows remain or you are stopped.
