# Upgrading scripts to Rems 0.3.0

This file has two parts:

1. **[Prompt](#part-1--prompt-for-the-ai-assistant)**: paste it into an AI coding assistant (Claude, Copilot Chat, etc.) together with one script that uses Rems. The assistant upgrades the script and tells you what to check.
2. **[Guideline](#part-2--guideline-for-people-doing-the-upgrade)**: the rollout steps, how to check the result, and fixes for common errors.

**Upgrade R before Rems.** Rems 0.3.0 needs **R 4.5 or newer** on Windows; **R 4.6.x is recommended**. On older R the install helper stops with a clear message.

---

## Part 1 – Prompt for the AI assistant

Copy everything from the line below down to the end of Part 1. Then paste your script after it.

---

You are upgrading an R script that downloads data from GE EMS using the in-house **Rems** package. The script currently uses **Rems 0.2.0**, probably on an old R (for example 4.0.2). It must be upgraded to **Rems 0.3.0** on **R 4.6.x**.

Keep the script's behaviour and output files exactly the same. Change only what is listed below, and flag anything else instead of changing it.

### Background: what changed in Rems 0.3.0

The calling style is unchanged: `connect()`, `flt_query()`, `set_database()`, `select()`, `filter()`, `get_top()`, `run()`, `tseries_query()`, `run_multiflts()`, `insert_query()` and `update_fieldtree()` / `save_metadata()` work as before. The internal behaviour that changed:

| Area | Rems 0.2.0 | Rems 0.3.0 |
|---|---|---|
| TLS | Certificate checks turned off for the whole R session (`ssl_verifypeer = 0`) | Certificates verified. New `connect(..., cainfo = "ca.pem")` for corporate CA bundles |
| Credentials | Password stored in the connection object as `conn$foqa$pwd` | Password kept only for the current R session; not in `conn` |
| Token | `conn$token`, `conn$token_type` | `conn$auth$token`, `conn$auth$token_type`; refreshed automatically when it expires (401) |
| Hanging | No timeout, so a dropped connection could hang forever | 60 s connect timeout; abort if no data for `stall_timeout` seconds (default 300, `connect(..., stall_timeout = 600)`) |
| Retries | None (the reconnect code was broken) | GET/DELETE retried on network errors, 429 and 5xx. POSTs are not retried |
| Async query failing part-way | Printed a message and returned partial data | Raises a **warning** containing "Async query stopped early" and returns partial data |
| `run_multiflts()` | One flight at a time | Flights fetched in parallel; `max_concurrent = 4` by default. Output order unchanged |
| `run()` / `async_run()` | Sequential pages | Still sequential by default; optional `max_concurrent` for parallel page reads |
| Metadata cache with no `data_file` | Written into the package install folder | Written to the user cache folder (`tools::R_user_dir("Rems", "cache")`) |
| Requirements | R ≥ 3.2.3, old httr/RSQLite | R ≥ 4.5 on Windows, RSQLite ≥ 3.53.2 (patched SQLite), httr ≥ 1.4.7, curl, jsonlite, dplyr, stringdist |
| Tarball | `Rems_0.2.0.tar.gz` | `Rems_0.3.0.tar.gz` (same folder on P:) |

### Step 1 – Find every Rems touch point

List each line in the script that:

- installs or loads Rems or its dependencies (`install.packages`, `library`, `require`);
- calls a Rems function: `connect`, `reconnect`, `request`, `flt_query`, `tseries_query`, `insert_query`, `select`, `filter`, `get_top`, `group_by`, `order_by.FltQuery`, `distinct`, `range`, `reset`, `run`, `simple_run`, `async_run`, `run_multiflts`, `search_param`, `set_database`, `update_fieldtree`, `update_dbtree`, `generate_preset_fieldtree`, `save_metadata`, `load_metadata`, `retrieve_ems_instances`;
- reads fields of the connection object (`conn$...`);
- calls `httr::set_config`, `httr::config`, `set_config(config(ssl_verifypeer = ...))` or similar;
- saves or loads a connection object (`saveRDS`, `save`, `load`, `.RData`);
- wraps Rems calls in retry loops, `Sys.sleep()`, `tryCatch()` or `reconnect()`.

### Step 2 – Required changes

Make these changes wherever they apply.

**2a. Replace the install block** with this helper, unchanged. Keep the script's own tarball path, only changing the filename to `Rems_0.3.0.tar.gz`. Put it before `library(Rems)`, and before any other `library()` call that loads Rems's dependencies (DBI, RSQLite, httr, dplyr). Packages that are already loaded can't be upgraded on Windows.

```r
url <- "P:/Trial_Run/Charlie/R/Package/Rems_0.3.0.tar.gz"

# Install or upgrade Rems and anything it needs that is missing or too old.
# The requirements are read from the tarball itself, so this keeps working when they change.
install_rems <- function(tarball, repos = "https://cloud.r-project.org") {
  if (.Platform$OS.type == "windows" && getRversion() < "4.5.0") {
    stop("Rems 0.3.0 needs R 4.5 or newer on Windows (CRAN only builds the patched RSQLite for R >= 4.5). ",
         "This machine has R ", getRversion(), ". Please upgrade R.")
  }
  exdir <- tempfile()
  untar(tarball, files = "Rems/DESCRIPTION", exdir = exdir)
  desc <- read.dcf(file.path(exdir, "Rems", "DESCRIPTION"))
  fields <- intersect(c("Depends", "Imports"), colnames(desc))
  reqs <- trimws(strsplit(gsub("\\s+", " ", paste(desc[, fields], collapse = ",")), ",")[[1]])
  reqs <- reqs[nzchar(reqs)]
  pkgs <- trimws(sub("\\(.*", "", reqs))
  mins <- ifelse(grepl(">=", reqs), trimws(sub(".*>=\\s*([^)]+)\\).*", "\\1", reqs)), NA)
  base <- rownames(installed.packages(priority = "base"))

  missing_or_old <- function() {
    need <- character(0)
    for (i in seq_along(pkgs)) {
      if (pkgs[i] %in% c("R", base)) next
      have <- tryCatch(packageVersion(pkgs[i]), error = function(e) NULL)
      if (is.null(have) || (!is.na(mins[i]) && have < mins[i])) need <- c(need, pkgs[i])
    }
    need
  }
  need <- missing_or_old()
  if (length(need) > 0) {
    message("Installing/upgrading: ", paste(need, collapse = ", "))
    install.packages(need, repos = repos)
    # CRAN stops building packages for old R versions, so an install can give an older version than required
    still <- missing_or_old()
    if (length(still) > 0) {
      stop("Could not install a new enough version of: ", paste(still, collapse = ", "),
           " for R ", getRversion(), ". Please upgrade R.")
    }
  }

  have_rems <- tryCatch(packageVersion("Rems"), error = function(e) NULL)
  if (is.null(have_rems) || have_rems < desc[, "Version"]) {
    install.packages(tarball, repos = NULL, type = "source")
  }
}
install_rems(url)
rm(url)
library(Rems)
```

Remove packages that were installed only for old Rems and aren't used elsewhere in the script (for example `xml2`). Keep install lines for packages the script itself uses (for example `data.table`).

**2b. Remove TLS workarounds.** Delete any `set_config(config(ssl_verifypeer = 0))`, `ssl_verifyhost = 0` or similar line. If the script had a comment about certificate errors, add a comment pointing to `connect(..., cainfo = "path/to/corporate-ca.pem")` instead. Never turn verification back off.

**2c. Fix connection-object fields.**
- `conn$token` → `conn$auth$token`, and `conn$token_type` → `conn$auth$token_type`.
- `conn$foqa$usr` / `conn$foqa$pwd` no longer exist. If the script needs the username, keep it in its own variable. Never read the password back from `conn`.
- If the script builds its own `Authorization` header for `request()`, prefer passing no `headers` argument. Only the default headers get refreshed automatically when the token expires.

**2d. Don't reuse a saved connection.** A `conn` restored with `readRDS()` / `load()` in a new R session can't reconnect. Replace it with a fresh `connect()` call.

**2e. Stop partial downloads from being written as complete files.** Wherever `run()` / `async_run()` on a flight query feeds into a file write, wrap it like this:

```r
df <- withCallingHandlers(run(qry), warning = function(w) {
  if (grepl("Async query stopped early", conditionMessage(w))) stop(conditionMessage(w))
})
```

**2f. Simplify old workarounds (optional, flag before changing).**
- Manual `reconnect()` calls on a timer, or `Sys.sleep()` before reconnecting, are no longer needed. Rems refreshes the token itself.
- Retry loops around Rems calls can stay. A stalled call now ends with an error instead of hanging, so the loop will actually get the chance to retry.

**2g. Clear credentials after use.** If credentials are read from a file or prompt, add `rm(<credential variable>)` right after `connect()`. Mention, without changing it, that storing a password in a plain-text file is a risk, and suggest the `keyring` package.

### Step 3 – Check for R 4.0 → 4.6 breaking changes

These are changes in R itself that can break old scripts. Check for each one and fix it when found.

1. **Conditions longer than 1 are now errors.** Since R 4.2, `if (x)` / `while (x)` where `x` has length > 1 is an error, not a warning. Since R 4.3, `&&` and `||` with length > 1 operands are errors too. A common trigger is `if (class(x) == "data.frame")`: `class()` can return two values, for example `c("matrix", "array")` or `c("data.table", "data.frame")`. Use `is.data.frame(x)`, `inherits(x, "...")` or `is.matrix(x)` instead.
2. **Windows now uses UTF-8 by default** (since R 4.2). Files read with `readLines()`, `read.csv()` or `fread()` that contain non-ASCII text, such as Chinese characters or accented names, may now decode differently. If a source file isn't UTF-8, set `fileEncoding=` / `encoding=` explicitly.
3. **Rems exports function names that mask other packages.** `select`, `filter`, `group_by`, `distinct`, `range`, `reset` and `run` are all exported by Rems:
   - `range()` from Rems masks **`base::range()`**. Any `range(x)` on a numeric vector after `library(Rems)` fails. Use `base::range(x)`, or `c(min(x), max(x))`.
   - If the script also loads **dplyr**, whichever package is loaded last wins. Call the Rems versions explicitly as `Rems::select()`, `Rems::filter()`, and the dplyr ones as `dplyr::select()`, and so on.
4. **Removed or defunct functions:** flag any calls that R 4.6 warns about or errors on. Check with a parse run (Step 4).

### Step 4 – Check EMS query logic (flag, don't change)

- **Date filters only support `<` and `>=`.** A filter like `'Flight Date (Exact)' < 'YYYY-12-31'` **excludes** 31 December. If the intent is "up to and including the year end", the bound should be `< '<next year>-01-01'`. Report it; don't change it without confirmation.
- Report field names in `select()` / `filter()` that look misspelled (for example "Comander"). Those names must match EMS exactly, so don't "fix" them.

### Step 5 – What to return

Reply with:

1. **Summary of changes**: a numbered list giving the line(s) changed, what changed and why, with a reference to the step above.
2. **The full upgraded script.**
3. **Items needing a human decision**: anything flagged but not changed.
4. **A validation checklist** for this script, filled in from Part 2, section 3 of the guideline (row counts, columns, output files to compare).

Don't invent EMS field names, servers or file paths. If something is ambiguous, ask.

**Script to upgrade:**

```r
<paste the script here>
```

---

## Part 2 – Guideline for people doing the upgrade

### 1. Rollout order

1. **Install R 4.6.x** (the latest from <https://cloud.r-project.org/bin/windows/base/>). It installs alongside the old R. In RStudio, select it under *Tools → Global Options → General → R version*, then restart RStudio.
2. **(Optional) Run the old script once on the new R** to separate "R changed" from "Rems changed". On a fresh R, the old install block needs `jsonlite`, `dplyr` and `stringdist` added:
   ```r
   install.packages(c("DBI", "RSQLite", "httr", "jsonlite", "dplyr", "stringdist"))
   install.packages("P:/Trial_Run/Charlie/R/Package/Rems_0.2.0.tar.gz", repos = NULL, type = "source")
   ```
3. **Copy `Rems_0.3.0.tar.gz`** to `P:/Trial_Run/Charlie/R/Package/`. Keep `Rems_0.2.0.tar.gz` there until every script has moved over.
4. **Upgrade each script** with the prompt in Part 1, review the changes, then validate it (section 3).
5. **Run each upgraded script in a fresh R session.** If R reports that a package is in use, restart R and run again.

### 2. Checking the environment

Run this on each machine after upgrading:

```r
R.version.string                  # expect R 4.6.x (4.5.x is the minimum)
packageVersion("Rems")            # expect 0.3.0
RSQLite::rsqliteVersion()         # bundled SQLite, expect >= 3.53.2
curl::curl_version()$version      # bundled libcurl; newer is better
```

### 3. Checking an upgraded script

Do this once per script before scheduling it.

1. **It parses:** `parse("script.R")` gives no error.
2. **Smoke test:** temporarily add `qry <- get_top(qry, 1000)` before `run(qry)` and run the script. Check that it connects, finds every field and returns 1,000 rows.
3. **Compare with the old output:** run the full script with old R + Rems 0.2.0 and with new R + Rems 0.3.0 for the same date range, writing to different folders. Then compare:
   ```r
   old <- data.table::fread("old/QAR_2026.csv"); new <- data.table::fread("new/QAR_2026.csv")
   stopifnot(identical(names(old), names(new)), nrow(old) == nrow(new))
   data.table::setkey(old, `Flight Record`); data.table::setkey(new, `Flight Record`)
   all.equal(old, new)   # expect TRUE
   ```
   Small differences can come from data that changed in EMS between the two runs. Re-run both close together, or compare a closed historic period.
4. **Check the timing** in the log. Async downloads should take about the same time or less, and `run_multiflts()` should be noticeably faster.
5. **Check the log for warnings,** especially "Async query stopped early".

### 4. Troubleshooting

| Message | Cause | Fix |
|---|---|---|
| `Rems 0.3.0 needs R 4.5 or newer on Windows` | Old R | Install R 4.6.x |
| `Could not install a new enough version of: RSQLite ...` | R too old for the patched RSQLite | Install R 4.6.x |
| `package 'RSQLite' 2.x was found, but >= 3.53.2 is required` / `lazy loading failed` | Rems installed without the helper on old R | Use the install helper on R ≥ 4.5 |
| `SSL certificate problem: unable to get local issuer certificate` | Corporate network re-signs HTTPS | Ask IT for the corporate CA file (PEM) and use `connect(..., cainfo = "C:/path/ca.pem")`. Don't turn off verification |
| `Credentials for this connection are not available in this R session` | `conn` was loaded from a saved file | Call `connect()` again |
| `Timeout was reached ... Operation too slow` | No data from the server for 5 minutes | Check the network/VPN. If EMS is just slow to start sending a large result, raise the limit with `connect(..., stall_timeout = 900)` |
| `Async query stopped early (...)` | Download interrupted part-way | Re-run. With the wrapper in step 2e, no partial file is written |
| `Auth token expired. Reconnecting to EMS...` | Informational | Nothing to do; the token is refreshed automatically |
| `cannot remove prior installation of package ...` | The package is loaded in the current session | Restart R and run the install again |
| Error from `range()` or `select()` in your own code | Rems masks `base::range` and `dplyr::select` | Use `base::range()` / `dplyr::select()` explicitly |

### 5. Rules for every script

- Never turn off TLS verification (`ssl_verifypeer = 0`).
- Don't keep passwords in scripts. Read them at runtime and `rm()` them after `connect()`; prefer `keyring`.
- Don't save connection objects to disk.
- Always use the install helper; don't install Rems from the tarball directly.
- Turn the "stopped early" warning into an error before writing output files.
- Leave `max_concurrent` on `run()` / `async_run()` at the default (1) until it has been tested against our EMS server. `run_multiflts()` can stay at its default of 4.
