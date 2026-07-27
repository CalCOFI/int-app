# Usage analytics

Two channels, **both sent by the browser**, so no Shiny reactive ever waits on a
network request:

| channel | what it is for | where it goes |
|---|---|---|
| **GA4** (`gtag`) | aggregate, bounded-cardinality behavior: sessions, tabs, downloads | property `G-VV117EV9ZT` |
| **Sheet log** | the per-query detail GA4 buckets into `(other)`: taxa, filter parameters, row counts, durations, error text | a Google Sheet, via an Apps Script `/exec` endpoint |

The code lives in **`calcofi4r`** (`R/analytics.R`), not in this app, so every
CalCOFI Shiny app logs the same shape and the Sheet, the Apps Script and the
client payload cannot drift.

- `app/ui.R` installs the snippet once: `calcofi4r::cc_ga_head("db-viz-hex", app_version = APP_VERSION)`
- `app/server.R` sends server-side facts with `calcofi4r::cc_track()` /
  `cc_track_query()` — a websocket message on the session's already-open
  connection, **no HTTP request**
- `app/global.R` sets `CALCOFI_LOG_URL`. Unset it and the Sheet leg is a silent
  no-op (GA4 still receives events), which is what you want locally

This replaced `app/logging.R`, whose `log_query()` performed a synchronous
`httr2::req_perform()` POST to Apps Script on the reactive thread — a visible
stall on every filter submit and every download.

## Events

| event | fired when | notable params |
|---|---|---|
| `session_start` | a session connects | `h3t`, `release` |
| `select_tab` | tab change (incl. About, Download) | `tab` |
| `select_theme` | dark/light toggle | `theme` |
| `open_filters` / `open_layers` / `open_transect` | the modals open | — |
| `start_tour` | guided tour starts | — |
| `select_layers` | Map Layers → Apply | `layers`, `n_layers` |
| `filter_submit` | Select Filters → Submit | taxa, env_var, quarters, dates, depths, `spatial`, zones |
| `map_query_sp` / `map_query_env` | the two DB queries behind a submit | `ms`, `status`, filter params |
| `filter_no_results` | a submit that returned zero observations | the filters that came up empty |
| `depth_profile_transect` | a transect is submitted | `buffer_km`, `transect_km`, `n_rows`, `n_env`, `ms` |
| `download_integrated_bundle` | the reproducible bio↔env bundle is built | `ms`, `status` (`ok`/`timeout`/`error`) |
| `download_bundle` | one row per Download click | `products`, `n_files`, `ms`, `status` |

`n_rows`, `ms`, `status` and `error` are **reserved** names: they get their own
Sheet columns (numeric, so they stay chartable) instead of going into the
`params` JSON blob.

## Sheet setup

The Sheet's first row must be exactly `calcofi4r::cc_log_header()`:

```
timestamp | ip | session | event | params | n_rows | ms | status | error |
app_version | app | client_id | session_id | page | referrer | user_agent
```

The first ten columns are the original query-log columns, so rows written before
the non-blocking rewrite keep their meaning; the last six were appended for the
browser-driven channel (`app` lets several apps share one Sheet).

1. **Add the six new headers** to the existing log Sheet (K1:P1 — `app`,
   `client_id`, `session_id`, `page`, `referrer`, `user_agent`).
2. Extensions → Apps Script, replace `Code.gs` with [`Code.gs`](Code.gs).
3. Deploy → **Manage deployments** → edit the existing web app → **New version**
   → Deploy. Editing the existing deployment keeps the `/exec` URL, so
   `CALCOFI_LOG_URL` in `app/global.R` needs no change. (A *new* deployment mints
   a new URL — then update `global.R`.)
4. Verify: open the `/exec` URL in a browser. `doGet()` answers
   `{"ok":true,"endpoint":"calcofi-usage-log","rows":N}`.

`Code.gs` is **generated** — regenerate it, don't hand-edit it:

```r
writeLines(calcofi4r::cc_apps_script(), "analytics/Code.gs")
```

## Notes

- The beacon body is `text/plain;charset=UTF-8` on purpose: that keeps it a CORS
  "simple request". An `application/json` body triggers a preflight `OPTIONS`,
  which an Apps Script `/exec` endpoint does not answer — every event would be
  silently dropped.
- Events are batched (10 events / 15 s / page-hide) and written with a single
  `setValues()` per batch, which is what keeps the Apps Script execution quota
  flat regardless of interaction rate.
- `ip` and `session` cannot be read from JavaScript; the server pushes them once
  per session via `cc_track_session()` and the client stamps them on every row.
