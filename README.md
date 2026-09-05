# CalCOFI Integrated Application

The app is available online here:

- https://app.calcofi.io/db-viz-hex
  ![calcofi\_int-app\_2025-10-08.png (2434×1644)](./figures/calcofi_int-app_2025-10-08.png)

You may be presented with a tour dialog when opening:

  <img src="./figures/calcofi_int-app_tour-init-msg_2025-10-08.png" alt="tour dialog" width="200">

The CalCOFI Integrated Application is using the 
[integrated database](https://calcofi.io/docs/db.html#integrated-database-ingestion-strategy) 
constructed from the following datasets:

- [Bottle Database – CalCOFI](https://calcofi.org/data/oceanographic-data/bottle-database/)
- [Fish Eggs & Larvae – CalCOFI](https://calcofi.org/data/marine-ecosystem-data/fish-eggs-larvae/)

This app is open-source with code found here:

- [github.com/CalCOFI/db-viz-hex](https://github.com/CalCOFI/db-viz-hex)

The following key technologies enable this app:

- [R](https://www.r-project.org/): programming language for statistical computing
- [Shiny](https://shiny.rstudio.com/): web application framework for R
- [DuckDB](https://duckdb.org/): in-process SQL OLAP database management system
- [H3](https://www.uber.com/blog/h3/): hexagonal hierarchical geospatial indexing system
- [mapgl](https://walker-data.com/mapgl/): R interface to MapLibre GL JS for interactive maps

## URL parameters

| parameter | what it does |
|---|---|
| `?datasets=a,b` | opens with those taxa datasets selected — a comma-separated list of release `dataset_key`s, the same values the **Taxa datasets** checkbox group carries. The opening map and the filter modal both honour it. |
| `?theme=dark` / `?theme=light` | brand v2 contract, honoured by `calcofi4r::cc_theme_init()` |
| `?tour=off` | suppresses the guided tour for this visit, so a screenshot shows the interface |

The URL is the source of truth **at load only**, parsed once on connect; after that the user's own
selection is, and it is written back with `updateQueryString(mode = "replace")` so any view is a
link you can send — carrying every other parameter through untouched, because a screenshot URL that
loses its `?theme=` is a broken screenshot. Keys that name no dataset in this release are dropped,
and a list with none left is ignored rather than applied: in this app an empty dataset selection
already means *all of them*, so a stale link must not read as a deliberate empty filter.

Every dataset page on calcofi.io builds one of these links from a `dataset_url:` template in
`CalCOFI.github.io/_data/products.yml` — the one place a product-to-dataset link is written
(UI plan D-6, Decision 10):

```
https://app.calcofi.io/hex/?datasets=swfsc_ichthyo
```

The rule itself is `parse_datasets_param()` in `app/functions.R`, kept pure so it can be tested
without a running app and a release database:

```bash
Rscript tests/test_url_datasets.R
```

This app was developed by:

- [Ivy Li](https://www.linkedin.com/in/ivy-li-268ab330a/): CalCOFI intern from UCSB (BS in Physics and Statistics & Data Science)
- [Ben Best](https://ecoquants.com/about/#ben-best-phd): CalCOFI contractor, marine data scientist at EcoQuants LLC
- [Erin Satterthwaite](https://calcofi.org/about/staff/erin-satterthwaite/): CalCOFI Program Coordinator 

## Deploy

The app runs on the CalCOFI **`shiny-server`** VM: this repo is cloned to
`/share/github/CalCOFI/db-viz-hex` and served (via `shiny-server` in the `rstudio`
container) from `/srv/shiny-server/db-viz-hex`, a symlink to its `app/` directory —
so `https://app.calcofi.io/db-viz-hex` serves `app/`.

After pushing UI/server R-code changes to `main`, pull and reload just this app:

```bash
ssh calcofi                                              # → ssh.calcofi.io (key-based)
git -C /share/github/CalCOFI/db-viz-hex pull --ff-only
touch /share/github/CalCOFI/db-viz-hex/app/restart.txt   # reloads on next request
```

When the release / DB schema changes, also rebuild the local DuckDB before restarting
(`docker exec -d rstudio bash -lc 'cd /share/github/CalCOFI/db-viz-hex && Rscript prep_db.R'`).
See [`CLAUDE.md`](CLAUDE.md#deploy-to-production-appcalcofiiodb-viz-hex) for details.
