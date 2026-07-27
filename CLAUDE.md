# CLAUDE.md - R Shiny App Development Guidelines

This file provides guidance for developing the CalCOFI Interactive Shiny Application.

## Shiny Architecture Principles

### Reactive Programming
- **Reactive sources**: Use `reactive()` for computed values used in multiple places
- **Reactive endpoints**: Use `observe()` for side effects; `render*()` for outputs
- **Isolation**: Use `isolate()` to prevent unwanted reactivity cascades
- **Event-driven**: Prefer `observeEvent()` and `bindEvent()` over `observe()` for explicit triggers
- **Debouncing**: Use `debounce()` or `throttle()` for expensive operations triggered by user input
- **Async operations**: Use `promises` and `future` packages for long-running database queries

### Modularization
- **Shiny modules**: Break complex UIs into reusable modules with `moduleServer()` and namespaced IDs
- **Function organization**: Extract business logic into `/app/functions.R` (or dedicated R package)
- **Separation of concerns**: Keep UI definitions (`ui.R`) separate from server logic (`server.R`)
- **Helper files**: Use `/app/` subdirectory for modular code loaded via `shiny::runApp()`

### Performance Optimization
- **Database queries**: Use `dbplyr` lazy evaluation; call `collect()` only when needed
- **Data caching**: Store expensive computations in `reactiveVal()` or `reactiveValues()`
- **Conditional panels**: Use `conditionalPanel()` to render UI elements only when needed
- **Bookmarking**: Implement state bookmarking with `enableBookmarking = "url"` for shareable sessions
- **Connection pooling**: Use `pool::dbPool()` instead of direct `DBI::dbConnect()` for database connections

### User Interface Design
- **Bootstrap framework**: Leverage `bslib` for modern, themeable UI components
- **Responsive layout**: Use `page_fillable()`, `layout_columns()`, and `card()` from `bslib`
- **Accessibility**: Provide `aria-label` attributes; ensure keyboard navigation; test with screen readers
- **Feedback**: Use `shiny::showNotification()`, `waiter` package, or `shinybusy` for loading indicators
- **Validation**: Use `req()` to prevent downstream errors; `validate()` for user-facing messages

## Code Style Guidelines

### Formatting
- **Indentation**: 2 spaces (no tabs)
- **Line length**: Aim for ≤80 characters; break long pipes/function calls
- **Alignment**: Vertically align `=` in function arguments when spanning multiple lines
- **Quotes**: Prefer double quotes `"` over single quotes `'`
- **Pipes**: Use base R pipe `|>` over magrittr `%>%` except where parsimonious (e.g., when using `.` placeholder)

### Naming Conventions
- **Variables/functions**: `snake_case`
- **Constants**: `UI_CAPS` or `SCREAMING_SNAKE_CASE`
- **Reactive values**: Use `reactiveValues()` with prefix `rx`, containing descriptive keys (e.g., `rx$df_sp`, `rx$map_env`)
- **Module IDs**: Use descriptive namespaces (e.g., `"species_filter"`, `"map_viewer"`)

#### Domain-Specific Naming
- **Prefixes by data type**:
  - `df_` - data frames or tibbles (e.g., `df_sp`, `df_env`)
  - `sf_` - spatial feature objects (e.g., `sf_hex`, `sf_sites`)
  - `lbl_` - labels or descriptive text (e.g., `lbl_env_var`)
  - `map_` - maplibre/leaflet map objects (e.g., `map_sp`, `map_env`)
  - `plot_` - plot objects (e.g., `plot_ts`, `plot_depth_profile`)

- **Prefixes by function type**:
  - `get_` - data retrieval from database (e.g., `get_sp()`, `get_env()`)
  - `map_` - create maps or spatial aggregations (e.g., `map_sp_hex()`, `map_env()`)
  - `plot_` - create plots (e.g., `plot_ts()`, `plot_scatter()`)
  - `build_` - construct UI elements or summaries (e.g., `build_filter_summary()`)

- **Suffixes by file type**:
  - `_csv` - CSV files (e.g., `species_csv`)
  - `_geo` - GeoJSON files (e.g., `hex_geo`)
  - `_db` - database files (e.g., `calcofi_db`)

- **Domain abbreviations**:
  - `sp` - species (replaces "species_")
  - `env` - environment/oceanographic (replaces "ocean_")
  - `ts` - time series
  - `hex` - hexagonal H3 binning

**Examples**:
```r
# Data objects
df_sp <- get_sp(sp_name = "Anchovy (Engraulis mordax)", qtr = 1:4, date_range)
df_env <- get_env(env_var = "t_deg_c", qtr = 1:4, date_range, min_depth = 0, max_depth = 100)
sf_hex <- st_read(hex_geo)

# Reactive values
rx <- reactiveValues(
  df_sp          = NULL,
  df_env         = NULL,
  lbl_env_var    = NULL,
  map_sp         = NULL,
  filter_summary = NULL
)

# Function names
get_sp()           # retrieve species data (formerly sp_retrieve)
get_env()          # retrieve environmental data (formerly ocean_retrieve)
join_sp_hex()      # join species to hexagons (unchanged)
join_env_hex()     # join environment to hexagons (formerly map_ocean_hex)
map_sp()           # create species map (formerly create_sp_map)
map_env()          # create environment map (formerly create_ocean_map)
plot_ts()          # create time series plot (unchanged)
build_ts_sp()      # build species time series data (formerly make_sp_ts)
build_ts_env()     # build environment time series data (formerly make_ocean_ts)
```

### Tidyverse Principles
- **Data manipulation**: Use `dplyr` verbs (`filter()`, `mutate()`, `summarize()`, etc.)
- **Piping**: Chain operations with `|>` for readability
- **Tidy data**: Maintain one observation per row; use `tidyr::pivot_*()` for reshaping
- **Functional programming**: Use `purrr::map*()` family for iteration over loops when appropriate

### Documentation
- **roxygen2**: Document all exported functions in `/app/functions.R` with `#'`
- **Examples**: Include `@examples` section with runnable code
- **Parameters**: Document all parameters with `@param name description`
- **Return values**: Specify with `@return description`
- **Organization**: Arrange functions alphabetically within file for maintainability

## Database Interactions

### dbplyr Best Practices
```r
# lazy evaluation - query built but not executed
sp_query <- tbl(con, "species") |>
  filter(common_name %in% selected_species) |>
  left_join(tbl(con, "larva"), by = "species_id")

# collect only when needed for visualization or local computation
sp_data <- sp_query |>
  collect()
```

### SQL Safety
- **Parameterization**: Use `glue_sql()` or `DBI::sqlInterpolate()` to prevent SQL injection
- **Validation**: Validate user inputs before incorporating into queries
- **Escaping**: Use `DBI::dbQuoteIdentifier()` for column/table names

## File Organization

```
db-viz-hex/
├── app.R                    # entry point (sources ui.R and server.R)
├── ui.R                     # UI definition
├── server.R                 # server logic
├── app/
│   ├── functions.R          # helper functions (roxygen2 documented)
│   ├── modules/             # Shiny modules (optional)
│   ├── utils.R              # utility functions (optional)
│   └── ...
├── data/                    # static data files (if needed)
├── www/                     # static assets (CSS, JS, images)
└── CLAUDE.md                # this file
```

## Testing and Debugging

- **Interactive debugging**: Use `browser()` in server functions; inspect reactive log with `options(shiny.reactlog = TRUE)`
- **Unit tests**: Test helper functions with `testthat` package
- **Integration tests**: Use `shinytest2` for end-to-end app testing
- **Profiling**: Use `profvis` package to identify performance bottlenecks

## Deployment Considerations

- **Environment variables**: Store database credentials in `.Renviron` or environment variables (never hardcode)
- **Dependencies**: Maintain `renv.lock` file for reproducible deployments
- **Resource limits**: Monitor memory usage; implement pagination for large datasets
- **Error handling**: Use `tryCatch()` and `safely()` from `purrr` to gracefully handle failures

### Deploy to production (`app.calcofi.io/db-viz-hex`)

The live app is served by `shiny-server` (inside the `rstudio` Docker container) on
the CalCOFI **`shiny-server`** VM. The repo is cloned on the host at
`/share/github/CalCOFI/db-viz-hex`, and `/srv/shiny-server/db-viz-hex` symlinks to
its `app/` directory — so the URL path (`db-viz-hex`) maps to `app/`. `shiny-server`
starts a fresh R process when an app's `restart.txt` mtime changes.

**UI / server R-code changes** (like `app/ui.R`, `app/server.R`, `app/global.R`) —
no DB or package changes — deploy with just a pull + restart:

```bash
# after pushing to main:
ssh calcofi                                              # → ssh.calcofi.io (key-based)
git -C /share/github/CalCOFI/db-viz-hex pull --ff-only
touch /share/github/CalCOFI/db-viz-hex/app/restart.txt   # reloads only this app
```

**When `calcofi4r` changes** (the app reads the DB *and* logs usage through it —
`cc_ga_head()` / `cc_track()`, see [`analytics/README.md`](analytics/README.md)),
reinstall it in the `rstudio` container *before* restarting the app; a version
below 1.4.1 lacks the analytics functions `app/ui.R` calls and the app will fail
to start:

```bash
docker exec rstudio Rscript -e 'remotes::install_github("calcofi/calcofi4r")'
```

**When the release / DB schema changes**, also rebuild the app's local DuckDB in the
`rstudio` container before restarting (heavy — background it with `docker exec -d`):

```bash
docker exec -d rstudio bash -lc 'cd /share/github/CalCOFI/db-viz-hex && Rscript prep_db.R'
```

Rebuild the `rstudio` image (in [`CalCOFI/server`](https://github.com/CalCOFI/server))
only when system or R **package** dependencies change. Full cross-repo deploy notes
live in `CalCOFI/workflows/CLAUDE.md` (§ Deploy) and `CalCOFI/apps/README.md`.

## Resources

- [Mastering Shiny](https://mastering-shiny.org/) by Hadley Wickham
- [Engineering Production-Grade Shiny Apps](https://engineering-shiny.org/) by Colin Fay et al.
- [Shiny in Production](https://shiny.posit.co/r/articles/improve/production/) - Posit documentation
- [Outstanding User Interfaces with Shiny](https://unleash-shiny.rinterface.com/) by David Granjon
- [bslib documentation](https://rstudio.github.io/bslib/) for modern UI components
- [dbplyr documentation](https://dbplyr.tidyverse.org/) for database interactions

## Example: Reactive Flow Pattern

```r
# server.R pattern
server <- function(input, output, session) {

  # reactive source: user-selected filters
  r_filters <- reactive({
    list(
      species   = input$sel_name,
      quarters  = input$sel_qtr,
      date_range = input$sel_date_range
    )
  }) |> bindEvent(input$submit)

  # reactive conductor: filtered data query
  r_species_data <- reactive({
    req(r_filters()$species)

    sp_retrieve(
      sp_name    = r_filters()$species,
      qtr        = r_filters()$quarters,
      date_range = r_filters()$date_range
    )
  })

  # reactive endpoint: render output
  output$species_map <- renderMaplibre({
    req(r_species_data())

    sp_hex <- r_species_data() |>
      map_sp_hex(res_range = 3:5) |>
      collect()

    create_sp_map(sp_hex, sp_scale_list)
  })
}
```

## Notes for Future Development

- Consider extracting functions from `app/functions.R` into a dedicated R package (`CalCOFIapp` or similar)
- Implement module-based architecture for map viewer, time series viewer, and data filters
- Add automated testing suite with `testthat` and `shinytest2`
- Document reactive dependencies with reactive graph visualization (`reactlog`)
