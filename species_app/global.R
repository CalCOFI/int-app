if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}

# libraries
librarian::shelf(
  bslib, DBI, dplyr, DT, duckdb, dygraphs, glue, h3jsr, here, leaflet.extras,
  lubridate, mapview, sf, shiny, tibble, tidyr,
  quiet = T)

# map options
mapviewOptions(
  basemaps       = "Esri.OceanBasemap",
  vector.palette = \(n) hcl.colors(n, palette = "Spectral"))

# set up db connection
url_dk <- "https://file.calcofi.io/calcofi.duckdb"
tmp_dk <- here("data/tmp.duckdb")

con <- dbConnect(duckdb(), dbdir = tmp_dk)
res <- dbExecute(con, glue("INSTALL httpfs; LOAD httpfs; ATTACH IF NOT EXISTS '{url_dk}' AS calcofi; USE calcofi"))

# extract species names
names <- tbl(con, "species") |>
  select(common_name, scientific_name) |>
  collect()

# extract date range
min_max_date <- tbl(con, "tow") |>
  collect() |>
  pull(time_start) |>
  range() |>
  as.Date()

# species data retrieval function
sp_retrieve <- function(name_type, sp_name, qtr, date_range) {
  sp_data <- tbl(con, "species") |>
    filter(!!sym(name_type) == !!sp_name) |>
    left_join(
      tbl(con, "larva"),
      by = "sp_id") |>
    left_join(
      tbl(con, "net"),
      by = "net_uuid") |>
    left_join(
      tbl(con, "tow"),
      by = "tow_uuid") |>
    left_join(
      tbl(con, "site"),
      by = "site_uuid") |>
    mutate(
      quarter = quarter(time_start)
    ) |>
    filter(
      !is.na(tally),
      between(time_start, !!date_range[1], !!date_range[2]),
      quarter %in% qtr)

  return(sp_data)
}

# hexagon mapping function
map_hex <- function(sp_data, hex_res) {
  hex_fld <- glue("hex_h3_res{hex_res}")
  hex_sp <- sp_data |>
    rename(
      hex_int = all_of(hex_fld)) |>
    group_by(hex_int) |>
    summarize(
      value = sum(tally),
      .groups = "drop") |>
    mutate(
      hex_id = sql("HEX(hex_int)")) |>
    select(hexid = hex_id, value) |>
    collect() |>
    mutate(
      geom = cell_to_polygon(hexid)) |>
    st_as_sf()
  return(hex_sp)
}

# helper function for time-series conversion
get_time_mutate_expr <- function(ts_res) {
  switch(ts_res,
         "year"    = expr(year(time_start)),
         "quarter" = expr(quarter(time_start)),
         "month"   = expr(month(time_start)),
         "day"     = expr(day(time_start))
  )
}

# time-series conversion function
make_ts <- function(sp_data, ts_res) {
  ts_data <- sp_data |>
    mutate(
      time = !!get_time_mutate_expr(ts_res)
    ) |>
    group_by(time) |>
    summarize(
      avg = mean(tally, na.rm = TRUE),
      std = sd(tally, na.rm = TRUE),
      n = n()
    ) |>
    mutate(
      upr = avg + std,
      lwr = avg - std
    )

  return(ts_data)
}

onStop(function() {
  dbDisconnect(con, shutdown = T); duckdb_shutdown(duckdb()); rm(con)
})
