if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}

# libraries
librarian::shelf(
  bslib, bsicons, DBI, data.table, dplyr, duckdb, geosphere, ggplot2, glue, here,
  highcharter, htmlwidgets, leaflet, lubridate, mapgl, plotly, purrr, readr, sf, shiny,
  tibble, tidyr,
  quiet = T)

# set up db connection
url_dk <- "https://file.calcofi.io/calcofi.duckdb"
tmp_dk <- here("data/tmp.duckdb")

con <- dbConnect(duckdb(), dbdir = tmp_dk)
res <- dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
res <- dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
res <- dbExecute(con, glue("ATTACH IF NOT EXISTS '{url_dk}' AS calcofi; USE calcofi"))

# load ocean data
ocean_subset <- fread(here("data/ocean_subset.csv"))
hex_geo_dt <- read_csv(here("data/hex_geo.csv"), show_col_types = FALSE) |>
  st_as_sf(wkt = "geometry", crs = 4326) |>
  as.data.table()

# load functions
source(here("larva_app/functions.R"))

# extract species names
names <- tbl(con, "species") |>
  mutate(
    name = paste0(common_name, " (", scientific_name, ")"),
  ) |>
  pull(name)

# extract date range
min_max_date <- tbl(con, "tow") |>
  collect() |>
  pull(time_start) |>
  range() |>
  as.Date()

# lists for select inputs
ts_res_choices <- list("Year" = "year", "Quarter" = "quarter", "Year, Quarter" = "year_quarter")
ocean_var_choices <- list("Temperature (ºC)" = "T_degC", "Salinity" = "Salnty")
ocean_stat_choices <- list("Average" = "mean", "Max" = "max", "Min" = "min", "Std. Dev." = "sd")

# mapping variables
min_res <- 1
max_res <- 10
res_range <- min_res:max_res
hex_res_breaks <- seq(1, 13, length.out = length(res_range) + 1)
hex_res_breaks[1] <- 0
hex_res_breaks[length(hex_res_breaks)] <- 22

onStop(function() {
  dbDisconnect(con, shutdown = T); duckdb_shutdown(duckdb())
})
