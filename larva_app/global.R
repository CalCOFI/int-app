# Install librarian if needed
if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}

# Load libraries
librarian::shelf(
  bslib, bsicons, DBI, data.table, dplyr, duckdb, geosphere, ggplot2, glue, here,
  highcharter, htmlwidgets, leaflet, lubridate, mapgl, plotly, purrr, readr, sf, shiny,
  shinyWidgets, tibble, tidyr,
  quiet = TRUE
)

# Set up DuckDB connection ----
url_dk <- "https://file.calcofi.io/calcofi.duckdb"
tmp_dk <- here("data/tmp.duckdb")
con <- dbConnect(duckdb(), dbdir = tmp_dk)

# Load DuckDB extensions with error handling
dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
dbExecute(con, "INSTALL spatial; LOAD spatial;")
dbExecute(con, glue("ATTACH IF NOT EXISTS '{url_dk}' AS calcofi; USE calcofi"))

# Load data files ----
ocean_subset <- fread(here("data/ocean_subset.csv"))
hex_geo_dt <- read_csv(here("data/hex_geo.csv"), show_col_types = FALSE) |>
  st_as_sf(wkt = "geometry", crs = 4326) |>
  as.data.table()

# Load functions ----
source(here("larva_app/functions.R"))

# Extract species names and date range ----
names <- tbl(con, "species") |>
  mutate(name = paste0(common_name, " (", scientific_name, ")")) |>
  pull(name)

min_max_date <- tbl(con, "tow") |>
  collect() |>
  pull(time_start) |>
  range() |>
  as.Date()

min_max_date[1] <- min(min_max_date[1],as.Date(min(ocean_subset[,datetime])))
min_max_date[2] <- max(min_max_date[2],as.Date(max(ocean_subset[,datetime])))

# Global constants ----
ts_res_choices <- list(
  "Year" = "year",
  "Quarter" = "quarter",
  "Year, Quarter" = "year_quarter"
)

ocean_var_choices <- list(
  "Temperature (ºC)" = "T_degC",
  "Salinity" = "Salnty",
  "Oxygen (µmol/kg)" = "Oxy_µmol/Kg"
)

ocean_stat_choices <- list(
  "Average" = "mean",
  "Max" = "max",
  "Min" = "min",
  "Std. Dev." = "sd"
)

# Mapping variables ----
min_res <- 1
max_res <- 10
res_range <- min_res:max_res
zoom_breaks <- seq(1, 13, length.out = length(res_range) + 1)
zoom_breaks[1] <- 0
zoom_breaks[length(zoom_breaks)] <- 22

# Cleanup on exit ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
  duckdb_shutdown(duckdb())
})
