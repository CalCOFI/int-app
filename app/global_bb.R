# libraries ----
if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}
librarian::shelf(
  bslib, bsicons, DBI, data.table, dplyr, duckdb, geosphere, ggplot2, glue, here,
  highcharter, htmlwidgets, leaflet, lubridate, mapgl, plotly, purrr, readr, sf, shiny,
  stringr, tibble, tidyr,
  quiet = T)
options(readr.show_col_types = F)

# paths ----
ocean_subset_csv <- here("data/ocean_subset.csv")
# hex_geo          <- here("data/hex_geo.csv")
hex_geo          <- here("data/hex.geojson")
extra_zip        <- here("data/extra.zip")
zip_files        <- c(ocean_subset_csv, hex_geo)
url_dk           <- "https://file.calcofi.io/calcofi.duckdb"
tmp_dk           <- here("data/tmp.duckdb")

if (!all(file.exists(zip_files)))
  unzip(here("data/extra.zip"), exdir = here(), overwrite = T)

# database ----
con <- dbConnect(duckdb(), dbdir = tmp_dk)
res <- dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
res <- dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
res <- dbExecute(con, glue("ATTACH IF NOT EXISTS '{url_dk}' AS calcofi; USE calcofi"))

# oceano ----
dir_dl     <- "~/Downloads/CalCOFI_Database_194903-202105_csv_16October2023"
bottle_csv <- glue("{dir_dl}/194903-202105_Bottle.csv")
cast_csv   <- glue("{dir_dl}/194903-202105_Cast.csv")

d_bottle <- read_csv(bottle_csv, guess_max = Inf)
names(d_bottle)
d_cast   <- read_csv(cast_csv, guess_max = Inf)

# ocean_subset ----
ocean_subset <- fread(ocean_subset_csv)
# hex_geo_dt   <- read_csv(hex_geo) |>
#   mutate(geom = eval(parse(text = geometry)))
#   st_as_sf(wkt = "geometry", crs = 4326) |>
#   as.data.table()
# hex_geo <- st_as_sf(hex_geo_dt, wkt = "geometry", crs = 4326)

# load hexagons
if (!file.exists(hex_geo)){
  hex_list <- list()

  hex_pfx <- "hex_h3_res"
  hex_resolutions <- dbListFields(con, "site") |>
    str_subset(hex_pfx) |>
    str_remove(hex_pfx) |>
    as.integer()

  for (hex_res in hex_resolutions){ # hex_res = 1
    hex_fld <- glue("{hex_pfx}{hex_res}")

    hex_list[[hex_res]] <- tbl(con, "site") |>
      rename(
        hex_int = .data[[hex_fld]]) |>
      group_by(hex_int) |>
      summarize(
        n_sites = n(),
        .groups = "drop") |>
      mutate(
        hex_id  = sql("HEX(hex_int)"),
        hex_res = !!hex_res) |>
      mutate(
        hex_wkt = sql("h3_cell_to_boundary_wkt(hex_id)")) |>
      select(hex_id, hex_res, n_sites, hex_wkt) |>
      # select(all_of(c("hex_id", "hex_res", "n_sites", "hex_wkt"))) |>
      collect() |>
      st_as_sf(
        wkt = "hex_wkt",
        crs = 4326) |>
      st_set_geometry("geometry")
  }
  # save hex_geo
  sf_hex <- bind_rows(hex_list)
  st_write(sf_hex, hex_geo, delete_dsn = T, quiet = T)
}
sf_hex <- st_read(hex_geo)

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
