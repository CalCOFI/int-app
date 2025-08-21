# species data retrieval function
sp_retrieve <- function(sp_name, qtr, date_range) {
  sp_data <- tbl(con, "species") |>
    mutate(
      name = paste0(common_name, " (", scientific_name, ")")
    ) |>
    filter(name %in% sp_name) |>
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
      quarter %in% qtr) |>
    mutate(
      std_tally = shf*tally
    )

  return(sp_data)
}

# oceanographic data retrieval
ocean_retrieve <- function(ocean_var, qtr, date_range) {

  # filter for selected quarter and dates
  ocean_data <- ocean_subset[Quarter %in% qtr & Date <= date_range[2] & Date >= date_range[1]]

  # rename selected variable
  setnames(ocean_data, ocean_var, "Qty")

  # drop non-selected variables
  ocean_data <- ocean_data[!is.na(Qty),-unname(unlist(setdiff(ocean_var_choices, ocean_var))), with = FALSE]

  return(ocean_data)
}

# hexagon mapping functions
map_sp_hex <- function(sp_data, res_range) {
  # precompute and store joins in a temporary table
  sp_data_temp <- sp_data |>
    compute()

  # create and combine tables for each resolution
  combined_res_tbl <- map(res_range, ~{
    hex_fld <- glue("hex_h3_res{.x}")

    sp_data_temp |>
      select(hex_int = all_of(hex_fld), std_tally) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_sp <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      sp.value = mean(std_tally, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(!is.na(hex_int)) |>
    mutate(hex_id = sql("HEX(hex_int)")) |>
    mutate(
      hex_wkt = sql("h3_cell_to_boundary_wkt(hex_id)"),
      tooltip = round(sp.value, 2)
    ) |>
    select(resolution, hexid = hex_id, sp.value, hex_wkt, tooltip) |>
    collect() |>
    st_as_sf(wkt = "hex_wkt", crs = 4326) |>
    group_split(resolution)

  return(hex_sp)
}

map_ocean_hex <- function(ocean_data, res_range, ocean_stat) {
  # define the columns to melt
  h3_cols <- paste0("hex_h3_res", res_range)

  # melt the data.table from wide to long format
  ocean_long_dt <- melt(ocean_data,
                        id.vars = "Qty",
                        measure.vars = h3_cols,
                        variable.name = "resolution",
                        value.name = "hex_id")

  # tidy up the resolution column
  ocean_hex <- ocean_long_dt[, resolution := as.integer(gsub("hex_h3_res", "", resolution))
                # perform aggregation
                ][!is.na(hex_id),
                  .(ocean.value = get(ocean_stat)(Qty, na.rm = TRUE)),
                  by = .(resolution, hex_id)
                  # add geometries
                  ][hex_geo_dt, on = .(hex_id), geometry := i.geometry
                    # add tooltip text
                    ][, tooltip := round(ocean.value, 2)]

  # split into list
  ocean_hex_list <- group_split(sf_result, resolution)

  return(ocean_hex_list)
}

create_sp_map <- function(sp_hex_list, sp_scale_list) {
  # base map
  sp_map <- maplibre(bounds = sp_hex_list[[1]])

  # add each resolution layer individually
  for (i in 1:length(res_range)) {
    sp_map <- sp_map |>
      add_fill_layer(id = paste0("sp", res_range[i]),
                     source = sp_hex_list[[i]],
                     fill_color = sp_scale_list[[i]]$expression,
                     fill_outline_color = "white",
                     fill_opacity = 0.6,
                     min_zoom = hex_res_breaks[i],
                     max_zoom = hex_res_breaks[i+1],
                     tooltip = "tooltip")
  }

  sp_map <- sp_map |>
    add_legend("Avg. Abundance (m^-2)",
               values = round(sp_scale_list[[1]]$breaks),
               colors = sp_scale_list[[1]]$colors,
               type = "continuous",
               position = "bottom-left") |>
    add_scale_control(position = "top-left", unit = "metric")

  return(sp_map)
}

create_ocean_map <- function(ocean_hex_list, ocean_scale_list) {
  # create base map
  ocean_map <- maplibre(bounds = ocean_hex_list[[1]])

  # add each resolution layer individually
  for (i in 1:length(res_range)) {
    ocean_map <- ocean_map |>
      add_fill_layer(id = paste0("ocean", res_range[i]),
                     source = ocean_hex_list[[i]],
                     fill_color = ocean_scale_list[[i]]$expression,
                     fill_outline_color = "white",
                     fill_opacity = 0.6,
                     min_zoom = hex_res_breaks[i],
                     max_zoom = hex_res_breaks[i+1],
                     tooltip = "ocean.value")
  }

  return(ocean_map)
}

# helper function for species time-series conversion
sp_time_mutate_expr <- function(ts_res) {
  switch(ts_res,
         "year"    = expr(sql("datetrunc('year', time_start)")),
         "quarter" = expr(sql("make_date(2000, month(datetrunc('quarter',time_start)), day(datetrunc('quarter',time_start)))")),
         "month"   = expr(sql("extract('month' FROM time_start)")),
         "day"     = expr(sql("extract('doy' FROM time_start)")),
         "year_quarter" = expr(sql("datetrunc('quarter',time_start)")),
         "year_month" = expr(sql("datetrunc('month',time_start)")),
         "year_day" = expr(sql("datetrunc('day',time_start)"))
  )
}

# species time-series conversion function
make_sp_ts <- function(sp_data, ts_res) {

  sp_ts_data <- sp_data |>
    mutate(
      time = !!sp_time_mutate_expr(ts_res)
    ) |>
    group_by(time, name) |>
    summarize(
      avg = mean(std_tally, na.rm = TRUE),
      std = sd(std_tally, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    mutate(
      upr = avg + std/n,
      lwr = avg - std/n
    )

  return(sp_ts_data)
}

# helper function for oceanographic time-series conversion
ocean_time_mutate_expr <- function(ts_res) {
  switch(ts_res,
         "year"    = expr(floor_date(Date, "year")),
         "quarter" = expr(`year<-`(floor_date(Date, "quarter"), 2000)),
         "month"   = expr(month(Date)),
         "day"     = expr(yday(Date)),
         "year_quarter" = expr(floor_date(Date, "quarter")),
         "year_month" = expr(floor_date(Date, "month")),
         "year_day" = expr(floor_date(Date, "day"))
  )
}

# oceanographic time-series conversion function
make_ocean_ts <- function(ocean_data, ts_res) {
  ocean_ts_data <- ocean_data[, time := eval(ocean_time_mutate_expr(ts_res))
                              # calculate average and standard deviation
                              ][, .(avg = mean(Qty, na.rm = TRUE),
                                    std = sd(Qty, na.rm = TRUE)/.N), by = .(time)
                                # create upper and lower bounds
                                ][, `:=`(upr = avg + std, lwr = avg - std)
                                  ][, .(time, avg, lwr, upr)]

  return(ocean_ts_data)
}


# function to prepare species data for scatterplot
sp_splot_prep <- function(sp_data, hex_res, ts_res) {
  hex_fld <- glue("hex_h3_res{hex_res}")

  sp_splot_data <- sp_data |>
    rename(
      hex_int = all_of(hex_fld)) |>
    mutate(
      time = !!sp_time_mutate_expr(ts_res)) |>
    group_by(
      time, hex_int, name) |>
    summarize(
      value = mean(std_tally),
      std = sd(std_tally),
      .groups = "drop") |>
    mutate(
      hex_id = sql("HEX(hex_int)")) |>
    mutate(
      hex_area = sql("h3_cell_area(hex_id, 'km^2')")) |>
    mutate(
      value = value/hex_area,
      std = std/hex_area) |>
    mutate(
      lwr = value - std,
      upr = value + std
    ) |>
    select(
      time, name, value, lwr, upr, hex_id)

  return(sp_splot_data)
}

# function to prepare ocean data for scatterplot
ocean_splot_prep <- function(ocean_data, hex_res, ts_res, ocean_stat) {
  hex_fld <- glue("hex_h3_res{hex_res}")

  ocean_splot_data <- ocean_data |>
    rename(
      hex_id = all_of(hex_fld)) |>
    mutate(
      time = !!ocean_time_mutate_expr(ts_res),
      hex_id = toupper(hex_id)) |>
    group_by(
      time, hex_id) |>
    summarize(
      value = get(ocean_stat)(Qty, na.rm = TRUE),
      std = sd(Qty, na.rm = TRUE),
      .groups = "drop") |>
    mutate(
      lwr = value - std,
      upr = value + std
    )

  return(ocean_splot_data)
}

# modal dialog for data selection
dataModal <- function(failed = FALSE) {
  modalDialog(
    title = "Data Selection",
    navset_tab(
      nav_panel(
        "Species",
        br(),

        selectizeInput(
          "sel_name",
          "Species Name",
          choices = NULL,
          multiple = TRUE
        ),
      ),
      nav_panel(
        "Oceanographic",
        br(),
        selectInput(
          "sel_ocean_var",
          "Oceanographic Variable",
          ocean_var_choices,
          selected = "Temperature"
        ),
        numericInput(
          "sel_depth",
          "Depth (m)",
          value = 512,
          min = 0,
          max = 512
        )
      ),
      nav_panel(
        "General",
        br(),
        selectInput(
          "sel_qtr",
          "Season",
          c(Winter = 1,
            Spring = 2,
            Summer = 3,
            Fall   = 4),
          selected = 1:4,
          multiple = T),

        dateRangeInput(
          "sel_date_range",
          "Date Range",
          startview = "year",
          start = min_max_date[1],
          end = min_max_date[2],
          min = min_max_date[1],
          max = min_max_date[2]),
      ),
    ),

    if (failed)
      div(tags$i("Sorry, no data found for selected species.", style = "color: red;")),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit", "Submit")
    ),

    size = "m",
    fade = FALSE
  )
}


quarter_label <- function(date) {
  q <- as.integer((as.POSIXlt(date)$mon %/% 3) + 1)
  paste0("Q", q)
}
