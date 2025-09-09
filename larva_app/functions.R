# modal dialog for data selection
dataModal <- function() {
  modalDialog(
    title = "Data Selection",
    navset_tab(
      nav_panel(
        "Species",
        br(),

        selectizeInput(
          "sel_name",
          "Species",
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
          "sel_min_depth",
          "Min Depth (m)",
          value = 0,
          min = 0,
          max = 512
        ),
        numericInput(
          "sel_max_depth",
          "Max Depth (m)",
          value = 512,
          min = 0,
          max = 512
        )
      ),
      nav_panel(
        "Temporal",
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
    nav_panel(
      "Spatial",
      br()
    ),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit", "Submit")
    ),

    size = "m",
    fade = FALSE
  )
}

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
      std_tally = shf*tally/propsorted
    )

  return(sp_data)
}

# oceanographic data retrieval
ocean_retrieve <- function(ocean_var, qtr, date_range, min_depth, max_depth) {

  if (!is.data.table(ocean_subset)) {
    ocean_subset <- as.data.table(ocean_subset)
  }

  # filter for selected quarter and dates
  ocean_data <- ocean_subset[Quarter %in% qtr & datetime <= date_range[2] & datetime >= date_range[1] & Depthm >= min_depth & Depthm <= max_depth]

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
                  ][hex_geo_dt, on = .(hex_id), geometry := geometry
                    # add tooltip text
                    ][, tooltip := round(ocean.value, 2)]

  ocean_hex <- st_sf(ocean_hex)

  # split into list
  ocean_hex_list <- group_split(ocean_hex, resolution)

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
    add_legend("Avg. Abundance (count/10m^2)",
               values = round(sp_scale_list[[1]]$breaks),
               colors = sp_scale_list[[1]]$colors,
               type = "continuous",
               position = "bottom-left") |>
    add_scale_control(position = "top-left", unit = "metric")

  return(sp_map)
}

create_ocean_map <- function(ocean_hex_list, ocean_scale_list, ocean_stat_label, ocean_var_label) {
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

  # add legend
  ocean_map <- ocean_map |>
    add_legend(paste(ocean_stat_label, ocean_var_label),
               values = signif(ocean_scale_list[[1]]$breaks, 2),
               colors = ocean_scale_list[[1]]$colors,
               type = "continuous",
               position = "bottom-right")

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
         "year"    = expr(floor_date(datetime, "year")),
         "quarter" = expr(`year<-`(floor_date(datetime, "quarter"), 2000)),
         "month"   = expr(month(datetime)),
         "day"     = expr(yday(datetime)),
         "year_quarter" = expr(floor_date(datetime, "quarter")),
         "year_month" = expr(floor_date(datetime, "month")),
         "year_day" = expr(floor_date(datetime, "day"))
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

# time-series plotting function
plot_ts <- function(sp_ts, ocean_ts, ts_res, sel_ocean_var) {
  # Add a 'panel' and consistent 'name' column to each dataset
  sp_ts_mod <- sp_ts |>
    mutate(panel_id = 0) # Assign to the first (top) panel

  ocean_ts_mod <- ocean_ts |>
    mutate(name = names(which(ocean_var_choices == sel_ocean_var)),
           panel_id = 1) # Assign to the second (bottom) panel

  # Combine into a single data frame
  combined_data <- bind_rows(sp_ts_mod, ocean_ts_mod) |>
    arrange(time) |>
    mutate(time_ts = datetime_to_timestamp(time))

  # Get a list of the unique series to create
  series_list <- combined_data |>
    distinct(name, panel_id)

  # Initialize the chart with its layout
  hc <- highchart() |>
    hc_chart(zoomType = "x") |>
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          enabled = FALSE # Disables the default hamburger menu
        ),
        customButton = list(
          text = "Reset Zoom",
          onclick = JS("function() { this.xAxis[0].setExtremes(null, null); }"),
          align = "left",
          y = -3,
          x = 20,
          # --- Add this theme list to style the button ---
          theme = list(
            'stroke-width' = 0,
            stroke = "#335cad",
            r = 5,
            style = list(
              color = "#335cad",
              size = 12
            ),
            states = list(
              hover = list(
                fill = '#ffffff'
              ),
              select = list(
                fill = '#ffffff'
              )
            )
          )
        )
      )
    ) |>
    hc_xAxis(type = "datetime", crosshair = TRUE) |>
    hc_yAxis_multiples(
      list(title = list(text = "Species Abundance"), height = "47%", top = "0%", offset = 0),
      list(title = list(text = names(which(ocean_var_choices == sel_ocean_var))), height = "47%", top = "53%", offset = 0)
    ) |>
    hc_tooltip(
      shared = TRUE,
      formatter = JS("
      function() {
        var header = '<b>' + Highcharts.dateFormat('%b %e, %Y', this.x) + '</b><br/>';
        var pointLines = this.points.map(function(point) {
          return '<span style=\"color:' + point.color + '\">●</span> ' +
                 point.series.name + ': <b>' + point.y.toFixed(2) + '</b>';
        }).join('<br/>');
        return header + pointLines;
      }
    "),
      useHTML = TRUE
    ) |>
    hc_rangeSelector(enabled = TRUE, buttons = list()) |>
    hc_plotOptions(
      series = list(
        marker = list(
          enabled = TRUE,
          radius = 0,
          states = list(hover = list(enabled = TRUE, radius = 5))
        )
      ),
      arearange = list(
        lineWidth = 0,
        fillOpacity = 0.3,
        enableMouseTracking = FALSE,
        showInLegend = FALSE
      )
    ) |>
    hc_legend(enabled = TRUE)

  # Loop through each series to add its line and ribbon
  for (i in 1:nrow(series_list)) {
    series_name <- series_list$name[i]
    panel_index <- series_list$panel_id[i]
    series_data <- combined_data |> filter(name == series_name)

    hc <- hc |>
      hc_add_series(
        data = series_data,
        type = "line",
        hcaes(x = time_ts, y = avg),
        name = series_name,
        id = series_name,
        yAxis = panel_index
      ) |>
      hc_add_series(
        data = series_data,
        type = "arearange",
        hcaes(x = time_ts, low = lwr, high = upr),
        name = series_name,
        linkedTo = series_name,
        yAxis = panel_index
      )
  }

  # Display the final chart
  return(hc)
}

# data prep function for scatterplot
splot_prep <- function(sp_data, ocean_data, ocean_stat, dist_within = 1000) {
  # convert species data to a data.table
  sp_dt <- sp_data |>
    as.data.table()

  # subset columns and convert time to Date format
  sp_dt <- sp_dt[, c("name", "std_tally", "time_start", "longitude", "latitude")][
    , time_start := as.Date(time_start)
  ]
  ocean_dt <- ocean_data[, c("datetime", "Qty", "Depthm", "Lat_Dec", "Lon_Dec")][
    , datetime := as.Date(datetime)
  ]

  # join data by date
  joined_by_time <- sp_dt[ocean_dt,
                          on = .(time_start = datetime),
                          allow.cartesian = TRUE, # Necessary for one-to-many matches
                          nomatch = 0 # Ensures we only keep rows that have a match
  ]

  # rename columns for clarity after the join
  setnames(joined_by_time, c("longitude", "latitude"),
           c("sp_lon", "sp_lat"))
  setnames(joined_by_time, c("Lon_Dec", "Lat_Dec"),
           c("ocean_lon", "ocean_lat"))

  # compute distances
  joined_by_time[, distance_m := distHaversine(
    p1 = cbind(sp_lon, sp_lat),
    p2 = cbind(ocean_lon, ocean_lat)
  )]

  # filter based on distance and aggregate
  splot_data <- joined_by_time[distance_m <= dist_within, .(Qty = get(ocean_stat)(Qty, na.rm = TRUE)),
                               by = .(name, std_tally, time_start, sp_lon, sp_lat)
  ]

  return(splot_data)
}

# Function to detect and split line at the dateline
split_at_dateline <- function(segment) {
  coords <- st_coordinates(segment)[, c("X", "Y")]
  lons <- coords[, "X"]

  # Check for dateline crossing (large longitude jump)
  lon_diff <- diff(lons)
  crosses_dateline <- any(abs(lon_diff) > 180)

  if (!crosses_dateline) return(segment)

  # Normalize longitudes to avoid discontinuity
  # Shift coords to a 0-360 range if crossing +180/-180
  if (any(lons < 0)) {
    coords[, "X"] <- ifelse(lons < 0, lons + 360, lons)
  }

  # Create new linestring
  new_segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # Optional: Split into multiple segments if needed
  # Use st_segmentize to add points across dateline for smoother buffer
  new_segment <- st_segmentize(new_segment, set_units(1000, "m"))

  return(new_segment)
}

# Main buffering function
create_buffer <- function(coords, buffer_dist = 5000) {
  # Create initial segment
  segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # Handle dateline crossing
  segment <- split_at_dateline(segment)

  # Get centroid to determine UTM zone
  centroid <- st_centroid(segment)
  cent_coords <- st_coordinates(centroid)
  lon <- cent_coords[1, "X"]
  lat <- cent_coords[1, "Y"]

  # Adjust longitude for UTM if it was shifted
  lon <- ifelse(lon > 180, lon - 360, lon)

  # Calculate UTM zone
  zone <- floor((lon + 180) / 6) + 1
  hemisphere <- if (lat >= 0) 32600 else 32700
  utm_crs <- hemisphere + zone

  # Transform to UTM, buffer, and transform back
  segment_utm <- st_transform(segment, utm_crs)
  buffer_utm <- st_buffer(segment_utm, dist = buffer_dist, endCapStyle = "FLAT")
  buffer <- st_transform(buffer_utm, 4326)

  # Ensure buffer is valid and handles dateline
  buffer <- st_wrap_dateline(buffer, options = c("WRAPDATELINE=YES"))

  return(list(utm_crs = utm_crs, segment = segment, segment_utm = segment_utm, buffer = buffer, buffer_utm = buffer_utm))
}
