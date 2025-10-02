server <- function(input, output, session) {

  # Reactive values ----
  sp_data_shared <- reactiveVal(NULL)
  ocean_data_shared <- reactiveVal(NULL)
  ocean_var_label_shared <- reactiveVal(NULL)
  sp_map_shared <- reactiveVal(NULL)
  splot_data_shared <- reactiveVal(NULL)
  filter_summary_shared <- reactiveVal(NULL)
  depth_profile_plot <- reactiveVal(NULL)

  # Map content ----
  output$map_content <- renderUI({
    if (is.null(sp_data_shared())) {
      placeholder_message(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      div(
        style = "width: 100%; height: 100%; position: relative;",
        maplibreCompareOutput("map", width = "100%", height = "100%")
      )
    }
  })

  # Time series content ----
  output$ts_content <- renderUI({
    if (is.null(sp_data_shared())) {
      placeholder_message(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      highchartOutput("ts_plot", height = "100%")
    }
  })

  # Scatterplot content ----
  output$splot_content <- renderUI({
    if (is.null(sp_data_shared())) {
      placeholder_message(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      plotlyOutput("splot", height = "100%")
    }
  })

  # Depth profile content ----
  output$dprof_content <- renderUI({
    if (is.null(sp_data_shared())) {
      placeholder_message(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else if (is.null(depth_profile_plot())) {
      placeholder_message(
        "No Depth Profile Generated",
        "Click 'Draw Transect' in the sidebar to create a depth profile."
      )
    } else {
      plotlyOutput("dprof_plot", height = "100%")
    }
  })

  # Data selection modal ----
  observeEvent(input$sel_data, {
    showModal(dataModal())
    updateSelectizeInput(session, 'sel_name', choices = names, server = TRUE)

    output$spatial_filter_map <- renderMaplibre({
      maplibre(
        style = carto_style("positron"),
        center = c(-120, 35),
        zoom = 5
      ) |>
        add_draw_control(
          position = "top-right",
          displayControlsDefault = FALSE,
          controls = list(polygon = TRUE, trash = TRUE)
        )
    })
  })

  # Submit data selection ----
  observeEvent(input$submit, {
    # Collect input selections
    sel_name <- input$sel_name
    sel_ocean_var <- input$sel_ocean_var
    sel_qtr <- input$sel_qtr
    sel_date_range <- input$sel_date_range
    sel_depth_range <- input$sel_depth_range

    # Get spatial filter
    drawn_polygon <- get_drawn_features(maplibre_proxy("spatial_filter_map"))

    # Retrieve data
    sp_data <- sp_retrieve(sel_name, sel_qtr, sel_date_range)
    ocean_data <- ocean_retrieve(sel_ocean_var, sel_qtr, sel_date_range,
                                 sel_depth_range[1], sel_depth_range[2])

    # Apply spatial filter
    if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
      polygon_wkt <- st_as_text(drawn_polygon$geometry[[1]])

      sp_data <- sp_data |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", polygon_wkt, "'))"
        )))

      ocean_sf <- st_as_sf(ocean_data, coords = c("Lon_Dec", "Lat_Dec"), crs = 4326)
      ocean_data <- ocean_data[as.vector(st_intersects(ocean_sf, drawn_polygon, sparse = FALSE)), ]
    }

    # Validate data
    if (sp_data |> summarize(n = n()) |> pull(n) == 0) {
      showNotification("No observations found for selected species.", type = "warning")
      showModal(dataModal())
      return(NULL)
    }

    # Store shared data
    sp_data_shared(sp_data)
    ocean_data_shared(ocean_data)
    ocean_var_label_shared(names(which(ocean_var_choices == sel_ocean_var)))

    # Build filter summary
    filter_summary_shared(
      build_filter_summary(sel_name, sel_ocean_var, sel_qtr, sel_date_range,
                           sel_depth_range, drawn_polygon)
    )

    # Generate map
    sp_hex_list <- map_sp_hex(sp_data, res_range)
    sp_scale_list <- lapply(sp_hex_list, interpolate_palette,
                            column = "sp.value",
                            palette = \(n) hcl.colors(n, palette = "Viridis"))
    sp_map <- create_sp_map(sp_hex_list, sp_scale_list)
    sp_map_shared(sp_map)

    output$map <- renderMaplibreCompare({
      ocean_stat_label <- names(which(ocean_stat_choices == input$sel_ocean_stat))
      ocean_hex_list <- map_ocean_hex(ocean_data, res_range, input$sel_ocean_stat)
      ocean_scale_list <- lapply(ocean_hex_list, interpolate_palette,
                                 column = "ocean.value",
                                 palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
      ocean_map <- create_ocean_map(ocean_hex_list, ocean_scale_list,
                                    ocean_stat_label, ocean_var_label_shared())
      compare(sp_map, ocean_map, elementId = "map")
    })

    # Generate time series
    output$ts_plot <- renderHighchart({
      sp_ts <- make_sp_ts(sp_data, input$sel_ts_res) |> arrange(time)
      ocean_ts <- make_ocean_ts(ocean_data, input$sel_ts_res)
      plot_ts(sp_ts, ocean_ts, input$sel_ts_res, sel_ocean_var)
    })

    # Generate scatterplot
    splot_data <- splot_prep(sp_data, ocean_data, "mean")
    splot_data_shared(splot_data)

    output$splot <- renderPlotly({
      plot_ly(
        data = splot_data,
        x = ~Qty,
        y = ~std_tally,
        color = ~name,
        type = "scattergl",
        mode = "markers",
        marker = list(size = 10, opacity = 0.8),
        customdata = ~1:nrow(splot_data),
        source = "scatterPlotSource",
        hoverinfo = "text",
        text = ~paste0(
          "<b>Date:</b> ", time_start,
          "<br><b>Species:</b> ", name,
          "<br><b>", ocean_var_label_shared(), ":</b> ", round(Qty, 2),
          "<br><b>Abundance:</b> ", round(std_tally, 2)
        )
      ) |>
        layout(
          xaxis = list(title = ocean_var_label_shared()),
          yaxis = list(title = "Species Abundance"),
          legend = list(title = "Species"),
          dragmode = "select"
        ) |>
        config(
          displaylogo = FALSE,
          scrollZoom = TRUE,
          modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian")
        )
    })

    # Reset depth profile
    depth_profile_plot(NULL)

    removeModal()
  })

  # Scatterplot interactions ----
  observeEvent(event_data("plotly_click", source = "scatterPlotSource"), {
    click_data <- event_data("plotly_click", source = "scatterPlotSource")
    req(click_data, splot_data_shared())

    clicked_point <- splot_data_shared()[click_data$customdata]

    showModal(modalDialog(
      title = "Location of Selected Point",
      leafletOutput("modalMap"),
      footer = modalButton("Close"),
      size = "l"
    ))

    output$modalMap <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.OceanBasemap) |>
        setView(lng = clicked_point$sp_lon, lat = clicked_point$sp_lat, zoom = 14) |>
        addMarkers(
          lng = clicked_point$sp_lon,
          lat = clicked_point$sp_lat,
          popup = paste0(
            "<b>Date:</b> ", clicked_point$time_start,
            "<br><b>Species:</b> ", clicked_point$name,
            "<br><b>", ocean_var_label_shared(), ":</b> ", round(clicked_point$Qty, 2),
            "<br><b>Abundance:</b> ", round(clicked_point$std_tally, 2)
          )
        )
    })
  })

  observeEvent(event_data("plotly_selected", source = "scatterPlotSource"), {
    selected_data <- event_data("plotly_selected", source = "scatterPlotSource")
    req(selected_data, splot_data_shared())

    selected_points <- splot_data_shared()[selected_data$customdata]

    if (nrow(selected_points) == 0) {
      showNotification("No points located within selection.", type = "warning")
      return(NULL)
    }

    showModal(modalDialog(
      title = "Locations of Selected Points",
      leafletOutput("modalMap"),
      footer = modalButton("Close"),
      size = "l"
    ))

    output$modalMap <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.OceanBasemap) |>
        setView(lng = mean(selected_points$sp_lon), lat = mean(selected_points$sp_lat), zoom = 14) |>
        addMarkers(
          lng = selected_points$sp_lon,
          lat = selected_points$sp_lat,
          popup = paste0(
            "<b>Date:</b> ", selected_points$time_start,
            "<br><b>Species:</b> ", selected_points$name,
            "<br><b>", ocean_var_label_shared(), ":</b> ", round(selected_points$Qty, 2),
            "<br><b>Abundance:</b> ", round(selected_points$std_tally, 2)
          )
        )
    })
  })

  # Depth profile modal ----
  observeEvent(input$open_transect_modal, {
    req(sp_map_shared())

    showModal(depthProfileModal())

    output$transect_map <- renderMaplibre({
      sp_map_shared() |>
        add_draw_control(
          position = "top-right",
          displayControlsDefault = FALSE,
          controls = list(line_string = TRUE, trash = TRUE)
        )
    })
  })

  # Generate depth profile ----
  observeEvent(input$submit_transect, {
    req(sp_data_shared(), ocean_data_shared())

    features <- get_drawn_features(maplibre_proxy("transect_map"))

    if (is.null(features) || nrow(features) == 0) {
      showNotification("No line drawn. Please draw a line on the map.", type = "warning")
      return(NULL)
    }

    if (nrow(features) > 1) {
      showNotification("Multiple lines detected; using the last one.", type = "message")
      features <- features[nrow(features), ]
    }

    coords <- st_coordinates(features)
    if (nrow(coords) > 2) {
      coords <- coords[(nrow(coords)-1):nrow(coords), c("X", "Y")]
    }

    buffer_res <- create_buffer(coords, buffer_dist = input$modal_buffer_dist * 1000)

    sp_sf <- st_as_sf(as.data.table(sp_data_shared()), coords = c("longitude", "latitude"), crs = 4326)
    ocean_sf <- st_as_sf(ocean_data_shared(), coords = c("Lon_Dec", "Lat_Dec"), crs = 4326)

    filt_sp_sf <- sp_sf[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)),]
    filt_sp_data <- as.data.table(sp_data_shared())[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)),]
    filt_ocean_data <- ocean_data_shared()[as.vector(st_intersects(ocean_sf, buffer_res$buffer, sparse = FALSE)),]

    segment_sfc <- st_geometry(buffer_res$segment_utm)
    filt_sp_data[, distance := st_line_project(segment_sfc, st_transform(filt_sp_sf, buffer_res$utm_crs) |> st_geometry()) / 1000]
    filt_ocean_data[, distance := st_line_project(segment_sfc, st_transform(st_as_sf(filt_ocean_data, coords = c("Lon_Dec", "Lat_Dec"), crs = 4326), buffer_res$utm_crs) |> st_geometry()) / 1000]

    segment_length <- st_length(buffer_res$segment_utm) / 1000

    profile_plot <- subplot(
      plot_ly(filt_sp_data, x = ~distance, y = ~std_tally, type = "scattergl", mode = "markers", showlegend = FALSE) |>
        layout(yaxis = list(title = "Species Abundance")),
      plot_ly(filt_ocean_data, x = ~distance, y = ~Depthm, type = "scattergl", mode = "markers",
              marker = list(color = ~Qty, colorbar = list(title = ocean_var_label_shared())), showlegend = FALSE) |>
        layout(xaxis = list(title = "Distance (km)", range = c(0, segment_length)),
               yaxis = list(title = "Depth (m)", autorange = "reversed")),
      nrows = 2, shareX = TRUE, heights = c(0.33, 0.67)
    ) |>
      config(displaylogo = FALSE, scrollZoom = TRUE,
             modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian"))

    depth_profile_plot(profile_plot)
    removeModal()
    showNotification("Depth profile generated!", type = "message")
  })

  output$dprof_plot <- renderPlotly({
    req(depth_profile_plot())
    depth_profile_plot()
  })

  # Dynamic UI outputs ----
  output$map_ocean_stat <- renderUI({
    selectInput("sel_ocean_stat",
                "Oceanographic Summary Statistic",
                choices = ocean_stat_choices,
                selected = input$sel_ocean_stat %||% "mean")
  })

  output$ts_res <- renderUI({
    selectInput("sel_ts_res",
                "Temporal Resolution",
                choices = ts_res_choices,
                selected = input$sel_ts_res %||% "year")
  })

  output$filter_summary <- renderUI({
    req(filter_summary_shared())

    div(
      class = "mt-3",
      accordion(
        accordion_panel(
          "Current Data Filters",
          div(class = "small", markdown(paste(filter_summary_shared(), collapse = "  \n")))
        ),
        open = FALSE
      )
    )
  })

  # Download handlers ----
  output$download_sp <- downloadHandler(
    filename = function() paste0("species_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(sp_data_shared())
      write.csv(sp_data_shared() |> collect(), file, row.names = FALSE)
    }
  )

  output$download_ocean <- downloadHandler(
    filename = function() paste0("ocean_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(ocean_data_shared())
      write.csv(ocean_data_shared(), file, row.names = FALSE)
    }
  )
}
