server <- function(input, output, session) {


  # observe sel_data ----
  observeEvent(input$sel_data, {
    showModal(dataModal())

    updateSelectizeInput(session, 'sel_name', choices = names, server = TRUE)
  })

  # observe submit ----
  # generate plots on submit
  observeEvent(input$submit, {
    # clear previous message
    output$msg <- renderText({
      NULL
    })

    # store inputs
    sel_name       <- input$sel_name
    sel_ocean_var  <- input$sel_ocean_var
    sel_depth      <- input$sel_depth
    sel_qtr        <- input$sel_qtr
    sel_date_range <- input$sel_date_range
    sel_min_depth  <- input$sel_min_depth
    sel_max_depth  <- input$sel_max_depth

    # pull species data from db
    sp_data <- sp_retrieve(sel_name, sel_qtr, sel_date_range)

    # pull ocean data
    ocean_data <- ocean_retrieve(sel_ocean_var, sel_qtr, sel_date_range, sel_min_depth, sel_max_depth)

    # check if there are observations
    if (sp_data |> collect() |> nrow() == 0) {
      showNotification("No observations found for selected species.", type = "warning")
      showModal(dataModal())
      return(NULL)
    }
    # if yes, proceed to retrieving data and generating plots

    # compute hexagons
    sp_hex_list <- map_sp_hex(sp_data, res_range)

    # compute color scales
    sp_scale_list <- lapply(sp_hex_list, interpolate_palette,
                            column = "sp.value",
                            palette = \(n) hcl.colors(n, palette = "Viridis"))

    ocean_var_label <- names(which(ocean_var_choices == sel_ocean_var))

    sp_map <- create_sp_map(sp_hex_list, sp_scale_list)

    # map ----
    # spatial map output
    output$map <- renderMaplibreCompare({
      ocean_stat_label <- names(which(ocean_stat_choices == input$sel_ocean_stat))

      # create ocean map
      ocean_hex_list <- map_ocean_hex(ocean_data, res_range, input$sel_ocean_stat)
      ocean_scale_list <- lapply(ocean_hex_list, interpolate_palette,
                                 column = "ocean.value",
                                 palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
      ocean_map <- create_ocean_map(ocean_hex_list, ocean_scale_list, ocean_stat_label, ocean_var_label)

      # create comparison map
      compare(sp_map, ocean_map, elementId = "map")
    })

    # time series
    output$ts_plot <- renderHighchart({
      # make species time-series
      sp_ts <- make_sp_ts(sp_data, input$sel_ts_res) |> arrange(time) |> collect()

      # make ocean time-series
      ocean_ts <- make_ocean_ts(ocean_data, input$sel_ts_res)

      # plot time-series
      plot_ts(sp_ts, ocean_ts, input$sel_ts_res, sel_ocean_var)
    })

    splot_data <- splot_prep(sp_data, ocean_data, "mean")

    # scatterplot
    output$splot <- renderPlotly({
      # plot scatterplot
      splot <- plot_ly(
        data = splot_data,
        x = ~Qty,
        y = ~std_tally,
        color = ~name,
        type = "scattergl",
        mode = "markers",
        marker = list(size = 10, opacity = 0.8),
        # Store the row index for later use in click and selection events
        customdata = ~1:nrow(splot_data),
        source = "scatterPlotSource",
        hoverinfo = "text",
        text = ~paste0(
          "<b>Date:</b> ", time_start,
          "<br><b>Species:</b> ", name,
          "<br><b>", ocean_var_label, ":</b> ", round(Qty, 2),
          "<br><b>Abundance: </b>", round(std_tally, 2)
        )) %>%
        layout(
          xaxis = list(title = ocean_var_label),
          yaxis = list(title = "Species Abundance"),
          legend = list(title = "Species"),
          dragmode = "select") %>%
        config(
          displaylogo = FALSE, scrollZoom = TRUE,
          modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian"))

      splot
    })

    # Observe single-point click events
    observeEvent(event_data("plotly_click", source = "scatterPlotSource"), {
      click_data <- event_data("plotly_click", source = "scatterPlotSource")

      # Ensure click data is not NULL
      req(click_data)

      # Get the row index of the clicked point from customdata
      row_index <- click_data$customdata

      # Get the data for the single clicked point
      clicked_point <- splot_data[row_index]

      # Show a modal dialog with a Leaflet map and the single marker
      showModal(modalDialog(
        title = "Location of Selected Point",
        leafletOutput("modalMap"),
        footer = modalButton("Close"),
        size = "l"
      ))

      output$modalMap <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.OceanBasemap) %>%
          # Set the view to the location of the clicked point
          setView(
            lng = clicked_point$sp_lon,
            lat = clicked_point$sp_lat,
            zoom = 14
          ) %>%
          addMarkers(
            lng = clicked_point$sp_lon,
            lat = clicked_point$sp_lat,
            popup = paste0(
              "<b>Date:</b> ", clicked_point$time_start,
              "<br><b>Species:</b> ", clicked_point$name,
              "<br><b>", ocean_var_label, ":</b> ", round(clicked_point$Qty, 2),
              "<br><b>Abundance: </b>", round(clicked_point$std_tally, 2)))
      })
    })

    # Observe selection events (box/lasso)
    observeEvent(event_data("plotly_selected", source = "scatterPlotSource"), {
      selected_data <- event_data("plotly_selected", source = "scatterPlotSource")

      # Ensure selected data is not NULL
      req(selected_data)

      # Get the row indices of the selected points from customdata
      row_indices <- selected_data$customdata

      # Get the data for the selected points
      selected_points <- splot_data[row_indices]

      # confirm that there are selected points and warn if not
      if (nrow(selected_points) == 0) {
        showNotification("No points located within selection.", type = "warning")
        return(NULL)
      }

      # Show a modal dialog with a Leaflet map and the markers
      showModal(modalDialog(
        title = "Locations of Selected Points",
        leafletOutput("modalMap"),
        footer = modalButton("Close"),
        size = "l"
      ))

      # Render the leaflet map within the modal
      output$modalMap <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.OceanBasemap) %>%
          # Set the view to the centroid of the selected points
          setView(
            lng = mean(selected_points$sp_lon),
            lat = mean(selected_points$sp_lat),
            zoom = 14
          ) %>%
          addMarkers(
            lng = selected_points$sp_lon,
            lat = selected_points$sp_lat,
            popup = paste0(
              "<b>Date:</b> ", selected_points$time_start,
              "<br><b>Species:</b> ", selected_points$name,
              "<br><b>", ocean_var_label, ":</b> ", round(selected_points$Qty, 2),
              "<br><b>Abundance: </b>", round(selected_points$std_tally, 2)))
      })
    })

    output$dprof_map <- renderMaplibre({
      sp_map |>
        add_draw_control(
          position = "top-right",
          displayControlsDefault = FALSE,
          controls = list(
            line_string = TRUE,
            trash = TRUE
          )
        )
    })

    observeEvent(input$get_features, {
      features <- get_drawn_features(maplibre_proxy("dprof_map"))

      if (is.null(features) || nrow(features) == 0) {
        showNotification("No line drawn yet. Please draw a line on the map.", type = "warning")
        return(NULL)
      }

      # Use the last drawn feature if multiple exist
      if (nrow(features) > 1) {
        showNotification("Multiple lines detected; using the last one.", type = "message")
        features <- features[nrow(features), ]
      }

      # Extract coordinates
      coords <- st_coordinates(features)

      if (nrow(coords) > 2) {
        showNotification("Multiple segments detected; using the last one.", type = "message")
        coords <- coords[(nrow(coords)-1):nrow(coords), c("X", "Y")]
      }

      # create buffer
      buffer_res <- create_buffer(coords, buffer_dist = input$sel_buffer_dist*1000)

      # filter points
      sp_sf <- st_as_sf(as.data.table(sp_data), coords = c("longitude", "latitude"), crs = 4326)
      ocean_sf <- st_as_sf(ocean_data, coords = c("Lon_Dec", "Lat_Dec"), crs = 4326)
      filt_sp_sf <- sp_sf[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)),]
      filt_sp_data <- as.data.table(sp_data)[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)),]
      filt_ocean_sf <- ocean_sf[as.vector(st_intersects(ocean_sf, buffer_res$buffer, sparse = FALSE)),]
      filt_ocean_data <- ocean_data[as.vector(st_intersects(ocean_sf, buffer_res$buffer, sparse = FALSE)),]

      # calculate distances
      segment_sfc <- st_geometry(buffer_res$segment_utm)
      sp_point_sfc <- st_transform(filt_sp_sf, buffer_res$utm_crs) |> st_geometry()
      ocean_point_sfc <- st_transform(filt_ocean_sf, buffer_res$utm_crs) |> st_geometry()
      segment_length <- st_length(buffer_res$segment_utm)/1000
      filt_sp_data[, distance := st_line_project(segment_sfc, sp_point_sfc)/1000]
      filt_ocean_data[, distance := st_line_project(segment_sfc, ocean_point_sfc)/1000]

      # plot result
      showModal(modalDialog(
        title = "Depth Profile",
        plotlyOutput("dprof"),
        footer = modalButton("Close"),
        size = "xl"
      ))

      output$dprof <- renderPlotly({
        sp_plot <- plot_ly(
          filt_sp_data,
          x = ~distance,
          y = ~std_tally,
          type = "scattergl",
          mode = "markers",
          showlegend = FALSE
        ) |>
          layout(
            yaxis = list(title = "Species Abundance"))
        ocean_plot <- plot_ly(
          filt_ocean_data,
          x = ~distance,
          y = ~Depthm,
          type = "scattergl",
          mode = "markers",
          marker = list(color = ~Qty,
                        colorbar = list(title = ocean_var_label)),
          showlegend = FALSE) |>
          layout(
            xaxis = list(title = "Distance (km)", range = c(0, segment_length)),
            yaxis = list(title = "Depth (m)", autorange = "reversed"))
        subplot(sp_plot, ocean_plot, nrows = 2, shareX = TRUE, heights = c(0.33, 0.67)) |>
          config(
            displaylogo = FALSE, scrollZoom = TRUE,
            modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian"))
      })
    })

    removeModal()
  })

  # map_ocean_stat -----
  output$map_ocean_stat <- renderUI({
    selectInput("sel_ocean_stat",
                label = tagList("Oceanographic Summary Statistic",
                                popover(bs_icon("info-circle", size = 14, class = "ms-1"),
                                        HTML("Statistic for oceanographic observations within hex cells."))),
                choices = list("Average" = "mean", "Max" = "max", "Min" = "min", "St. Dev." = "sd"),
                selected = input$sel_ocean_stat
    )
  })

  # ts_res ----
  output$ts_res <- renderUI({
    selectInput("sel_ts_res", "Temporal Resolution", ts_res_choices, selected = input$sel_ts_res)
  })

  # splot_ocean_stat ----
  output$splot_ocean_stat <- renderUI({
    selectInput("sel_ocean_stat",
                label = tagList("Oceanographic Summary Statistic",
                                popover(bs_icon("info-circle", size = 14, class = "ms-1"),
                                        HTML("Statistic for oceanographic observations within hex cells."))),
                choices = list("Average" = "mean", "Max" = "max", "Min" = "min", "St. Dev." = "sd"),
                selected = input$sel_ocean_stat
    )
  })

  output$splot_ts_res <- renderUI({
    selectInput("sel_ts_res", "Temporal Resolution", ts_res_choices, selected = input$sel_ts_res)
  })
}
