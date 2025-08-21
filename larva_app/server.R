server <- function(input, output, session) {

  observeEvent(input$sel_data, {
    showModal(dataModal())

    updateSelectizeInput(session, 'sel_name', choices = names, server = TRUE)
  })

  # generate plots on submit
  observeEvent(input$submit, {
    # clear previous message
    output$msg <- renderText({
      NULL
    })

    # store inputs
    sel_name <- input$sel_name
    sel_ocean_var <- input$sel_ocean_var
    sel_depth <- input$sel_depth
    sel_qtr <- input$sel_qtr
    sel_date_range <- input$sel_date_range

    # pull species data from db
    sp_data <- sp_retrieve(sel_name, sel_qtr, sel_date_range)

    # pull ocean data
    ocean_data <- ocean_retrieve(sel_ocean_var, sel_qtr, sel_date_range)

    # check if there are observations
    if (sp_data |> collect() |> nrow() == 0) {
      showModal(dataModal(TRUE))
    }
    # if yes, remove data selection window plot
    else {

      # compute hexagons
      sp_hex_list <- map_sp_hex(sp_data, res_range)

      # compute color scales
      sp_scale_list <- lapply(sp_hex_list, interpolate_palette,
                              column = "sp.value",
                              palette = \(n) hcl.colors(n, palette = "Viridis"))

      ocean_label <- names(which(ocean_var_choices == sel_ocean_var))

      # create maps
      sp_map <- create_sp_map(sp_hex_list, sp_scale_list)

      # spatial map output
      output$map <- renderMaplibreCompare({

        ocean_stat_label <- names(which(ocean_stat_choices == input$sel_ocean_stat))

        ocean_hex_list <- map_ocean_hex(ocean_data, res_range, input$sel_ocean_stat)
        ocean_scale_list <- lapply(ocean_hex_list, interpolate_palette,
                                   column = "ocean.value",
                                   palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
        ocean_map <- create_ocean_map(ocean_hex_list, ocean_scale_list)

        # add legend to ocean map
        ocean_map <- ocean_map |>
          add_legend(paste(ocean_stat_label, ocean_label),
                     values = signif(ocean_scale_list[[1]]$breaks, 2),
                     colors = ocean_scale_list[[1]]$colors,
                     type = "continuous",
                     position = "bottom-right") |>
          add_scale_control(position = "top-left", unit = "metric")

        map <- compare(sp_map, ocean_map)
      })

      # time series
      output$sp_ts_plot <- renderDygraph({
        sp_ts <- make_sp_ts(sp_data, input$sel_ts_res) |>
          select(time, avg, lwr, upr, name) |>
          collect()

        sp_ts <- sp_ts |>
          pivot_wider(
            values_from = c(avg, lwr, upr),
            names_from = name
          )

        sp_ts_plot <- sp_ts |>
          dygraph(group = "time-series") |>
          dyOptions(connectSeparatedPoints=TRUE) |>
          dyAxis("y",  label = "Avg. Abundance (m^-2)")

        for (sp in sel_name) {
          sp_ts_plot <- sp_ts_plot |>
            dySeries(
              c(paste0("lwr_", sp), paste0("avg_", sp), paste0("upr_", sp)),
              label = sp
            )
        }

        if (input$sel_ts_res == "quarter") {
          sp_ts_plot <- sp_ts_plot |>
            dyAxis(
              "x",
              valueFormatter = htmlwidgets::JS("
                function(d) {
                  var month = new Date(d).getUTCMonth();
                  var quarter = Math.floor(month / 3) + 1;
                  return 'Q' + quarter;
                }
              "),
              axisLabelFormatter = htmlwidgets::JS("
                function(d) {
                  var month = d.getUTCMonth();
                  var quarter = Math.floor(month / 3) + 1;
                  // Only show label at first month of quarter
                  if (month % 3 === 0) {
                    return 'Q' + quarter;
                  } else {
                    return '';
                  }
                }
              ")
            )
        }

        sp_ts_plot
      })

      output$ocean_ts_plot <- renderDygraph({
        ocean_ts <- make_ocean_ts(ocean_data, input$sel_ts_res) |>
          select(time, avg, lwr, upr)

        ocean_label <- names(which(ocean_var_choices == sel_ocean_var))
        ts_res_label <- names(which(ts_res_choices == input$sel_ts_res))

        ocean_ts_plot <- ocean_ts |>
          dygraph(group = "time-series") |>
          dyOptions(connectSeparatedPoints = TRUE) |>
          dyAxis("y", label = ocean_label) |>
          dyAxis("x", label = ts_res_label) |>
          dySeries(
            c("lwr", "avg", "upr"),
            label = ocean_label
          )

        if (input$sel_ts_res == "quarter") {
          ocean_ts_plot <- ocean_ts_plot |>
            dyAxis(
              "x",
              valueFormatter = htmlwidgets::JS("
                function(d) {
                  var month = new Date(d).getUTCMonth();
                  var quarter = Math.floor(month / 3) + 1;
                  return 'Q' + quarter;
                }
              "),
              axisLabelFormatter = htmlwidgets::JS("
                function(d) {
                  var month = d.getUTCMonth();
                  var quarter = Math.floor(month / 3) + 1;
                  // Only show label at first month of quarter
                  if (month % 3 === 0) {
                    return 'Q' + quarter;
                  } else {
                    return '';
                  }
                }
              ")
            )
        }

        ocean_ts_plot
      })

      # scatterplot
      if (FALSE) {
      base_splot <- reactive({
        sp_splot_data <- sp_splot_prep(sp_data, input$sel_hex_res, input$sel_ts_res) |> collect()
        ocean_splot_data <- ocean_splot_prep(ocean_data, input$sel_hex_res, input$sel_ts_res, input$sel_ocean_stat)

        splot_data <- inner_join(sp_splot_data, ocean_splot_data,
                   by = join_by(time, hex_id),
                   suffix = c(".sp", ".ocean")) |>
          filter(!is.na(name))

        ocean_label <- names(which(ocean_var_choices == sel_ocean_var))
        ts_res_label <- names(which(ts_res_choices == input$sel_ts_res))
        ocean_stat_label <- names(which(ocean_stat_choices == input$sel_ocean_stat))
        time_label <- switch(input$sel_ts_res,
               "year" = "{year(time)}",
               "quarter" = "Q{quarter(time)}",
               "year_quarter" = "{year(time)} Q{quarter(time)}")

        # generate plot
         splot_data |>
          ggplot(aes(
            x = value.ocean,
            y = value.sp,
            color = name,
            text = glue(time_label, "<br>Species: {name}<br>Count: {round(value.sp, 2)}<br>{ocean_label}: {round(value.ocean,2)}")
          )) +
          geom_point() +
          labs(
            x = paste(ocean_stat_label, ocean_label),
            y = "Count (km^-2)",
            color = "Species"
          ) +
          theme_minimal()
      })

      splot_err <- reactive({
        base_splot() +
          geom_errorbar(aes(ymin = lwr.sp, ymax = upr.sp)) +
          geom_errorbar(aes(xmin = lwr.ocean, xmax = upr.ocean))
      })

      output$splot <- renderPlotly({
        if (input$sel_splot_err) {
          splot <- splot_err()
        } else {
          splot <- base_splot()
        }

        ggplotly(splot, tooltip = "text") |>
          config(displayModeBar = FALSE)
      })}

      removeModal()
    }
  })

  output$map_hex_res <- renderUI({
    tagList(
      sliderInput("sel_hex_res",
                  label = tagList("Hexagon Resolution",
                                  popover(
                                    bs_icon("info-circle", size = 14, class = "ms-1"),
                                    HTML("Higher value = smaller hexagons.<br>
                <a href='https://h3geo.org/docs/core-library/restable/#average-area-in-km2'
                target='_blank'>More info</a>")
                                  )),
                  1, 10, input$sel_hex_res, step = 1)
    )
  })

  output$map_ocean_stat <- renderUI({
    selectInput("sel_ocean_stat",
                label = tagList("Oceanographic Summary Statistic",
                                popover(bs_icon("info-circle", size = 14, class = "ms-1"),
                                        HTML("Statistic for oceanographic observations within hex cells."))),
                choices = list("Average" = "mean", "Max" = "max", "Min" = "min", "St. Dev." = "sd"),
                selected = input$sel_ocean_stat
    )
  })

  output$ts_res <- renderUI({
    selectInput("sel_ts_res", "Temporal Resolution", ts_res_options, selected = input$sel_ts_res)
  })

  output$splot_hex_res <- renderUI({
    tagList(
      sliderInput("sel_hex_res",
                  label = tagList("Hexagon Resolution",
                                  popover(
                                    bs_icon("info-circle", size = 14, class = "ms-1"),
                                    HTML("Higher value = smaller hexagons.")
                                  )),
                  1, 10, input$sel_hex_res, step = 1)
    )
  })

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
    selectInput("sel_ts_res", "Temporal Resolution", ts_res_options, selected = input$sel_ts_res)
  })

  observe({
    # Wait for both plots to be rendered
    session$onFlushed(function() {
      session$sendCustomMessage(type = "syncDygraphs", message = list())
    }, once = TRUE)
  })
}
