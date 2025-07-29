server <- function(input, output, session) {

  # update name selection dynamically
  disp_names <- reactive({
    names |>
      pull(all_of(input$sel_name_type))
  })

  observe({
    updateSelectizeInput(
      session,
      "sel_name",
      choices = disp_names(),
      server = TRUE)
  })

  # generate plots on submit
  observeEvent(input$submit, {
    # clear previous message
    output$msg <- renderText({
      NULL
    })

    # pull data from db
    sp_data <- sp_retrieve(input$sel_name_type, input$sel_name, input$sel_qtr,
                           input$sel_date_range)

    # check if there are observations
    if (sp_data |> collect() |> nrow() == 0) {
      showModal(
        modalDialog(
          easyClose = TRUE,
          footer = NULL,
          size = 'm',
          "Sorry, no observations found."
        )
      )
    }
    # if yes, plot
    else {
      # spatial map
      m <- map_hex(sp_data, input$sel_hex_res) |>
        mapView(zcol = "value", layer.name = input$sel_name)

      output$map <- renderLeaflet({
        removeMapJunk(m, c("layersControl", "homeButton"))
      })

      # time series
      ts <- make_ts(sp_data, input$sel_ts_res) |>
        select(time, avg, lwr, upr) |>
        collect() |>
        dygraph() |>
        dySeries(
          c("lwr", "avg", "upr"),
          label = input$sel_name
        )

      output$ts_plot <- renderDygraph({
        ts
      })
    }
  })
}
