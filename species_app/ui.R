ui <- page_sidebar(
  title = "Species App",

  sidebar = sidebar(
    accordion(

      # data subsetting options
      accordion_panel(
        "Data Selection",

        selectInput(
          "sel_name_type",
          "Search By",
          list("Common Name" = "common_name", "Scientific Name" = "scientific_name"),
          selected = "common"),

        selectizeInput(
          "sel_name",
          "Species Name",
          choices = NULL
        ),

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

      # plotting options
      accordion_panel(
        "Plot Controls",
        conditionalPanel(
          "input.outputPanel === 'Map'",

          sliderInput("sel_hex_res", "Hexagon Resolution", 1, 10, 3, step = 1),
        ),

        conditionalPanel(
          "input.outputPanel === `Time Series`",

          #selectInput("sel_ts_type", "Plot Type", c("Trend", "Seasonal"), selected = "Trend"),
          selectInput(
            "sel_ts_res",
            "Temporal Resolution",
            list("Year" = "year", "Quarter" = "quarter", "Month" = "month", "Day" = "day"),
            selected = "Year"
          )
        )
      )
    ),

    actionButton("submit", "Submit")
  ),

  # output panels
  navset_card_underline(

    id = "outputPanel",

    nav_spacer(),

    nav_panel(
      "Map",
      leafletOutput("map")
    ),

    nav_panel(
      "Time Series",
      dygraphOutput("ts_plot")
    ),

    nav_panel(
      "Scatterplot",
      "Scatterplot"
    ),

    nav_panel(
      "Depth Profile",
      "Depth Profile"
    )
  )
)
