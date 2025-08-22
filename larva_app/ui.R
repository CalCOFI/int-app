ui <- page_sidebar(
  title = "CalCOFI Larva App",

  sidebar = sidebar(
    actionButton("sel_data", "Data Selection"),

    # Hidden container for shared inputs
    div(style = "display:none;",
        sliderInput("sel_hex_res", "Hexagon Resolution", 1, 10, 3, step = 1),
        selectInput("sel_ts_res", "Temporal Resolution", ts_res_choices, selected = "Year"),
        selectInput("sel_ocean_stat", "Oceanographic Summary Statistic",
                    list("Average" = "mean", "Max" = "max", "Min" = "min", "St. Dev." = "sd"),
                    selected = "Average")
    ),
    conditionalPanel("input.outputPanel === 'Map'",
                     uiOutput("map_ocean_stat")
    ),

    conditionalPanel("input.outputPanel === 'Time Series'",
                     uiOutput("ts_res")
    ),

    conditionalPanel("input.outputPanel === 'Scatterplot'",
                     uiOutput("splot_hex_res"),
                     uiOutput("splot_ts_res"),
                     uiOutput("splot_ocean_stat"),
                     input_switch("sel_splot_err", "Display errorbars")
    ),
  ),

  # output panels
  navset_card_underline(

    id = "outputPanel",
    full_screen = TRUE,

    nav_spacer(),

    nav_panel(
      "Map",
      maplibreCompareOutput("map")
    ),

    nav_panel(
      "Time Series",
      dygraphOutput("sp_ts_plot"),
      dygraphOutput("ocean_ts_plot")
    ),

    nav_panel(
      "Scatterplot",
      "Scatterplot",
      if (FALSE) {plotlyOutput("splot")}
    ),

    nav_panel(
      "Depth Profile",
      "Depth Profile"
    )
  )
)
