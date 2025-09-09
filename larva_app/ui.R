ui <- page_sidebar(
  title = "CalCOFI Larva App",

  sidebar = sidebar(
    actionButton("sel_data", "Data Selection"),

    # Hidden container for shared inputs
    div(style = "display:none;",
        selectInput("sel_ts_res", "Temporal Resolution", ts_res_choices, selected = "Year"),
        selectInput("sel_ocean_stat", "Oceanographic Summary Statistic",
                    list("Average" = "mean", "Max" = "max", "Min" = "min", "St. Dev." = "sd"),
                    selected = "Average")
    ),

    conditionalPanel("input.outputPanel === 'Map'",
                     uiOutput("map_ocean_stat")),

    conditionalPanel("input.outputPanel === 'Time Series'",
                     uiOutput("ts_res")),

    conditionalPanel("input.outputPanel === 'Scatterplot'",
                     "Click on a point or use the box/lasso tool to select a group of points to see their location."),

    conditionalPanel("input.outputPanel === 'Depth Profile'",
                     "Draw a line on the map, then click 'Generate Profile' to create a cross-section. Use the buffer distance to set how far points can be from the line.",
                     numericInput("sel_buffer_dist", "Buffer Distance (km)", 5, min = 0),
                     actionButton("get_features", "Generate Profile")),

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
      highchartOutput("ts_plot")
    ),

    nav_panel(
      "Scatterplot",
      plotlyOutput("splot")
    ),

    nav_panel(
      "Depth Profile",
      maplibreOutput("dprof_map")
    )
  )
)
