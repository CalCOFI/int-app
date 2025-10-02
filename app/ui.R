ui <- page_sidebar(
  title = "CalCOFI Larva App",

  useBusyIndicators(spinners = TRUE, fade = TRUE),

  # Sidebar ----
  sidebar = sidebar(
    width = 300,

    # Action buttons
    actionButton("sel_data", "Data Selection", width = "100%", class = "mb-2"),
    downloadButton("download_sp", "Download Species Data", class = "btn-secondary mb-2", style = "width: 100%;"),
    downloadButton("download_ocean", "Download Ocean Data", class = "btn-secondary mb-2", style = "width: 100%;"),

    # Conditional panels for tab-specific controls
    conditionalPanel(
      "input.outputPanel === 'Map'",
      uiOutput("map_ocean_stat")
    ),

    conditionalPanel(
      "input.outputPanel === 'Time Series'",
      uiOutput("ts_res")
    ),

    conditionalPanel(
      "input.outputPanel === 'Scatterplot'",
      p("Click on a point or use the box/lasso tool to select points to see their location.",
        class = "small text-muted mt-2")
    ),

    conditionalPanel(
      "input.outputPanel === 'Depth Profile'",
      actionButton("open_transect_modal", "Draw Transect",
                   width = "100%", class = "mt-2")
    ),

    # Filter summary accordion
    uiOutput("filter_summary"),
  ),

  # Main content area ----
  navset_card_underline(
    id = "outputPanel",
    height = "100%",

    nav_spacer(),

    nav_panel(
      "Map",
      uiOutput("map_content")
    ),

    nav_panel(
      "Time Series",
      uiOutput("ts_content")
    ),

    nav_panel(
      "Scatterplot",
      uiOutput("splot_content")
    ),

    nav_panel(
      "Depth Profile",
      uiOutput("dprof_content")
    )
  )
)
