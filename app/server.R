server <- function(input, output, session) {

  # thematic for bslib theming ----
  thematic::thematic_shiny()

  # usage tracking ----
  # calcofi4r::cc_track() only pushes a message down the websocket the session
  # already has open — it makes NO http request, so instrumenting a hot control
  # cannot add latency to the query that follows. (The previous log_query() ran
  # a synchronous httr2 POST to Apps Script on every submit and every download,
  # stalling the reactive thread for the whole round-trip.)
  # `ignoreInit = TRUE` on the observers below so app startup doesn't emit a
  # burst of synthetic "selections" the user never made.
  calcofi4r::cc_track_session(session)   # the ip + token JS cannot read itself
  trk <- function(event, ...) calcofi4r::cc_track(session, event, ...)
  trk("session_start", h3t = USE_H3T, release = H3T_RELEASE)

  # rx$params is nested (map_params, ts_params, …) and carries Dates, which do
  # not survive unlist(); flatten it to the readable scalars the Sheet wants, so
  # every download row carries the filters that produced it.
  trk_filters <- function(p) list(
    taxa             = p$taxa,
    n_taxa           = length(p$taxa),
    env_var          = p$env_var,
    quarters         = p$sel_qtr %||% p$quarters,
    date_beg         = as.character(p$date_range[1]),
    date_end         = as.character(p$date_range[2]),
    depth_min        = p$depth_range[1],
    depth_max        = p$depth_range[2],
    include_children = p$ck_children %||% p$include_children,
    zones            = p$zones,
    time_window      = p$time_window,
    dist_window      = p$dist_window)

  # which tab users actually work in (the navset carries id = "outputPanel", so
  # this covers About and Download as well as the four visualization panels)
  observeEvent(input$outputPanel, trk("select_tab", tab = input$outputPanel),
               ignoreInit = TRUE)
  observeEvent(input$dark_toggle, trk("select_theme", theme = input$dark_toggle),
               ignoreInit = TRUE)

  # the two modals that gate everything else — a high open_filters count with
  # few filter_submit rows means people are bouncing off the filter dialog.
  observeEvent(input$sel_data,            trk("open_filters"),   ignoreInit = TRUE)
  observeEvent(input$btn_layers,          trk("open_layers"),    ignoreInit = TRUE)
  observeEvent(input$open_transect_modal, trk("open_transect"),  ignoreInit = TRUE)

  # tour ----
  # launch the guided tour on load, unless suppressed with ?tour=off in the URL
  # (also accepts false/0/no) — handy for clean screenshots; see the db-viz-hex
  # recipe in CalCOFI.github.io/shots.yml
  if (is_tour_on) {
    observeEvent(TRUE, {
      tour_q   <- getQueryString()[["tour"]]
      tour_off <- !is.null(tour_q) && tolower(tour_q) %in% c("off", "false", "0", "no")
      if (!tour_off) {
        trk("start_tour")
        tour$init()$start()
      }
    }, once = TRUE)
  }

  # rx ----
  rx <- reactiveValues(
    df_sp          = NULL,
    df_env         = NULL,
    env_hex_list   = NULL,  # cached env hex list for first map render
    env_var        = NULL,  # stores the env_var code (e.g., "temperature")
    lbl_env_var    = NULL,  # stores the label (e.g., "Temperature (ºC)")
    sel_zones      = NULL,
    map_sp         = NULL,
    sp_scale       = NULL,  # scale list for sp map
    env_scale      = NULL,  # scale list for env map
    df_splot       = NULL,
    df_dprof       = NULL,
    filter_summary = NULL,
    summary_stats  = NULL,
    plot_depth     = NULL,
    params = list( # filter/analysis params
      taxa             = default_sp_name,
      env_var          = "temperature",
      quarters         = 1:4,
      date_range       = min_max_date,
      depth_range      = c(0, 212),
      include_children = TRUE,
      zones            = NULL,
      time_window      = NULL,
      dist_window      = NULL,
      map_params       = list(env_stat   = NULL),
      ts_params        = list(ts_res     = NULL),
      splot_params     = list(time_window = NULL,
                              dist_window = NULL,
                              method      = NULL),
      dprof_params     = list(transect   = NULL,
                              buffer     = NULL)
    ))

  # session.once -> ... ----
  observeEvent(session$clientData, once = TRUE, {
    tryCatch({
      if (debug) message("\n=== LOADING DEFAULT DATA ===")

      # default selections
      sel_name        <- default_sp_name
      sel_env_var     <- "temperature"
      sel_qtr         <- 1:4
      sel_date_range  <- min_max_date
      sel_depth_range <- c(0, 212)
      ck_children     <- TRUE
      env_stat        <- "mean"

      if (debug) message("Loading default species: ", sel_name)

      # retrieve data (lazy tables from database) -- always needed for ts, splot, etc.
      df_sp  <- get_sp(sel_name, sel_qtr, sel_date_range, ck_children)
      df_env <- get_env(sel_env_var, sel_qtr, sel_date_range, sel_depth_range[1], sel_depth_range[2])

      if (USE_H3T) {
        # h3t path: skip the 10-resolution sf preload entirely. hex data is
        # served on-demand per viewport; we only need a single color scale per
        # side (from /h3t/stats) for the legend.
        if (debug) message("USE_H3T: fetching stats instead of preloading hex lists")

        # extract scientific_name from UI-formatted label "Common name (rank: Scientific name)"
        sci_name <- sub(".*\\(.*:\\s*([^)]+)\\).*", "\\1", sel_name)
        sp_sql  <- build_sp_sql(sci_name, sel_qtr, sel_date_range, include_children = FALSE)
        env_sql <- build_env_sql(sel_env_var, sel_qtr, sel_date_range, sel_depth_range, stat = env_stat)

        sp_stats  <- fetch_h3t_stats(sp_sql,  H3T_RELEASE)
        env_stats <- fetch_h3t_stats(env_sql, H3T_RELEASE)
        if (debug) {
          message("sp stats:  "); print(sp_stats)
          message("env stats: "); print(env_stats)
        }

        sp_scale  <- build_h3t_scale(sp_stats,
          palette = \(n) hcl.colors(n, palette = "Viridis"))
        env_scale <- build_h3t_scale(env_stats,
          palette = \(n) rev(hcl.colors(n, palette = "Spectral")))

        sp_tile_url  <- h3t_tile_url(sp_sql,  H3T_RELEASE)
        env_tile_url <- h3t_tile_url(env_sql, H3T_RELEASE)

        rx$map_sp      <- map_sp_h3t(sp_tile_url,  sp_scale)
        rx$env_tile_url <- env_tile_url
        rx$env_scale_single <- env_scale

        # keep the zoom observer happy: length-10 list of copies
        rx$sp_scale    <- rep(list(sp_scale),  length(res_range))
        rx$env_scale   <- rep(list(env_scale), length(res_range))

        # summary stats: build_h3t_scale gave us min/max/n; reuse
        rx$summary_stats <- prep_summary_stats(df_sp, df_env)

      } else {
        # classic path: 10-resolution sf preload (with RDS cache)
        cached <- load_cache(cache_dir, db_path)

        if (!is.null(cached)) {
          if (debug) message("using cached default data")
          sp_hex_list   <- cached$sp_hex_list
          env_hex_list  <- cached$env_hex_list
          summary_stats <- cached$summary_stats
        } else {
          if (debug) message("cache miss -- computing default data")
          n_sp <- df_sp |> summarize(n = n()) |> pull(n)
          if (debug) message("Default species data: found ", n_sp, " rows")
          if (n_sp == 0) {
            if (debug) message("WARNING: No data found for default species")
            return(NULL)
          }
          sp_hex_list   <- prep_sp_hex(df_sp, res_range)
          env_hex_list  <- prep_env_hex(df_env, res_range, env_stat)
          summary_stats <- prep_summary_stats(df_sp, df_env)
          save_cache(cache_dir, db_path, sp_hex_list, env_hex_list, summary_stats)
        }

        rx$summary_stats <- summary_stats

        if (debug) message("Generating default species map...")
        sp_scale_list <- lapply(
          sp_hex_list,
          interpolate_palette,
          column  = "sp.value",
          palette = \(n) hcl.colors(n, palette = "Viridis"))
        rx$map_sp       <- map_sp(sp_hex_list, sp_scale_list)
        rx$sp_scale     <- sp_scale_list
        rx$env_hex_list <- env_hex_list
      }

      # store shared data (both paths)
      rx$df_sp       <- df_sp
      rx$df_env      <- df_env
      rx$env_var     <- sel_env_var
      rx$lbl_env_var <- names(which(env_var_choices == sel_env_var))
      rx$params$taxa        <- sel_name
      rx$params$env_var     <- sel_env_var
      rx$params$sel_qtr     <- sel_qtr
      rx$params$date_range  <- sel_date_range
      rx$params$depth_range <- sel_depth_range
      rx$params$ck_children <- ck_children

      rx$filter_summary <- prep_filter_summary(
        sel_name, sel_env_var, sel_qtr, sel_date_range,
        sel_depth_range, drawn_polygon = NULL, rx$sel_zones, ck_children)

      rx$plot_depth <- NULL

      if (debug) message("=== DEFAULT DATA LOADED ===\n")
    }, error = function(e) {
      message("ERROR in default data initialization: ", conditionMessage(e))
      traceback()
    })
  })

  # ts_content ----
  output$ts_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      highchartOutput("ts_plot", height = "100%")
    }
  })

  # splot_content ----
  output$splot_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      plotlyOutput("splot", height = "100%")
    }
  })

  # dprof_content ----
  output$dprof_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else if (is.null(rx$plot_depth)) {
      ui_placeholder(
        "No Depth Profile Generated",
        "Click 'Draw Transect' in the sidebar to create a depth profile."
      )
    } else {
      plotlyOutput("dprof_plot", height = "100%")
    }
  })

  # map ----
  # defer rendering until after the first Shiny flush cycle so the
  # maplibreCompareOutput DOM element is fully initialized on the client
  map_ready <- reactiveVal(FALSE)
  session$onFlushed(function() map_ready(TRUE), once = TRUE)

  output$map <- renderMaplibreCompare({
    req(map_ready(), rx$df_env, rx$map_sp)

    if (debug) message("renderMaplibreCompare: generating environmental map...\n")

    env_stat       <- input$sel_env_stat %||% "mean"
    env_stat_label <- names(which(env_stat_choices == env_stat))

    if (USE_H3T) {
      # h3t path: reuse the tile_url + scale computed in the preload block.
      # if env_stat changes from the default, we rebuild the URL/scale here.
      if (is.null(rx$env_tile_url) || env_stat != "mean") {
        env_sql <- build_env_sql(
          rx$env_var, rx$params$sel_qtr, rx$params$date_range,
          rx$params$depth_range, stat = env_stat)
        env_stats <- fetch_h3t_stats(env_sql, H3T_RELEASE)
        env_scale <- build_h3t_scale(env_stats,
          palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
        env_tile_url <- h3t_tile_url(env_sql, H3T_RELEASE)
        rx$env_scale_single <- env_scale
        rx$env_scale <- rep(list(env_scale), length(res_range))
      } else {
        env_tile_url <- rx$env_tile_url
        env_scale    <- rx$env_scale_single
      }
      map_env_obj <- map_env_h3t(env_tile_url, env_scale,
                                 env_stat_label, rx$lbl_env_var)
      rx$params$map_params$env_stat <- env_stat
      return(compare(rx$map_sp, map_env_obj, elementId = "map"))
    }

    # classic path
    if (!is.null(rx$env_hex_list) && env_stat == "mean") {
      env_hex_list    <- rx$env_hex_list
      rx$env_hex_list <- NULL
    } else {
      env_hex_list <- prep_env_hex(rx$df_env, res_range, env_stat)
    }
    env_scale_list <- lapply(
      env_hex_list,
      interpolate_palette,
      column  = "env.value",
      palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
    map_env_obj <- map_env(
      env_hex_list,
      env_scale_list,
      env_stat_label,
      rx$lbl_env_var)

    rx$env_scale <- env_scale_list
    rx$params$map_params$env_stat <- env_stat

    if (debug) {
      message("renderMaplibreCompare: creating comparison map")
      message("rx$map_sp class: ", paste(class(rx$map_sp), collapse = ", "))
      message("map_env_obj class: ", paste(class(map_env_obj), collapse = ", "))
    }

    compare(rx$map_sp, map_env_obj, elementId = "map")
  })
  # dark_toggle -> map.style ----
  observeEvent(input$dark_toggle, {
    style  <- ifelse(
      input$dark_toggle == "dark",
      "dark-matter",
      "voyager")

    if (debug)
      message("maplibre_compare_proxy -> set_style: ", style)

    maplibre_compare_proxy("map", map_side = "before") |>
      set_style(carto_style(style))

    maplibre_compare_proxy("map", map_side = "after") |>
      set_style(carto_style(style))
  })

  # map layers modal ----
  # track which spatial layers are enabled
  rx$spatial_visible <- d_spatial_layers |>
    filter(default_visible) |>
    pull(dataset_id)

  observeEvent(input$btn_layers, {
    # build checkbox groups from registry
    layer_choices <- split(
      setNames(d_spatial_layers$dataset_id, d_spatial_layers$layer),
      d_spatial_layers$group)

    grp_names <- names(layer_choices)
    n         <- length(grp_names)
    mid       <- ceiling(n / 2)

    make_col <- function(grps) {
      tagList(lapply(grps, function(grp) {
        input_id <- paste0("lyr_", make.names(grp))
        checkboxGroupInput(
          input_id,
          grp,
          choices  = layer_choices[[grp]],
          selected = intersect(
            rx$spatial_visible,
            layer_choices[[grp]]))
      }))
    }

    showModal(modalDialog(
      title = "Map Layers",
      size  = "l",
      fluidRow(
        column(6, make_col(grp_names[1:mid])),
        column(6, make_col(grp_names[(mid + 1):n]))),
      footer = tagList(
        actionButton("btn_layers_apply", "Apply", class = "btn-primary"),
        modalButton("Cancel"))
    ))
  })

  observeEvent(input$btn_layers_apply, {
    # collect selected layer IDs from all checkbox groups
    all_groups <- unique(d_spatial_layers$group)
    selected   <- unlist(lapply(all_groups, function(grp) {
      input_id <- paste0("lyr_", make.names(grp))
      input[[input_id]]
    }))
    if (is.null(selected)) selected <- character(0)

    rx$spatial_visible <- selected

    # which reference layers people actually turn on — the full id list goes to
    # the Sheet leg (GA4 would bucket a many-valued dimension into "(other)")
    trk("select_layers", layers = selected, n_layers = length(selected))

    # toggle visibility on both sides of compare map
    polygon_layers <- d_spatial_layers |>
      filter(geom_type == "polygon") |>
      pull(dataset_id)

    for (lyr_id in d_spatial_layers$dataset_id) {
      vis <- ifelse(lyr_id %in% selected, "visible", "none")
      for (side in c("before", "after")) {
        maplibre_compare_proxy("map", map_side = side) |>
          set_layout_property(lyr_id, "visibility", vis)
        # also toggle outline layer for polygons
        if (lyr_id %in% polygon_layers) {
          maplibre_compare_proxy("map", map_side = side) |>
            set_layout_property(
              paste0(lyr_id, "_outline"), "visibility", vis)
        }
      }
    }

    # rebuild layers control on both sides with only selected layers
    hex_ids <- c(paste0("sp", res_range), paste0("env", res_range))
    ctrl    <- build_layers_control(selected, d_spatial_layers, hex_ids)

    for (side in c("before", "after")) {
      maplibre_compare_proxy("map", map_side = side) |>
        clear_controls(controls = "layers") |>
        add_layers_control(
          position     = "top-right",
          layers       = ctrl,
          collapsible  = TRUE,
          margin_right = 45)
    }

    removeModal()
  })

  # map zoom ----
  observeEvent(input$map_before_view, {
    req(rx$sp_scale, rx$env_scale)

    view <- input$map_before_view
    req(view$zoom)

    z <- view$zoom
    i <- findInterval(z, zoom_breaks, rightmost.closed = TRUE)

    # Guard against weird zoom values
    if (i < 1 || i > length(rx$sp_scale)) return(NULL)

    sp_scale  <- rx$sp_scale[[i]]
    env_scale <- rx$env_scale[[i]]

    env_stat <- input$sel_env_stat %||% "mean"
    lbl_env_stat <- names(which(env_stat_choices == env_stat))

    # Species legend (left / before)
    maplibre_compare_proxy("map", map_side = "before") |>
      add_legend(
        # CPUE (density): count/10m² for oblique/vertical tows, count/100m³ for
        # manta — the map value averages across net types, so the legend names
        # the measure without a single unit (per-tow unit is in the download).
        legend_title = "Avg. CPUE (density)",
        values       = round(sp_scale$breaks, 2),
        colors       = sp_scale$colors,
        type         = "continuous",
        position     = "bottom-left",
        width        = "275px",
        target       = "compare",
        style        = legend_style(background_opacity = 0.5),
        add          = FALSE
      )

    # Environmental legend (right / after)
    maplibre_compare_proxy("map", map_side = "after") |>
      add_legend(
        legend_title = paste(lbl_env_stat, rx$lbl_env_var),
        values       = signif(env_scale$breaks, 4),
        colors       = env_scale$colors,
        type         = "continuous",
        position     = "bottom-right",
        width        = "275px",
        target       = "compare",
        style        = legend_style(background_opacity = 0.5),
        add         = TRUE
      )
  })

  # ts_plot ----
  output$ts_plot <- renderHighchart({
    req(rx$df_sp, rx$df_env, rx$env_var)

    if (debug) message("renderHighchart: generating time series plot\n")

    ts_res <- input$sel_ts_res %||% "year"
    sp_ts  <- prep_ts_sp(rx$df_sp, ts_res) |> arrange(time)
    env_ts <- prep_ts_env(rx$df_env, ts_res)

    rx$params$ts_params$ts_res <- ts_res

    plot_ts(sp_ts, env_ts, ts_res, rx$env_var, input$dark_toggle == "dark")
  })

  # splot ----
  output$splot <- renderPlotly({
    df_splot <- prep_splot(rx$df_sp, rx$df_env, "mean",
                           method = input$splot_method,
                           max_hours_diff = input$splot_max_hours_diff,
                           max_meters_diff = input$splot_max_meters_diff)
    rx$df_splot <- df_splot
    rx$params$splot_params <- list(
      time_window = input$splot_max_hours_diff,
      dist_window = input$splot_max_meters_diff,
      method      = input$splot_method
    )

    req(rx$df_splot)

    if (debug) message("renderPlotly: generating scatterplot with ggplotly\n")

    # prepare data with customdata and hover text for plotly
    df_plot <- rx$df_splot |>
      collect() |>
      mutate(
        customdata = 1:n(),
        hover_text = paste0(
          "<b>Date:</b> ", sp_dtime,
          "<br><b>Species:</b> ", sp_name,
          "<br><b>", rx$lbl_env_var, ":</b> ", round(env_qty, 2),
          "<br><b>CPUE (density):</b> ", round(sp_tally, 2) ))

    # create ggplot (thematic will apply bslib theme automatically)
    p <- ggplot(
      df_plot,
      aes(
        x          = env_qty,
        y          = sp_tally,
        color      = sp_name,
        text       = hover_text,
        customdata = customdata)) +
      geom_point(size = 3, alpha = 0.6) +
      labs(
        x     = rx$lbl_env_var,
        y     = "Species Abundance",
        color = "Species")

    # convert to plotly with bslib theme support
    ggplotly(p, tooltip = "text", source = "scatterPlotSource") |>
      layout(dragmode = "select") |>
      config(
        displaylogo            = FALSE,
        scrollZoom             = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian") ) |>
      toWebGL() # for performance
  })

  # sel_data -> modal_data(), spatial_filter_map ----
  observeEvent(input$sel_data, {
    showModal(modal_data())
    updateSelectizeInput(session, 'sel_name', choices = sp_names, server = TRUE)

    output$spatial_filter_map <- renderMaplibre({
      if (input$sel_places_cat == "Custom") {
        maplibre(
          style = carto_style(ifelse(
            input$dark_toggle == "dark",
            "dark-matter",
            "voyager"))) |>
          add_draw_control(
            position = "top-right",
            displayControlsDefault = FALSE,
            controls = list(polygon = TRUE, trash = TRUE))
      } else {
        places <- cc_places |>
          filter(
            category == input$sel_places_cat
          )

        maplibre(
          style = carto_style(ifelse(
            input$dark_toggle == "dark",
            "dark-matter",
            "voyager")),
          bounds = places) |>
          add_fill_layer(
            id = 'base-zones',
            places,
            fill_color = match_expr(
              "name",
              values = unique(places$name),
              stops = hcl.colors(length(unique(places$name)))),
            fill_opacity = 0.5,
            fill_outline_color = "black") |>
          add_fill_layer(
            id = 'sel-zones',
            places,
            fill_color = match_expr(
              "name",
              values = unique(places$name),
              stops = hcl.colors(length(unique(places$name)))),
            fill_opacity = 0.0,
            fill_outline_color = NULL) |>
          add_line_layer(
            id = 'sel-zones-outline',
            places,
            line_color = ifelse(input$dark_toggle == "dark", "#dee2e6", "#333333"),
            line_width = 3,
            line_opacity = 0.0)
      }
     })

    output$tbl_places <- renderDataTable({
      cc_places |>
        as.data.frame() |>
        filter(
          category == input$sel_places_cat
        ) |>
        select(name)
    })
  })

  # Observe clicks on the grid layer of spatial filter map
  observeEvent(input$spatial_filter_map_feature_click, {

    custom <- input$sel_places_cat == "Custom"

    click <- input$spatial_filter_map_feature_click

    # Only process clicks on the grid layer
    if (!is.null(click$properties$name) & !custom) {
      clicked_place <- click$properties$name
      current_places <- rx$sel_places

      # Toggle zone selection
      if (clicked_place %in% current_places) {
        # Remove if already selected
        new_places <- setdiff(current_places, clicked_place)
      } else {
        # Add to selection
        new_places <- c(current_places, clicked_place)
      }

      rx$sel_places <- new_places

      # Update map styling to highlight selected zones
      if (length(new_places) > 0) {
        maplibre_proxy("spatial_filter_map") |>
          set_filter("sel-zones",
                     list("in", list("get", "name"), list("literal", new_places))) |>
          set_paint_property("sel-zones", "fill-opacity", 0.8) |>
          set_paint_property("sel-zones", "fill-outline-color", "black") |>
          set_filter("sel-zones-outline",
                     list("in", list("get", "name"), list("literal", new_places))) |>
          set_paint_property("sel-zones-outline", "line-opacity", 1.0)
      } else {
        # Reset filter if no zones selected
        maplibre_proxy("spatial_filter_map") |>
          set_paint_property("sel-zones", "fill-opacity", 0.0) |>
          set_paint_property("sel-zones", "fill-outline-color", NULL) |>
          set_paint_property("sel-zones-outline", "line-opacity", 0.0)
      }

      # Update table row selection
      places_tbl <- cc_places |>
        filter(category == input$sel_places_cat)
      rows_to_select <- which(places_tbl$name %in% new_places)

      tbl_proxy <- dataTableProxy("tbl_places")
      selectRows(tbl_proxy, rows_to_select)
    }
  })

  # Observe clicks on table rows
  observeEvent(input$tbl_places_rows_selected, {
    req(input$sel_places_cat)

    sel_rows <- input$tbl_places_rows_selected

    places_tbl <- cc_places |>
      filter(category == input$sel_places_cat)

    if (is.null(sel_rows) || length(sel_rows) == 0) {
      new_places <- character(0)
      rx$sel_places <- character(0)
    } else {
      # Map selected rows to keys
      new_places <- places_tbl$name[sel_rows]

      # Update reactive selection
      rx$sel_places <- new_places
    }

    # Update map styling to highlight selected zones
    if (length(new_places) > 0) {
      maplibre_proxy("spatial_filter_map") |>
        set_filter("sel-zones",
                   list("in", list("get", "name"), list("literal", new_places))) |>
        set_paint_property("sel-zones", "fill-opacity", 0.8) |>
        set_paint_property("sel-zones", "fill-outline-color", "black") |>
        set_filter("sel-zones-outline",
                   list("in", list("get", "name"), list("literal", new_places))) |>
        set_paint_property("sel-zones-outline", "line-opacity", 1.0)
    } else {
      # Reset filter if no zones selected
      maplibre_proxy("spatial_filter_map") |>
        set_paint_property("sel-zones", "fill-opacity", 0.0) |>
        set_paint_property("sel-zones", "fill-outline-color", NULL) |>
        set_paint_property("sel-zones-outline", "line-opacity", 0.0)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # submit -> ... ----
  observeEvent(input$submit, {
    if (debug) message("\n=== DATA SELECTION SUBMITTED ===\n")

    # collect input selections
    sel_name        <- input$sel_name
    sel_env_var     <- input$sel_env_var
    sel_qtr         <- input$sel_qtr
    sel_date_range  <- input$sel_date_range
    sel_depth_range <- input$sel_depth_range
    ck_children     <- input$ck_children

    if (debug) message("Selections: sp_name =", sel_name, ", env_var =", sel_env_var)

    # get spatial filter
    drawn_polygon <- get_drawn_features(maplibre_proxy("spatial_filter_map"))
    if (debug) message("Spatial filter:", if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) "custom polygon" else "none")

    # THE headline signal: the whole filter set, in one row. The taxa names are
    # exactly what makes the log readable, and are why this detail belongs in
    # the Sheet leg — GA4 buckets a dimension this wide into "(other)".
    trk("filter_submit",
        taxa             = sel_name,
        n_taxa           = length(sel_name),
        env_var          = sel_env_var,
        quarters         = sel_qtr,
        date_beg         = sel_date_range[1],
        date_end         = sel_date_range[2],
        depth_min        = sel_depth_range[1],
        depth_max        = sel_depth_range[2],
        include_children = ck_children,
        spatial          = if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) "polygon"
                           else if (length(rx$sel_zones) > 0) "zones" else "none",
        zones            = rx$sel_zones)

    # retrieve data (lazy tables from database) — timed + logged, non-blocking
    df_sp <- calcofi4r::cc_track_query(session, "map_query_sp",
      list(taxa = sel_name, quarters = sel_qtr, date_beg = sel_date_range[1],
           date_end = sel_date_range[2], include_children = ck_children),
      get_sp(sel_name, sel_qtr, sel_date_range, ck_children))
    df_env <- calcofi4r::cc_track_query(session, "map_query_env",
      list(env_var = sel_env_var, quarters = sel_qtr, date_beg = sel_date_range[1],
           date_end = sel_date_range[2], depth_min = sel_depth_range[1],
           depth_max = sel_depth_range[2]),
      get_env(sel_env_var, sel_qtr, sel_date_range, sel_depth_range[1], sel_depth_range[2]))

    # Apply spatial filter based on priority: drawn polygon > selected zones > all data
    if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
      # Use drawn polygon (existing code)
      polygon_wkt <- st_as_text(drawn_polygon$geometry[[1]])

      df_sp <- df_sp |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", polygon_wkt, "'))"
        )))

      df_env <- df_env |>
        filter(sql(paste0(
          "ST_Within(ST_Point(lon_dec, lat_dec), ST_GeomFromText('", polygon_wkt, "'))"
        )))

    } else if (!is.null(rx$sel_zones) && length(rx$sel_zones) > 0) {
      # Filter by selected grid zones

      zones_wkt <- cc_grid_zones |>
        filter(zone_key %in% rx$sel_zones) |>
        pull(geom) |>
        st_union() |>
        st_as_text()

      # Filter species data by zones
      df_sp <- df_sp |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", zones_wkt, "'))"
        )))

      # Filter ocean data by zones
      df_env <- df_env |>
        filter(sql(paste0(
          "ST_Within(ST_Point(lon_dec, lat_dec), ST_GeomFromText('", zones_wkt, "'))"
        )))
    }

    # validate data (only collect count, not full data)
    n_sp <- df_sp |> summarize(n = n()) |> pull(n)
    if (debug) message("Species data: found", n_sp, "rows\n")

    if (n_sp == 0) {
      # a dead end the user hit — countable, so an empty combination that keeps
      # recurring (a taxon with no observations in the chosen window) shows up
      # instead of being invisible next to the successful submits
      trk("filter_no_results", taxa = sel_name, env_var = sel_env_var,
          quarters = sel_qtr, date_beg = sel_date_range[1],
          date_end = sel_date_range[2], status = "empty")
      showNotification("No observations found for selected species.", type = "warning")
      showModal(modal_data())
      return(NULL)
    }

    # store shared data (still lazy tables)
    rx$df_sp       <- df_sp
    rx$df_env      <- df_env
    rx$env_var     <- sel_env_var
    rx$lbl_env_var <- names(which(env_var_choices == sel_env_var))

    rx$params$taxa        <- sel_name
    rx$params$env_var     <- sel_env_var
    rx$params$sel_qtr     <- sel_qtr
    rx$params$date_range  <- sel_date_range
    rx$params$depth_range <- sel_depth_range
    rx$params$zones       <- rx$zones
    rx$params$ck_children <- ck_children
    if (debug) message("Stored reactive data: df_sp, df_env, lbl_env_var =", rx$lbl_env_var)

    # build filter summary
    rx$filter_summary <- prep_filter_summary(
      sel_name,
      sel_env_var,
      sel_qtr,
      sel_date_range,
      sel_depth_range,
      drawn_polygon,
      rx$sel_zones,
      ck_children)

    # build summary stats
    rx$summary_stats <- prep_summary_stats(
      rx$df_sp,
      rx$df_env
    )

    # generate map
    if (debug) message("Generating species map...\n")
    is_dark       <- input$dark_toggle == "dark"
    sp_hex_list   <- prep_sp_hex(df_sp, res_range)
    sp_scale_list <- lapply(
      sp_hex_list,
      interpolate_palette,
      column  = "sp.value",
      palette = \(n) hcl.colors(n, palette = "Viridis"))
    rx$map_sp     <- map_sp(sp_hex_list, sp_scale_list, is_dark = is_dark)
    if (debug) message("Species map generated and stored in rx$map_sp\n")

    # prepare scatterplot data
    df_splot <- prep_splot(df_sp, df_env, "mean")
    rx$df_splot <- df_splot

    # reset depth profile
    rx$plot_depth <- NULL

    removeModal()
  })

  # plotly_click -> ... ----
  observeEvent(event_data("plotly_click", source = "scatterPlotSource"), {
    click_data <- event_data("plotly_click", source = "scatterPlotSource")
    req(click_data, rx$df_splot)

    clicked_point <- collect(rx$df_splot)[click_data$customdata, ]

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
            "<b>Date:</b> ", clicked_point$sp_dtime,
            "<br><b>Species:</b> ", clicked_point$sp_name,
            "<br><b>", rx$lbl_env_var, ":</b> ", round(clicked_point$env_qty, 2),
            "<b>CPUE (density):</b> ", round(clicked_point$sp_tally, 2)
          )
        )
    })
  })

  observeEvent(event_data("plotly_selected", source = "scatterPlotSource"), {
    selected_data <- event_data("plotly_selected", source = "scatterPlotSource")
    req(selected_data, rx$df_splot)

    selected_points <- collect(rx$df_splot)[selected_data$customdata, ]

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
            "<b>Date:</b> ", selected_points$sp_dtime,
            "<br><b>Species:</b> ", selected_points$sp_name,
            "<br><b>", rx$lbl_env_var, ":</b> ", round(selected_points$env_qty, 2),
            "<br><b>CPUE (density):</b> ", round(selected_points$sp_tally, 2)
          )
        )
    })
  })

  # open_transect_modal -> ... ----
  observeEvent(input$open_transect_modal, {
    req(rx$map_sp)

    showModal(modal_depth_profile())

    output$transect_map <- renderMaplibre({
      rx$map_sp |>
        add_draw_control(
          position = "top-right",
          displayControlsDefault = FALSE,
          controls = list(line_string = TRUE, trash = TRUE)
        )
    })
  })

  # submit_transect -> ... ----
  observeEvent(input$submit_transect, {
    req(rx$df_sp, rx$df_env)
    t_transect <- Sys.time()

    features <- get_drawn_features(maplibre_proxy("transect_map"))

    if (is.null(features) || nrow(features) == 0) {
      trk("depth_profile_transect", status = "no_line")
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

    buffer_res <- buffer_transect(coords, buffer_dist = input$modal_buffer_dist * 1000)

    # collect data for depth profile (need full data for spatial operations)
    df_sp_collected <- rx$df_sp |> collect()
    df_env_collected <- rx$df_env |> collect()

    sp_sf <- st_as_sf(df_sp_collected, coords = c("longitude", "latitude"), crs = 4326)
    env_sf <- st_as_sf(df_env_collected, coords = c("lon_dec", "lat_dec"), crs = 4326)

    filt_sp_sf <- sp_sf[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)), ]
    filt_sp_data <- df_sp_collected[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)), ]
    filt_env_data <- df_env_collected[as.vector(st_intersects(env_sf, buffer_res$buffer, sparse = FALSE)), ]

    segment_sfc <- st_geometry(buffer_res$segment_utm)
    filt_sp_data$distance <- st_line_project(
      segment_sfc,
      st_transform(filt_sp_sf, buffer_res$utm_crs) |> st_geometry()) / 1000
    filt_env_data$distance <- st_line_project(
      segment_sfc,
      st_transform(
        st_as_sf(filt_env_data, coords = c("lon_dec", "lat_dec"), crs = 4326),
        buffer_res$utm_crs) |> st_geometry()) / 1000

    segment_length <- st_length(buffer_res$segment_utm) / 1000

    # the only step that collects BOTH full datasets into R and runs spatial
    # intersects, so its duration is the one worth watching on this tab
    trk("depth_profile_transect",
        buffer_km   = input$modal_buffer_dist,
        transect_km = round(as.numeric(segment_length), 1),
        n_env       = nrow(filt_env_data),
        n_rows      = nrow(filt_sp_data),
        ms          = as.numeric(difftime(Sys.time(), t_transect, units = "secs")) * 1000,
        status      = if (nrow(filt_sp_data) == 0) "empty" else "ok")

    dist_bin_size <- 5
    depth_bin_size <- 20

    sp_plot <- filt_sp_data |>
      mutate(
        tooltip = paste0(
          "Species: ", name, "<br>",
          "CPUE: ", round(std_tally, 2), "<br>",
          "Distance: ", round(distance, 2), " km<br>",
          "Date: ", time_start)
      ) |>
      ggplot(
        aes(
          x = distance,
          y = std_tally,
          color = name,
          text = tooltip
        )
      ) +
      geom_point(alpha = 0.6) +
      labs(
        y = "Species Abundance",
        x = "Distance (km)",
        color = "Species"
      )

    proc_env_data <- filt_env_data |>
      mutate(
        dist_bins = filt_env_data$distance %>%
          cut(seq(0, by = dist_bin_size, length.out = ceiling(max(.))/dist_bin_size+1), include.lowest = TRUE),
        depth_bins = filt_env_data$depth_m %>%
          cut(seq(min(.), by = depth_bin_size, length.out = ceiling(max(.)/depth_bin_size)+1), include.lowest = TRUE) ) |>
      group_by(
        dist_bins, depth_bins) |>
      summarize(
        n          =  sum(!is.na(qty)),
        qty        =  mean(qty, na.rm = TRUE),
        min_dtime  =  min(dtime, na.rm = TRUE),
        max_dtime  =  max(dtime, na.rm = TRUE),
        .groups    =  "drop") |>
      mutate(
        min_dist   =  as.numeric(sub("[\\[\\(]([0-9]+),.+", "\\1", dist_bins)),
        max_dist   =  as.numeric(sub(".+,([0-9]+)]",        "\\1", dist_bins)),
        min_depth  =  as.numeric(sub("[\\[\\(]([0-9]+),.+", "\\1", depth_bins)),
        max_depth  =  as.numeric(sub(".+,([0-9]+)]",        "\\1", depth_bins))) |>
      mutate(
        tooltip = paste0(
          "Distance: ", min_dist, "-", max_dist, " km<br>",
          "Depth: ", min_depth, "-", max_depth, " m<br>",
          rx$lbl_env_var, ": ", round(qty, 2), "<br>",
          "Num. Obs: ", n, "<br>",
          "Date Range: ", min_dtime, " to ", max_dtime)
      )

    env_plot <- proc_env_data |>
      ggplot(
        aes(
          xmin = min_dist,
          xmax = max_dist,
          ymin = min_depth,
          ymax = max_depth,
          fill = qty,
          text = tooltip)) +
      geom_rect() +
      scale_y_reverse() +
      scale_fill_continuous(palette = rev(hcl.colors(10, palette = "Spectral"))) +
      labs(
        x = "Distance (km)",
        y = "Depth (m)",
        fill = paste0("Average ", rx$lbl_env_var)
      )

    rx$df_dprof <- list(filt_sp_data, proc_env_data)
    rx$params$dprof_params <- list(
      buffer   = input$modal_buffer_dist,
      transect = paste0(
        "start = (", round(coords[1, "X"], 4), ", ", round(coords[1, "Y"], 4), ")",
        "; end = (", round(coords[nrow(coords), "X"], 4), ", ", round(coords[nrow(coords), "Y"], 4), ")"
      )
    )

    profile_plot <- subplot(
      ggplotly(sp_plot, tooltip = "text"),
      ggplotly(env_plot, tooltip = "text"),
      nrows = 2,
      shareX = TRUE,
      heights = c(0.33, 0.67)
    ) |>
      layout(
        showlegend = TRUE,
        legend = list(title = list(text = "Species")),
        yaxis = list(title = "Species Abundance"),
        yaxis2 = list(title = "Depth (m)"),
        xaxis = list(title = "Distance (km)", range = c(0, segment_length))
      ) |>
      config(
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian")
      )

    rx$plot_depth <- profile_plot
    removeModal()
    showNotification("Depth profile generated!", type = "message")
  })

  output$dprof_plot <- renderPlotly({
    req(rx$plot_depth)
    rx$plot_depth
  })

  output$filter_summary <- renderUI({
    req(rx$filter_summary)
    div(class = "small", markdown(paste(rx$filter_summary, collapse = "  \n")))
  })

  output$summary_statistics <- renderUI({
    req(rx$summary_stats)
    div(class = "small", markdown(paste(rx$summary_stats, collapse = "  \n")))
  })

  output$taxa_tree <- renderUI ({
    req(rx$df_sp)

    tagList(
      div(
        id = "taxa-tree-heading",
        class = "small",
        style = "margin: 0 !important; padding: 0 !important;",
        tags$style(HTML("
          #taxa-tree-heading p {
            margin-top: 0 !important;
            margin-bottom: 0 !important;
            padding-top: 0 !important;
            padding-bottom: 0 !important;
            line-height: 1.1 !important;
          }
        ")),
        markdown("**Observations by Selected Taxa**")),
      div(
        style = "margin-top: 0;",
        taxa_tree_builder(rx$df_sp))) })

  # download_data ----
  # Bundles original + summarized data with reproducible SQL. The integrated
  # bio<->env match is built and run by calcofi4r::cc_match_bio_env() against
  # public GCS release parquet (see functions.R::build_download_bundle), so the
  # query/ folder lets anyone re-run it in DuckDB and get identical rows.
  output$download_data <- downloadHandler(
    filename = function() paste0("calcofi_data_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content = function(file) {

      raw_sel  <- input$sel_raw_data_download %||% character(0)
      proc_sel <- input$sel_proc_data_download %||% character(0)
      all_sel  <- c(raw_sel, proc_sel)

      if (length(all_sel) == 0) {
        # tracked from inside content(), not on the button, so a click that
        # never produces a file is counted as the dead end it is rather than
        # as a download
        trk("download_bundle", status = "no_selection")
        showNotification("Select at least one dataset.", type = "warning")
        return(NULL)
      }

      # download timing + budget. The zip only streams to the browser at the very
      # END (after all the CSVs are built), so a server-side build that runs long
      # almost certainly outlived the client connection — the user gets a
      # truncated response ("Site wasn't available") even though the server
      # "succeeded". We log any build past this budget as a `timeout` error so the
      # log Sheet shows the real user-facing failure instead of a false ok.
      # Env-overridable (CALCOFI_DOWNLOAD_TIMEOUT_SEC).
      dl_t0        <- Sys.time()
      dl_elapsed   <- function() as.numeric(difftime(Sys.time(), dl_t0, units = "secs"))
      dl_budget_s  <- suppressWarnings(as.numeric(
        Sys.getenv("CALCOFI_DOWNLOAD_TIMEOUT_SEC", "120")))
      if (is.na(dl_budget_s) || dl_budget_s <= 0) dl_budget_s <- 120

      zip_root <- tempfile(pattern = "calcofi_download_", tmpdir = tempdir())
      dir.create(zip_root, showWarnings = FALSE, recursive = TRUE)
      paths   <- character()

      write_data <- function(df, rel_path) {
        full_path <- file.path(zip_root, rel_path)
        dir.create(dirname(full_path), showWarnings = FALSE, recursive = TRUE)
        write.csv(df, full_path, row.names = FALSE, quote = TRUE)
        paths <<- c(paths, rel_path)       # <<- adds to the outer variable
      }

      # keep time/dist windows in rx$params so the README + bundle agree
      rx$params$time_window <- input$time_window %||% default_max_hours_diff
      rx$params$dist_window <- input$dist_window %||% default_max_meters_diff

      # Wrap the whole build: any failure below (an errored product, a missing
      # reactive) is logged as status="error" and surfaced, instead of aborting
      # the handler before the success log — which made failed downloads (e.g. a
      # broken product path) silently show as "ok" in the log Sheet.
      tryCatch({
      withProgress(message = "Preparing download", value = 0, {
      for (i in all_sel) {
        incProgress(1 / length(all_sel), detail = i)

        if (i == "raw_sp") {
          req(rx$df_sp)
          # CPUE-forward schema (E. Weber request): expose the raw tally, the
          # effort fields it standardizes by, and the reconstructed density +
          # unit (count/10m2 for oblique/vertical tows, count/100m3 for manta).
          sp_out <- rx$df_sp |>
            collect() |>
            rename(cpue = std_tally) |>
            relocate(tow_type, tally, std_haul_factor, prop_sorted,
                     volume_sampled, cpue, cpue_unit, .after = quarter)
          write_data(sp_out, "data/original/species.csv")

        } else if (i == "raw_env") {
          req(rx$df_env)
          write_data(rx$df_env |> collect(), "data/original/environment.csv")

        } else if (i == "int") {
          # reproducible bundle: data/original/{bio,env}.csv +
          # data/integrated/integrated_<method>.csv + query/ (per-file *.sql,
          # manifest.json, REPRODUCE.md) — single source of truth via
          # calcofi4r::cc_match_bio_env() against GCS release parquet
          req(rx$params$taxa)
          .t0 <- Sys.time()
          .ms <- function() as.numeric(difftime(Sys.time(), .t0, units = "secs")) * 1000
          bundle_paths <- tryCatch(
            build_download_bundle(zip_root, isolate(rx$params)),
            error = function(e) {
              do.call(trk, c(
                list("download_integrated_bundle"), trk_filters(isolate(rx$params)),
                list(ms = .ms(), status = "error", error = conditionMessage(e))))
              showNotification(
                paste("Integrated data / SQL bundle failed:", conditionMessage(e)),
                type = "error", duration = NULL)
              character(0)
            })
          if (length(bundle_paths)) {
            .over   <- .ms() > dl_budget_s * 1000
            .status <- if (.over) "timeout" else "ok"
            .errmsg <- if (.over) sprintf(
              "integrated bundle build took %.0fs (> %.0fs budget); client likely disconnected before the zip streamed",
              .ms() / 1000, dl_budget_s) else ""
            do.call(trk, c(
              list("download_integrated_bundle"), trk_filters(isolate(rx$params)),
              list(n_rows = length(bundle_paths), ms = .ms(),
                   status = .status, error = .errmsg)))
            if (.over)
              showNotification(paste(
                "The integrated data bundle took longer than expected to build,",
                "so your download may not have started. Narrow the filters",
                "(fewer taxa, shorter date range) and try again."),
                type = "warning", duration = NULL)
          }
          paths <- c(paths, bundle_paths)

        } else if (i == "map") {
          req(rx$df_sp, rx$df_env)
          if (is.null(rx$params$map_params$env_stat)) {rx$params$map_params$env_stat <- "mean"}
          sp_hex  <- prep_sp_hex(rx$df_sp, res_range) |> bind_rows() |> select(-tooltip)
          env_hex <- prep_env_hex(rx$df_env, res_range,
                                  rx$params$map_params$env_stat) |>
            bind_rows() |> select(-tooltip)

          write_data(sp_hex , "data/summarized/map/species_map.csv")
          write_data(env_hex, "data/summarized/map/env_map.csv")

        } else if (i == "ts") {
          req(rx$df_sp, rx$df_env)
          if (is.null(rx$params$ts_params$ts_res)) {rx$params$ts_params$ts_res <- "year"}
          sp_ts  <- prep_ts_sp(rx$df_sp, rx$params$ts_params$ts_res)
          env_ts <- prep_ts_env(rx$df_env, rx$params$ts_params$ts_res)

          write_data(sp_ts , "data/summarized/time_series/species_ts.csv")
          write_data(env_ts, "data/summarized/time_series/ocean_ts.csv")

        } else if (i == "splot") {
          req(rx$df_sp, rx$df_env)

          if (is.null(rx$params$splot_params$method)) rx$params$splot_params$method <- "nearest_time"
          if (is.null(rx$params$splot_params$time_window)) {rx$params$splot_params$time_window <- default_max_hours_diff}
          if (is.null(rx$params$splot_params$dist_window)) {rx$params$splot_params$dist_window <- default_max_meters_diff}

          data <- rx$df_splot %||%
            prep_splot(rx$df_sp, rx$df_env, "mean",
                       method = rx$params$splot_params$method,
                       max_hours_diff  = rx$params$splot_params$time_window,
                       max_meters_diff = rx$params$splot_params$dist_window)

          write_data(data, "data/summarized/scatterplot.csv")

        } else if (i == "dprof") {
          # df_dprof is only built once the Depth Profile tab is opened with a
          # transect selected; skip gracefully (don't crash the whole download).
          if (is.null(rx$df_dprof) || length(rx$df_dprof) < 2) {
            showNotification(paste(
              "Depth Profile data isn't ready — open the Depth Profile tab and pick",
              "a transect, then re-download. Skipping it for now."),
              type = "warning", duration = NULL)
          } else {
            write_data(rx$df_dprof[[1]], "data/summarized/depth_profile/species_dprof.csv")
            write_data(rx$df_dprof[[2]], "data/summarized/depth_profile/env_dprof.csv")
          }
        }
      }
      })  # withProgress

      readme_path <- file.path(zip_root, "README.md")

      params <- isolate(rx$params)

      # Create a YAML-friendly copy
      yaml_params <- params
      # Coerce date_range to ISO strings if they are Dates
      if (inherits(yaml_params$date_range, "Date")) {
        yaml_params$date_range <- as.character(yaml_params$date_range)
      }

      yaml_block <- yaml::as.yaml(yaml_params)

      body_lines <- c(
        "# CalCOFI Download",
        "",
        "This archive contains data filtered with the following criteria:",
        "",
        glue::glue("- Taxa: {paste(params$taxa, collapse = ', ')}"),
        glue::glue("- Environmental variable: {params$env_var}"),
        glue::glue(
          "- Quarters: {paste(params$sel_qtr %||% params$quarters, collapse = ', ')}"),
        glue::glue("- Date range: {params$date_range[1]} to {params$date_range[2]}"),
        glue::glue("- Depth range (m): {params$depth_range[1]}–{params$depth_range[2]}"),
        glue::glue(
          "- Include children: {params$ck_children %||% params$include_children}"),
        glue::glue(
          "- Spatial filter (zones): {if (is.null(params$zones))
       'All locations' else paste(params$zones, collapse = ', ')}"
        ),
        glue::glue("- Integrated join time window (hours): {params$time_window}"),
        glue::glue("- Integrated join distance window (m): {params$dist_window}"),
        glue::glue("- Map env statistic: {params$map_params$env_stat}"),
        glue::glue("- Time series resolution: {params$ts_params$ts_res}"),
        glue::glue(
          "- Scatterplot matching: method = {params$splot_params$method}, ",
          "time_window = {params$splot_params$time_window} hours, ",
          "dist_window = {params$splot_params$dist_window} m"
        ),
        glue::glue("- Depth profile transect: {params$dprof_params$transect %||% 'NA'}"),
        glue::glue("- Depth profile buffer (km): {params$dprof_params$buffer %||% 'NA'}"),
        "",
        "## Bundle layout",
        "",
        "- `data/original/` — raw species + environmental observations",
        paste(
          "  Species rows carry the raw `tally` (count), the tow effort it is",
          "standardized by (`tow_type`, `std_haul_factor`, `prop_sorted`,",
          "`volume_sampled`), and the resulting `cpue` (catch per unit effort /",
          "density) with its `cpue_unit`: **count/10m²** for oblique & vertical",
          "tows (C1, CB, CV, PV; cpue = tally × std_haul_factor / prop_sorted) and",
          "**count/100m³** for manta surface tows (MT; cpue = tally / prop_sorted /",
          "volume_sampled × 100)."),
        "- `data/summarized/` — aggregated map / time-series / scatterplot / depth-profile data",
        "- `data/integrated/` — species matched to environment in time + space",
        "- `query/` — the **exact, portable SQL** behind each file, plus",
        "  `manifest.json` and `REPRODUCE.md`",
        "",
        paste(
          "If you included the integrated data, see **`query/REPRODUCE.md`** to",
          "re-run the same queries against the public CalCOFI release parquet in",
          "DuckDB (CLI, Python or R) and get identical rows.")
      )

      md <- c(
        "---",
        yaml_block,
        "---",
        "",
        body_lines
      )
      writeLines(md, readme_path)

      litedown::mark(readme_path)
      paths <- c(paths, "README.md", "README.html")

      zip::zip(zipfile = file, files = paths, root = zip_root, include_directories = TRUE)

      # overall download log — one row per Download click. Flagged `timeout` (an
      # error state) when the total server build exceeded the budget, since the
      # user almost certainly never received the zip. See dl_budget_s above.
      .dl_over   <- dl_elapsed() > dl_budget_s
      do.call(trk, c(
        list("download_bundle"), trk_filters(isolate(rx$params)),
        list(products = all_sel, n_files = length(paths),
             n_rows = length(paths), ms = dl_elapsed() * 1000,
             status = if (.dl_over) "timeout" else "ok",
             error  = if (.dl_over) sprintf(
               "total download build %.0fs (> %.0fs budget); client likely disconnected",
               dl_elapsed(), dl_budget_s) else "")))
      }, error = function(e) {
        emsg <- conditionMessage(e)
        if (!nzchar(emsg)) emsg <- "download aborted (a required input was not available)"
        do.call(trk, c(
          list("download_bundle"), trk_filters(isolate(rx$params)),
          list(products = all_sel, n_files = length(paths),
               n_rows = length(paths), ms = dl_elapsed() * 1000,
               status = "error", error = emsg)))
        showNotification(paste("Download failed:", emsg), type = "error",
                         duration = NULL)
        stop(e)  # re-raise so the browser gets a clean error, not a partial zip
      })
    },
    contentType = "application/zip"
  )
}
