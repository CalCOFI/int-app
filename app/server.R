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
  # hexagons vs a named boundary layer — which layers people actually summarize
  # within is the signal for whether more are worth adding to the registry
  observeEvent(input$sel_agg_unit,
               trk("select_agg_unit", agg_unit = input$sel_agg_unit),
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
  # (also accepts false/0/no; the brand contract's rule, cc_tour_enabled()) —
  # handy for clean screenshots; see the db-viz-hex recipe in
  # CalCOFI.github.io/shots.yml
  if (is_tour_on) {
    observeEvent(TRUE, {
      if (calcofi4r::cc_tour_enabled()) {
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
    # Map-view bookkeeping lives HERE and not in rx$params, deliberately.
    # reactiveValues dependencies are per NAME: output$map reads
    # rx$params$sel_qtr, so it depends on `params` as a whole, and any write to
    # rx$params$map_params$* re-renders the compare widget. That is what made
    # the polygon summary vanish the instant it was drawn — apply_poly() added
    # the layers by proxy, then set rx$params$map_params$agg_unit, which
    # rebuilt the widget in hex mode on the same flush.
    agg_unit       = "hex",
    env_stat       = "mean",
    # WKT of the active spatial filter (drawn polygon or grid zones), NULL for
    # none — the h3t tile SQL needs it, and it is part of the env tile cache key
    spatial_wkt    = NULL,
    env_tile_key   = NULL,  # hash of the filters the env tile URL was built from
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

  # map builders ----
  # ONE definition each, called by BOTH the startup preload and the Submit
  # handler. They were written out twice and the copies drifted: Submit never
  # grew an h3t branch, so the first Submit of a session silently downgraded the
  # species side to the classic 10-resolution sf path — ~4 s of server work
  # shipping an 88 MB widget to the browser — while the environmental side
  # beside it kept drawing h3t tiles. The h3t path costs one /h3t/stats call
  # (~0.5 s) and sends a URL.
  #
  # `spec$scales` is a length-10 list indexed by resolution because that is what
  # the zoom observer's legend lookup expects; h3t colors every zoom from one
  # scale, so it hands over the same scale ten times.

  sp_map_spec <- function(df_sp, sel_name, sel_qtr, sel_date_range,
                          ck_children, datasets = NULL, poly_wkt = NULL,
                          is_dark = TRUE) {
    if (USE_H3T) {
      # Resolve taxa HERE and hand the taxon_keys to the SQL builder, so the
      # tiles filter on exactly what get_sp() filtered on — same children walk
      # (ITIS for birds, WoRMS otherwise), same dataset checkboxes, same
      # spatial filter.
      ids   <- resolve_sp_ids(sel_name, ck_children)
      sql   <- build_sp_sql(ids$taxon_keys, sel_qtr, sel_date_range,
                            datasets = datasets, poly_wkt = poly_wkt)
      stats <- fetch_h3t_stats(sql, H3T_RELEASE)
      if (debug) { message("sp stats:"); print(stats) }
      scale <- build_h3t_scale(stats, palette = \(n) hcl.colors(n, palette = "Viridis"))
      list(
        map       = map_sp_h3t(h3t_tile_url(sql, H3T_RELEASE), scale, is_dark = is_dark),
        layer_ids = "sp",
        scales    = rep(list(scale), length(res_range)))
    } else {
      hex_list <- prep_sp_hex(df_sp, res_range)
      scales   <- lapply(hex_list, interpolate_palette, column = "sp.value",
                         palette = \(n) hcl.colors(n, palette = "Viridis"))
      list(
        map       = map_sp(hex_list, scales, is_dark = is_dark),
        layer_ids = paste0("sp", res_range),
        scales    = scales)
    }
  }

  # h3t only: the environmental side is assembled into a widget inside
  # output$map (it needs the stat/variable labels), so this returns the two
  # pieces that depend on the filters rather than a finished map.
  env_tile_spec <- function(env_var, sel_qtr, sel_date_range, sel_depth_range,
                            env_stat, poly_wkt = NULL) {
    sql   <- build_env_sql(env_var, sel_qtr, sel_date_range, sel_depth_range,
                           stat = env_stat, poly_wkt = poly_wkt)
    stats <- fetch_h3t_stats(sql, H3T_RELEASE)
    if (debug) { message("env stats:"); print(stats) }
    scale <- build_h3t_scale(stats, palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
    list(tile_url = h3t_tile_url(sql, H3T_RELEASE), scale = scale)
  }

  # Everything the env tile URL depends on. output$map rebuilds the URL when
  # this changes and reuses it when it does not. Previously the URL was cached
  # under `env_stat == "mean"` alone and rx$env_tile_url was only ever written
  # at startup — so submitting a DIFFERENT environmental variable relabeled the
  # legend while the tiles kept showing the old one, with nothing to see it by.
  env_tile_key <- function(env_stat) rlang::hash(list(
    rx$env_var, rx$params$sel_qtr, rx$params$date_range,
    rx$params$depth_range, env_stat, rx$spatial_wkt))

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

        # the theme the page opened in (?theme= / cookie); isolated so a later toggle
        # restyles via the dark_toggle observer instead of rebuilding the map
        spec <- sp_map_spec(df_sp, sel_name, sel_qtr, sel_date_range, ck_children,
                            is_dark = isolate(calcofi4r::cc_is_dark(input)))
        rx$map_sp       <- spec$map
        rx$sp_layer_ids <- spec$layer_ids
        rx$sp_scale     <- spec$scales

        env <- env_tile_spec(sel_env_var, sel_qtr, sel_date_range,
                             sel_depth_range, env_stat)
        rx$env_tile_url     <- env$tile_url
        rx$env_scale_single <- env$scale
        rx$env_scale        <- rep(list(env$scale), length(res_range))

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
        rx$sp_layer_ids <- paste0("sp", res_range)
        rx$sp_scale     <- sp_scale_list
        rx$env_hex_list <- env_hex_list
      }

      # store shared data (both paths)
      rx$df_sp       <- df_sp
      # unit breakdown alongside the data it describes, so the legend and the
      # sidebar note can never disagree with what is mapped
      rx$sp_units    <- sp_unit_summary(df_sp)
      rx$df_env      <- df_env
      rx$env_var     <- sel_env_var
      rx$lbl_env_var <- env_var_label(sel_env_var)
      rx$params$taxa        <- sel_name
      rx$params$env_var     <- sel_env_var
      rx$params$sel_qtr     <- sel_qtr
      rx$params$date_range  <- sel_date_range
      rx$params$depth_range <- sel_depth_range
      rx$params$ck_children <- ck_children

      # stamp the key AFTER rx$params is populated (it hashes those fields), so
      # output$map reuses the tile URL just fetched instead of fetching it again
      if (USE_H3T) rx$env_tile_key <- env_tile_key(env_stat)

      rx$filter_summary <- prep_filter_summary(
        sel_name, sel_env_var, sel_qtr, sel_date_range,
        sel_depth_range, drawn_polygon = NULL, rx$sel_zones, ck_children)

      rx$plot_depth <- NULL

      if (debug) message("=== DEFAULT DATA LOADED ===\n")
    }, error = function(e) {
      message("ERROR in default data initialization: ", conditionMessage(e))
      traceback()
    })

    # Release the map render, once, now that everything it reads exists. This
    # sits OUTSIDE the tryCatch on purpose: if the preload fails, the map should
    # still be renderable once a Submit provides data, rather than stay disabled
    # for the life of the session.
    map_ready(TRUE)
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

  # plotly registers a source's events only when the plot is RENDERED, and the
  # Scatterplot tab is hidden at startup (Shiny suspends hidden outputs), so
  # calling event_data() before then warns once per event per flush:
  #   "The 'plotly_click' event tied a source ID of 'scatterPlotSource' is not
  #    registered."
  # This gates the two listeners on the plot existing. event_register() on the
  # plot covers the other half — without it plotly never wires the JS handlers
  # up at all, so a click would silently deliver nothing.
  splot_ready <- reactiveVal(FALSE)

  # map ----
  # This render has EXACTLY TWO reactive dependencies, by design: map_ready()
  # and map_rebuild(). Everything else it touches is isolated.
  #
  # It used to depend on rx$df_env, rx$map_sp, rx$env_tile_url, rx$env_var,
  # rx$lbl_env_var and rx$env_scale_single. The preload observer sets all six in
  # sequence and `session$onFlushed` flipped map_ready independently, so the
  # widget was invalidated repeatedly while an earlier render was still in
  # flight — rebuilding an expensive compare widget several times per startup
  # and producing three client errors on every page load:
  #   "sent a progress message for map, but the output is in an unexpected
  #    state of: running" / "'map' is recalculating, but ... 'idle'" /
  #   "'map' has been recalculated, but ... 'idle'"
  # Those were Shiny's output state machine reporting overlapping recalculation
  # cycles for one output, which is exactly what was happening.
  #
  # map_ready is now flipped by the preload itself, once, AFTER everything the
  # render reads exists — so the render runs once at startup and once per
  # explicit rebuild, and there is no window where it can run half-fed.
  map_ready <- reactiveVal(FALSE)

  # Explicit trigger for a FULL widget rebuild. `input$sel_env_stat` used to be a
  # direct dependency of this render, which meant a polygon summary could be
  # wiped out by an env-stat change re-rendering the widget underneath it. Now
  # the env-stat observer decides: rebuild in hex mode (unchanged behavior), or
  # go through the proxy in polygon mode.
  map_rebuild <- reactiveVal(0)

  output$map <- renderMaplibreCompare({
    map_rebuild()
    req(map_ready())

    isolate({

    req(rx$df_env, rx$map_sp)

    if (debug) message("renderMaplibreCompare: generating environmental map...\n")

    env_stat       <- isolate(input$sel_env_stat) %||% "mean"
    env_stat_label <- names(which(env_stat_choices == env_stat))

    # The aggregation unit is read WITHOUT taking a reactive dependency: the
    # polygon summary is applied to the live maps by `apply_poly()` below,
    # through the compare proxy, never by re-rendering this widget. See the
    # comment on apply_poly() for why re-rendering is not an option.
    rx$agg_unit <- isolate(input$sel_agg_unit) %||% "hex"

    # Name the legend from what the data actually holds. "(density)" was false
    # for anything not gear-standardized; a bare "CPUE" was still wrong for a raw
    # occurrence count, since nothing was divided by effort. The breakdown goes in
    # the sidebar note — a legend title cannot carry it and should not try.
    #
    # Reads rx$sp_units, set beside rx$df_sp in BOTH observers that load a
    # selection. It is not computed here: `df_sp` is a local of the startup
    # observer above, out of scope in this render block, and computing it here
    # silently produced the empty-summary label on every render.
    rx$lbl_sp_value <- sp_value_label(rx$sp_units)

    if (USE_H3T) {
      # h3t path: reuse the tile_url + scale from the preload while the filters
      # behind them are unchanged, refetch when any of them moves.
      #
      # The condition used to be `is.null(rx$env_tile_url) || env_stat != "mean"`
      # and the rebuild branch never wrote rx$env_tile_url back — so after
      # startup set it once, the URL was frozen. A Submit that picked a
      # different environmental variable, date range or depth range updated
      # rx$lbl_env_var (the legend) and left the tiles showing temperature.
      key <- env_tile_key(env_stat)
      if (is.null(rx$env_tile_url) || !identical(key, rx$env_tile_key)) {
        env <- env_tile_spec(
          rx$env_var,
          isolate(rx$params$sel_qtr), isolate(rx$params$date_range),
          isolate(rx$params$depth_range), env_stat, rx$spatial_wkt)
        rx$env_tile_url     <- env$tile_url
        rx$env_scale_single <- env$scale
        rx$env_scale        <- rep(list(env$scale), length(res_range))
        rx$env_tile_key     <- key
      }
      map_env_obj <- map_env_h3t(rx$env_tile_url, rx$env_scale_single,
                                 env_stat_label, rx$lbl_env_var,
                                 is_dark = isolate(calcofi4r::cc_is_dark(input)))
      rx$params$map_params$env_stat <- env_stat
      rx$env_stat <- env_stat
      # the hex layer IDs actually on the map, so the polygon switch can hide
      # exactly those and no others — set_layout_property is NOT guarded against
      # a missing layer on the client, it throws
      rx$env_layer_ids <- "env"
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
      rx$lbl_env_var,
      is_dark = isolate(calcofi4r::cc_is_dark(input)))

    rx$env_scale <- env_scale_list
    rx$params$map_params$env_stat <- env_stat
    rx$env_stat <- env_stat
    rx$env_layer_ids <- paste0("env", res_range)

    if (debug) {
      message("renderMaplibreCompare: creating comparison map")
      message("rx$map_sp class: ", paste(class(rx$map_sp), collapse = ", "))
      message("map_env_obj class: ", paste(class(map_env_obj), collapse = ", "))
    }

    compare(rx$map_sp, map_env_obj, elementId = "map")

    })  # isolate
  })

  # summarize within polygons ----
  # The polygon summary is applied to the LIVE maps through the compare proxy
  # rather than by re-rendering `output$map`.
  #
  # Re-rendering does work — but it rebuilds both maps from scratch, which
  # throws away the viewport and every layer toggle the user has set, for what
  # is a change of one overlay. The proxy touches only the layers that change.
  #
  # Two traps cost a lot of time here and are both load-bearing:
  #   * over the proxy, a layer's source must be registered with add_source()
  #     FIRST and referenced by id — see add_poly_side();
  #   * nothing in this block may write to rx$params — see the note on the
  #     rx$agg_unit / rx$env_stat fields.
  # Each produced the same symptom, an empty map with no error anywhere.
  POLY_IDS <- list(
    before = c("sp_poly",  "sp_poly_nodata",  "sp_poly_nodata_hit"),
    after  = c("env_poly", "env_poly_nodata", "env_poly_nodata_hit"))

  hex_ids_for <- function(side)
    if (side == "before") rx$sp_layer_ids else rx$env_layer_ids

  # Layer removal, not visibility: mapgl's own layers control decides what is
  # listed, so a layer that is not on the map simply stops being offered.
  # `clear_layer()` on a compare proxy was a no-op until the fix in
  # bbest/mapgl (the R side sends `layer`, both compare handlers read
  # `message.layer_id`), which is why the hexagons stayed put underneath the
  # first polygon summary and a second switch would have thrown "Layer with id
  # sp_poly already exists". Needs mapgl >= that commit.
  remove_layers <- function(side, ids) {
    p <- maplibre_compare_proxy("map", map_side = side)
    for (id in ids) p <- p |> clear_layer(id)
    invisible(p)
  }

  clear_poly_layers <- function() {
    for (side in c("before", "after")) remove_layers(side, POLY_IDS[[side]])
  }

  # Remove the hex layers rather than setting visibility "none": on the client
  # `clear_layer` is wrapped in `if (map.getLayer(id))` but `set_layout_property`
  # is NOT, so hiding a layer that failed to add throws and takes the rest of the
  # proxy message batch with it. That is not hypothetical — the h3t tile layers
  # are absent whenever the tile service or its custom protocol is unavailable,
  # which is exactly when we would be reaching for them blind.
  clear_hex_layers <- function() {
    for (side in c("before", "after")) {
      ids <- hex_ids_for(side)
      if (!is.null(ids)) remove_layers(side, ids)
    }
  }

  # Add one side's polygon layers: outline + hoverable wash for the unsampled
  # polygons, fill for the summarized ones.
  #
  # The source is registered with add_source() FIRST and referenced by id, rather
  # than passing the sf straight to add_*_layer(). Over the compare proxy those
  # are not equivalent: add_source() serializes the sf to a GeoJSON string that
  # the client's "add_source" handler understands, while add_layer() forwards
  # `source` to map.addLayer() untouched — so an inline sf arrives as
  # `{geojson: …}`, maplibre accepts it as a geojson source spec, and the layer
  # renders NOTHING with no error (verified: querySourceFeatures() == 0 while the
  # layer itself was present on both maps).
  #
  # Each layer gets its OWN source under the same id because clear_layer()
  # removes the layer and the identically-named source together; two layers
  # sharing one source id would leave the second pointing at nothing.
  add_poly_side <- function(side, sf_poly, d_val, scale, is_dark) {
    ids    <- POLY_IDS[[side]]
    sf_all <- sf_poly |> left_join(d_val |> select(-spatial_name), by = "spatial_key")
    # only what the tooltip and the colour expression read: POSIXct columns have
    # no GeoJSON representation and the dates are already in the tooltip string
    keep   <- \(x) x |> select(any_of(c("spatial_key", "spatial_name", "value",
                                        "n", "tooltip")))
    sf_dat <- sf_all |> filter(!is.na(value)) |> keep()
    sf_nul <- sf_all |>
      filter(is.na(value)) |>
      mutate(tooltip = paste0("<strong>", spatial_name, "</strong><br>no data")) |>
      keep()
    p <- \() maplibre_compare_proxy("map", map_side = side)

    if (nrow(sf_nul) > 0) {
      p() |> add_source(id = ids[3], data = sf_nul)
      p() |> add_fill_layer(
        id            = ids[3],
        source        = ids[3],
        fill_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        fill_opacity  = 0.08,
        tooltip       = "tooltip",
        hover_options = list(fill_opacity = 0.25))
      p() |> add_source(id = ids[2], data = sf_nul)
      p() |> add_line_layer(
        id            = ids[2],
        source        = ids[2],
        line_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        line_width    = 1,
        line_opacity  = 0.6,
        hover_options = list(line_color = "#ffeb3b", line_opacity = 1))
    }
    if (nrow(sf_dat) > 0 && !is.null(scale)) {
      p() |> add_source(id = ids[1], data = sf_dat)
      p() |> add_fill_layer(
        id                 = ids[1],
        source             = ids[1],
        fill_color         = scale$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.65,
        tooltip            = "tooltip",
        hover_options      = list(fill_outline_color = "#ffeb3b",
                                  fill_opacity       = 0.85))
    }
    nrow(sf_dat)
  }

  apply_poly <- function(agg_unit, env_stat) {
    req(rx$df_sp, rx$df_env)
    is_dark <- (input$dark_toggle %||% "dark") == "dark"

    sf_poly <- get_layer_sf(agg_unit)
    if (is.null(sf_poly)) {
      showNotification(paste0("No geometry available for '", agg_unit, "'."),
                       type = "error")
      updateSelectInput(session, "sel_agg_unit", selected = "hex")
      return(invisible(NULL))
    }

    sp_poly  <- calcofi4r::cc_track_query(session, "map_query_sp_poly",
      list(layer = agg_unit, taxa = rx$params$taxa),
      prep_sp_poly(rx$df_sp, agg_unit))
    env_poly <- calcofi4r::cc_track_query(session, "map_query_env_poly",
      list(layer = agg_unit, env_var = rx$env_var, env_stat = env_stat),
      prep_env_poly(rx$df_env, agg_unit, env_stat))

    rx$df_sp_poly  <- sp_poly
    rx$df_env_poly <- env_poly
    rx$n_poly      <- nrow(sf_poly)

    sp_scale  <- poly_scale(sp_poly$data,
      palette = \(n) hcl.colors(n, palette = "Viridis"))
    env_scale <- poly_scale(env_poly,
      palette = \(n) rev(hcl.colors(n, palette = "Spectral")))

    if (is.null(sp_scale) && is.null(env_scale))
      showNotification(paste0(
        "No observations fall within any polygon of '", agg_unit,
        "' for the current filters."), type = "warning")

    clear_poly_layers()
    clear_hex_layers()
    add_poly_side("before", sf_poly, sp_poly$data, sp_scale,  is_dark)
    add_poly_side("after",  sf_poly, env_poly,     env_scale, is_dark)

    # Rebuild the floating layers control so it lists what is actually on the
    # map: the boundary layer being summarized (named after itself, not
    # "Polygon Summary"), and no "Hexagon Data" entry, because the hexagons are
    # gone. Each side gets only its own ids — a control cannot reach the other
    # map, and the ui.R mirror script handles the cross-map half.
    for (side in c("before", "after")) {
      ctrl <- build_layers_control(
        rx$spatial_visible, d_spatial_layers, POLY_IDS[[side]],
        label = agg_unit)
      maplibre_compare_proxy("map", map_side = side) |>
        clear_controls(controls = "layers") |>
        add_layers_control(
          position     = "top-right",
          layers       = ctrl,
          collapsible  = TRUE,
          margin_right = 45)
    }

    # a polygon summary has no zoom-dependent scale, so hand the legend observer
    # the same one at every level (the h3t path does the same)
    rx$sp_scale     <- rep(list(sp_scale),  length(res_range))
    rx$env_scale    <- rep(list(env_scale), length(res_range))
    rx$lbl_sp_value <- if (!is.na(sp_poly$unit))
      paste0("Avg. CPUE (", sp_poly$unit, ")") else "Avg. CPUE"
    rx$agg_unit <- agg_unit
    rx$env_stat <- env_stat

    # fit to the polygons that HAVE data, so switching unit lands where the
    # observations are rather than at the full extent of a statewide layer. This
    # also fires the moveend the legend observer listens for.
    sf_dat <- sf_poly |>
      inner_join(sp_poly$data |> select(spatial_key), by = "spatial_key")
    if (nrow(sf_dat) == 0) sf_dat <- sf_poly
    for (side in c("before", "after"))
      maplibre_compare_proxy("map", map_side = side) |> fit_bounds(bbox = sf_dat)

    invisible(NULL)
  }

  # Returning to hexagons drops the polygon layers and rebuilds the widget, which
  # is the only path that knows how to reconstruct BOTH hex paths (h3t tile
  # source, or ten sf resolution layers) — apply_poly() removed them outright
  # rather than hiding them, for the reason given on clear_hex_layers().
  restore_hex <- function() {
    clear_poly_layers()
    # reuse the breakdown computed for the current selection rather than a fixed
    # string — the polygon path may have narrowed the label to its one chosen unit
    rx$lbl_sp_value <- sp_value_label(rx$sp_units %||%
                                        sp_unit_summary(req(rx$df_sp)))
    rx$agg_unit <- "hex"
    map_rebuild(map_rebuild() + 1)
  }

  observeEvent(input$sel_agg_unit, {
    agg_unit <- input$sel_agg_unit %||% "hex"
    if (debug) message("sel_agg_unit -> ", agg_unit)
    if (agg_unit == "hex") restore_hex()
    else apply_poly(agg_unit, input$sel_env_stat %||% "mean")
  }, ignoreInit = TRUE)

  # env_stat: in polygon mode recompute through the proxy; in hex mode rebuild
  # the widget, which is exactly what this control did before.
  observeEvent(input$sel_env_stat, {
    agg_unit <- input$sel_agg_unit %||% "hex"
    if (agg_unit == "hex") map_rebuild(map_rebuild() + 1)
    else apply_poly(agg_unit, input$sel_env_stat %||% "mean")
  }, ignoreInit = TRUE)

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

    # Rebuild the layers control on both sides, each with only ITS OWN data
    # layer ids. Handing every side both sides' ids is why the data toggle only
    # worked one way: the control lists e.g. "env" on the species map, where no
    # such layer exists, and the client's set_layout_property is NOT guarded by
    # `if (map.getLayer(id))` — so switching the group back ON throws and the
    # toggle dies half-applied. The ids come from what was actually added to
    # each map (h3t: "sp"/"env"; classic: sp1..sp10 / env1..env10), not from a
    # hardcoded classic-path guess.
    for (side in c("before", "after")) {
      ctrl <- build_layers_control(
        selected, d_spatial_layers,
        if (side == "before") rx$sp_layer_ids else rx$env_layer_ids)
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
    # In hex mode the title is unit-free: CPUE is count/10m² for oblique and
    # vertical tows but count/100m³ for manta, and the hexagon value averages
    # across net types (per-tow unit is in the download). The polygon path picks
    # ONE cpue_unit and names it here instead — rx$lbl_sp_value carries whichever
    # applies. A NULL scale means nothing was summarizable, so draw no legend
    # rather than an empty one.
    if (!is.null(sp_scale)) {
      maplibre_compare_proxy("map", map_side = "before") |>
        add_legend(
          legend_title = rx$lbl_sp_value %||% "Avg. CPUE",
          values       = round(sp_scale$breaks, 2),
          colors       = sp_scale$colors,
          type         = "continuous",
          position     = "bottom-left",
          width        = "275px",
          target       = "compare",
          style        = legend_style(background_opacity = 0.5),
          add          = FALSE
        )
    }

    # Environmental legend (right / after)
    if (!is.null(env_scale)) {
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
    }
  })

  # poly_note ----
  # What the polygon summary actually covers: how many of the layer's polygons
  # carry data, which cpue_unit the species side settled on, and how many
  # observations that excluded. The unit choice is not cosmetic — std_tally is a
  # gear-standardized density only where a net tow supports it — so it is stated
  # in the sidebar rather than left to be inferred from the legend.
  output$poly_note <- renderUI({
    agg_unit <- input$sel_agg_unit %||% "hex"

    # HEX MODE: the legend can only name the quantity; it cannot say that two
    # rows under it were measured differently. `std_tally` is a gear-standardized
    # density only where a net tow supports it — Pacific sardine from an oblique
    # tow is count/10m2, computed as tally x std_haul_factor / prop_sorted —
    # whereas Dungeness megalopae are an occurrence count in a lab-examined
    # aliquot of an archived catch, with no tow volume to divide by. Both used to
    # render under one legend reading "Avg. CPUE (density)", which was false for
    # the second and unverifiable for the first. So state it here, from the data.
    if (agg_unit == "hex") {
      u <- rx$sp_units
      if (is.null(u) || !nrow(u)) return(NULL)
      n_tot <- sum(u$n)
      return(div(
        class = "small text-muted mb-3",
        if (nrow(u) > 1) div(
          class = "fw-semibold",
          sprintf("Heads up: this selection mixes %d units, and the hexagon value averages across them.",
                  nrow(u))),
        div(
          class = "mt-1",
          lapply(seq_len(nrow(u)), function(i) div(
            sprintf("%s — %s obs (%.0f%%), %s",
                    u$cpue_unit[i], format(u$n[i], big.mark = ","),
                    100 * u$n[i] / n_tot,
                    if (isTRUE(u$standardized[i]))
                      "standardized by tow effort"
                    else "as published by the source, not effort-standardized")))),
        if (any(!u$standardized)) div(
          class = "mt-1 fst-italic",
          "Rows that are not effort-standardized are not catch-per-unit-effort: ",
          "no tow volume or haul factor exists for them, so the published value ",
          "is shown as-is. Compare those only with each other.")))
    }

    req(rx$df_sp_poly)

    sp_poly <- rx$df_sp_poly
    n_with  <- nrow(sp_poly$data)
    n_tot   <- rx$n_poly %||% n_with

    div(
      class = "small text-muted mb-3",
      div(sprintf("%s of %s polygons contain observations; the rest are drawn as outlines marked “no data”.",
                  format(n_with, big.mark = ","), format(n_tot, big.mark = ","))),
      if (!is.na(sp_poly$unit)) div(
        class = "mt-1",
        sprintf("Species values are averaged in %s.", sp_poly$unit),
        if (sp_poly$n_excluded > 0) sprintf(
          " %s observation(s) in %d other unit(s) are excluded — averaging across units is not a quantity.",
          format(sp_poly$n_excluded, big.mark = ","), nrow(sp_poly$units) - 1L))
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

    # the package's plot_ts (calcofi4r >= 1.10.0), which no longer reaches for
    # the app-global env_var_choices: hand it the label
    calcofi4r::plot_ts(
      sp_ts, env_ts, ts_res, rx$env_var,
      is_dark   = calcofi4r::cc_is_dark(input),
      env_label = env_var_label(rx$env_var))
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
          "<br><b>CPUE:</b> ", round(sp_tally, 2) ))

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
    p_out <- ggplotly(p, tooltip = "text", source = "scatterPlotSource") |>
      layout(dragmode = "select") |>
      config(
        displaylogo            = FALSE,
        scrollZoom             = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian") ) |>
      # Declare the events the observers below consume. Two observers call
      # event_data("plotly_click" / "plotly_selected", source =
      # "scatterPlotSource"), and plotly warns once per unregistered event on
      # every render because it only wires up the JS handlers for events the
      # plot has registered — without these, clicking a point could silently
      # deliver nothing.
      event_register("plotly_click") |>
      event_register("plotly_selected") |>
      toWebGL() # for performance

    splot_ready(TRUE)
    p_out
  })

  # sel_data -> modal_data(), spatial_filter_map ----
  observeEvent(input$sel_data, {
    # carry BOTH the variable and the dataset narrowing across a reopen; the
    # modal is rebuilt from scratch each time, so anything not passed here silently
    # resets to its default the next time the user opens the filters
    showModal(modal_data(
      env_var = rx$env_var %||% "temperature",
      bio_ds  = isolate(input$sel_bio_ds)))
    updateSelectizeInput(
      session, "sel_name",
      choices  = sp_names_for(isolate(input$sel_bio_ds)),
      selected = isolate(rx$params$taxa),
      server   = TRUE)

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

  # dataset -> taxa list ----
  # Narrowing the datasets narrows the taxa offered. The current selection is
  # carried over where it survives, so unchecking an unrelated dataset does not
  # silently clear what the user already picked.
  observeEvent(input$sel_bio_ds, {
    keep <- intersect(input$sel_name, sp_names_for(input$sel_bio_ds))
    updateSelectizeInput(
      session, "sel_name",
      choices  = sp_names_for(input$sel_bio_ds),
      selected = keep,
      server   = TRUE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # show-all -> environmental variable list ----
  # No dataset filter on this tab: every measurement type belongs to exactly one
  # dataset, so the grouped list already carries that information.
  observeEvent(input$sel_env_all_vars, {
    ch  <- env_var_choices(show_all = isTRUE(input$sel_env_all_vars))
    cur <- input$sel_env_var
    # keep the current variable if its dataset is still checked, else fall back
    # to the first on offer rather than leaving a value the list no longer has
    sel <- if (!is.null(cur) && cur %in% unlist(ch)) cur else unlist(ch)[1]
    updateSelectInput(session, "sel_env_var", choices = ch, selected = sel)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
      get_sp(sel_name, sel_qtr, sel_date_range, ck_children,
             datasets = input$sel_bio_ds))
    df_env <- calcofi4r::cc_track_query(session, "map_query_env",
      list(env_var = sel_env_var, quarters = sel_qtr, date_beg = sel_date_range[1],
           date_end = sel_date_range[2], depth_min = sel_depth_range[1],
           depth_max = sel_depth_range[2]),
      get_env(sel_env_var, sel_qtr, sel_date_range, sel_depth_range[1], sel_depth_range[2]))

    # Apply spatial filter based on priority: drawn polygon > selected zones > all data.
    # The WKT is kept in `spatial_wkt` (and on rx) so the h3t tile SQL can apply
    # the SAME constraint — the tiles are a separate query against a separate
    # service, so a filter applied only to the dbplyr tables would leave the map
    # showing observations the plots beside it exclude.
    spatial_wkt <- NULL
    if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
      spatial_wkt <- st_as_text(drawn_polygon$geometry[[1]])

    } else if (!is.null(rx$sel_zones) && length(rx$sel_zones) > 0) {
      spatial_wkt <- cc_grid_zones |>
        filter(zone_key %in% rx$sel_zones) |>
        pull(geom) |>
        st_union() |>
        st_as_text()
    }
    rx$spatial_wkt <- spatial_wkt

    if (!is.null(spatial_wkt)) {
      df_sp <- df_sp |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", spatial_wkt, "'))"
        )))

      df_env <- df_env |>
        filter(sql(paste0(
          "ST_Within(ST_Point(lon_dec, lat_dec), ST_GeomFromText('", spatial_wkt, "'))"
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
      # reopened on an empty result, so it must come back with what the user
      # actually chose — resetting it here hides the filter that emptied it
      showModal(modal_data(env_var = sel_env_var, bio_ds = input$sel_bio_ds))
      return(NULL)
    }

    # store shared data (still lazy tables)
    rx$df_sp       <- df_sp
    rx$sp_units    <- sp_unit_summary(df_sp)
    rx$df_env      <- df_env
    rx$env_var     <- sel_env_var
    rx$lbl_env_var <- env_var_label(sel_env_var)

    rx$params$taxa        <- sel_name
    rx$params$env_var     <- sel_env_var
    rx$params$sel_qtr     <- sel_qtr
    rx$params$date_range  <- sel_date_range
    rx$params$depth_range <- sel_depth_range
    rx$params$zones       <- rx$zones
    rx$params$ck_children <- ck_children
    # so the download README and the usage log record which datasets the
    # numbers came from, not just which taxa
    rx$params$bio_datasets <- input$sel_bio_ds
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
      ck_children,
      bio_datasets = input$sel_bio_ds)

    # build summary stats
    rx$summary_stats <- prep_summary_stats(
      rx$df_sp,
      rx$df_env
    )

    # generate map
    if (debug) message("Generating species map...\n")
    spec <- sp_map_spec(
      df_sp, sel_name, sel_qtr, sel_date_range, ck_children,
      datasets = input$sel_bio_ds, poly_wkt = spatial_wkt,
      is_dark  = input$dark_toggle == "dark")
    rx$map_sp       <- spec$map
    rx$sp_layer_ids <- spec$layer_ids
    # rx$sp_scale was NOT updated here, in either path — so after a Submit the
    # species legend kept redrawing the breaks of the STARTUP selection every
    # time the zoom observer fired.
    rx$sp_scale     <- spec$scales
    if (debug) message("Species map generated and stored in rx$map_sp\n")

    # output$map no longer depends on rx$map_sp (it isolates everything but its
    # two triggers), so a new selection has to ask for the rebuild explicitly.
    map_rebuild(map_rebuild() + 1)

    # prepare scatterplot data
    df_splot <- prep_splot(df_sp, df_env, "mean")
    rx$df_splot <- df_splot

    # reset depth profile
    rx$plot_depth <- NULL

    removeModal()
  })

  # plotly_click -> ... ----
  observeEvent(
    {
      req(splot_ready())
      event_data("plotly_click", source = "scatterPlotSource")
    }, {
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
            "<b>CPUE:</b> ", round(clicked_point$sp_tally, 2)
          )
        )
    })
  })

  observeEvent(
    {
      req(splot_ready())
      event_data("plotly_selected", source = "scatterPlotSource")
    }, {
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
            "<br><b>CPUE:</b> ", round(selected_points$sp_tally, 2)
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
          env_stat <- rx$env_stat %||% rx$params$map_params$env_stat
          agg_unit <- rx$agg_unit %||% "hex"

          if (agg_unit != "hex") {
            # "Map data" must be the data the map is showing. Recomputed rather
            # than lifted from rx so the CSV cannot lag a filter change, and it
            # is cheap (~0.05s) next to everything else in this bundle.
            sp_poly  <- prep_sp_poly(rx$df_sp, agg_unit)
            env_poly <- prep_env_poly(rx$df_env, agg_unit, env_stat)

            write_data(
              sp_poly$data |>
                select(-tooltip) |>
                mutate(layer = agg_unit, cpue_unit = sp_poly$unit),
              "data/summarized/map/species_polygon.csv")
            write_data(
              env_poly |>
                select(-tooltip) |>
                mutate(layer = agg_unit, env_stat = env_stat),
              "data/summarized/map/env_polygon.csv")

            # the units this summary had to leave out, so the CSV is not the
            # only record that a choice was made
            write_data(
              sp_poly$units |> mutate(summarized = cpue_unit == sp_poly$unit),
              "data/summarized/map/species_polygon_units.csv")

          } else {
            # agg_*, not prep_* — the aggregate WITHOUT the hexagon polygons.
            # Joining them cost a 5.6 s / 153 MB read of hex.geojson and then
            # wrote an sfc column that write.csv() renders as an R literal
            # (`list(c(-113.6, ..., 16.5))`), not WKT — so the geometry column
            # was unreadable to every tool a CSV is opened in. `hexid` is an H3
            # index: a reader recovers the polygon from it with h3_cell_to_boundary().
            sp_hex  <- agg_sp_hex(rx$df_sp, res_range) |> select(-tooltip)
            env_hex <- agg_env_hex(rx$df_env, res_range, env_stat) |> select(-tooltip)

            write_data(sp_hex , "data/summarized/map/species_map.csv")
            write_data(env_hex, "data/summarized/map/env_map.csv")
          }

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
          "volume_sampled × 100). Where the gear does not support standardization",
          "— no `tow_type` or no `std_haul_factor` — `cpue` is the value the source",
          "published, in its own `cpue_unit`, and is NOT a density: cdfw_dungeness-crab",
          "is occurrence in a lab-examined aliquot of an archived catch, and the",
          "euphausiid / ZooScan series publish their own per-area units. Read",
          "`cpue_unit` before comparing rows."),
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
