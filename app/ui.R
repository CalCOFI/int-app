# the About panel is the same on every page load — render the markdown once at
# startup rather than per request, now that the UI is built per request
about_html <- HTML(mark(here("app/about.md"), output = NA))

# ui is a FUNCTION of the request, not a static object, for one reason: the
# client IP. shiny-server does not proxy the websocket upgrade — it opens a
# fresh localhost connection to the R worker — so the server session sees
# REMOTE_ADDR 127.0.0.1 and no X-Forwarded-For (Caddy sets it correctly; it is
# lost at the shiny-server hop). This page request is the only one that still
# carries the real address, so it is captured here and baked into the analytics
# snippet.
ui <- function(req) page_sidebar(
  window_title = "CalCOFI.io Integrated Database Application",
  title = tagList(
    span(
      a(
        img(src = "./logo_calcofi.svg",       height="50px",
            class = "intapp-logo-dark",  .noWS = "after"),
        img(src = "./logo_calcofi_light.svg", height="50px",
            class = "intapp-logo-light", .noWS = "after"),
        href = "https://calcofi.io"),
      "Integrated App",
      # Which frozen database release everything on screen came from. In the
      # TITLE rather than the sidebar or the About tab because it has to survive
      # a collapsed sidebar and every tab switch: a figure someone screenshots
      # out of here is only reproducible if the release travelled with it.
      span(class = "intapp-release",
           title = paste0(
             "CalCOFI database release ", DB_RELEASE,
             " — every value shown comes from this frozen release"),
           DB_RELEASE))),

  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "logo_calcofi.svg"),
    # usage tracking: GA4 (aggregate) + a batched beacon to the usage-log Sheet
    # (per-query detail). Both legs are sent by the BROWSER, so no reactive ever
    # performs network I/O — server-side facts reach it via calcofi4r::cc_track()
    # over the websocket the session already has open. The Sheet leg is a silent
    # no-op unless CALCOFI_LOG_URL is set (global.R), so local dev writes nothing.
    calcofi4r::cc_ga_head("db-viz-hex", app_version = APP_VERSION,
                          ip = calcofi4r::cc_client_ip(req)),
    tags$style(HTML("
    /* swap the logo variant based on the page's bslib theme — the
       original SVG has WHITE 'CalCOFI.io' text, hidden on light bg. */
    [data-bs-theme='light'] .intapp-logo-dark  { display: none; }
    [data-bs-theme='dark']  .intapp-logo-light { display: none; }

    /* release tag in the title: present, never competing with it. Sized off
       the title's own font so it tracks the responsive header, and set in the
       tabular figures the version string deserves. */
    .intapp-release {
      font-size: 0.55em;
      font-weight: 400;
      opacity: 0.5;
      margin-left: 0.45rem;
      vertical-align: 0.35em;
      white-space: nowrap;
      font-variant-numeric: tabular-nums;
      letter-spacing: 0.01em;
      cursor: help;
    }
    .intapp-release:hover { opacity: 0.8; }
    .treeview {
      list-style: none;
      padding-left: 0.1rem;
      margin: 0;
      margin-top: 0;
      font-size: 0.9rem;
    }

    .treeview ul {
      list-style: none;
      margin: 0;
      padding-left: 1.1rem;
    }

    .treeview li {
      position: relative;
      margin: 0.15rem 0;
      padding: 0;
    }

    .tree-label {
      font-weight: 400;
      cursor: pointer;
      display: block;
      padding-left: 1.0rem; /* space for triangle icon */
    }

    /* Flex layout inside the label for name vs obs */
    .tree-label-inner {
      display: flex;
      justify-content: space-between;
      align-items: baseline;
      gap: 0.5rem;
      width: 100%;
    }

    /* Name and obs styling */
    .tree-name i {
      font-style: italic;
    }

    .tree-obs {
      white-space: nowrap;
    }

    /* Checkbox used as toggle (invisible) */
    .tree-toggle {
      position: absolute;
      left: 0;
      top: 0.15rem;
      width: 1rem;
      height: 1rem;
      opacity: 0;
      cursor: pointer;
    }

    /* Triangles for branches only */
    .tree-branch > .tree-label::before {
      content: '\\25B8'; /* right-pointing triangle */
      position: absolute;
      left: 0;
      top: 0;
      width: 1rem;
      text-align: center;
      font-size: 1.0rem;
    }

    /* Expanded state */
    .tree-branch > .tree-toggle:checked + .tree-label::before {
      content: '\\25BE'; /* down-pointing triangle */
    }

    /* Children visibility */
    .tree-branch > .tree-toggle + .tree-label + ul {
      display: none;
    }
    .tree-branch > .tree-toggle:checked + .tree-label + ul {
      display: block;
    }

    /* Hover state for consistency with other sidebar text */
    .tree-label:hover {
      /* color: #000; */
    }

    /* --- Data Selection modal ---------------------------------------- */
    /* Shiny gives .shiny-input-container a fixed 300px width, which in a wide
       modal wrapped every dataset checkbox onto a second line and left the
       Taxa box a narrow stub in a mostly empty dialog. Let the controls use
       the width the modal already has. */
    .modal .shiny-input-container {
      width: 100%;
      max-width: 100%;
    }
    /* one dataset per line, with the checkbox aligned to the first line of a
       label that may still wrap on a narrow window */
    .modal .shiny-input-checkboxgroup .checkbox label {
      display: flex;
      align-items: baseline;
      gap: 0.4rem;
    }

    /* The dataset heading in the Variable picker is the only thing
       distinguishing `nitrite` (Bottle) from `btl_nitrite` (CTD Cast), so it
       has to be legible — it renders muted gray by default.

       selectInput() is selectize = TRUE by default, so this is a
       div.optgroup-header inside .selectize-dropdown, NOT an <optgroup>
       element. Styling `select optgroup` had no effect on it whatsoever; that
       rule is kept only for any genuinely native select. */
    .selectize-dropdown .optgroup-header,
    select optgroup {
      font-weight: 600 !important;
      font-style: normal;
      opacity: 1;
    }
    [data-bs-theme='dark'] .selectize-dropdown .optgroup-header,
    [data-bs-theme='dark'] select optgroup {
      color: #f8f9fa !important;
      background-color: #343a40 !important;
    }
    [data-bs-theme='light'] .selectize-dropdown .optgroup-header,
    [data-bs-theme='light'] select optgroup {
      color: #212529 !important;
      background-color: #e9ecef !important;
    }
    /* separate one dataset's variables from the next */
    .selectize-dropdown .optgroup + .optgroup .optgroup-header {
      border-top: 1px solid rgba(128, 128, 128, 0.4);
    }

    /* darken maplibre tooltip and popup text for readability */
    .maplibregl-popup-content,
    .mapboxgl-popup-content {
      color: #1a1a1a !important;
      font-weight: 500;
    }

    /* --- small-screen / phone fixes ---------------------------------- */
    /* paired with sidebar(open='desktop'): the sidebar now collapses behind
       a toggle on phones instead of stacking above the map. as a belt-and-
       suspenders guard, guarantee the map keeps a usable height even if the
       bslib fillable height chain under-resolves on a mobile browser (the
       map wrapper relies on height:100% inside a flex column). */

    /* hint to OPEN the sidebar: hidden everywhere by default, shown only
       while the sidebar is collapsed on a phone (its mirror — the 'collapse
       to view the map' note — lives inside the sidebar via d-sm-none). */
    .mobile-open-hint { display: none; align-items: center; gap: 0.5rem; }

    @media (max-width: 575.98px) {
      #map { min-height: 70vh; }
      #map .maplibregl-map,
      #map .mapboxgl-map { min-height: 70vh; }

      .bslib-sidebar-layout.sidebar-collapsed .mobile-open-hint { display: flex; }
    }
    ")) ),

  # Make the floating layers control drive BOTH sides of the compare widget.
  #
  # mapgl's control is deliberately per-map, and in a swipe compare both maps are
  # full-size and stacked, so the two controls land on the same screen position
  # and only the top one (the "after"/right map's) is clickable. The left map's
  # control is underneath, unreachable — so a toggle appeared to work on half the
  # map and nowhere else.
  #
  # This wraps _setVisibility rather than the click handler, so it rides on top
  # of whatever the control decides, including its own `map.getLayer()` guard.
  # Layer ids are side-specific ("sp_poly" vs "env_poly", "sp3" vs "env3") and
  # are mapped across; the shared PMTiles boundary ids are identical on both maps
  # and pass through unchanged. If mapgl ever grows a first-class option for
  # this (add_layers_control(sync_compare = TRUE)), drop this in favor of it.
  tags$script(HTML("
    (function () {
      function counterpart(id) {
        if (/^sp($|[0-9_])/.test(id))  return 'env' + id.slice(2);
        if (/^env($|[0-9_])/.test(id)) return 'sp'  + id.slice(3);
        return id;   // shared overlay: same id on both maps
      }
      function patch() {
        if (!window.MapglLayersControl) return false;
        if (window.__ccLayersMirrorPatched) return true;
        window.__ccLayersMirrorPatched = true;
        var proto = window.MapglLayersControl.prototype;
        var orig  = proto._setVisibility;
        proto._setVisibility = function (layerId, visibility) {
          orig.call(this, layerId, visibility);
          try {
            var el = this._map && this._map.getContainer();
            if (!el || !window.HTMLWidgets) return;
            var m = /^(.*)-(before|after)$/.exec(el.id);
            if (!m) return;
            var inst = HTMLWidgets.find('#' + m[1]);
            if (!inst || !inst.getBeforeMap) return;
            var other = m[2] === 'before' ? inst.getAfterMap() : inst.getBeforeMap();
            var oid = counterpart(layerId);
            if (other && other.getLayer(oid))
              other.setLayoutProperty(oid, 'visibility', visibility);
          } catch (e) {
            // mirroring is a convenience; never let it break the real toggle
          }
        };
        return true;
      }
      if (!patch()) {
        var tries = 0;
        var t = setInterval(function () {
          if (patch() || ++tries > 80) clearInterval(t);
        }, 250);
      }
    })();
  ")),

  useConductor(),
  useBusyIndicators(spinners = TRUE, fade = TRUE),

  # sidebar ----
  sidebar = sidebar(
    width = 300,
    # open = "desktop": sidebar open on desktop, but COLLAPSIBLE behind a
    # toggle on phones. the bslib default resolves to open-mobile="always",
    # which forces a stacked "flow" layout that (a) hides the sidebar toggle
    # and (b) pushes the map below the filters, collapsing it to a sliver.
    open  = "desktop",

    # mobile-only hint: on phones the sidebar opens as a full-screen overlay,
    # so tell users how to collapse it back to the map. hidden on desktop
    # (>= 576px) via Bootstrap's d-sm-none; only shows while the panel is open.
    div(
      class = "d-sm-none alert alert-info d-flex align-items-center gap-2 py-2 px-2 mb-2 small",
      role  = "note",
      bs_icon("arrow-up-left"),
      span("Collapse this panel with the ", tags$strong("toggle at the top-left"),
           " to view the map.") ),

    conditionalPanel(
      "input.outputPanel === 'Map'",
      actionButton("btn_layers", "Map Layers",
                   width = "100%", class = "mb-2",
                   icon = icon("layer-group")) ),

    # action buttons
    actionButton("sel_data", "Select Filters", width = "100%", class = "mb-2", icon = icon("filter")),

    conditionalPanel(
      "input.outputPanel === 'Scatterplot'",
      p("Click on a point or use the box/lasso tool to select points to see their location.",
        class = "small text-muted mt-2")),

    conditionalPanel(
      "input.outputPanel === 'Depth Profile'",
      actionButton("open_transect_modal", "Draw Transect",
                   width = "100%", class = "mt-2") ),

    accordion(
      accordion_panel(
        "Filter Summary",
        uiOutput("filter_summary") ),

      accordion_panel(
        "Summary Statistics",
        uiOutput("summary_statistics"),
        uiOutput("taxa_tree") ),

      conditionalPanel(
        "input.outputPanel === 'Map' || input.outputPanel === 'Time Series' || input.outputPanel === 'Scatterplot'",
        accordion_panel(
          "Plot Options",

          conditionalPanel(
            "input.outputPanel === 'Map'",
            selectInput(
              "sel_agg_unit",
              tagList(
                "Summarize Within",
                popover(
                  bs_icon("question-circle"),
                  HTML("Choose the unit the map aggregates into.<br>
                        <strong>Hexagons (H3)</strong> bins observations into
                        equal-area cells that get finer as you zoom in.<br>
                        A <strong>boundary layer</strong> instead reports one
                        value per polygon — per MPA, per county, per ecoregion.<br><br>
                        One layer at a time: the layers overlap (a station can sit
                        inside a county, an ecoregion <em>and</em> an MPA, and all
                        three are true), so summarizing across them would count the
                        same observation more than once.<br><br>
                        Polygons with no CalCOFI samples are drawn as an outline
                        and labelled <em>no data</em> — they are not zero."))),
              choices  = agg_unit_choices,
              selected = "hex"),
            uiOutput("poly_note"),
            selectInput(
              "sel_env_stat",
              "Environmental Summary Statistic",
              choices  = env_stat_choices,
              selected = "mean") ),

          conditionalPanel(
            "input.outputPanel === 'Time Series'",
            selectInput(
              "sel_ts_res",
              "Temporal Resolution",
              choices  = ts_res_choices,
              selected = "year") ),

          conditionalPanel(
            "input.outputPanel === 'Scatterplot'",
            numericInput("splot_max_hours_diff",
                         tagList("Time Window (Hrs.)",
                                 popover(bs_icon("question-circle"),
                                         "The default thresholds of 2 km and 6 hours to match CTD casts and net tows for larval fish have been selected to represent approximate spatial and temporal scales within a single tidal cycle and over which water masses are expected to remain broadly similar (although this assumption may vary depending on proximity to coastal fronts or distance from shore). The 2 km threshold also falls within the typical range of potential station drift and/or delay between CTD casts and net tows during sampling operations. Additionally, all stations are spaced farther apart than 2km.")),
                                 value = default_max_hours_diff,  min = 0, max = 72),
            numericInput("splot_max_meters_diff",
                         tagList("Distance Window (m)",
                                 popover(bs_icon("question-circle"),
                                         "The default thresholds of 2 km and 6 hours to match CTD casts and net tows for larval fish have been selected to represent approximate spatial and temporal scales within a single tidal cycle and over which water masses are expected to remain broadly similar (although this assumption may vary depending on proximity to coastal fronts or distance from shore). The 2 km threshold also falls within the typical range of potential station drift and/or delay between CTD casts and net tows during sampling operations. Additionally, all stations are spaced farther apart than 2km.")),
                         value = default_max_meters_diff, min = 0, max = 5000),
            selectInput("splot_method",
                        tagList("Join Method",
                                popover(bs_icon("question-circle"),
                                        HTML("Specify how <strong>environmental observations</strong> within the time and distance windows should be joined to the species observations.<br>
                                              <strong>Single Nearest Cast:</strong> Averages observations in the nearest cast (in time or distance).<br>
                                              <strong>Average Within Range:</strong> Averages all observations within the chosen window.")) ),
                        c("Single nearest cast (by time)"      =  "nearest_time",
                          "Single nearest cast (by distance)"  =  "nearest_dist",
                          "Average within range"               =  "average"),
                        selected = "nearest_time"))))) ),

  # main content ----
  # mobile-only hint: when the sidebar is collapsed on a phone (the default),
  # the filters/layers are hidden, so point users to the toggle. lives in the
  # main area (the sidebar content is hidden while collapsed) and is shown only
  # for .sidebar-collapsed on small screens — see the media query in tags$head.
  div(
    class = "mobile-open-hint alert alert-info py-2 px-2 mb-2 small",
    role  = "note",
    bs_icon("arrow-up-left"),
    span("Tap the ", tags$strong("toggle at the top-left"),
         " to open Filters & Layers.") ),

  navset_card_underline(
    id = "outputPanel",
    height = "100%",

    nav_panel(
      "Map",
      div(
        style = "width: 100%; height: 100%; position: relative;",
        maplibreCompareOutput("map", width = "100%", height = "100%")) ),

    nav_panel(
      "Time Series",
      uiOutput("ts_content") ),

    nav_panel(
      "Scatterplot",
      uiOutput("splot_content") ),

    nav_panel(
      "Depth Profile",
      uiOutput("dprof_content") ),

    nav_spacer(),

    nav_item(
      input_dark_mode(id = "dark_toggle", mode = "dark") ),

    nav_panel(
      "Download",
      markdown(
        "Select the datasets you'd like to download, then click **Download**.
         The bundle is organized into `data/original/`, `data/summarized/` and
         `data/integrated/`, each paired with the exact, portable SQL in a
         `query/` folder (`manifest.json`, per-file `*.sql`, `REPRODUCE.md`) so
         the integrated bio↔env match can be re-run anywhere against the
         public CalCOFI release parquet."),
      checkboxGroupInput(
        "sel_raw_data_download",
        "Raw datasets",
        c(
          "Raw environmental data"    =  "raw_env",
          "Raw species data"          =  "raw_sp",
          "Integrated data + reproducible SQL
           (species matched to environment;
           adds the query/ folder)"   =  "int"),
        width = "100%"),
      conditionalPanel(
        condition = "input.sel_raw_data_download.includes('int')",
        div(
          style = "margin-top: 8px; padding-left: 20px; border-left: 3px solid #337ab7;",
          div(
            div(
              style = "display: inline-block; margin-right: 20px;",
                numericInput(
                  "time_window",
                  "Time window (hours)",
                  value = default_max_hours_diff,
                  min   = 0,
                  width = 200) ),
            div(
              style = "display: inline-block;",
              numericInput(
                "dist_window",
                "Distance window (meters)",
                value = default_max_meters_diff,
                min   = 0,
                width = 200
              ) ) ),
          helpText(tagList("These windows define the tolerance for joining environmental and species data.",
                           popover(bs_icon("question-circle"),
                                   "The default thresholds of 2 km and 6 hours to match CTD casts and net tows for larval fish have been selected to represent approximate spatial and temporal scales within a single tidal cycle and over which water masses are expected to remain broadly similar (although this assumption may vary depending on proximity to coastal fronts or distance from shore). The 2 km threshold also falls within the typical range of potential station drift and/or delay between CTD casts and net tows during sampling operations. Additionally, all stations are spaced farther apart than 2km.")))
        )
      ),
      checkboxGroupInput(
        "sel_proc_data_download",
        "Processed datasets (the aggregated data used for the visualizations)",
        c(
          "Map data"                  =  "map",
          "Time-series data"          =  "ts",
          "Scatterplot data"          =  "splot",
          "Depth Profile data"        =  "dprof"),
        width = "100%"),
        downloadButton("download_data", "Download", class = "btn-secondary mb-2") ),

    nav_panel(
      "About",
      about_html ))
)
