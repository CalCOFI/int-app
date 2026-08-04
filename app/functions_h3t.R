# h3t tile-source companions to the sf-based map builders in functions.R.
# these are only used when USE_H3T is on (set via the H3T_USE env var or via
# the app UI later). the sf-based path in functions.R remains the fallback.

# ---------------------------------------------------------------------- paths

# default base URL for the h3t API. override with env var for deploys.
h3t_base_url <- function() {
  Sys.getenv("H3T_BASE_URL", "http://127.0.0.1:8889/h3t")
}

# ------------------------------------------------------------------- sql build

# species SELECT: projects (cell_id, value, n) from bio_obs. res_placeholder
# is a literal that the server will substitute per tile based on zoom; we use
# a fixed resolution for now (configurable via the `res` arg).
build_sp_sql <- function(sp_name, qtr, date_range, include_children = TRUE) {
  # note: emits `h3_cell_to_parent(hex_id, {{res}})` with the literal placeholder
  # `{{res}}`. the h3t API substitutes `{{res}}` with the tile's effective H3
  # resolution (derived from zoom) before parsing, so one cached SQL string
  # serves every zoom level. the parent cell (UBIGINT, < 2^63) casts to BIGINT
  # as before — the API's cell_id contract is unchanged. requires `LOAD h3`.
  hex_col <- DBI::SQL("h3_cell_to_parent(hex_id, {{res}})")
  base_sql <- glue::glue_sql(
    "SELECT {hex_col} AS cell_id, AVG(std_tally) AS value, COUNT(*) AS n
       FROM bio_obs
      WHERE std_tally IS NOT NULL
        AND scientific_name = {sp_name}
        AND quarter IN ({qtr*})
        AND time_start BETWEEN {as.character(date_range[1])} AND {as.character(date_range[2])}
      GROUP BY 1",
    .con = DBI::ANSI(), sp_name = sp_name, qtr = qtr,
    date_range = date_range, hex_col = hex_col)

  if (isTRUE(include_children)) {
    base_sql <- glue::glue_sql(
      "WITH RECURSIVE children AS (
         SELECT worms_id FROM species WHERE scientific_name = {sp_name}
         UNION ALL
         SELECT t.taxonID::INTEGER FROM taxon t JOIN children c ON t.parentNameUsageID = CAST(c.worms_id AS VARCHAR)
       )
       SELECT {hex_col} AS cell_id, AVG(std_tally) AS value, COUNT(*) AS n
         FROM bio_obs
        WHERE std_tally IS NOT NULL
          AND worms_id IN (SELECT worms_id FROM children)
          AND quarter IN ({qtr*})
          AND time_start BETWEEN {as.character(date_range[1])} AND {as.character(date_range[2])}
        GROUP BY 1",
      .con = DBI::ANSI(), sp_name = sp_name, qtr = qtr,
      date_range = date_range, hex_col = hex_col)
  }

  as.character(base_sql)
}

# env SELECT: projects (cell_id, value, n) from env_obs at a fixed resolution.
build_env_sql <- function(measurement_type, qtr, date_range, depth_range,
                          stat = c("mean", "median", "min", "max", "sd")) {
  stat <- match.arg(stat)
  hex_col <- DBI::SQL("h3_cell_to_parent(hex_id, {{res}})")
  agg <- switch(stat,
    mean   = "AVG(qty)",
    median = "MEDIAN(qty)",
    min    = "MIN(qty)",
    max    = "MAX(qty)",
    sd     = "STDDEV_SAMP(qty)"
  )
  sql <- glue::glue_sql(
    "SELECT {hex_col} AS cell_id, {DBI::SQL(agg)} AS value, COUNT(*) AS n
       FROM env_obs
      WHERE qty IS NOT NULL AND NOT isnan(qty) AND isfinite(qty)
        AND measurement_type = {measurement_type}
        AND quarter IN ({qtr*})
        AND datetime_utc BETWEEN {as.character(date_range[1])} AND {as.character(date_range[2])}
        AND depth_m BETWEEN {depth_range[1]} AND {depth_range[2]}
      GROUP BY 1",
    .con = DBI::ANSI(), measurement_type = measurement_type, qtr = qtr,
    date_range = date_range, depth_range = depth_range, hex_col = hex_col)
  as.character(sql)
}

# -------------------------------------------------------- URL / stats helpers

h3t_b64 <- function(sql) {
  # URL-safe base64 (RFC 4648 §5): swap + → -, / → _, strip padding '='
  raw <- charToRaw(sql)
  b64 <- base64enc::base64encode(raw)
  b64 <- chartr("+/", "-_", b64)
  gsub("=+$", "", b64)
}

h3t_tile_url <- function(sql, release = "", base = h3t_base_url()) {
  q <- h3t_b64(sql)
  qs <- paste0("q=", q)
  if (nzchar(release)) qs <- paste0(qs, "&release=", utils::URLencode(release, reserved = TRUE))
  # replace http(s) prefix with h3tiles:// so MapLibre dispatches to the custom protocol
  host_path <- sub("^https?://", "", base)
  sprintf("h3tiles://%s/{z}/{x}/{y}.h3t?%s", host_path, qs)
}

# pull min/max (and p02/p98) across the whole user SQL via /h3t/stats
fetch_h3t_stats <- function(sql, release = "", base = h3t_base_url(),
                            timeout_s = 5) {
  q <- h3t_b64(sql)
  url <- paste0(
    sub("/+$", "", base), "/stats?q=", q,
    if (nzchar(release)) paste0("&release=", utils::URLencode(release, reserved = TRUE)) else ""
  )
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout_s) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) >= 400) return(NULL)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# build a single mapgl color-scale (matching the existing interpolate_palette
# shape) from stats $min/$max and a palette function.
build_h3t_scale <- function(stats, palette = \(n) hcl.colors(n, "Viridis"),
                            n_stops = 5L, column = "value") {
  if (is.null(stats) || is.null(stats$min) || is.null(stats$max) ||
      !is.finite(stats$min) || !is.finite(stats$max) || stats$min == stats$max) {
    # degenerate: one flat color, no interpolation
    cols <- palette(2)
    return(list(
      breaks     = c(stats$min %||% 0, stats$max %||% 1),
      colors     = cols,
      expression = cols[1]
    ))
  }
  # clamp to p02..p98 when available to avoid outlier domination
  lo <- if (!is.null(stats$p02) && is.finite(stats$p02)) stats$p02 else stats$min
  hi <- if (!is.null(stats$p98) && is.finite(stats$p98)) stats$p98 else stats$max
  if (lo >= hi) { lo <- stats$min; hi <- stats$max }
  breaks <- seq(lo, hi, length.out = n_stops)
  colors <- palette(n_stops)
  list(
    breaks     = breaks,
    colors     = colors,
    expression = mapgl::interpolate(column = column, values = breaks, stops = colors)
  )
}

# ------------------------------------------------------------- map builders

map_sp_h3t <- function(tile_url, scale, bbox = c(-125, 30, -115, 38),
                       is_dark = TRUE) {
  m <- mapgl::maplibre(
    style = mapgl::carto_style(ifelse(is_dark, "dark-matter", "voyager")),
    center = c(mean(bbox[c(1,3)]), mean(bbox[c(2,4)])), zoom = 5
  ) |>
    mapgl::fit_bounds(bbox = bbox) |>
    mapgl::add_scale_control(position = "top-left", unit = "metric") |>
    mapgl::add_navigation_control()

  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  m <- m |> add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  m <- m |>
    mapgl::add_h3t_source(
      id          = "sp",
      tiles       = tile_url,
      sourcelayer = "sp"
    ) |>
    mapgl::add_fill_layer(
      id                 = "sp",
      source             = "sp",
      source_layer       = "sp",
      fill_color         = scale$expression,
      fill_outline_color = "white",
      fill_opacity       = 0.7,
      tooltip            = "value"
    )

  # this map's own layer id only — a control listing "env" here throws when
  # toggled back on, since that layer lives on the other map (see map_sp())
  ctrl <- build_layers_control(vis_ids, d_spatial_layers, "sp")
  m |> mapgl::add_layers_control(
    position = "top-right", layers = ctrl, collapsible = TRUE, margin_right = 45
  )
}

map_env_h3t <- function(tile_url, scale, env_stat_label, env_var_label,
                        bbox = c(-125, 30, -115, 38), is_dark = TRUE) {
  m <- mapgl::maplibre(
    style = mapgl::carto_style(ifelse(is_dark, "dark-matter", "voyager")),
    center = c(mean(bbox[c(1,3)]), mean(bbox[c(2,4)])), zoom = 5
  ) |>
    mapgl::fit_bounds(bbox = bbox) |>
    mapgl::add_scale_control(position = "top-left", unit = "metric") |>
    mapgl::add_navigation_control()

  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  m <- m |> add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  m <- m |>
    mapgl::add_h3t_source(
      id          = "env",
      tiles       = tile_url,
      sourcelayer = "env"
    ) |>
    mapgl::add_fill_layer(
      id                 = "env",
      source             = "env",
      source_layer       = "env",
      fill_color         = scale$expression,
      fill_outline_color = "white",
      fill_opacity       = 0.7,
      tooltip            = "value"
    )

  ctrl <- build_layers_control(vis_ids, d_spatial_layers, "env")
  m |> mapgl::add_layers_control(
    position = "top-right", layers = ctrl, collapsible = TRUE, margin_right = 45
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
