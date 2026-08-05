# h3t tile-source companions to the sf-based map builders in functions.R.
# these are only used when USE_H3T is on (set via the H3T_USE env var or via
# the app UI later). the sf-based path in functions.R remains the fallback.

# ---------------------------------------------------------------------- paths

# default base URL for the h3t API. override with env var for deploys.
h3t_base_url <- function() {
  Sys.getenv("H3T_BASE_URL", "http://127.0.0.1:8889/h3t")
}

# ------------------------------------------------------------------- sql build

# Both builders emit `h3_cell_to_parent(hex_id, {{res}})` with the literal
# placeholder `{{res}}`. The h3t API substitutes `{{res}}` with the tile's
# effective H3 resolution (derived from zoom) before parsing, so one cached SQL
# string serves every zoom level. The parent cell (UBIGINT, < 2^63) casts to
# BIGINT — the API's cell_id contract is unchanged. Requires `LOAD h3`.
HEX_COL <- DBI::SQL("h3_cell_to_parent(hex_id, {{res}})")

# Point-in-polygon clause for a spatial filter (drawn polygon or grid zones),
# in the same shape the dbplyr path uses in server.R. The h3t service runs
# DuckDB with `spatial` loaded, so ST_Within is available there too — verified
# against /h3t/stats. lon/lat columns differ between the two tables.
poly_clause <- function(poly_wkt, lon_col, lat_col) {
  if (is.null(poly_wkt) || !nzchar(poly_wkt)) return(NULL)
  glue::glue_sql(
    "ST_Within(ST_Point({DBI::SQL(lon_col)}, {DBI::SQL(lat_col)}),
                ST_GeomFromText({poly_wkt}))",
    .con = DBI::ANSI())
}

#' Species tile SELECT: projects (cell_id, value, n) from bio_obs.
#'
#' Takes the ALREADY-RESOLVED id sets from \code{resolve_sp_ids()} rather than
#' resolving taxa itself. It used to take one display name and walk the
#' hierarchy in a recursive CTE, which meant three ways to disagree with the
#' rest of the app, all of them silent:
#'   * only the FIRST selected taxon reached the tiles (glue_sql interpolates a
#'     length-n vector into `scientific_name = …`, not an IN list);
#'   * ITIS-only taxa (seabirds, marine mammals) have no WoRMS id, so the CTE
#'     matched nothing for them while `get_sp()` matched them by name;
#'   * the dataset checkboxes and the spatial filter were not applied at all.
#' Filtering on exactly what `get_sp()` filtered on makes map and tables agree
#' by construction.
build_sp_sql <- function(worms_ids, sci_names, qtr, date_range,
                         datasets = NULL, poly_wkt = NULL) {
  taxa <- if (length(worms_ids) > 0 && length(sci_names) > 0) {
    glue::glue_sql("(worms_id IN ({worms_ids*}) OR scientific_name IN ({sci_names*}))",
                   .con = DBI::ANSI())
  } else if (length(worms_ids) > 0) {
    glue::glue_sql("worms_id IN ({worms_ids*})", .con = DBI::ANSI())
  } else if (length(sci_names) > 0) {
    glue::glue_sql("scientific_name IN ({sci_names*})", .con = DBI::ANSI())
  } else {
    # nothing resolves; an empty IN () is a SQL error, so say so explicitly
    DBI::SQL("FALSE")
  }

  where <- c(
    "std_tally IS NOT NULL",
    as.character(taxa),
    as.character(glue::glue_sql("quarter IN ({qtr*})", .con = DBI::ANSI())),
    as.character(glue::glue_sql(
      "time_start BETWEEN {as.character(date_range[1])} AND {as.character(date_range[2])}",
      .con = DBI::ANSI())),
    if (!is.null(datasets) && length(datasets) > 0)
      as.character(glue::glue_sql("dataset_key IN ({datasets*})", .con = DBI::ANSI())),
    as.character(poly_clause(poly_wkt, "longitude", "latitude")))

  as.character(glue::glue_sql(
    "SELECT {hex_col} AS cell_id, AVG(std_tally) AS value, COUNT(*) AS n
       FROM bio_obs
      WHERE {DBI::SQL(paste(where, collapse = ' AND '))}
      GROUP BY 1",
    .con = DBI::ANSI(), hex_col = HEX_COL))
}

# env SELECT: projects (cell_id, value, n) from env_obs.
build_env_sql <- function(measurement_type, qtr, date_range, depth_range,
                          stat = c("mean", "median", "min", "max", "sd"),
                          poly_wkt = NULL) {
  stat <- match.arg(stat)
  agg <- switch(stat,
    mean   = "AVG(qty)",
    median = "MEDIAN(qty)",
    min    = "MIN(qty)",
    max    = "MAX(qty)",
    sd     = "STDDEV_SAMP(qty)"
  )

  where <- c(
    "qty IS NOT NULL AND NOT isnan(qty) AND isfinite(qty)",
    as.character(glue::glue_sql("measurement_type = {measurement_type}", .con = DBI::ANSI())),
    as.character(glue::glue_sql("quarter IN ({qtr*})", .con = DBI::ANSI())),
    as.character(glue::glue_sql(
      "datetime_utc BETWEEN {as.character(date_range[1])} AND {as.character(date_range[2])}",
      .con = DBI::ANSI())),
    as.character(glue::glue_sql(
      "depth_m BETWEEN {depth_range[1]} AND {depth_range[2]}", .con = DBI::ANSI())),
    as.character(poly_clause(poly_wkt, "lon_dec", "lat_dec")))

  as.character(glue::glue_sql(
    "SELECT {hex_col} AS cell_id, {DBI::SQL(agg)} AS value, COUNT(*) AS n
       FROM env_obs
      WHERE {DBI::SQL(paste(where, collapse = ' AND '))}
      GROUP BY 1",
    .con = DBI::ANSI(), hex_col = HEX_COL))
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
