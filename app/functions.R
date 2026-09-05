# data retrieval functions ----

#' Retrieve Taxon Children from Database
#'
#' Queries the taxonomy table to find all child taxa of a given taxonID,
#' using a recursive CTE. Returns a tibble with taxon details and depth levels.
#'
#' `taxonID` is a `taxon_key` (`"worms:137202"` / `"itis:1255050"`), so the walk
#' stays inside one authority's tree by construction — a key's parent is always
#' in the same authority. That is what lets seabirds expand through their ITIS
#' families and orders, which have no WoRMS ids to walk at all.
#'
#' @param taxonID Character `taxon_key` of the parent to query
#' @param con DuckDB database connection object
#' @param authority optional authority to scope to ("WoRMS"/"ITIS"); NULL (the
#'   default) walks whichever tree the key belongs to, which is what callers want
get_taxon_children <- function(taxonID, con, authority = NULL) {

  query_sql <- glue("
    WITH RECURSIVE taxon_children AS (
      -- Base case: find the parent taxon
      SELECT
        taxonID,
        acceptedNameUsageID,
        parentNameUsageID,
        scientificName,
        taxonRank,
        0 as depth_level
      FROM taxon
      WHERE taxonID = ?

      UNION ALL

      -- Recursive case: find children taxa
      SELECT
        t.taxonID,
        t.acceptedNameUsageID,
        t.parentNameUsageID,
        t.scientificName,
        t.taxonRank,
        tc.depth_level + 1 as depth_level
      FROM taxon t
      INNER JOIN taxon_children tc ON t.parentNameUsageID = tc.taxonID
      WHERE
        t.parentNameUsageID IS NOT NULL
        {if (is.null(authority)) '' else glue(\"AND t.authority = '{authority}'\")}
    )
    SELECT tc.*, COALESCE(tr.rank_order, 99) as rank_order
    FROM taxon_children tc
    LEFT JOIN taxa_rank tr ON tc.taxonRank = tr.taxonRank
    ORDER BY tc.depth_level, COALESCE(tr.rank_order, 99), tc.scientificName")

  dbGetQuery(con, query_sql, params = list(taxonID)) |>
    tibble()
}


get_taxon_parentage <- function(taxonID, con, authority = "WoRMS"){

  query_sql <- glue("
    WITH RECURSIVE taxon_hierarchy AS (
      -- Base case: find the initial taxon (and resolve to accepted if it's a synonym)
      SELECT
        taxonID,
        acceptedNameUsageID,
        parentNameUsageID,
        scientificName,
        scientificNameAuthorship,
        taxonRank,
        taxonomicStatus,
        nomenclaturalStatus,
        namePublishedInYear,
        0 as level
      FROM taxon
      WHERE taxonID = ?

      UNION ALL

      -- Recursive case: find parent taxa (using the accepted parent)
      SELECT
        t.taxonID,
        t.acceptedNameUsageID,
        t.parentNameUsageID,
        t.scientificName,
        t.scientificNameAuthorship,
        t.taxonRank,
        t.taxonomicStatus,
        t.nomenclaturalStatus,
        t.namePublishedInYear,
        th.level + 1 as level
      FROM taxon t
      INNER JOIN taxon_hierarchy th ON t.taxonID = th.parentNameUsageID
      -- Ensure we're getting the accepted version of the parent
      WHERE t.taxonID = t.acceptedNameUsageID  -- Only accepted taxa (not synonyms)
        AND th.level < 50  -- Safety limit
    )
    SELECT * FROM taxon_hierarchy
    ORDER BY level, taxonRank")

  dbGetQuery(con, query_sql, params = list(taxonID)) |>
    mutate(
      authority           = !!authority,
      namePublishedInYear = as.character(namePublishedInYear)) |>
    relocate(authority) |>
    tibble()
}

#' Retrieve Species Larval Abundance Data from Database
#'
#' Queries species, larva, net, tow, and site tables with temporal filters,
#' computing standardized tally values. Returns a dbplyr lazy table for
#' efficient downstream processing.
#'
#' @param sp_name Character vector of species names (format: "Common Name (Scientific Name)")
#' @param qtr Numeric vector of quarters to include (1-4)
#' @param date_range Date vector of length 2 (start date, end date)
#' @param ck_children Boolean (TRUE or FALSE) whether to include taxonomic children
#'
#' @return dbplyr lazy table with columns:
#'   \itemize{
#'     \item \code{name} - species name (common + scientific)
#'     \item \code{tally} - raw larval count
#'     \item \code{tow_type} - net gear code (C1/CB/CV/PV oblique/vertical; MT manta)
#'     \item \code{std_haul_factor}, \code{prop_sorted}, \code{volume_sampled} - tow effort
#'     \item \code{std_tally} - CPUE where the gear supports it, else the value
#'       the source published; net-type-aware, see details
#'     \item \code{cpue_unit} - \code{count/10m2} (oblique/vertical),
#'       \code{count/100m3} (manta), or the source's own unit where neither
#'       formula applies (e.g. cdfw_dungeness-crab, an occurrence count in a
#'       lab-examined aliquot, which is NOT a density)
#'     \item \code{time_start} - tow start datetime
#'     \item \code{longitude}, \code{latitude} - spatial coordinates
#'     \item \code{quarter} - quarter (1-4)
#'     \item \code{hex_id} - H3 cell index at resolution 10 (coarser resolutions
#'       derived at query time via \code{h3_cell_to_parent()})
#'   }
#'
#' @details
#' \code{std_tally} is CPUE (catch per unit effort / density), standardized by
#' net type (materialized in \code{prep_db.R::bio_obs}): oblique & vertical tows
#' (C1, CB, CV, PV) give counts per 10 m^2 via
#' \code{tally * std_haul_factor / prop_sorted}; manta surface tows (MT) give
#' counts per 100 m^3 via \code{tally / prop_sorted / volume_sampled * 100}
#' (the manta haul factor does not standardize to volume).
#' Only records with non-NA tally values are returned.
#'
#' @examples
#' \dontrun{
#' # retrieve anchovy data for all quarters 2010-2020
#' df_sp <- get_sp(
#'   sp_name    = "Anchovy (Engraulis mordax)",
#'   qtr        = 1:4,
#'   date_range = as.Date(c("2010-01-01", "2020-12-31"))
#' )
#' df_sp |> collect()
#' }
#'
#' @seealso \code{\link{prep_sp_hex}} for spatial aggregation
#' @seealso \code{\link{prep_ts_sp}} for temporal aggregation
#'
#' @importFrom dplyr tbl mutate filter left_join between
#' @importFrom lubridate quarter
#'
#' @export
#' Resolve Selected Taxa Names to the Ids `bio_obs` is Filtered On
#'
#' Turns the picker's display names into the two id sets that identify the
#' selection in \code{bio_obs}: WoRMS ids (optionally including the taxonomic
#' children) and, for taxa with no WoRMS id, scientific names.
#'
#' Split out of \code{\link{get_sp}} so the h3t tile SQL
#' (\code{build_sp_sql()}) filters on the SAME ids the tables and plots do.
#' While the tile query resolved its own taxa — with a recursive CTE, and only
#' ever for one name — the map could and did show a different set of
#' observations than the Time Series beside it.
#'
#' @param sp_name Character vector of picker labels, "Common (rank: Scientific)"
#' @param ck_children Include taxonomic children of the selected taxa
#'
#' @return list with \code{taxon_keys} (character `taxon_key`s)
#'
#' @export
resolve_sp_ids <- function(sp_name, ck_children = TRUE) {
  # Memoized: a Submit resolves the same selection twice — once for get_sp()'s
  # table query and once for the tile SQL — and the children walk runs one
  # recursive CTE PER selected taxon, so a 50-taxon selection is 50 round trips
  # each time. Keyed on the arguments; nothing below depends on session state.
  key <- rlang::hash(list(sort(sp_name), ck_children))
  if (!is.null(sp_ids_cache[[key]])) return(sp_ids_cache[[key]])
  # resolve selected names to taxon_keys via species + taxon tables
  # name format must match global.R: "Common Name (rank: Scientific Name)"
  #
  # Joined on taxon_key, not worms_id, and NOT filtered to authority WoRMS: the
  # seabirds and marine mammals key `itis:`, and scoping this join to one
  # authority is what left them with no taxonRank and no hierarchy to walk.
  sp_taxon <- tbl(con, "species") |>
    left_join(
      tbl(con, "taxon"),
      by = join_by(taxon_key == taxonID)) |>
    mutate(
      rank_part = ifelse(
        is.na(taxonRank) | taxonRank == "",
        "",
        paste0(tolower(taxonRank), ": ")),
      name_part = ifelse(
        is.na(common_name) | common_name == "",
        "",
        paste0(common_name, " ")),
      name = paste0(name_part, "(", rank_part, scientific_name, ")"))

  sel <- sp_taxon |>
    filter(name %in% sp_name) |>
    select(name, scientific_name, taxon_key) |>
    collect()

  if (ck_children) {
    # walks whichever authority's tree the key belongs to — WoRMS for most,
    # ITIS for the seabirds and marine mammals
    df_sel <- sel |>
      filter(!is.na(taxon_key)) |>
      mutate(children = map(taxon_key, get_taxon_children, con = con)) |>
      unnest(children)
    taxon_keys <- unique(df_sel$acceptedNameUsageID)
  } else {
    taxon_keys <- unique(sel$taxon_key[!is.na(sel$taxon_key)])
  }

  # One key space for both authorities, so there is no id to be missing and no
  # name-matching fallback to keep in sync. The previous version matched on
  # `worms_id` and fell back to `scientific_name` for the ITIS-keyed taxa —
  # which worked, but meant two code paths whose results had to agree, and the
  # name path could not expand children at all.
  out <- list(taxon_keys = taxon_keys[!is.na(taxon_keys)])

  sp_ids_cache[[key]] <- out
  out
}
# survives for the life of the R process; the taxonomy it reads is fixed by the
# release the app opened, so a stale entry is not possible without a restart
sp_ids_cache <- new.env(parent = emptyenv())


get_sp <- function(sp_name, qtr, date_range, ck_children = TRUE, datasets = NULL) {
  if (debug)
    message(
      "get_sp: sp_name = ", paste(sp_name, collapse = ", "),
      ", qtr = ", paste(qtr, collapse = ","),
      ", date_range = ", paste(date_range, collapse = " to "),
      ", ck_children = ", ck_children)

  ids        <- resolve_sp_ids(sp_name, ck_children)
  taxon_keys <- ids$taxon_keys

  df_sp <- tbl(con, "bio_obs")
  df_sp <- if (length(taxon_keys) > 0) {
    df_sp |> filter(taxon_key %in% taxon_keys)
  } else {
    df_sp |> filter(FALSE)   # nothing selected resolves; an empty IN () is a SQL error
  }

  # a taxon sampled by two programs keeps only the datasets asked for
  if (!is.null(datasets) && length(datasets) > 0)
    df_sp <- df_sp |> filter(dataset_key %in% datasets)

  df_sp <- df_sp |>
    filter(
      between(time_start, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    mutate(
      name = ifelse(
        is.na(common_name) | common_name == "",
        paste0("(", scientific_name, ")"),
        paste0(common_name, " (", scientific_name, ")")))

  if (debug) {
    n_rows <- df_sp |> summarize(n = n()) |> pull(n)
    message("get_sp: returning lazy table with ", n_rows, " rows")
  }

  df_sp
}


#' Retrieve Environmental Data from Database
#'
#' Queries environmental bottle cast data with temporal, depth, and variable filters.
#' Returns a dbplyr lazy table for efficient downstream processing.
#'
#' @param env_var Character string of database column name for environmental variable (e.g., "temperature", "salnty")
#' @param qtr Character or numeric vector of quarters to include (1-4)
#' @param date_range Date vector of length 2 (start date, end date)
#' @param min_depth Numeric minimum depth in meters
#' @param max_depth Numeric maximum depth in meters
#'
#' @return dbplyr lazy table with columns:
#'   \itemize{
#'     \item \code{date} - date of cast
#'     \item \code{time} - time of cast (seconds since midnight)
#'     \item \code{dtime} - datetime (computed via SQL CAST and INTERVAL)
#'     \item \code{depth_m} - depth in meters
#'     \item \code{lat_dec} - latitude (decimal degrees)
#'     \item \code{lon_dec} - longitude (decimal degrees)
#'     \item \code{qty} - environmental variable value
#'     \item \code{hex_id} - H3 cell index at resolution 10 (coarser resolutions
#'       derived at query time via \code{h3_cell_to_parent()})
#'   }
#'
#' @details
#' Queries \code{env_obs} materialized table (pre-joined casts + bottle +
#' bottle_measurement with H3 columns and quarter). Only records with non-NA
#' values for the selected measurement_type are returned.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env(
#'   env_var    = "temperature",
#'   qtr        = c(1, 2),
#'   date_range = as.Date(c("2010-01-01", "2020-12-31")),
#'   min_depth  = 0,
#'   max_depth  = 100)
#' df_env |> collect()
#' }
#'
#' @seealso \code{\link{prep_env_hex}} for spatial aggregation
#' @seealso \code{\link{prep_ts_env}} for temporal aggregation
#'
#' @importFrom dplyr tbl filter rename select starts_with between
#'
#' @export
get_env <- function(env_var, qtr, date_range, min_depth, max_depth) {
  if (debug) message("get_env: env_var = ", env_var, ", qtr = ", paste(qtr, collapse = ","),
                     ", date_range = ", paste(date_range, collapse = " to "),
                     ", depth = ", min_depth, "-", max_depth)

  df_env <- tbl(con, "env_obs") |>
    filter(
      measurement_type == env_var,
      !is.na(qty),
      between(depth_m, min_depth, max_depth),
      between(datetime_utc, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    rename(dtime = datetime_utc) |>
    select(
      dtime,
      cast_id,
      depth_m,
      lat_dec,
      lon_dec,
      qty,
      hex_id)

  if (debug) {
    n_rows <- df_env |> summarize(n = n()) |> pull(n)
    message("get_env: returning lazy table with ", n_rows, " rows")
  }

  df_env
}


# data preparation functions ----

#' Hexagon Geometry, Loaded on First Use
#'
#' \code{data/hex.geojson} is 153 MB — 434,218 polygons covering all 10 H3
#' resolutions — and \code{st_read()}ing it costs ~5.6 s and ~370 MB of RSS in
#' every R process the app starts. global.R used to read it eagerly, so every
#' session paid that before the UI appeared.
#'
#' Exactly two callers need it: \code{\link{prep_sp_hex}} and
#' \code{\link{prep_env_hex}}, which attach geometry to their aggregates. When
#' \code{USE_H3T} is on — the normal case — neither runs, because the tile
#' service derives each cell's boundary per tile. So the eager read paid the
#' full cost for something nothing then used.
#'
#' Loading here instead means the cost is paid once, in the one session that
#' actually asks for hexagon geometry (a classic-path fallback, or a "Map data"
#' download that wants geometry), and never otherwise.
#'
#' @return sf object of hexagons with \code{hex_id} and \code{hex_res}
#'
#' @export
get_sf_hex <- function() {
  if (is.null(hex_cache$sf_hex)) {
    if (debug) message("get_sf_hex: reading ", hex_geo, " (first use) ...")
    hex_cache$sf_hex <- sf::st_read(hex_geo, quiet = TRUE)
  }
  hex_cache$sf_hex
}
hex_cache <- new.env(parent = emptyenv())

# TODO (long-term): replace preloaded hex layers with h3t tile endpoint
#   - plumber API at /api/h3t/{z}/{x}/{y} accepting SQL query params
#   - determines H3 resolution from zoom z, filters by tile extent x/y
#   - returns h3j-format JSON ([{h3: "hex_id", value: ...}, ...])
#   - map sources switch from add_fill_layer(source = sf_data) to
#     add_h3j_source(url = api_endpoint) or future add_h3t_source()
#   - eliminates preloading all resolutions; data fetched on-demand per viewport
#   - see: https://github.com/INSPIDE/h3j-h3t
#   - see: https://walker-data.com/mapgl/reference/add_h3j_source.html

#' Aggregate Species Data into H3 Hexagons
#'
#' Converts species occurrence/abundance data into multi-resolution H3 hexagonal
#' bins with aggregated statistics and geometries for mapping.
#'
#' @param df_sp dbplyr lazy table with columns: \code{hex_id}, \code{std_tally}
#' @param res_range Integer vector of H3 resolution levels to generate (e.g., 3:5)
#'
#' @return List of sf objects, one per resolution level, each with columns:
#'   \itemize{
#'     \item \code{resolution} - H3 resolution level
#'     \item \code{hexid} - H3 hexagon identifier
#'     \item \code{sp.value} - mean standardized tally
#'     \item \code{tooltip} - rounded value for display
#'     \item \code{geometry} - sf geometry (hexagon polygon)
#'   }
#'
#' @details
#' This function uses dbplyr lazy evaluation to efficiently aggregate data
#' across multiple H3 resolutions via \code{union_all}. Geometries are joined
#' from a pre-computed sf object (\code{sf_hex}).
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
#' }
#'
#' @seealso \code{\link{map_sp}} for visualization
#' @seealso \code{\link{get_sp}} for data retrieval
#'
#' @importFrom dplyr select mutate group_by summarize filter collect left_join group_split
#' @importFrom purrr map reduce
#' @importFrom glue glue
#' @importFrom dbplyr compute sql
#'
#' @export
#' Aggregate Species Data into H3 Cells (no geometry)
#'
#' The database half of \code{\link{prep_sp_hex}}: one row per (resolution,
#' cell) with the aggregate, and no polygon attached.
#'
#' Separated out because attaching geometry means reading the 153 MB
#' \code{hex.geojson} (see \code{\link{get_sf_hex}}), and the callers that
#' write a CSV do not want it — the sfc column write.csv() emits is an R
#' \code{list(c(...))} literal, not WKT, so it was unusable anyway.
#'
#' @inheritParams prep_sp_hex
#' @return tibble with \code{resolution}, \code{hexid}, \code{sp.value},
#'   \code{n}, \code{min_dtime}, \code{max_dtime}, \code{tooltip}
#' @export
agg_sp_hex <- function(df_sp, res_range) {
  if (debug) message("agg_sp_hex: aggregating species data for resolutions ", paste(res_range, collapse = ","))

  # precompute and store joins in a temporary table
  df_sp_temp <- df_sp |>
    compute()

  # create and combine tables for each resolution — derive the parent H3 cell at
  # resolution .x from the stored res-10 hex_id (runs in DuckDB via h3_cell_to_parent)
  combined_res_tbl <- map(res_range, ~{
    df_sp_temp |>
      mutate(hex_int = h3_cell_to_parent(hex_id, .x)) |>
      select(hex_int, std_tally, time_start) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_sp_collected <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      sp.value   =  mean(std_tally, na.rm = T),
      n          =  sum(std_tally,  na.rm = T),
      min_dtime  =  min(time_start, na.rm = T),
      max_dtime  =  max(time_start, na.rm = T),
      .groups = "drop") |>
    filter(
      !is.na(hex_int),
      !is.na(sp.value)) |>
    mutate(
      hex_id  = sql("HEX(hex_int)"),
      # unit-free: the hex average spans cpue_units, and std_tally is a
      # gear-standardized density only where a net tow supports it (see prep_db.R)
      tooltip = paste0("Avg. CPUE: ", round(sp.value, 2),
                 "</br>Num. Samples: ", n,
                 "</br>Date Range: ", min_dtime, " to ", max_dtime)) |>
    select(resolution, hexid = hex_id, sp.value, n, min_dtime, max_dtime, tooltip) |>
    collect()

  if (debug) message("agg_sp_hex: collected ", nrow(hex_sp_collected), " hex records")

  hex_sp_collected
}


prep_sp_hex <- function(df_sp, res_range) {
  hex_sp <- agg_sp_hex(df_sp, res_range) |>
    left_join(
      get_sf_hex() |>
        select(hexid = hex_id, hex_res, geometry),
      join_by(
        hexid,
        resolution == hex_res)) |>
    group_split(resolution)

  if (debug) {
    message("prep_sp_hex: created ", length(hex_sp), " hex layers")
    for (i in seq_along(hex_sp)) {
      message("  Resolution ", res_range[i], ": ", nrow(hex_sp[[i]]), " hexagons")
    }
  }

  return(hex_sp)
}


#' Aggregate Environmental Data into H3 Hexagons
#'
#' Converts environmental point data into multi-resolution H3 hexagonal bins
#' with aggregated statistics and geometries for mapping. Uses dbplyr lazy
#' evaluation to defer collection until after aggregation.
#'
#' @param df_env dbplyr lazy table with H3 index column (\code{hex_id}) and \code{qty} column
#' @param res_range Integer vector of H3 resolution levels to generate (e.g., 3:5)
#' @param env_stat Character string specifying aggregation function: "mean", "median", "min", "max", "sd"
#'
#' @return List of sf objects, one per resolution level, each with columns:
#'   \itemize{
#'     \item \code{resolution} - H3 resolution level
#'     \item \code{hexid} - H3 hexagon identifier
#'     \item \code{env.value} - aggregated environmental value
#'     \item \code{tooltip} - rounded value for display
#'     \item \code{geometry} - sf geometry (hexagon polygon)
#'   }
#'
#' @details
#' This function uses dbplyr lazy evaluation to efficiently aggregate data
#' across multiple H3 resolutions via \code{union_all}. Geometries are joined
#' from a pre-computed sf object (\code{sf_hex}).
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
#' }
#'
#' @seealso \code{\link{map_env}} for visualization
#' @seealso \code{\link{get_env}} for data retrieval
#'
#' @importFrom dplyr select mutate group_by summarize filter collect left_join group_split
#' @importFrom purrr map reduce
#' @importFrom glue glue
#' @importFrom dbplyr compute sql
#'
#' @export
#' Aggregate Environmental Data into H3 Cells (no geometry)
#'
#' The database half of \code{\link{prep_env_hex}} — see \code{\link{agg_sp_hex}}
#' for why the geometry join is separable.
#'
#' @inheritParams prep_env_hex
#' @return tibble with \code{resolution}, \code{hexid}, \code{env.value},
#'   \code{tooltip}
#' @export
agg_env_hex <- function(df_env, res_range, env_stat) {
  if (debug) message("agg_env_hex: aggregating env data for resolutions ", paste(res_range, collapse = ","),
                     ", stat = ", env_stat)

  # precompute and store joins in a temporary table
  df_env_temp <- df_env |>
    compute()

  # create and combine tables for each resolution — derive the parent H3 cell at
  # resolution .x from the stored res-10 hex_id (runs in DuckDB via h3_cell_to_parent)
  combined_res_tbl <- map(res_range, ~{
    df_env_temp |>
      mutate(hex_int = h3_cell_to_parent(hex_id, .x)) |>
      select(hex_int, qty, dtime) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_env_collected <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      env.value = case_when(
        env_stat == "mean"   ~ mean(qty, na.rm = TRUE),
        env_stat == "median" ~ median(qty, na.rm = TRUE),
        env_stat == "min"    ~ min(qty, na.rm = TRUE),
        env_stat == "max"    ~ max(qty, na.rm = TRUE),
        env_stat == "sd"     ~ sd(qty, na.rm = TRUE),
        TRUE ~ mean(qty, na.rm = TRUE)
      ),
      n          =  sum(!is.na(qty)),
      min_dtime  =  min(dtime, na.rm = TRUE),
      max_dtime  =  max(dtime, na.rm = TRUE),
      .groups = "drop") |>
    filter(
      !is.na(hex_int),
      !is.na(env.value)) |>
    mutate(
      hex_id  = sql("HEX(hex_int)"),
      tooltip = paste0("Value: ", round(env.value, 2),
                       "</br>Num. Samples: ", n,
                       "</br>Date Range: ", min_dtime, " to ", max_dtime)) |>
    select(resolution, hexid = hex_id, env.value, tooltip) |>
    collect()

  if (debug) message("agg_env_hex: collected ", nrow(hex_env_collected), " hex records")

  hex_env_collected
}


prep_env_hex <- function(df_env, res_range, env_stat) {
  hex_env <- agg_env_hex(df_env, res_range, env_stat) |>
    left_join(
      get_sf_hex() |>
        select(hexid = hex_id, hex_res, geometry),
      join_by(
        hexid,
        resolution == hex_res)) |>
    group_split(resolution)

  if (debug) {
    message("prep_env_hex: created ", length(hex_env), " hex layers")
    for (i in seq_along(hex_env)) {
      message("  Resolution ", res_range[i], ": ", nrow(hex_env[[i]]), " hexagons")
    }
  }

  return(hex_env)
}


# spatial polygon summaries ----
# The hex path bins observations into H3 cells; this path bins them into the
# polygons of a named boundary layer (an MPA, a county, an ecoregion) instead,
# and reports one value per polygon.
#
# The join is `sample_spatial`, materialized by prep_db.R at the SAMPLE grain —
# an observation's position IS its sample's position, so a 1.5M-row
# point-in-polygon join answers the same question as a 20M-row one. bio_obs
# carries `sample_key`; env_obs carries the same key named `cast_id`.
#
# ONE layer at a time, deliberately: the layers overlap (a station sits inside a
# county AND an ecoregion AND an MPA, and every one of those is true), so a
# summary spanning layers would count the same observation more than once. The
# UI picker is single-select for exactly this reason — see `agg_unit_choices`
# in global.R.

# boundary geometry, keyed "{layer}@{tolerance}". Read once per R process rather
# than per switch: the summary query is ~0.05s but the geometry is up to ~900 KB
# of GeoJSON, and users toggle back and forth between layers.
poly_geom_cache <- new.env(parent = emptyenv())


#' Summarize Species Observations Within a Spatial Layer's Polygons
#'
#' Aggregates species CPUE into the polygons of one boundary layer (e.g.
#' "Marine Protected Areas"), the polygon counterpart of \code{\link{prep_sp_hex}}.
#'
#' @param df_sp dbplyr lazy table from \code{\link{get_sp}} (carries
#'   \code{sample_key}, \code{std_tally}, \code{cpue_unit}, \code{time_start})
#' @param sel_layer Character name of the spatial layer, matching
#'   \code{sample_spatial.layer} (e.g. "CA Counties")
#'
#' @return List with:
#'   \itemize{
#'     \item \code{data} - tibble of \code{spatial_key}, \code{spatial_name},
#'       \code{value} (mean CPUE), \code{n}, date range, \code{tooltip}
#'     \item \code{unit} - the single \code{cpue_unit} summarized
#'     \item \code{n_excluded} - observations dropped because they carry a
#'       different unit
#'     \item \code{units} - the full unit mix, most-represented first
#'   }
#'
#' @details
#' \strong{One unit, named.} \code{std_tally} is a gear-standardized density only
#' where a net tow supports it; elsewhere it is the value the source published,
#' in its own \code{cpue_unit}. A mean over a mix of units is not a quantity —
#' and the mix is not hypothetical: \emph{Sardinops sagax} alone spans
#' \code{count/10m2} (oblique/vertical tows), \code{count/100m3} (manta) and a
#' bare \code{count} from \code{swfsc_cufes}, the last outnumbering the others
#' 4:1. So this summarizes the most-represented unit only, returns that unit for
#' the legend to name, and returns how many observations that excluded.
#'
#' @seealso \code{\link{map_poly}} for visualization
#' @seealso \code{\link{prep_sp_hex}} for the hexagon equivalent
#'
#' @importFrom dplyr filter inner_join select count group_by summarize collect mutate arrange desc
#'
#' @export
prep_sp_poly <- function(df_sp, sel_layer) {
  if (debug)
    message("prep_sp_poly: summarizing species within '", sel_layer, "'")

  d <- df_sp |>
    filter(!is.na(std_tally), !is.na(cpue_unit)) |>
    inner_join(
      tbl(con, "sample_spatial") |>
        filter(layer == !!sel_layer) |>
        select(sample_key, spatial_key, spatial_name),
      by = "sample_key")

  # unit mix first, so the summary below is over a single unit
  d_units <- d |>
    count(cpue_unit) |>
    collect() |>
    arrange(desc(n))

  if (nrow(d_units) == 0) {
    if (debug) message("prep_sp_poly: no observations fall in this layer")
    # SHAPED empty, not tibble(): map_poly() joins this onto the layer geometry
    # by name, so a zero-COLUMN tibble errors where a zero-ROW one draws every
    # polygon as "no data" — which is the honest answer here.
    return(list(
      data = tibble(
        spatial_key  = character(), spatial_name = character(),
        value        = numeric(),   n            = integer(),
        min_dtime    = as.POSIXct(character()),
        max_dtime    = as.POSIXct(character()),
        tooltip      = character()),
      unit = NA_character_, n_excluded = 0, units = d_units))
  }

  unit       <- d_units$cpue_unit[1]
  n_excluded <- sum(d_units$n[-1])

  d_sum <- d |>
    filter(cpue_unit == !!unit) |>
    group_by(spatial_key, spatial_name) |>
    summarize(
      value     = mean(std_tally, na.rm = TRUE),
      n         = n(),
      min_dtime = min(time_start, na.rm = TRUE),
      max_dtime = max(time_start, na.rm = TRUE),
      .groups   = "drop") |>
    collect() |>
    mutate(
      tooltip = paste0(
        "<strong>", spatial_name, "</strong>",
        "<br>Avg. CPUE (", unit, "): ", round(value, 2),
        "<br>Num. Obs.: ", n,
        "<br>Date Range: ", as.Date(min_dtime), " to ", as.Date(max_dtime)))

  if (debug)
    message("prep_sp_poly: ", nrow(d_sum), " polygons with data, unit = ", unit,
            ", ", n_excluded, " obs excluded in ", nrow(d_units) - 1,
            " other unit(s)")

  list(data = d_sum, unit = unit, n_excluded = n_excluded, units = d_units)
}


#' Summarize Environmental Observations Within a Spatial Layer's Polygons
#'
#' Aggregates an environmental variable into the polygons of one boundary layer,
#' the polygon counterpart of \code{\link{prep_env_hex}}.
#'
#' @param df_env dbplyr lazy table from \code{\link{get_env}} (carries
#'   \code{cast_id}, \code{qty}, \code{dtime})
#' @param sel_layer Character name of the spatial layer
#' @param env_stat Character aggregation: "mean", "median", "min", "max", "sd"
#'
#' @return tibble of \code{spatial_key}, \code{spatial_name}, \code{value},
#'   \code{n}, date range and \code{tooltip}
#'
#' @details
#' \code{env_obs} carries the sample key as \code{cast_id}, so the join to
#' \code{sample_spatial} is \code{cast_id = sample_key}.
#'
#' @seealso \code{\link{map_poly}} for visualization
#'
#' @importFrom dplyr filter inner_join select group_by summarize collect mutate case_when join_by
#'
#' @export
prep_env_poly <- function(df_env, sel_layer, env_stat) {
  if (debug)
    message("prep_env_poly: summarizing ", env_stat, " within '", sel_layer, "'")

  d_sum <- df_env |>
    filter(!is.na(qty)) |>
    inner_join(
      tbl(con, "sample_spatial") |>
        filter(layer == !!sel_layer) |>
        select(sample_key, spatial_key, spatial_name),
      by = join_by(cast_id == sample_key)) |>
    group_by(spatial_key, spatial_name) |>
    summarize(
      value = case_when(
        env_stat == "mean"   ~ mean(qty, na.rm = TRUE),
        env_stat == "median" ~ median(qty, na.rm = TRUE),
        env_stat == "min"    ~ min(qty, na.rm = TRUE),
        env_stat == "max"    ~ max(qty, na.rm = TRUE),
        env_stat == "sd"     ~ sd(qty, na.rm = TRUE),
        TRUE                 ~ mean(qty, na.rm = TRUE)),
      n         = n(),
      min_dtime = min(dtime, na.rm = TRUE),
      max_dtime = max(dtime, na.rm = TRUE),
      .groups   = "drop") |>
    collect() |>
    mutate(
      tooltip = paste0(
        "<strong>", spatial_name, "</strong>",
        "<br>Value: ", round(value, 2),
        "<br>Num. Obs.: ", n,
        "<br>Date Range: ", as.Date(min_dtime), " to ", as.Date(max_dtime)))

  if (debug)
    message("prep_env_poly: ", nrow(d_sum), " polygons with data")

  d_sum
}


#' Read a Spatial Layer's Polygon Geometry
#'
#' Reads the boundary geometry for one layer out of the app's local DuckDB
#' \code{spatial} table, simplified for the browser, and caches it for the life
#' of the R process.
#'
#' @param sel_layer Character name of the spatial layer
#' @param tolerance Numeric simplification tolerance in degrees
#'   (\code{POLY_SIMPLIFY_DEG})
#'
#' @return sf data frame with \code{spatial_key}, \code{spatial_name}, geometry
#'
#' @details
#' The geometry comes from the DB rather than the PMTiles overlay so the join to
#' the summary is the exact \code{spatial_key} — the PMTiles carry only a
#' per-file \code{id}, which equals \code{spatial.id} for single-layer groups but
#' NOT for a multi-layer group like \code{noaa_maritime_boundaries}.
#' \code{ST_SimplifyPreserveTopology} keeps a MULTIPOLYGON's parts, and at the
#' default tolerance (\code{POLY_SIMPLIFY_DEG}, ~11 m) stays finer than the
#' smallest hexagon the map draws.
#'
#' \code{NOT ST_IsEmpty(geom)} is not paranoia: releases up to and including
#' v2026.08.02 ship National Marine Sanctuaries, CA Watersheds (HUC8) and Ocean
#' Disposal Sites as \code{GEOMETRYCOLLECTION EMPTY} — the ingest bound the
#' per-layer sf objects together by column name, so any source whose geometry
#' column was not called \code{geometry} lost its shape. Fixed in
#' \code{workflows/ingest_spatial.qmd} (\code{normalize_geom_col()}); this guard
#' keeps the app honest against a release built before that.
#'
#' @importFrom glue glue_sql
#' @importFrom sf st_as_sf st_set_geometry
#'
#' @export
get_layer_sf <- function(sel_layer, tolerance = POLY_SIMPLIFY_DEG) {
  key <- paste0(sel_layer, "@", tolerance)
  if (!is.null(poly_geom_cache[[key]])) {
    if (debug) message("get_layer_sf: cache hit for '", sel_layer, "'")
    return(poly_geom_cache[[key]])
  }

  q <- glue_sql(
    "SELECT spatial_key,
            COALESCE(name, spatial_key) AS spatial_name,
            ST_AsText(ST_SimplifyPreserveTopology(geom, {tolerance})) AS geom_wkt
       FROM spatial
      WHERE layer = {sel_layer}
        AND geom IS NOT NULL
        AND NOT ST_IsEmpty(geom)",
    .con = con)

  d <- dbGetQuery(con, q)
  if (nrow(d) == 0) {
    if (debug) message("get_layer_sf: no geometry for '", sel_layer, "'")
    return(NULL)
  }

  sf_poly <- d |>
    st_as_sf(wkt = "geom_wkt", crs = 4326) |>
    st_set_geometry("geometry")

  if (debug)
    message("get_layer_sf: ", nrow(sf_poly), " polygons for '", sel_layer, "'")

  poly_geom_cache[[key]] <- sf_poly
  sf_poly
}


# cache helpers ----

#' compute cache key from default parameters and database modification time
cache_key <- function(db_path) {
  params <- list(
    sp_name    = default_sp_name,
    env_var    = "temperature",
    quarters   = 1:4,
    date_range = as.character(min_max_date),
    depth      = c(0, 212),
    children   = TRUE,
    env_stat   = "mean",
    res_range  = res_range)
  db_mtime <- as.character(file.info(db_path)$mtime)
  rlang::hash(c(params, db_mtime = db_mtime))
}

#' load cached default data if valid; returns list or NULL
load_cache <- function(cache_dir, db_path) {
  key_file <- file.path(cache_dir, "cache_key.rds")
  if (!file.exists(key_file)) return(NULL)

  saved_key   <- readRDS(key_file)
  current_key <- cache_key(db_path)
  if (saved_key != current_key) return(NULL)

  files <- c("sp_hex_list.rds", "env_hex_list.rds", "summary_stats.rds")
  paths <- file.path(cache_dir, files)
  if (!all(file.exists(paths))) return(NULL)

  list(
    sp_hex_list   = readRDS(paths[1]),
    env_hex_list  = readRDS(paths[2]),
    summary_stats = readRDS(paths[3]))
}

#' save default data to cache
save_cache <- function(cache_dir, db_path, sp_hex_list, env_hex_list, summary_stats) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  saveRDS(cache_key(db_path), file.path(cache_dir, "cache_key.rds"))
  saveRDS(sp_hex_list,        file.path(cache_dir, "sp_hex_list.rds"))
  saveRDS(env_hex_list,       file.path(cache_dir, "env_hex_list.rds"))
  saveRDS(summary_stats,      file.path(cache_dir, "summary_stats.rds"))

  if (debug) message("cache saved to ", cache_dir)
}


#' Build Species Time Series Data
#'
#' Aggregates species abundance data by temporal resolution, computing mean and
#' standard error for visualization in time series plots. Uses dbplyr lazy
#' evaluation for efficient database queries.
#'
#' @param df_sp dbplyr lazy table or data.frame with columns: \code{time_start}, \code{name}, \code{std_tally}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return data.frame with columns:
#'   \itemize{
#'     \item \code{time} - aggregated time value
#'     \item \code{name} - species name
#'     \item \code{avg} - mean standardized tally
#'     \item \code{std} - standard error (sd/n)
#'     \item \code{n} - number of observations
#'     \item \code{upr} - upper confidence bound (avg + std)
#'     \item \code{lwr} - lower confidence bound (avg - std)
#'   }
#'
#' @details
#' For seasonal plots (\code{ts_res = "quarter"}), the function adds a wrapping
#' row to ensure visual continuity across the year boundary. Data is collected
#' from database before aggregation.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_ts <- prep_ts_sp(df_sp, ts_res = "year")
#' }
#'
#' @seealso \code{\link{expr_time_sp}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom dplyr mutate group_by summarize collect filter bind_rows
#'
#' @export
prep_ts_sp <- function(df_sp, ts_res) {

  sp_ts_data <- df_sp |>
    mutate(
      time = !!expr_time_sp(ts_res)
    ) |>
    group_by(time, name) |>
    summarize(
      avg = mean(std_tally, na.rm = TRUE),
      std = sd(std_tally, na.rm = TRUE),
      n = n(),
      .groups = "drop") |>
    mutate(
      upr = avg + std/n,
      lwr = avg - std/n,
      std = std/n) |>
    collect()

  # add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    sp_ts_data <- sp_ts_data |>
      bind_rows(
        sp_ts_data |>
          filter(
            time == as.Date("2000-01-01")) |>
          mutate(
            time = time + 366))
  }

  # Break the line where nothing was sampled. Highcharts connects consecutive
  # points and a species series is mostly zeros, so an unsampled stretch drew a
  # flat line along zero — which reads as "we looked and found none" when the
  # truth is "nobody looked".
  #
  # Metacarcinus magister is the case that surfaced it: its sorted-archive effort
  # exists in nine years only (1984, 1988, 1998, 2004-2009), because the sorting
  # log records which archived jars have been examined and most have not, yet the
  # chart drew a continuous zero from 1984 to 2008.
  #
  # Calls calcofi4r rather than reimplementing: this file carries its own copy of
  # prep_ts_sp() which SHADOWS the package's, so fixing only the package left the
  # app unchanged and the fix silently inert (2026-08-14). One implementation,
  # two callers. Needs calcofi4r >= 1.7.0.
  sp_ts_data <- calcofi4r::cc_ts_gaps(sp_ts_data, ts_res)

  return(sp_ts_data)
}


#' Build Environmental Time Series Data
#'
#' Aggregates environmental data by temporal resolution, computing mean and
#' standard error for visualization in time series plots. Uses dbplyr lazy
#' evaluation for efficient database queries.
#'
#' @param df_env dbplyr lazy table with columns: \code{dtime}, \code{qty}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return data.frame with columns:
#'   \itemize{
#'     \item \code{time} - aggregated time value
#'     \item \code{avg} - mean of \code{qty}
#'     \item \code{std} - standard error of \code{qty} (sd/n)
#'     \item \code{upr} - upper confidence bound (avg + std)
#'     \item \code{lwr} - lower confidence bound (avg - std)
#'   }
#'
#' @details
#' For seasonal plots (\code{ts_res = "quarter"}), the function adds a wrapping
#' row to ensure visual continuity across the year boundary. Data is collected
#' from database only at the end of aggregation.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_ts <- prep_ts_env(df_env, ts_res = "year")
#' }
#'
#' @seealso \code{\link{expr_time_env}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom dplyr mutate group_by summarize collect filter bind_rows n
#'
#' @export
prep_ts_env <- function(df_env, ts_res) {

  env_ts_data <- df_env |>
    mutate(
      time = !!expr_time_env(ts_res) ) |>
    group_by(time) |>
    summarize(
      avg = mean(qty, na.rm = T),
      std = sd(qty, na.rm = T) / n(),
      .groups = "drop"
    ) |>
    mutate(
      upr = avg + std,
      lwr = avg - std) |>
    collect()

  # add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    env_ts_data <- env_ts_data |>
      bind_rows(
        env_ts_data |>
          filter(
            time == as.Date("2000-01-01")) |>
          mutate(
            time = time + 366))
  }

  return(env_ts_data)
}


#' Prepare Data for Species-Environment Scatterplot
#'
#' Joins species and environmental data by matching observations that are close
#' in time and space, enabling correlation analysis between abundance and
#' environmental variables.
#'
#' @param df_sp dbplyr lazy table or data.frame with species data
#' @param df_env dbplyr lazy table or data.frame with environmental data
#' @param env_stat Character string specifying aggregation function (e.g., "mean", "median")
#' @param max_hours_diff Numeric maximum time difference (in hours) for matching observations (default: 6)
#' @param max_meters_diff Numeric maximum spatial distance (in meters) for matching observations (default: 2000)
#'
#' @return data.frame with matched species-environment observations
#'
#' @details
#' This function performs a fuzzy join based on temporal proximity using
#' \code{fuzzyjoin::difference_inner_join()}. For each species observation,
#' the closest environmental measurement (within \code{max_hours_diff}) is
#' selected. Data is collected from database before joining.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' df_splot <- prep_splot(df_sp, df_env, env_stat = "mean")
#' }
#'
#' @seealso \code{\link{get_sp}} for species data retrieval
#' @seealso \code{\link{get_env}} for environmental data retrieval
#'
#' @importFrom dplyr select collect mutate group_by slice_min ungroup
#' @importFrom fuzzyjoin difference_inner_join
#'
#' @export
prep_splot <- function(df_sp, df_env, env_stat, method = "nearest_time",
                       max_hours_diff = 6, max_meters_diff = 2000) {

  # prepare species data
  d_sp <- df_sp |>
    select(
      sp_name  = name,
      sp_dtime = time_start,
      sp_tally = std_tally,
      sp_lon   = longitude,
      sp_lat   = latitude) # |>
  # compute()

  # prepare environmental data
  d_env <- df_env |>
    select(
      env_dtime = dtime,
      env_qty   = qty,
      env_cst   = cast_id,
      env_depth = depth_m,
      env_lon   = lon_dec,
      env_lat   = lat_dec) |>
    mutate(
      env_dtime_lwr = sql(glue("env_dtime - INTERVAL {max_hours_diff} HOUR")),
      env_dtime_upr = sql(glue("env_dtime + INTERVAL {max_hours_diff} HOUR")))

  # join by time difference
  d_sp_env_raw <- d_sp |>
      left_join(
        d_env,
        # join species to env observations within desired time interval
        by = join_by(between(sp_dtime, env_dtime_lwr, env_dtime_upr))) |>
      # compute distance between species and ocean observations
      mutate(
        dist_m = sql("ST_Distance_Sphere(ST_Point(sp_lon, sp_lat), ST_Point(env_lon, env_lat))")) |>
      # get pairs within desired distance
      filter(
        dist_m <= max_meters_diff)

  order_by <- if (method == "nearest_time") {
    expr(tibble(time_diff,dist_m,env_cst))
  } else if (method == "nearest_dist" ) {
    expr(tibble(dist_m,time_diff,env_cst))
  }

  d_sp_env <- if (method == "nearest_time" | method == "nearest_dist") {
    d_sp_env_raw |>
      mutate(
        time_diff = if_else(sp_dtime - env_dtime > lubridate::seconds(0),
                            sp_dtime - env_dtime,
                            env_dtime - sp_dtime)) |>
      group_by(
        sp_name, sp_tally, sp_dtime, sp_lon, sp_lat) |>
      slice_min(
        !!order_by,
        with_ties = TRUE) |>
      summarize(
        env_qty = mean(env_qty, na.rm = TRUE),
        .groups = "drop")
  } else {
    d_sp_env_raw |>
      group_by(
        sp_name, sp_tally, sp_dtime, sp_lon, sp_lat) |>
      summarize(
        env_qty = mean(env_qty, na.rm = TRUE),
        .groups = "drop") |>
      select(
        sp_name, sp_dtime, sp_lon, sp_lat, sp_tally,
        env_qty) }

  d_sp_env
}


#' Build Filter Summary for Display
#'
#' Creates a formatted list of filter criteria for display in the UI.
#' Summarizes species, environmental variables, temporal filters, depth ranges,
#' and spatial constraints into human-readable markdown strings.
#'
#' @param sel_name Character vector of selected species names (format: "Common Name (Scientific Name)")
#' @param sel_env_var Character string of selected environmental variable (e.g., "temperature")
#' @param sel_qtr Numeric vector of selected quarters (1-4)
#' @param sel_date_range Date vector of length 2 (start date, end date)
#' @param sel_depth_range Numeric vector of length 2 (min depth, max depth) in meters
#' @param drawn_polygon sf object or data.frame representing user-drawn polygon (or NULL)
#'
#' @return Character vector of markdown-formatted filter descriptions
#'
#' @examples
#' prep_filter_summary(
#'   sel_name        = c("Anchovy (Engraulis mordax)", "Sardine (Sardinops sagax)"),
#'   sel_env_var     = "temperature",
#'   sel_qtr         = c(1, 2),
#'   sel_date_range  = as.Date(c("2000-01-01", "2020-12-31")),
#'   sel_depth_range = c(0, 100),
#'   drawn_polygon   = NULL
#' )
#'
#' @seealso \code{\link{modal_data}} for the modal dialog that captures these filters
#'
#' @export
prep_filter_summary <- function(sel_name, sel_env_var, sel_qtr, sel_date_range,
                                sel_depth_range, drawn_polygon, selected_grid_zones,
                                ck_children, bio_datasets = NULL) {
  filter_list <- list()

  # Which datasets the numbers came from. The app reads all 9 bio and 5 env
  # datasets, and nothing on screen used to say so — a CPUE could be ichthyo
  # net tows, CUFES egg-pump counts, or both, with no way to tell.
  n_bio_all <- nrow(d_bio_datasets)
  if (is.null(bio_datasets) || length(bio_datasets) == 0 ||
      length(bio_datasets) == n_bio_all) {
    bio_txt <- paste0("all ", n_bio_all)
  } else {
    bio_txt <- paste(dataset_label(bio_datasets), collapse = ", ")
  }
  filter_list <- c(filter_list, paste0("**Taxa datasets:** ", bio_txt))

  # species
  if (!is.null(sel_name) && length(sel_name) > 0) {
    filter_list <- c(filter_list,
                     if (length(sel_name) <= 10) {
                       paste0("**Taxa:** ", paste(sel_name, collapse = ", "))
                     } else {
                       paste0("**Taxa:** ", length(sel_name), " selected")
                     }
    )
  }

  # variable
  env_ds <- d_env_vars$dataset_key[match(sel_env_var, d_env_vars$measurement_type)]
  filter_list <- c(filter_list, paste0(
    "**Variable:** ", env_var_label(sel_env_var),
    if (!is.na(env_ds)) paste0(" \u2014 ", dataset_label(env_ds)) else ""))

  # quarters
  quarter_names <- c("1" = "Q1", "2" = "Q2", "3" = "Q3", "4" = "Q4")
  filter_list <- c(filter_list, paste0("**Quarters:** ", paste(quarter_names[as.character(sel_qtr)], collapse = ", ")))

  # date range
  filter_list <- c(filter_list, paste0("**Date Range:** ",
                                       format(sel_date_range[1], "%Y-%m-%d"), " to ",
                                       format(sel_date_range[2], "%Y-%m-%d")))

  # depth range
  filter_list <- c(filter_list, paste0("**Depth Range:** ", sel_depth_range[1], " - ", sel_depth_range[2], " m"))

  # spatial
  if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
    filter_list <- c(filter_list, "**Spatial:** Custom polygon defined")
  } else if (!is.null(selected_grid_zones) && length(selected_grid_zones) > 0) {
    zone_text <- if (length(selected_grid_zones) <= 5) {
      paste(selected_grid_zones, collapse = ", ")
    } else {
      paste(length(selected_grid_zones), "zones selected")
    }
    filter_list <- c(filter_list, paste0("**Spatial:** Grid zones - ", zone_text))
  } else {
    filter_list <- c(filter_list, "**Spatial:** All locations")
  }

  # taxonomic children
  filter_list <- c(filter_list, paste0("**Include Children:** ", ifelse(ck_children, "Yes", "no")))

  return(filter_list)
}

prep_summary_stats <- function(df_sp, df_env) {

  # Count IN the database. This was `nrow(df |> collect())` on both tables,
  # which pulled every selected row into R to look at nrow() and throw it away
  # — 639K rows and ~0.4 s for the default selection, on the startup path, and
  # it grows with the filter: a broad environmental variable over the full date
  # range is millions of rows materialized for two integers nobody sees.
  n_sp  <- df_sp  |> summarize(n = n()) |> pull(n)
  n_env <- df_env |> summarize(n = n()) |> pull(n)

  list(
    "**Species Data**",
    paste0(format(n_sp, big.mark = ","), " observations"),
    "\n**Environmental Data**",
    paste0(format(n_env, big.mark = ","), " observations"))
}

taxa_tree_builder <- function(df_sp) {
  # observed taxa: match by taxon_key, NOT the display-name string. get_sp() builds
  # df_sp$name as "common (scientific)" while the species/taxon join below builds
  # it as "common (rank: scientific)" — since the unified-taxon reprep populated
  # taxonRank, those two formats diverge and a name-based filter silently returns
  # zero rows (then the downstream select on acceptedNameUsageID errors).
  # taxon_key is the stable key both sides already carry, and unlike worms_id it
  # exists for every taxon — no bird family or order has an AphiaID.
  sel_keys <- unique(df_sp |> pull(taxon_key))

  # get counts by taxa in data
  df_counts <- df_sp |>
    summarize(
      n = sum(!is.na(std_tally)),
      .by = c(taxon_key, parent_id)) |>
    collect()

  # build data.tree of taxa
  tree_counts <- tbl(con, "species") |>
    filter(taxon_key %in% sel_keys) |>
    select(taxon_key) |>
    collect() |>
    # get children of user-selected taxa
    mutate(
      children = map(taxon_key, get_taxon_children, con = con) ) |>
    unnest(children) |>
    select(taxon_key = acceptedNameUsageID, parent_id = parentNameUsageID, sci_name = scientificName) |>
    unique() |>
    # combine with observation counts
    left_join(
      df_counts, by = join_by(taxon_key, parent_id)
    ) |>
    mutate(
      # a parent outside the selected set becomes the tree root. "0" rather than
      # 0 because taxon_key is a string now.
      parent_id = ifelse(!(parent_id %in% taxon_key) | is.na(parent_id), "0", parent_id),
      sci_name = paste0("<i>", sci_name, "</i>"),
      n = ifelse(is.na(n),0,n)
    ) |>
    # transform to data.tree
    FromDataFrameNetwork()

  # aggregate counts and add leaves for unidentified observations
  tree_counts$Do(function(node) {
    if (!isLeaf(node)) {
      unident_n <- node$n %||% 0
      if (unident_n > 0) {
        child <- node$AddChild("Unidentified")
        child$sci_name  <-  paste0("Unidentified ", node$sci_name)
        child$n                <-  unident_n
      }
      node$n <- Aggregate(node, "n", sum)
    }
  }, traversal = "post-order")

  # remove taxa with no observations
  Prune(tree_counts, function(node) {node$n > 0})

  # helper function to transform nodes to HTML <li>
  makeTreeItem <- function(node) {
    id <- paste0("node_", gsub("[^A-Za-z0-9_]", "_", node$path))

    # Flex container for name + obs
    label_inner <- tags$span(
      class = "tree-label-inner",
      tags$span(class = "tree-name",  HTML(node$sci_name %||% node$name)),
      tags$span(class = "tree-obs",   HTML(format(node$n, big.mark = ",")  %||% ""))
    )

    if (length(node$children) == 0) {
      tags$li(
        class = "tree-leaf",
        tags$span(class = "tree-label", label_inner)
      )
    } else {
      tags$li(
        class = "tree-branch",
        tags$input(type = "checkbox", id = id, class = "tree-toggle"),
        tags$label(`for` = id, class = "tree-label", label_inner),
        tags$ul(lapply(node$children, makeTreeItem))
      )
    }
  }

  # helper function to make full HTML tree
  makeTree <- function(root) {
    tags$ul(class = "treeview", lapply(root$children, makeTreeItem))
  }

  taxa_tree_html <- makeTree(tree_counts)

  return(taxa_tree_html)
}

# spatial boundary layers ----

#' Add spatial boundary layers from PMTiles sources
#'
#' Adds all spatial boundary layers registered in \code{d_spatial_layers}
#' to a maplibre map. Layers are created hidden by default; visibility
#' controlled via the layers control or proxy. Includes yellow hover
#' highlight and tooltip from the \code{tooltip_field} column.
#'
#' @param map A maplibre map object
#' @param d_layers Data frame from \code{metadata/spatial_layers.csv}
#' @param is_dark Logical; TRUE for dark theme styling
#'
#' @return Modified maplibre map with PMTiles sources and layers (no control)
add_spatial_layers <- function(map, d_layers, visible_ids = NULL, is_dark = TRUE) {

  # determine which layers are visible
  if (is.null(visible_ids))
    visible_ids <- d_layers |> filter(default_visible) |> pull(dataset_id)

  # add one pmtiles source per unique dataset_group
  # promote_id = "id" so setFeatureState works for hover highlighting
  for (grp in unique(d_layers$dataset_group)) {
    url <- glue("{pmtiles_base_url}/{grp}.pmtiles")
    map <- map |>
      add_pmtiles_source(id = grp, url = url, promote_id = "id")
  }

  # add one layer per row
  for (i in seq_len(nrow(d_layers))) {
    row <- d_layers[i, ]
    vis <- ifelse(row$dataset_id %in% visible_ids, "visible", "none")

    # parse filter expression if present
    filt <- if (!is.na(row$filter_expr)){
      jsonlite::fromJSON(row$filter_expr, simplifyVector = FALSE)
    } else {
      NULL
    }

    # tooltip: "<strong>name</strong> - layer" (always available in PMTiles)
    tt <- list(
      "concat",
      "<strong>", list("get", "name"), "</strong>",
      " - ", list("get", "layer"))

    if (row$geom_type == "line") {
      map <- map |>
        add_line_layer(
          id            = row$dataset_id,
          source        = row$dataset_group,
          source_layer  = row$dataset_group,
          line_color    = row$line_color,
          line_width    = row$line_width,
          line_opacity  = 0.7,
          visibility    = vis,
          filter        = filt,
          tooltip       = tt,
          popup         = tt,
          hover_options = list(
            line_color = "#ffeb3b",
            line_width = row$line_width + 2))

    } else if (row$geom_type == "polygon") {
      map <- map |>
        add_fill_layer(
          id                 = row$dataset_id,
          source             = row$dataset_group,
          source_layer       = row$dataset_group,
          fill_color         = row$fill_color,
          fill_opacity       = row$fill_opacity,
          fill_outline_color = row$line_color,
          visibility         = vis,
          filter             = filt,
          tooltip            = tt,
          popup              = tt,
          hover_options      = list(
            # line_opacity = 0.7
            # fill_color = "#ffeb3b")
            # fill_color = "yellow",
            # fill_opacity = 1
            fill_opacity = min(row$fill_opacity + 0.35, 0.6))) |>
        add_line_layer(
          id           = paste0(row$dataset_id, "_outline"),
          source       = row$dataset_group,
          source_layer = row$dataset_group,
          line_color   = row$line_color,
          line_width   = row$line_width,
          line_opacity = 0.7,
          visibility   = vis,
          filter       = filt,
          hover_options = list(
            line_color = "yellow",
            line_opacity = 1,
            line_width = row$line_width * 2))

    } else if (row$geom_type == "point") {
      map <- map |>
        add_circle_layer(
          id              = row$dataset_id,
          source          = row$dataset_group,
          source_layer    = row$dataset_group,
          circle_color    = row$fill_color,
          circle_radius   = 4,
          circle_opacity  = 0.8,
          visibility      = vis,
          filter          = filt,
          tooltip         = tt,
          popup           = tt,
          hover_options   = list(
            circle_color  = "#ffeb3b",
            circle_radius = 7))
    }
  }

  map
}

#' Build grouped layers control list for the map
#'
#' Constructs the named list for \code{add_layers_control(layers = ...)}
#' with one entry per visible spatial layer (polygons pair fill + outline)
#' plus a "Hexagon Data" entry for all hex layer IDs.
#'
#' @param visible_ids Character vector of currently visible spatial layer IDs
#' @param d_layers Full spatial layers registry data frame
#' @param hex_layer_ids Character vector of data layer IDs (both sp + env)
#' @param label Character name for the data-layer entry. Defaults to
#'   "Hexagon Data"; the polygon-summary path passes its own so the control
#'   names what is actually drawn.
#'
#' @return Named list suitable for \code{add_layers_control(layers = ...)}
build_layers_control <- function(visible_ids, d_layers, hex_layer_ids,
                                 label = "Hexagon Data") {
  visible <- d_layers |> filter(dataset_id %in% visible_ids)

  # each layer is its own toggle entry; polygons pair fill + outline
  layer_entries <- lapply(seq_len(nrow(visible)), function(i) {
    row <- visible[i, ]
    ids <- row$dataset_id
    if (row$geom_type == "polygon")
      ids <- c(ids, paste0(ids, "_outline"))
    setNames(list(ids), row$layer)
  })

  # combine data layers (BOTH sp + env IDs) + individual layer entries
  c(setNames(list(hex_layer_ids), label),
    unlist(layer_entries, recursive = FALSE))
}

# visualization functions ----

#' Create Interactive Species Distribution Map with Hexagonal Binning
#'
#' Generates a multi-resolution maplibre map displaying species abundance
#' aggregated into H3 hexagons with color-coded values and interactive tooltips.
#'
#' @param sp_hex_list List of sf objects, one per H3 resolution level, containing hexagonal geometries and aggregated species abundance
#' @param sp_scale_list List of color scale specifications, one per resolution level (from \code{scales::col_numeric()})
#'
#' @return maplibre object with multi-resolution hexagonal layers, legend, and scale control
#'
#' @details
#' The map uses zoom-dependent layer visibility controlled by \code{zoom_breaks}.
#' Each resolution level displays at appropriate zoom ranges to balance detail
#' and performance. Abundance values are standardized as count per 10m² surface area.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp(sp_name = "Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
#' sp_scale <- lapply(sp_hex, function(x) scales::col_numeric("YlOrRd", domain = range(x$sp.value)))
#' map_sp(sp_hex, sp_scale)
#' }
#'
#' @seealso \code{\link{prep_sp_hex}} for data aggregation
#' @seealso \code{\link{get_sp}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend add_scale_control
#'
#' @export
map_sp <- function(sp_hex_list, sp_scale_list, is_dark = T) {
  if (debug) {
    message("map_sp: creating species map with ", length(sp_hex_list), " resolution layers")
    message("map_sp: first layer has ", nrow(sp_hex_list[[1]]), " hexagons")
  }

  # base map
  sp_map <- maplibre(
    style = carto_style(ifelse(is_dark, "dark-matter", "voyager"))) |>
    fit_bounds(bbox = st_as_sf(sp_hex_list[[1]])) |>
    add_scale_control(position = "top-left", unit = "metric") |>
    add_navigation_control()

  # add spatial boundary layers first (below hexagons)
  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  sp_map  <- sp_map |>
    add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  # add each resolution layer with hover highlight
  for (i in 1:length(res_range)) {
    sp_map <- sp_map |>
      add_fill_layer(
        id                 = paste0("sp", res_range[i]),
        source             = st_as_sf(sp_hex_list[[i]]),
        fill_color         = sp_scale_list[[i]]$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.6,
        min_zoom           = zoom_breaks[i],
        max_zoom           = zoom_breaks[i+1],
        tooltip            = "tooltip",
        hover_options      = list(
          fill_outline_color = "#ffeb3b",
          fill_opacity       = 0.85))
  }

  # add layers control with hexagons + visible spatial layers. Only THIS map's
  # own layer ids: a control listing the other side's ids cannot toggle them
  # (different map) and throws on the way back ON, because the client does not
  # guard set_layout_property against a missing layer.
  ctrl    <- build_layers_control(vis_ids, d_spatial_layers,
                                  paste0("sp", res_range))
  sp_map  <- sp_map |>
    add_layers_control(
      position     = "top-right",
      layers       = ctrl,
      collapsible  = TRUE,
      margin_right = 45)

  return(sp_map)
}


#' Create Interactive Environmental Map with Hexagonal Binning
#'
#' Generates a multi-resolution maplibre map displaying environmental data
#' aggregated into H3 hexagons with color-coded values and interactive tooltips.
#'
#' @param env_hex_list List of sf objects, one per H3 resolution level, containing hexagonal geometries and aggregated environmental values
#' @param env_scale_list List of color scale specifications, one per resolution level (from \code{scales::col_numeric()})
#' @param env_stat_label Character string describing the statistic (e.g., "Mean", "Median")
#' @param env_var_label Character string describing the variable (e.g., "Temperature (°C)")
#'
#' @return maplibre object with multi-resolution hexagonal layers and legend
#'
#' @details
#' The map uses zoom-dependent layer visibility controlled by \code{zoom_breaks}.
#' Each resolution level displays at appropriate zoom ranges to balance detail
#' and performance.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
#' env_scale <- lapply(env_hex, function(x) scales::col_numeric("viridis", domain = range(x$env.value)))
#' map_env(env_hex, env_scale, "Mean", "Temperature (°C)")
#' }
#'
#' @seealso \code{\link{prep_env_hex}} for data aggregation
#' @seealso \code{\link{get_env}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend
#'
#' @export
map_env <- function(env_hex_list, env_scale_list, env_stat_label, env_var_label, is_dark = T) {
  if (debug) {
    message("map_env: creating environmental map with ", length(env_hex_list), " resolution layers")
    message("map_env: first layer has ", nrow(env_hex_list[[1]]), " hexagons")
  }

  # create base map
  env_map <- maplibre(
    style = carto_style(ifelse(is_dark, "dark-matter", "voyager"))) |>
    fit_bounds(bbox = st_as_sf(env_hex_list[[1]])) |>
    add_scale_control(position = "top-left", unit = "metric") |>
    add_navigation_control()

  # add spatial boundary layers first (below hexagons)
  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  env_map <- env_map |>
    add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  # add each resolution layer with hover highlight
  for (i in 1:length(res_range)) {
    env_map <- env_map |>
      add_fill_layer(
        id                 = paste0("env", res_range[i]),
        source             = st_as_sf(env_hex_list[[i]]),
        fill_color         = env_scale_list[[i]]$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.6,
        min_zoom           = zoom_breaks[i],
        max_zoom           = zoom_breaks[i+1],
        tooltip            = "tooltip",
        hover_options      = list(
          fill_outline_color = "#ffeb3b",
          fill_opacity       = 0.85))
  }

  # add layers control with hexagons + visible spatial layers (this map's ids
  # only — see the note in map_sp())
  ctrl    <- build_layers_control(vis_ids, d_spatial_layers,
                                  paste0("env", res_range))
  env_map <- env_map |>
    add_layers_control(
      position     = "top-right",
      layers       = ctrl,
      collapsible  = TRUE,
      margin_right = 45)

  return(env_map)
}


#' Build a Colour Scale for a Polygon Summary
#'
#' \code{interpolate_palette} wrapper that survives the two degenerate cases a
#' polygon summary hits routinely: no polygon has data, and every polygon that
#' does has the same value (one sampled MPA, say). MapLibre rejects an
#' \code{interpolate} expression whose stops are not strictly ascending, so the
#' single-value case returns a flat colour instead — the same shape
#' \code{build_h3t_scale} returns.
#'
#' @param d tibble with a numeric \code{value} column
#' @param palette Function of n returning n colours
#' @param n_stops Integer number of colour stops
#'
#' @return List with \code{breaks}, \code{colors}, \code{expression}, or NULL
#'   when there is nothing to scale
#'
#' @export
poly_scale <- function(d, palette, n_stops = 5L) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  v <- d$value[is.finite(d$value)]
  if (length(v) == 0) return(NULL)

  if (min(v) == max(v)) {
    cols <- palette(2)
    return(list(breaks = c(min(v), max(v)), colors = cols, expression = cols[1]))
  }

  interpolate_palette(d, column = "value", palette = palette, n = n_stops)
}


#' Create Interactive Map Summarized Within a Spatial Layer's Polygons
#'
#' The polygon counterpart of \code{\link{map_sp}} / \code{\link{map_env}}: one
#' fill layer coloured by the per-polygon summary, plus an outline-only layer for
#' the polygons of the same boundary layer that contain no observations.
#'
#' @param sf_poly sf of the layer's polygons from \code{\link{get_layer_sf}}
#' @param d_val tibble from \code{\link{prep_sp_poly}}\code{$data} or
#'   \code{\link{prep_env_poly}}, joined on \code{spatial_key}
#' @param scale Colour scale list (\code{$expression}) from
#'   \code{interpolate_palette}, or NULL when no polygon has data
#' @param side Either "sp" (species, left) or "env" (environment, right);
#'   determines the layer IDs
#' @param is_dark Logical, dark basemap
#'
#' @return maplibre object
#'
#' @details
#' \strong{Empty polygons are drawn, not omitted.} Most polygons in a layer
#' contain no CalCOFI samples — 125 of 155 MPAs, for instance. Dropping them
#' would make the layer look sparser than it is and leave no way to tell an
#' unsampled polygon from one outside the layer; filling them would read as zero.
#' So they get an outline and a "no data" tooltip, and no fill.
#'
#' The map fits to the polygons that \emph{have} data, so switching aggregation
#' unit lands the user where the observations are rather than at the full extent
#' of a statewide layer. That \code{fit_bounds} is also what makes the legend
#' appear: legends are drawn by the \code{map_before_view} observer in server.R,
#' which fires on the resulting \code{moveend}.
#'
#' @seealso \code{\link{prep_sp_poly}}, \code{\link{prep_env_poly}},
#'   \code{\link{get_layer_sf}}
#'
#' @importFrom mapgl maplibre add_fill_layer add_line_layer add_scale_control
#' @importFrom dplyr left_join filter
#'
#' @export
map_poly <- function(sf_poly, d_val, scale, side = c("sp", "env"),
                     is_dark = TRUE) {
  side <- match.arg(side)

  sf_all <- sf_poly |>
    left_join(d_val |> select(-spatial_name), by = "spatial_key")

  sf_dat <- sf_all |> filter(!is.na(value))
  sf_nul <- sf_all |>
    filter(is.na(value)) |>
    mutate(tooltip = paste0("<strong>", spatial_name, "</strong><br>no data"))

  if (debug)
    message("map_poly (", side, "): ", nrow(sf_dat), " polygons with data, ",
            nrow(sf_nul), " without")

  # Set the view at CONSTRUCTION as well as via fit_bounds.
  #
  # fit_bounds() alone is not enough here: `output$map` re-renders the compare
  # widget when the aggregation unit changes, and on a re-render the client
  # applies the constructor's center/zoom but NOT `fitBounds` — leaving a map
  # parked at the default zoom 0, which on the globe projection is a pea-sized
  # planet and reads as "the feature is broken". Constructing with the view
  # already set works either way, and fit_bounds() stays because it is what
  # fires the `moveend` the legend observer in server.R listens for.
  bb    <- st_bbox(if (nrow(sf_dat) > 0) sf_dat else sf_all)
  span  <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
  zoom  <- if (is.finite(span) && span > 0)
    max(0, min(14, log2(360 / span) - 0.5)) else 5

  m <- maplibre(
    style  = carto_style(ifelse(is_dark, "dark-matter", "voyager")),
    center = c(mean(bb[c("xmin", "xmax")]), mean(bb[c("ymin", "ymax")])),
    zoom   = zoom) |>
    fit_bounds(bbox = if (nrow(sf_dat) > 0) sf_dat else sf_all) |>
    add_scale_control(position = "top-left", unit = "metric") |>
    add_navigation_control()

  # boundary reference layers first (below the summary)
  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  m <- m |>
    add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  id_dat <- paste0(side, "_poly")
  id_nul <- paste0(side, "_poly_nodata")

  # Unsampled polygons: outline, so they read as "no data" and never as zero.
  #
  # The near-transparent grey fill underneath is there to be HOVERED, not seen —
  # an outline alone is a ~1px hit target, so "why does this one not have a
  # tooltip?" becomes indistinguishable from "I missed it by two pixels". A
  # neutral grey at 8% cannot be mistaken for a value on either scale (viridis
  # runs dark purple → yellow, Spectral blue → red).
  if (nrow(sf_nul) > 0) {
    m <- m |>
      add_fill_layer(
        id            = paste0(id_nul, "_hit"),
        source        = sf_nul,
        fill_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        fill_opacity  = 0.08,
        tooltip       = "tooltip",
        hover_options = list(fill_opacity = 0.25)) |>
      add_line_layer(
        id            = id_nul,
        source        = sf_nul,
        line_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        line_width    = 1,
        line_opacity  = 0.6,
        hover_options = list(line_color = "#ffeb3b", line_opacity = 1))
  }

  if (nrow(sf_dat) > 0 && !is.null(scale)) {
    m <- m |>
      add_fill_layer(
        id                 = id_dat,
        source             = sf_dat,
        fill_color         = scale$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.65,
        tooltip            = "tooltip",
        hover_options      = list(
          fill_outline_color = "#ffeb3b",
          fill_opacity       = 0.85))
  }

  ctrl <- build_layers_control(
    vis_ids, d_spatial_layers,
    c(id_dat, id_nul, paste0(id_nul, "_hit")),
    label = "Polygon Summary")

  m |>
    add_layers_control(
      position     = "top-right",
      layers       = ctrl,
      collapsible  = TRUE,
      margin_right = 45)
}


# UI component functions ----

#' Label for the Taxa tab's dataset-picker trigger
#'
#' States what the filter will actually do, which is why zero selected reads the
#' same as all selected: an empty selection is not a filter that matches nothing
#' (\code{\link{get_sp}} skips the dataset predicate entirely), so calling it
#' "none" would be a label that contradicts the results.
#'
#' Mirrored in JavaScript in \code{app/ui.R}, which rewrites this label on every
#' click; R only paints it the first time. Change the two together, or the
#' button tells the truth exactly once.
#'
#' @param selected character vector of selected \code{dataset_key}
#' @param n_all total number of datasets on offer
#' @return length-1 character label, e.g. \code{"3 of 14 datasets"}
#'
#' @export
bio_ds_label <- function(selected, n_all = nrow(d_bio_datasets)) {
  n <- length(selected)
  if (n == 0 || n >= n_all) sprintf("All %d datasets", n_all)
  else                      sprintf("%d of %d datasets", n, n_all)
}


#' Data Selection Modal Dialog
#'
#' Creates a multi-tabbed modal dialog for selecting species, environmental
#' variables, temporal filters, depth ranges, and spatial regions.
#'
#' @return Shiny modal dialog object with four tabs:
#'   \itemize{
#'     \item Species - selectizeInput for multiple species selection
#'     \item Environmental - variable and depth range selection
#'     \item Temporal - quarter and date range selection
#'     \item Spatial - interactive map for polygon drawing
#'   }
#'
#' @details
#' The modal dialog uses \code{bslib::navset_tab()} for tab organization and
#' \code{shiny::input_task_button()} for submission handling. Spatial filtering
#' is implemented via \code{maplibre} with drawing capabilities.
#'
#' @examples
#' \dontrun{
#' # in server.R
#' observeEvent(input$show_filters, {
#'   showModal(modal_data())
#' })
#' }
#'
#' @seealso \code{\link{prep_filter_summary}} for filter summary generation
#'
#' @importFrom shiny modalDialog selectizeInput selectInput numericRangeInput dateRangeInput modalButton tagList
#' @importFrom bslib navset_tab nav_panel
#' @importFrom maplibre maplibreOutput
#'
#' @export
#' @param env_var currently selected environmental measurement_type, so
#'   reopening the modal does not silently reset it
#' @param bio_ds currently selected biological \code{dataset_key}s, for the same
#'   reason; \code{NULL} selects every dataset
modal_data <- function(env_var = "temperature", bio_ds = NULL) {
  bio_ds <- if (is.null(bio_ds)) d_bio_datasets$dataset_key
            else intersect(bio_ds, d_bio_datasets$dataset_key)

  modalDialog(
    title = "Data Selection",
    navset_tab(

      nav_panel(
        "Taxa", br(),
        # Which datasets contribute, and a way to work in just one. The app
        # reads every biological dataset in the release (it used to be ichthyo
        # alone), so without this the taxa list is 1,377 names with no
        # indication of where any of them came from. 80 taxa appear in more than
        # one dataset, so this filters observations too, not only the picker.
        #
        # In a POPOVER rather than inline, because the list is one line longer
        # per ingest and there is no end to that: at 14 datasets it filled the
        # dialog and pushed the taxa picker (the thing this tab is for) below
        # the fold. The trigger states the selection, so nothing is hidden by
        # being one click away.
        div(
          class = "d-flex align-items-center flex-wrap gap-2 mb-3",
          tags$label(class = "col-form-label fw-semibold py-0", "Datasets"),
          popover(
            tags$button(
              type  = "button",
              class = "btn btn-outline-secondary btn-sm d-inline-flex align-items-center gap-2",
              bs_icon("database"),
              # the one element the JS rewrites; it lives in the TRIGGER, which
              # stays put, not in the content Bootstrap moves in and out
              tags$span(id = "bio_ds_count", bio_ds_label(bio_ds)),
              bs_icon("chevron-down")),
            div(
              class = "d-flex justify-content-between align-items-start gap-3 mb-2",
              tags$span(
                class = "small text-muted",
                "Unchecking a dataset removes its taxa from the list of
                 selectable taxa plus its observations from the results.
                 (Unchecking all datasets is the same as checking them all.)"),
              tags$button(
                type  = "button",
                class = "btn btn-outline-secondary btn-sm flex-shrink-0 cc-ds-all",
                "Select all")),
            checkboxGroupInput(
              "sel_bio_ds",
              NULL,
              choices  = setNames(d_bio_datasets$dataset_key, d_bio_datasets$label),
              selected = bio_ds,
              width    = "100%"),
            title     = "Taxa datasets",
            placement = "bottom",
            # container: the popover content is otherwise re-parented to <body>,
            # outside the modal, where Bootstrap's modal focus trap yanks focus
            # straight back out of it and the checkboxes cannot be reached by
            # keyboard at all. Keeping it inside #shiny-modal satisfies the trap.
            options   = list(
              container   = "#shiny-modal",
              customClass = "cc-ds-popover"))),
        selectizeInput(
          "sel_name",
          "Taxa",
          choices  = NULL,
          multiple = TRUE,
          width    = "100%" ),
        checkboxInput(
          "ck_children",
          value = TRUE,
          label = tooltip(
            trigger = list(
              "Include taxonomic children",
              bs_icon("info-circle")),
            "Include observations recorded at finer taxonomic levels, e.g.
            include observations recorded to Genus and Species levels if Taxa
            selected is at the higher level of Family; otherwise only show
            observations to the given taxonomic level." ))),

      nav_panel(
        "Environmental", br(),
        # No dataset picker here, deliberately: every environmental measurement
        # type belongs to exactly one dataset, so the Variable list below is
        # already grouped by source and a separate filter would say the same
        # thing twice.
        div(
          class = "small text-muted mb-2",
          sprintf(paste(
            "Variables are grouped by the dataset they come from \u2014 %s.",
            "Each measurement type belongs to exactly one, so choosing a",
            "variable chooses its dataset."),
            paste(dataset_label(d_env_datasets$dataset_key), collapse = ", "))),
        selectInput(
          "sel_env_var",
          "Variable",
          # grouped by dataset; `selected` is a measurement_type, not a label —
          # it used to be "Temperature", which matched nothing and silently fell
          # through to whatever happened to be first
          choices  = env_var_choices(),
          selected = env_var,
          width    = "100%"),
        checkboxInput(
          "sel_env_all_vars",
          tagList(
            sprintf("Show all %d variables", nrow(d_env_vars)),
            popover(
              bs_icon("question-circle"),
              "Off, the list shows the headline variables people usually plot.
               On, it adds the instrument channels (transmissometer, ISUS
               voltage, PAR reference), the pre-QC reported series, and the
               averaged / corrected / per-replicate variants.")),
          value = FALSE),
        numericRangeInput(
          "sel_depth_range",
          "Depth Range (m)",
          c(0, 212), # TODO: pull from data
          width = "100%",
          separator = " to ",
          min = 0,
          max = 512 )),

      nav_panel(
        "Temporal", br(),
        selectInput(
          "sel_qtr",
          "Quarter",
          c(Q1 = 1,
            Q2 = 2,
            Q3 = 3,
            Q4 = 4),
          selected = 1:4,
          multiple = TRUE),
        dateRangeInput(
          "sel_date_range",
          "Date Range",
          startview = "year",
          start = min_max_date[1],
          end = min_max_date[2],
          min = min_max_date[1],
          max = min_max_date[2]) ),

      nav_panel(
        "Spatial",
        br(),
        "Select pre-defined zones by clicking on the map or table. Click selected zones again to deselect them. Alternatively, use the \"Custom\" category to drawn your own region of interest.",
        selectInput(
          "sel_places_cat",
          tags$b("Category"),
          selected = "CalCOFI Zones",
          c(unique(cc_places$category), "Custom")),
        fluidRow(
          column(6, maplibreOutput("spatial_filter_map", height = "400px")),
          column(6, DTOutput("tbl_places") )
        )
      ),
    ),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit", "Submit") ),


    size = "xl",
    fade = FALSE
  )
}


#' Depth Profile Modal Dialog
#'
#' Creates a modal dialog for defining a transect line segment and buffer
#' distance to generate environmental depth profiles.
#'
#' @param map_sp maplibre object (currently unused in implementation, retained for future enhancement)
#'
#' @return Shiny modal dialog object with transect drawing interface and buffer distance input
#'
#' @details
#' Users draw a line segment on the map to define a transect. The buffer distance
#' controls the width of the corridor around the transect for data aggregation.
#' Default buffer is 5 km.
#'
#' @examples
#' \dontrun{
#' # in server.R
#' observeEvent(input$create_profile, {
#'   showModal(modal_depth_profile(map_sp = NULL))
#' })
#' }
#'
#' @seealso \code{\link{buffer_transect}} for transect buffer generation
#'
#' @importFrom shiny modalDialog numericInput modalButton tagList
#' @importFrom maplibre maplibreOutput
#'
#' @export
modal_depth_profile <- function(map_sp) {
  modalDialog(
    title = "Create Depth Profile",

    p("Draw a line segment on the map to define your transect."),

    numericInput(
      "modal_buffer_dist",
      "Buffer Distance (km)",
      value = 5
    ),

    maplibreOutput("transect_map", height = "500px"),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit_transect", "Generate Profile")
    ),

    size = "l",
    fade = FALSE
  )
}


#' Create Placeholder Message UI
#'
#' Generates a centered placeholder message for empty or loading states in the
#' Shiny UI. Useful for displaying instructions or status messages when no data
#' is available.
#'
#' @param title Character string for heading text
#' @param message Character string for body text
#'
#' @return shiny.tag div element with centered, styled placeholder content
#'
#' @examples
#' \dontrun{
#' output$map_placeholder <- renderUI({
#'   ui_placeholder("No Data Selected", "Please select species from the filter menu.")
#' })
#' }
#'
#' @importFrom shiny div h4 p
#'
#' @export
ui_placeholder <- function(title, message) {
  div(
    class = "d-flex align-items-center justify-content-center",
    style = "height: 80vh;",
    div(
      class = "text-center text-muted",
      h4(title),
      p(message)
    )
  )
}


# utility functions ----

#' Generate Time Aggregation Expression for Species Data
#'
#' Creates a SQL-based expression for temporal aggregation of species time series
#' using DuckDB date functions. Used internally by \code{\link{prep_ts_sp}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} with dbplyr
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
#' \code{date_trunc()} and \code{extract()} functions for database-side computation.
#'
#' @examples
#' \dontrun{
#' df_sp |> mutate(time = !!expr_time_sp("year"))
#' }
#'
#' @seealso \code{\link{prep_ts_sp}} for usage context
#'
#' @importFrom rlang expr
#' @importFrom dbplyr sql
#'
#' @keywords internal
expr_time_sp <- function(ts_res) {
  switch(ts_res,
         "year"         = expr(sql("date_trunc('year', time_start)")),
         "quarter"      = expr(sql("make_date(2000, month(date_trunc('quarter', time_start)), day(date_trunc('quarter', time_start)))")),
         "month"        = expr(sql("extract('month' FROM time_start)")),
         "day"          = expr(sql("extract('doy' FROM time_start)")),
         "year_quarter" = expr(sql("date_trunc('quarter', time_start)")),
         "year_month"   = expr(sql("date_trunc('month', time_start)")),
         "year_day"     = expr(sql("date_trunc('day', time_start)"))
  )
}


#' Generate Time Aggregation Expression for Environmental Data
#'
#' Creates a SQL-based expression for temporal aggregation of environmental
#' time series using DuckDB date functions. Used internally by \code{\link{prep_ts_env}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} with dbplyr
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
#' \code{date_trunc()} and \code{extract()} functions for database-side computation.
#'
#' @examples
#' \dontrun{
#' df_env |> mutate(time = !!expr_time_env("year"))
#' }
#'
#' @seealso \code{\link{prep_ts_env}} for usage context
#'
#' @importFrom rlang expr
#' @importFrom dbplyr sql
#'
#' @keywords internal
expr_time_env <- function(ts_res) {
  switch(ts_res,
         "year"         = expr(sql("date_trunc('year', dtime)")),
         "quarter"      = expr(sql("make_date(2000, month(date_trunc('quarter', dtime)), day(date_trunc('quarter', dtime)))")),
         "month"        = expr(sql("extract('month' FROM dtime)")),
         "day"          = expr(sql("extract('doy' FROM dtime)")),
         "year_quarter" = expr(sql("date_trunc('quarter', dtime)")),
         "year_month"   = expr(sql("date_trunc('month', dtime)")),
         "year_day"     = expr(sql("date_trunc('day', dtime)"))
  )
}


#' Create Buffer Around Line Segment with Dateline Handling
#'
#' Creates a buffered polygon around a line segment (transect), handling
#' dateline crossings and projecting to appropriate UTM zone for accurate
#' distance calculations.
#'
#' @param coords Matrix or data.frame of coordinates (longitude, latitude) defining the line segment
#' @param buffer_dist Numeric buffer distance in meters (default: 5000)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{utm_crs} - EPSG code for the UTM projection used
#'     \item \code{segment} - sf linestring object in WGS84 (EPSG:4326)
#'     \item \code{segment_utm} - sf linestring object in UTM projection
#'     \item \code{buffer} - sf polygon buffer in WGS84 (EPSG:4326)
#'     \item \code{buffer_utm} - sf polygon buffer in UTM projection
#'   }
#'
#' @details
#' The function automatically detects the appropriate UTM zone based on the
#' centroid of the input segment. Dateline crossings are handled by normalizing
#' coordinates to 0-360 range when necessary.
#'
#' @examples
#' # create transect across California Current
#' coords <- matrix(c(-120, 34, -118, 36), ncol = 2, byrow = TRUE)
#' result <- buffer_transect(coords, buffer_dist = 10000)
#' plot(result$buffer)
#'
#' @seealso \code{\link{fix_dateline_crossing}} for dateline crossing detection
#' @seealso \code{\link{modal_depth_profile}} for UI implementation
#'
#' @importFrom sf st_sf st_sfc st_linestring st_centroid st_coordinates st_transform st_buffer st_wrap_dateline
#' @importFrom units set_units
#'
#' @export
buffer_transect <- function(coords, buffer_dist = 5000) {
  # create initial segment
  segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # handle dateline crossing
  segment <- fix_dateline_crossing(segment)

  # get centroid to determine UTM zone
  centroid <- st_centroid(segment)
  cent_coords <- st_coordinates(centroid)
  lon <- cent_coords[1, "X"]
  lat <- cent_coords[1, "Y"]

  # adjust longitude for UTM if it was shifted
  lon <- ifelse(lon > 180, lon - 360, lon)

  # calculate UTM zone
  zone <- floor((lon + 180) / 6) + 1
  hemisphere <- if (lat >= 0) 32600 else 32700
  utm_crs <- hemisphere + zone

  # transform to UTM, buffer, and transform back
  segment_utm <- st_transform(segment, utm_crs)
  buffer_utm <- st_buffer(segment_utm, dist = buffer_dist, endCapStyle = "FLAT")
  buffer <- st_transform(buffer_utm, 4326)

  # ensure buffer is valid and handles dateline
  buffer <- st_wrap_dateline(buffer, options = c("WRAPDATELINE=YES"))

  return(list(
    utm_crs     = utm_crs,
    segment     = segment,
    segment_utm = segment_utm,
    buffer      = buffer,
    buffer_utm  = buffer_utm))
}


#' Detect and Handle Dateline Crossing in Line Segments
#'
#' Normalizes longitude coordinates when a line segment crosses the ±180°
#' dateline, preventing discontinuities in buffering and visualization.
#'
#' @param segment sf linestring object representing a transect or track
#'
#' @return sf linestring object with normalized coordinates (0-360° range if dateline is crossed)
#'
#' @details
#' Dateline crossings are detected by checking for longitude jumps > 180°.
#' When detected, negative longitudes are shifted to 0-360° range. The segment
#' is then segmentized to 1000m intervals for smooth buffering.
#'
#' @examples
#' \dontrun{
#' # transect crossing the dateline
#' coords <- matrix(c(175, 30, -175, 35), ncol = 2, byrow = TRUE)
#' segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))
#' normalized <- fix_dateline_crossing(segment)
#' }
#'
#' @seealso \code{\link{buffer_transect}} for usage in buffering workflow
#'
#' @importFrom sf st_coordinates st_sf st_sfc st_linestring st_segmentize
#' @importFrom units set_units
#'
#' @keywords internal
fix_dateline_crossing <- function(segment) {
  coords <- st_coordinates(segment)[, c("X", "Y")]
  lons <- coords[, "X"]

  # check for dateline crossing (large longitude jump)
  lon_diff <- diff(lons)
  crosses_dateline <- any(abs(lon_diff) > 180)

  if (!crosses_dateline) return(segment)

  # normalize longitudes to avoid discontinuity
  # shift coords to a 0-360 range if crossing +180/-180
  if (any(lons < 0)) {
    coords[, "X"] <- ifelse(lons < 0, lons + 360, lons)
  }

  # create new linestring
  new_segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # optional: split into multiple segments if needed
  # use st_segmentize to add points across dateline for smoother buffer
  new_segment <- st_segmentize(new_segment, set_units(1000, "m"))

  return(new_segment)
}


# reproducible downloads ----
# The download bundle pairs every data file with the exact, portable SQL that
# produced it. The integration query is built and executed by
# calcofi4r::cc_match_bio_env() against the public GCS release parquet, so
# anyone can re-run query/integrated_*.sql in DuckDB (CLI, Python or R) and get
# identical rows. See query/REPRODUCE.md inside any downloaded bundle.

#' Resolve the frozen-release version powering the app database
#'
#' Parses the version (e.g. \code{"v2026.05.14"}) out of the app's DuckDB file
#' name; falls back to the GCS \code{latest.txt} pointer.
#'
#' @param path Path to the app database (default: the \code{db_path} global).
#' @return Release version string.
#' @export
release_version <- function(path = db_path) {
  v <- sub(".*calcofi_(v[0-9][0-9.]*)\\.duckdb$", "\\1", basename(path))
  if (grepl("^v[0-9]", v)) return(v)
  tryCatch(
    trimws(readLines(
      "https://storage.googleapis.com/calcofi-db/ducklake/releases/latest.txt",
      warn = FALSE)[1]),
    error = function(e) "latest")
}

#' A release catalog, fetched once per version
#'
#' @param version Release version string from \code{\link{release_version}}.
#' @return The catalog as a nested list (\code{calcofi4r::cc_catalog()}).
#' @export
release_catalog <- local({
  cache <- new.env(parent = emptyenv())
  function(version) {
    version <- as.character(version)
    if (is.null(cache[[version]]))
      cache[[version]] <- calcofi4r::cc_catalog(version)
    cache[[version]]
  }
})

#' Per-table \code{read_parquet()} SQL for a release
#'
#' Every release table is resolved through the catalog by
#' \code{calcofi4r::cc_release_sources()} — the one sanctioned map from a table
#' to its parquet bytes: content-addressed objects
#' (\code{ducklake/tables/{table}/{hash}/…}) since the v2026.09 releases, the
#' per-release \code{releases/{version}/parquet/…} path before that (an
#' \code{s3://} glob for a legacy partitioned table such as \code{obs}, which
#' needs the anonymous-S3 settings of \code{\link{gcs_s3_settings_sql}}). Never
#' build that path by hand. Same shape as \code{calcofi4r:::.cc_read_parquet()}.
#'
#' @param version Release version string from \code{\link{release_version}}.
#' @return A function \code{table -> "read_parquet(...)"} SQL fragment.
#' @export
release_read_parquet <- function(version) {
  cat_ <- release_catalog(version)
  function(table)
    calcofi4r::cc_read_parquet_sql(calcofi4r::cc_release_sources(cat_, table))
}

#' Resolved parquet sources of release tables, for manifest provenance
#'
#' @param version Release version string.
#' @param tables Character vector of release table names.
#' @return Named list (one per table) of \code{urls}, \code{hive_partitioning}
#'   and, on a content-addressed release, \code{content_hash} per object.
#' @export
release_table_sources <- function(version, tables) {
  cat_ <- release_catalog(version)
  setNames(lapply(tables, function(tb) {
    s   <- calcofi4r::cc_release_sources(cat_, tb)
    out <- list(urls = as.list(as.character(s$urls)), hive_partitioning = isTRUE(s$hive))
    if (any(!is.na(s$hashes))) out$content_hash <- as.list(unname(s$hashes))
    out
  }), tables)
}

#' DuckDB settings that let an \code{s3://} release glob read GCS anonymously
#'
#' Only a legacy (pre-v2026.09) partitioned table resolves to one; the five
#' \code{SET}s are those of \code{calcofi4r:::.cc_setup_gcs_httpfs()}.
#'
#' @param sql SQL string(s); the settings are emitted only if one reads \code{s3://}.
#' @return A SQL string (possibly empty).
#' @export
gcs_s3_settings_sql <- function(sql) {
  if (!any(startsWith(extract_source_urls(sql), "s3://"))) return("")
  paste(
    "SET s3_region = 'auto';",
    "SET s3_endpoint = 'storage.googleapis.com';",
    "SET s3_url_style = 'path';",
    "SET s3_access_key_id = '';",
    "SET s3_secret_access_key = '';",
    sep = "\n")
}

# the release tables the portable bio + env match queries read
MATCH_TABLES <- c("obs", "taxon", "sample_measurement")

#' Extract a scientific name from a UI species label
#'
#' Species dropdown labels look like \code{"Common name (rank: Scientific
#' name)"} (or \code{"(Scientific name)"} when no common name). Returns the
#' scientific name.
#'
#' @param label Character species label.
#' @return Scientific name (character).
#' @export
extract_scientific_name <- function(label) {
  # "Common (rank: Scientific)" -> "Scientific" (last parenthetical, after ': ')
  sci <- sub(".*\\(.*:\\s*([^)]+)\\).*", "\\1", label)
  if (identical(sci, label))
    # fallback "Common (Scientific)" / "(Scientific)" -> last parenthetical
    sci <- sub(".*\\(([^)]+)\\)\\s*$", "\\1", label)
  trimws(sci)
}

#' Distinct read_parquet() source URLs referenced in a SQL string
#'
#' Handles both \code{read_parquet('url')} and the explicit file-list form
#' \code{read_parquet(['url', 'url'], hive_partitioning = true)} of a
#' content-addressed partitioned table.
#'
#' @param sql One or more SQL strings.
#' @return Sorted unique character vector of parquet URLs.
#' @export
extract_source_urls <- function(sql) {
  one  <- paste(sql, collapse = "\n")
  hits <- regmatches(one, gregexpr("read_parquet\\(\\[?\\s*'[^']+'(\\s*,\\s*'[^']+')*", one))[[1]]
  urls <- unlist(regmatches(hits, gregexpr("'[^']+'", hits)))
  sort(unique(gsub("^'|'$", "", urls)))
}

#' Build the biological (ichthyoplankton) match subquery
#'
#' Emits a portable \code{SELECT} over the release's GCS parquet, shaped for
#' \code{calcofi4r::cc_match_bio_env()} (columns \code{bio_id}, \code{bio_datetime},
#' \code{bio_lon}, \code{bio_lat}, \code{bio_value} plus descriptive columns).
#' When \code{include_children} is \code{TRUE} the species filter is expanded
#' via a recursive walk of the WoRMS \code{taxon.parentNameUsageID} tree.
#'
#' @param sci_names Character vector of scientific names.
#' @param qtr Integer vector of quarters (1-4).
#' @param date_range Length-2 date vector (tow start bounds).
#' @param version Release version string.
#' @param include_children Include descendant taxa (default: TRUE).
#' @return SQL \code{SELECT} string.
#' @export
build_bio_match_sql <- function(
    sci_names, qtr, date_range, version, include_children = TRUE) {

  rp   <- release_read_parquet(version)
  nm   <- paste0("'", gsub("'", "''", sci_names), "'", collapse = ", ")
  qtrs <- paste(as.integer(qtr), collapse = ", ")
  d1   <- as.character(date_range[1])
  d2   <- as.character(date_range[2])

  prefix        <- ""
  species_where <- glue("t.scientific_name IN ({nm})")
  if (isTRUE(include_children)) {
    # unified taxon: seed by scientific_name, walk descendants via parent_taxon_key
    prefix <- glue(
      "WITH RECURSIVE taxon_tree AS (
      SELECT taxon_key
      FROM {rp('taxon')}
      WHERE scientific_name IN ({nm})
    UNION ALL
      SELECT t.taxon_key
      FROM {rp('taxon')} t
      JOIN taxon_tree tt ON t.parent_taxon_key = tt.taxon_key
  )
  ")
    species_where <- "o.taxon_key IN (SELECT taxon_key FROM taxon_tree)"
  }

  # read the consolidated core `obs` (ichthyo abundance) + effort from
  # `sample_measurement` — the per-dataset ichthyo/net/tow/site tables are retired.
  # Mirrors calcofi4r::.cc_bio_sql_ichthyo (kept 1:1 with the app matcher).
  glue(
    "  {prefix}SELECT
    o.obs_id::VARCHAR AS bio_id,
    o.datetime        AS bio_datetime,
    o.longitude       AS bio_lon,
    o.latitude        AS bio_lat,
    o.measurement_value * shf.measurement_value / nullif(ps.measurement_value, 0) AS bio_value,
    t.scientific_name,
    t.common_name,
    t.worms_id,
    o.life_stage,
    o.measurement_value AS tally,
    extract(quarter FROM o.datetime)::INTEGER AS quarter
  FROM {rp('obs')} o
  JOIN {rp('taxon')} t ON t.taxon_key = o.taxon_key
  LEFT JOIN {rp('sample_measurement')} shf ON shf.sample_key = o.sample_key AND shf.measurement_type = 'std_haul_factor'
  LEFT JOIN {rp('sample_measurement')} ps  ON ps.sample_key  = o.sample_key AND ps.measurement_type = 'prop_sorted'
  WHERE o.realm = 'bio' AND o.dataset_key = 'swfsc_ichthyo' AND o.measurement_type = 'abundance'
    AND o.measurement_value IS NOT NULL
    AND {calcofi4r::cc_qual_ok_sql('o')}
    AND o.datetime IS NOT NULL
    AND o.longitude IS NOT NULL
    AND o.latitude IS NOT NULL
    AND {species_where}
    AND extract(quarter FROM o.datetime) IN ({qtrs})
    AND o.datetime >= TIMESTAMP '{d1}'
    AND o.datetime <= TIMESTAMP '{d2}'")
}

#' Build the environmental (CTD-bottle) match subquery
#'
#' Emits a portable \code{SELECT} over the release's GCS parquet, shaped for
#' \code{calcofi4r::cc_match_bio_env()} (columns \code{env_id}, \code{env_datetime},
#' \code{env_lon}, \code{env_lat}, \code{env_value}, \code{env_depth_m},
#' \code{measurement_type}). The date window is padded by \code{pad_hours} so
#' boundary matches survive the downstream interval join.
#'
#' @param env_var Environmental \code{measurement_type}.
#' @param qtr Integer vector of quarters (1-4).
#' @param date_range Length-2 date vector (cast datetime bounds).
#' @param depth_range Length-2 numeric vector (bottle depth bounds, meters).
#' @param version Release version string.
#' @param pad_hours Hours to pad the date window (default: 6).
#' @return SQL \code{SELECT} string.
#' @export
build_env_match_sql <- function(
    env_var, qtr, date_range, depth_range, version, pad_hours = 6) {

  rp   <- release_read_parquet(version)
  qtrs <- paste(as.integer(qtr), collapse = ", ")
  d1   <- as.character(date_range[1])
  d2   <- as.character(date_range[2])
  dmin <- depth_range[1]
  dmax <- depth_range[2]

  # read the consolidated core `obs` (env realm, bottle) — the per-dataset
  # bottle_measurement/bottle/casts tables are retired. Mirrors
  # calcofi4r::.cc_env_sql.
  glue(
    "  SELECT
    o.obs_id             AS env_id,
    o.datetime           AS env_datetime,
    o.longitude          AS env_lon,
    o.latitude           AS env_lat,
    o.measurement_value  AS env_value,
    o.depth_min_m        AS env_depth_m,
    o.measurement_type   AS measurement_type
  FROM {rp('obs')} o
  WHERE o.realm = 'env' AND o.dataset_key = 'calcofi_bottle' AND o.measurement_type = '{env_var}'
    AND o.measurement_value IS NOT NULL
    AND {calcofi4r::cc_qual_ok_sql('o')}
    AND o.datetime IS NOT NULL
    AND o.longitude IS NOT NULL
    AND o.latitude IS NOT NULL
    AND o.depth_min_m >= {dmin}
    AND o.depth_min_m <= {dmax}
    AND extract(quarter FROM o.datetime) IN ({qtrs})
    AND o.datetime >= TIMESTAMP '{d1}' - INTERVAL '{pad_hours} hours'
    AND o.datetime <= TIMESTAMP '{d2}' + INTERVAL '{pad_hours} hours'")
}

#' Render the CITATION.md for a download bundle
#'
#' One dataset per file this app draws from (the ichthyoplankton bio side, the
#' bottle env side — see \code{\link{build_download_bundle}}), plus the
#' integrated release's own citation, via \code{calcofi4r::cc_cite()} (>=
#' 1.19.0) so this bundle's citations, licences, DOIs and dataset-page links
#' can never disagree with the app's other citation surfaces (the About page,
#' \code{cc_cite()} elsewhere). With an older calcofi4r installed (no
#' \code{cc_cite()} page line yet), degrades to the release/dataset page URLs
#' alone rather than erroring — a bundle must always ship a CITATION.md.
#'
#' @param version Release version the bundle was built against.
#' @param datasets `dataset_key`s this bundle draws from.
#' @return Character vector of markdown lines.
#' @export
citation_md <- function(version, datasets = c("calcofi_bottle", "swfsc_ichthyo")) {
  page_url <- function(key) sprintf("https://calcofi.io/datasets/%s/", key)
  if (!requireNamespace("calcofi4r", quietly = TRUE) ||
      utils::packageVersion("calcofi4r") < "1.19.0") {
    return(c(
      "# Citing this data",
      "",
      "Cite the CalCOFI integrated database and each dataset in this bundle:",
      "",
      sprintf("- **%s**: %s", datasets, page_url(datasets)),
      "",
      sprintf("The integrated database, release %s: https://calcofi.io/db-schema/?v=%s",
             version, version),
      "",
      "(calcofi4r >= 1.19.0 is not installed here, so the formatted citation,",
      "licence and DOI from calcofi4r::cc_cite() are not available — the",
      "dataset pages above carry them.)"))
  }
  lines <- calcofi4r::cc_cite(datasets, version = version, format = "text")
  c("# Citing this data",
    "",
    "Cite the CalCOFI integrated database AND every dataset in this bundle.",
    "",
    "## The integrated database", "", lines[1], "",
    unlist(lapply(seq_along(datasets), function(i)
      c(sprintf("## %s", datasets[i]), "", lines[i + 1], ""))))
}

#' Render the REPRODUCE.md walk-through for a download bundle
#'
#' @param manifest The manifest list assembled by \code{\link{build_download_bundle}}.
#' @return Character vector of markdown lines.
#' @export
reproduce_md <- function(manifest) {
  v <- manifest$release_version
  c(
    "# Reproducing this CalCOFI download",
    "",
    glue(
      "Every file under `data/` was produced by a SQL query in `query/`, run ",
      "against the **public** Parquet files of CalCOFI release `{v}` on Google ",
      "Cloud Storage. Re-run any `.sql` file in DuckDB and you get identical ",
      "rows — no credentials, no API and no app required."),
    "",
    "## What's in this bundle",
    "",
    "| data file | query | description |",
    "|---|---|---|",
    "| `data/original/bio.csv` | `query/bio.sql` | net-tow ichthyoplankton (standardized tally) |",
    "| `data/original/env.csv` | `query/env.sql` | CTD-bottle environmental measurements |",
    "| `data/integrated/integrated_<method>.csv` | `query/integrated_<method>.sql` | bio matched to env in time + space |",
    "| `query/manifest.json` | — | release version, filters, row counts, md5 checksums |",
    "",
    glue(
      "`<method>` is one of `nearest_time`, `nearest_dist`, `average` — how the ",
      "environmental observations within the match window are reduced per ",
      "biological observation."),
    "",
    "## Re-run the integration query",
    "",
    "### DuckDB CLI",
    "",
    "```sh",
    "duckdb < query/integrated_nearest_time.sql",
    "```",
    "",
    "(each `.sql` file is prefixed with the `INSTALL`/`LOAD` of `httpfs` + `spatial`,",
    "plus the anonymous-S3 `SET`s when a release table is read as an `s3://` glob)",
    "",
    "### Python",
    "",
    "```python",
    "import duckdb",
    "con = duckdb.connect()",
    "df = con.sql(open('query/integrated_nearest_time.sql').read()).df()",
    "```",
    "",
    "### R",
    "",
    "```r",
    "library(DBI)",
    "con <- dbConnect(duckdb::duckdb())",
    "sql <- paste(readLines('query/integrated_nearest_time.sql'), collapse = '\\n')",
    "df  <- dbGetQuery(con, sql)",
    "```",
    "",
    "Or with the **calcofi4r** package — the same helper that generated this bundle:",
    "",
    "```r",
    "# remotes::install_github('calcofi/calcofi4r')",
    "library(calcofi4r)",
    "d <- cc_match_ichthyo_by_name(",
    "  'Sardinops sagax', env_var = 'temperature',",
    "  date_min = '2018-01-01', date_max = '2018-03-31', relax_matching = TRUE)",
    "cat(attr(d, 'sql'))   # the portable SQL behind it",
    "```",
    "",
    "## Verify integrity",
    "",
    "```sh",
    "md5sum data/integrated/integrated_nearest_time.csv",
    "# compare against query/manifest.json -> files[...].md5",
    "```")
}

#' Assemble the reproducible portion of a download bundle
#'
#' Builds the portable bio + env subqueries from the current filter
#' \code{params}, runs them (and the integration, once per join method) against
#' the public GCS release parquet via \code{calcofi4r::cc_match_bio_env()}, and
#' writes \code{data/original/}, \code{data/integrated/} and \code{query/}
#' (per-file \code{*.sql}, \code{manifest.json}, \code{REPRODUCE.md}) under
#' \code{zip_root}. The SQL that is serialized is exactly the SQL that was run.
#'
#' @param zip_root Directory to write the bundle into.
#' @param params Filter params list (from \code{rx$params}): \code{taxa},
#'   \code{env_var}, \code{sel_qtr}/\code{quarters}, \code{date_range},
#'   \code{depth_range}, \code{ck_children}/\code{include_children},
#'   \code{time_window}, \code{dist_window}, \code{zones}.
#' @param version Release version string; defaults to \code{\link{release_version}()}.
#' @return Character vector of bundle-relative paths written.
#' @export
build_download_bundle <- function(zip_root, params, version = NULL) {

  if (utils::packageVersion("calcofi4r") < "1.11.0")
    stop(
      "build_download_bundle() needs calcofi4r >= 1.11.0 ",
      "(cc_match_bio_env + cc_release_sources). ",
      "Update with: remotes::install_github('calcofi/calcofi4r')")

  version <- version %||% release_version()

  # resolve filters from params --------------------------------------------
  taxa             <- params$taxa
  sci_names        <- vapply(taxa, extract_scientific_name, character(1),
                             USE.NAMES = FALSE)
  include_children <- isTRUE(params$ck_children %||%
                             params$include_children %||% TRUE)
  qtr         <- params$sel_qtr %||% params$quarters %||% 1:4
  date_range  <- params$date_range
  depth_range <- params$depth_range %||% c(0, 5000)
  max_time_hr <- params$time_window %||% default_max_hours_diff
  max_dist_km <- (params$dist_window %||% default_max_meters_diff) / 1000

  # two portable subqueries ------------------------------------------------
  bio_sql <- build_bio_match_sql(
    sci_names, qtr, date_range, version, include_children)
  env_sql <- build_env_match_sql(
    params$env_var, qtr, date_range, depth_range, version,
    pad_hours = max_time_hr)

  # GCS-capable connection (httpfs + spatial) ------------------------------
  con_gcs <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con_gcs, shutdown = TRUE), add = TRUE)
  dbExecute(con_gcs, "INSTALL httpfs; LOAD httpfs;")
  dbExecute(con_gcs, "INSTALL spatial; LOAD spatial;")
  # a legacy (pre-v2026.09) partitioned table resolves to an s3:// glob, which
  # DuckDB expands through its S3 client pointed anonymously at GCS; the same
  # settings go into the bundle's .sql files so they stay copy-paste runnable
  s3_sql <- gcs_s3_settings_sql(c(bio_sql, env_sql))
  if (nzchar(s3_sql)) dbExecute(con_gcs, s3_sql)

  # writer helpers ---------------------------------------------------------
  paths      <- character()
  files_meta <- list()
  write_file <- function(rel, x) {
    full <- file.path(zip_root, rel)
    dir.create(dirname(full), showWarnings = FALSE, recursive = TRUE)
    if (is.data.frame(x)) {
      write.csv(x, full, row.names = FALSE, na = "")
    } else {
      writeLines(as.character(x), full)
    }
    paths <<- c(paths, rel)
    full
  }
  # .sql files are written copy-paste runnable: prefixed with the extension
  # loads they need to read GCS parquet over HTTPS + compute spatial distance
  sql_header <- paste(
    "-- Re-run in DuckDB (CLI, Python or R) against public CalCOFI release",
    "-- parquet. See REPRODUCE.md. No credentials or API required.",
    "INSTALL httpfs; LOAD httpfs;",
    "INSTALL spatial; LOAD spatial;",
    if (nzchar(s3_sql)) s3_sql,
    "", "", sep = "\n")
  write_sql <- function(rel, sql) write_file(rel, paste0(sql_header, sql, "\n"))
  add_meta <- function(rel_csv, rel_sql, df, extra = list()) {
    files_meta[[rel_csv]] <<- c(
      list(
        sql    = rel_sql,
        n_rows = nrow(df),
        md5    = unname(tools::md5sum(file.path(zip_root, rel_csv)))),
      extra)
  }

  # original bio + env -----------------------------------------------------
  write_sql("query/bio.sql", bio_sql)
  write_sql("query/env.sql", env_sql)
  # Materialize bio + env into local temp tables ONCE. The three join methods
  # below then compute against these (fast) instead of each re-embedding the
  # bio/env subqueries and re-scanning the 17.5M-row obs.parquet over HTTPS —
  # which made the bundle take ~5 min (3×~95s) and blow the proxy timeout, so
  # the browser got a truncated response ("Site wasn't available"). GCS is now
  # scanned twice total (here), not eight times, cutting the bundle to ~30s.
  dbExecute(con_gcs, glue("CREATE TEMP TABLE _bio_src AS {bio_sql}"))
  dbExecute(con_gcs, glue("CREATE TEMP TABLE _env_src AS {env_sql}"))
  d_bio <- dbGetQuery(con_gcs, "SELECT * FROM _bio_src")
  d_env <- dbGetQuery(con_gcs, "SELECT * FROM _env_src")
  write_file("data/original/bio.csv", d_bio)
  write_file("data/original/env.csv", d_env)
  add_meta("data/original/bio.csv", "query/bio.sql", d_bio)
  add_meta("data/original/env.csv", "query/env.sql", d_env)

  # integrated match, once per join method ---------------------------------
  # Compute against the local temp tables (fast). Write the PORTABLE GCS-parquet
  # SQL (built from bio_sql/env_sql via return_sql, runnable anywhere) to the
  # bundle's .sql files — the temp tables are only a local compute shortcut, so
  # the .sql re-run against GCS yields byte-identical rows to the CSV.
  methods <- c("nearest_time", "nearest_dist", "average")
  for (m in methods) {
    d <- calcofi4r::cc_match_bio_env(
      "SELECT * FROM _bio_src", "SELECT * FROM _env_src",
      max_dist_km = max_dist_km, max_time_hr = max_time_hr,
      join_method = m, con = con_gcs, version = version, collect = TRUE)
    portable_sql <- as.character(calcofi4r::cc_match_bio_env(
      bio_sql, env_sql,
      max_dist_km = max_dist_km, max_time_hr = max_time_hr,
      join_method = m, version = version, return_sql = TRUE))
    rel_csv <- glue("data/integrated/integrated_{m}.csv")
    rel_sql <- glue("query/integrated_{m}.sql")
    write_sql(rel_sql, portable_sql)
    write_file(rel_csv, as.data.frame(d))
    add_meta(rel_csv, rel_sql, d, list(join_method = m))
  }

  # manifest.json ----------------------------------------------------------
  manifest <- list(
    schema_version    = "1.0",
    generated_at      = format(Sys.time(), tz = "UTC", usetz = TRUE),
    release_version   = version,
    calcofi4r_version = as.character(utils::packageVersion("calcofi4r")),
    # per release table: the parquet object(s) the catalog resolved it to, with
    # content_hash on a content-addressed (v2026.09+) release
    release_sources   = release_table_sources(version, MATCH_TABLES),
    filters = list(
      taxa             = as.list(taxa),
      scientific_names = as.list(sci_names),
      include_children = include_children,
      env_var          = params$env_var,
      quarters         = as.list(as.integer(qtr)),
      date_range       = as.character(date_range),
      depth_range_m    = as.list(depth_range),
      zones            = if (is.null(params$zones))
        "all locations" else as.list(params$zones)),
    match_params = list(
      max_dist_km  = max_dist_km,
      max_time_hr  = max_time_hr,
      join_methods = as.list(methods)),
    gcs_source_urls = as.list(extract_source_urls(c(bio_sql, env_sql))),
    files           = files_meta)
  write_file(
    "query/manifest.json",
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"))

  # REPRODUCE.md -----------------------------------------------------------
  write_file("query/REPRODUCE.md", reproduce_md(manifest))

  # CITATION.md (plan 2026-09-05 D-4/D-6: every dataset the bundle draws
  # from — bio via ichthyo, env via bottle — gets its own citation + page) --
  write_file("query/CITATION.md", citation_md(version))

  paths
}


#' Which quantities a species selection actually contains
#'
#' `std_tally` is not one quantity. Where a net tow supports standardization it is
#' a gear-standardized density (`count/10m2` for oblique/vertical, `count/100m3`
#' for manta); everywhere else it is the value the source published, in that
#' source's own unit — an areal density for the euphausiid and ZooScan series, a
#' bare occurrence count for `cdfw_dungeness-crab`, which is a lab-examined
#' aliquot of an archived catch with no tow volume to divide by.
#'
#' Calling all of that "CPUE" is wrong in the last case: nothing was divided by
#' effort. Calling it "density" is wrong too — that was the bug this replaced. So
#' the app asks the data what it is holding, and says so.
#'
#' Deliberately keyed on `tow_type`/`std_haul_factor` presence rather than on
#' dataset or unit names: those are the same columns `prep_db.R` branches on to
#' compute `cpue_unit`, so a dataset added later is classified correctly without
#' anything here knowing its name.
#'
#' @param df_sp species table (lazy or collected) carrying `cpue_unit`,
#'   `tow_type`, `std_haul_factor`
#' @return a tibble with `cpue_unit`, `standardized` (logical), `n`, ordered by
#'   `n` descending; zero rows if nothing is summarizable
#' @export
sp_unit_summary <- function(df_sp) {
  # WARN, never swallow. An earlier version returned an empty tibble on any
  # error, which turned a scope bug (`df_sp` not visible in the render block)
  # into a plausible-looking "Avg. value" legend with no note — indistinguishable
  # from a genuinely empty selection, and invisible in the log. If this cannot
  # summarize, the reason belongs in the log.
  out <- tryCatch(
    df_sp |>
      dplyr::filter(!is.na(std_tally), !is.na(cpue_unit)) |>
      # `cpue_standardized` comes from prep_db.R, computed in the same CASE that
      # produces cpue_unit. Do NOT re-derive it here from tow_type/effort columns:
      # an earlier version did, and drifted immediately — it treated any tow
      # carrying a volume as standardized, though prep_db only standardizes by
      # volume for manta. One rule, one place.
      dplyr::mutate(standardized = as.logical(cpue_standardized)) |>
      dplyr::count(cpue_unit, standardized) |>
      dplyr::collect(),
    error = function(e) {
      warning("sp_unit_summary(): ", conditionMessage(e), call. = FALSE)
      NULL
    })
  if (is.null(out) || !nrow(out))
    return(tibble::tibble(cpue_unit = character(), standardized = logical(), n = integer()))
  dplyr::arrange(out, dplyr::desc(n))
}

#' Legend title for a species layer, named by what the values actually are
#'
#' One unit: name it (`"Avg. count/10m2"`). Several: say so rather than picking a
#' label that is true of only some rows — the hexagon value averages across them,
#' and a mean over a mix of units is not a quantity. The sidebar note carries the
#' breakdown; see [sp_unit_summary()].
#'
#' @param u a [sp_unit_summary()] tibble
#' @return a character legend title
#' @export
sp_value_label <- function(u) {
  if (!nrow(u))        return("Avg. value")
  if (nrow(u) == 1L)   return(paste0("Avg. ", u$cpue_unit[1]))
  paste0("Avg. value (", nrow(u), " mixed units)")
}
