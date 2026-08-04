# prep_db.R - build optimized local database for db-viz-hex
#
# usage:
#   Rscript prep_db.R              # uses latest release
#   Rscript prep_db.R v2026.04.06  # uses specific version
#
# re-run when a new calcofi4r release is available. idempotent:
# skips already-downloaded parquets, rebuilds materialized tables.

# remotes::install_github("calcofi/calcofi4r")
devtools::load_all("../calcofi4r")
librarian::shelf(
  calcofi / calcofi4r,
  DBI,
  duckdb,
  glue,
  here,
  sf,
  purrr,
  stringr,
  quiet = TRUE
)

# resolve version from command line or default to latest
args <- commandArgs(trailingOnly = TRUE)
db_version <- if (length(args) > 0) args[1] else "latest"
db_dir <- here("data")
hex_geo <- here("data/hex.geojson")

# minimal set of source tables needed to build the app database:
#   obs                — consolidated observations (realm bio|env); carries
#                        hex_id (H3 res-10) and is partitioned by dataset_key
#   sample_measurement — std_haul_factor + prop_sorted + volume_sampled for CPUE
#   sample             — event dimension; carries tow_type (net gear) for
#                        net-type-aware CPUE (release >= v2026.07.17)
#   taxon/dataset_taxon — unified taxon reference + per-dataset crosswalk; the
#                        app's picker + taxa-tree queries want the legacy
#                        `species` + WoRMS-hierarchy `taxon` shapes, so we derive
#                        those (+ `taxa_rank`) locally from these below.
#   measurement_type   — units + is_canonical, so a dataset with no gear-based
#                        CPUE can still be shown honestly in its own units
#   spatial/…_attribute — polygon layers (MPAs, counties, sanctuaries, …) for
#                        summarizing within a boundary rather than within a hex
keep_tables <- c("obs", "sample_measurement", "sample", "taxon", "dataset_taxon",
                 "measurement_type", "spatial", "spatial_attribute")

cat("fetching catalog for version:", db_version, "\n")
info       <- cc_db_info(version = db_version)
all_tables <- info$tables$name
missing    <- setdiff(keep_tables, all_tables)
if (length(missing) > 0)
  stop("release ", db_version, " is missing required tables: ",
       paste(missing, collapse = ", "))
cat("tables to load:", paste(keep_tables, collapse = ", "), "\n")

# step A: download parquets + create local DuckDB tables ----
# delete stale DuckDB to avoid schema conflicts from prior materialized views
db_version_resolved <- if (db_version == "latest") {
  trimws(readLines(
    "https://storage.googleapis.com/calcofi-db/ducklake/releases/latest.txt",
    warn = FALSE)[1])
} else db_version
db_file <- file.path(db_dir, paste0("calcofi_", db_version_resolved, ".duckdb"))
if (file.exists(db_file)) {
  cat("removing stale db:", db_file, "\n")
  file.remove(db_file)
}

con <- cc_get_db(
  version = db_version,
  local_data = TRUE,
  cache_dir = db_dir,
  tables = keep_tables,
  refresh = TRUE
)

# load extensions
dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# bound DuckDB's footprint ----
# The server is a 16 GB n2-standard-4 shared with ~11 containers (postgis, erddap,
# plumber, varnish, rstudio, …) that already hold several GB at rest, so DuckDB's
# default limit of ~80% of TOTAL RAM sizes it against memory it does not actually
# have. Building v2026.08.04 overcommitted, filled the 7 GB swapfile, and thrashed
# the box until it had to be power-cycled — sshd could not complete a banner
# exchange for 20 minutes, and every hosted app went down with it. The step that
# crosses the line is sample_spatial below: a deliberately many-to-many spatial
# join whose hash table is far larger than its ~2.3M output rows.
#
# With an explicit limit DuckDB spills to temp_directory instead of to swap. That
# is slower, but it is BOUNDED and the machine stays reachable — swapping is
# neither. Keep temp_directory on /ssd (where db_dir lives), not on the 40 GB
# boot disk.
dbExecute(con, "SET memory_limit = '8GB'")
dbExecute(con, "SET threads = 3")  # leave a core for the other containers
dbExecute(con, glue("SET temp_directory = '{file.path(db_dir, 'duckdb_tmp')}'"))

# step A2: legacy taxon shims from the unified refs ----
# The release now ships a single unified `taxon` (keyed by taxon_key = worms:<id>
# / itis:<id>) + a `dataset_taxon` crosswalk, replacing the old per-dataset
# `species` / WoRMS-hierarchy `taxon` / `taxa_rank`. The app's picker (global.R)
# and taxa-tree (functions.R::taxa_tree_builder / get_taxon_children) still expect
# those legacy shapes, so derive them here — no app-code change needed.
dbExecute(con, "ALTER TABLE taxon RENAME TO taxon_u")
# legacy `species` + a taxon_key column so bio_obs joins obs.
#
# Was restricted to dataset_key = 'swfsc_ichthyo', which is the main reason the
# app could only see one of the nine bio datasets: any obs whose taxon was not in
# the ichthyo vocabulary simply failed this join and vanished. Now every bio
# dataset's vocabulary is included.
#
# ONE ROW PER taxon_key is load-bearing: a taxon appearing in two datasets
# (Appendicularia is in both zoodb and zooscan) would otherwise fan the bio_obs
# join out and double-count it. species_id is the ichthyo integer code where one
# exists — QUALIFY prefers the row that has it, so the ichthyo tree keeps its ids.
dbExecute(con, "
  CREATE OR REPLACE TABLE species AS
  SELECT * EXCLUDE (rn) FROM (
    SELECT dt.taxon_key,
           TRY_CAST(dt.ds_taxa_code AS INTEGER) AS species_id,
           t.scientific_name, t.common_name, t.worms_id, t.itis_id,
           row_number() OVER (
             PARTITION BY dt.taxon_key
             ORDER BY (dt.dataset_key = 'swfsc_ichthyo') DESC,
                      TRY_CAST(dt.ds_taxa_code AS INTEGER) IS NULL, dt.dataset_key) AS rn
    FROM dataset_taxon dt JOIN taxon_u t USING (taxon_key)
  ) WHERE rn = 1")
# legacy WoRMS-hierarchy `taxon` (authority/taxonID/parentNameUsageID/…) from the
# worms:-keyed rows; parentNameUsageID = the integer in the parent's taxon_key
dbExecute(con, "
  CREATE OR REPLACE TABLE taxon AS
  SELECT 'WoRMS'                                              AS authority,
         worms_id                                            AS taxonID,
         worms_id                                            AS acceptedNameUsageID,
         TRY_CAST(replace(parent_taxon_key,'worms:','') AS INTEGER) AS parentNameUsageID,
         scientific_name                                     AS scientificName,
         rank                                                AS taxonRank,
         taxonomic_status                                    AS taxonomicStatus
  FROM taxon_u WHERE taxon_key LIKE 'worms:%' AND worms_id IS NOT NULL")
# legacy `taxa_rank` (rank -> order) folded into unified taxon.rank_order
dbExecute(con, "
  CREATE OR REPLACE TABLE taxa_rank AS
  SELECT DISTINCT rank AS taxonRank, rank_order
  FROM taxon_u WHERE rank IS NOT NULL")

# step B: bio_obs materialized table ----
# ichthyo (larvae/eggs + folded inverts) observations from the consolidated
# `obs` table: realm='bio', dataset_key='swfsc_ichthyo', measurement_type=
# 'abundance'. obs.taxon_key joins the legacy `species` shim (carries taxon_key).
# std_tally = CPUE (catch per unit effort / density), net-type-aware:
#   * oblique + vertical tows (C1, CB, CV, PV): counts / 10 m^2
#       = tally * std_haul_factor / prop_sorted           (the standard haul factor
#         already standardizes these gears to a 10 m^2 sea-surface column)
#   * manta / surface tows (MT): counts / 100 m^3
#       = tally / prop_sorted / volume_sampled * 100      (volume-based; the manta
#         std_haul_factor does NOT standardize to volume — using it understates
#         manta density ~50x, hence the split)
# std_haul_factor, prop_sorted, volume_sampled + tow_type are carried through so the
# CPUE is reconstructable and appears in the raw-data download (per E. Weber's ask).
# hex_id (H3 res-10) carried through; coarser resolutions derived at query time via
# h3_cell_to_parent(). sorted by scientific_name, time_start.
cat("building bio_obs...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE bio_obs AS
  SELECT
    o.sample_key,
    o.life_stage        AS source,
    sp.scientific_name,
    sp.common_name,
    sp.species_id,
    sp.worms_id,
    tx.parentNameUsageID AS parent_id,
    o.measurement_value AS tally,
    smp.tow_type,
    shf.measurement_value AS std_haul_factor,
    ps.measurement_value  AS prop_sorted,
    vol.measurement_value AS volume_sampled,
    -- CPUE where the gear supports it, the published value where it does not.
    -- The two ichthyo net formulas below only mean anything for a net tow with a
    -- standard haul factor; the other bio datasets publish their own densities
    -- (euphausiids in numberPerMeterSquared, zooscan per m^2, …) and forcing them
    -- through a haul-factor formula would invent a number. So: fall back to the
    -- raw value AND carry its own unit, so the map never shows a quantity
    -- labelled as something it is not.
    CASE
      WHEN smp.tow_type = 'MT'
        THEN o.measurement_value / NULLIF(ps.measurement_value, 0)
               / NULLIF(vol.measurement_value, 0) * 100
      WHEN smp.tow_type IS NOT NULL AND shf.measurement_value IS NOT NULL
        THEN o.measurement_value * shf.measurement_value
               / NULLIF(ps.measurement_value, 0)
      ELSE o.measurement_value
    END                 AS std_tally,
    CASE
      WHEN smp.tow_type = 'MT' THEN 'count/100m3'
      WHEN smp.tow_type IS NOT NULL AND shf.measurement_value IS NOT NULL
        THEN 'count/10m2'
      ELSE COALESCE(mt.units, o.measurement_type)
    END                 AS cpue_unit,
    o.dataset_key,
    o.measurement_type,
    o.datetime          AS time_start,
    o.longitude,
    o.latitude,
    EXTRACT(QUARTER FROM o.datetime)::INTEGER   AS quarter,
    o.hex_id
  FROM obs o
  JOIN species sp
    ON sp.taxon_key = o.taxon_key
  -- WoRMS parent taxon id, needed by taxa_tree_builder's (worms_id, parent_id) grouping
  LEFT JOIN taxon tx
    ON sp.worms_id = tx.taxonID AND tx.authority = 'WoRMS'
  LEFT JOIN sample_measurement shf
    ON o.sample_key = shf.sample_key AND shf.measurement_type = 'std_haul_factor'
  LEFT JOIN sample_measurement ps
    ON o.sample_key = ps.sample_key  AND ps.measurement_type = 'prop_sorted'
  LEFT JOIN sample_measurement vol
    ON o.sample_key = vol.sample_key AND vol.measurement_type = 'volume_sampled'
  -- net gear (tow_type) now carried on the core sample table (calcofi4db >= 2.10.0);
  -- obs.sample_key is the net sample_key, which sample denormalizes tow_type onto.
  LEFT JOIN sample smp
    ON o.sample_key = smp.sample_key
  LEFT JOIN measurement_type mt
    ON mt.measurement_type = o.measurement_type
  -- was: dataset_key = 'swfsc_ichthyo' AND measurement_type = 'abundance', which
  -- hid eight of the nine bio datasets (cufes, phytoplankton, zooscan,
  -- euphausiids, bird-mammal, zoodb, phyllosoma, mesopelagic-fish). The realm is
  -- the only restriction that is actually about what this table means.
  WHERE o.realm = 'bio'
    AND o.measurement_value IS NOT NULL
  ORDER BY sp.scientific_name, o.datetime"
)

bio_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM bio_obs")$n
cat("  bio_obs:", format(bio_n, big.mark = ","), "rows\n")

# step C: env_obs materialized table ----
# bottle observations from the consolidated `obs` table: realm='env',
# dataset_key='calcofi_bottle', restricted to the measurement types exposed in
# the app UI. hex_id (H3 res-10) is carried through; coarser resolutions derived
# at query time via h3_cell_to_parent(). column names mirror the prior schema
# (cast_id, lat_dec, lon_dec, datetime_utc, qty) so downstream code is unchanged.
cat("building env_obs...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE env_obs AS
  SELECT
    o.sample_key        AS cast_id,
    o.datetime          AS datetime_utc,
    EXTRACT(QUARTER FROM o.datetime)::INTEGER AS quarter,
    o.latitude          AS lat_dec,
    o.longitude         AS lon_dec,
    o.depth_min_m       AS depth_m,
    o.measurement_type,
    o.measurement_value AS qty,
    o.dataset_key,
    mt.units,
    o.hex_id
  FROM obs o
  LEFT JOIN measurement_type mt
    ON mt.measurement_type = o.measurement_type
  -- was: dataset_key = 'calcofi_bottle' AND a literal list of 15 types, which
  -- excluded the entire 7.3M-row CTD series plus METS, DIC and picoplankton —
  -- together the largest single omission in the app. The type list is now driven
  -- by the registry's own `is_canonical` flag rather than restated here, so a
  -- newly-canonical type appears without editing this file.
  WHERE o.realm = 'env'
    AND o.measurement_value IS NOT NULL
    AND COALESCE(mt.is_canonical, TRUE)
  ORDER BY o.measurement_type, o.datetime"
)

env_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM env_obs")$n
cat("  env_obs:", format(env_n, big.mark = ","), "rows\n")

# step C2: sample_spatial — which polygons each sampling event falls in ----
# Lets the app summarize within a boundary (MPA, county, sanctuary, EEZ, …)
# rather than only within an H3 hexagon.
#
# Joined at the SAMPLE level, not at obs: 1.5M sample rows against 20M obs rows
# is a ~13x smaller point-in-polygon join for exactly the same answer, because an
# observation's position IS its sample's position. bio_obs/env_obs both carry
# sample_key, so the app reaches a polygon through one join.
#
# Deliberately many-to-many: layers overlap (a station can sit inside a county, a
# sanctuary and an MPA at once) and every one of those memberships is true.
#
# The alternative — crosswalking hexes to polygons — is cheaper but wrong at
# every boundary, and we have exact geometry, so there is no reason to approximate.
cat("building sample_spatial...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE sample_spatial AS
  SELECT s.sample_key,
         sp.spatial_key,
         sp.layer,
         sp.name AS spatial_name
  FROM sample s
  JOIN spatial sp
    -- Relabel BOTH sides to the same CRS rather than coercing one to match the
    -- other. DuckDB refuses ST_Intersects across differing CRS tags, and which
    -- tag each side carries depends on the release:
    --   up to v2026.08.02  sample.geom OGC:CRS84 (ST_Point) vs spatial.geom
    --                      EPSG:4326 (ST_Read over GeoJSON) -> the join ERRORED
    --   from v2026.08.03   both EPSG:4326, normalized at release time
    -- Forcing sp.geom to OGC:CRS84 fixed the first case and would BREAK the
    -- second, recreating the very mismatch it was added for. Setting both makes
    -- this work against either release. ST_SetCRS relabels without transforming;
    -- every one of these is WGS 84 lon/lat regardless of the label.
    ON ST_Intersects(ST_SetCRS(sp.geom, 'EPSG:4326'),
                     ST_SetCRS(s.geom,  'EPSG:4326'))
  -- `geom IS NOT NULL` is NOT sufficient. Release v2026.08.02 shipped 1,590 rows
  -- with NaN coordinates, and ST_Point(NaN, NaN) is a real non-NULL GEOMETRY, so
  -- they pass that check — and they do not merely add junk rows, they CORRUPT the
  -- whole result: with them present this join returns a different number of
  -- matches at different thread counts, dropping valid unrelated pairs. Measured
  -- on one county polygon:
  --      NaN points in : 17,937 (1 thread) / 17,771 (2) / 20,070 (8)
  --      NaN points out: 20,101 / 20,101 / 20,101
  -- The correct answer is higher than any corrupted one, so this was silently
  -- UNDER-counting, and differently on every machine — which is how it was found
  -- (laptop 2,300,433 memberships vs server 2,131,201 on identical inputs).
  -- calcofi4db 3.4.2 stops such geometries being minted; this keeps the join
  -- correct against releases already published without it.
  WHERE s.geom IS NOT NULL
    AND NOT isnan(s.latitude)  AND NOT isinf(s.latitude)
    AND NOT isnan(s.longitude) AND NOT isinf(s.longitude)"
)
ss_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM sample_spatial")$n
ss_s <- dbGetQuery(con, "SELECT COUNT(DISTINCT sample_key) AS n FROM sample_spatial")$n
cat("  sample_spatial:", format(ss_n, big.mark = ","), "memberships across",
    format(ss_s, big.mark = ","), "samples\n")
print(dbGetQuery(con, "
  SELECT layer, COUNT(*) AS memberships, COUNT(DISTINCT sample_key) AS samples
  FROM sample_spatial GROUP BY 1 ORDER BY 2 DESC LIMIT 10"))

# step D: generate hex.geojson ----
# geometries for every H3 cell referenced by bio_obs / env_obs, at each
# resolution 1-10. finest (res-10) cells come straight from obs' hex_id; coarser
# cells are derived with h3_cell_to_parent() and rendered via h3_cell_to_boundary_wkt().
cat("generating hex.geojson...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE hex_base AS
  SELECT DISTINCT hex_id FROM bio_obs WHERE hex_id IS NOT NULL
  UNION
  SELECT DISTINCT hex_id FROM env_obs WHERE hex_id IS NOT NULL"
)
hex_list <- map(1:10, function(res) {
  dbGetQuery(
    con,
    glue(
      "
    WITH parents AS (
      SELECT h3_cell_to_parent(hex_id, {res}) AS parent FROM hex_base)
    SELECT
      HEX(parent)                          AS hex_id,
      {res}                                AS hex_res,
      COUNT(*)                             AS n_sites,
      h3_cell_to_boundary_wkt(HEX(parent)) AS hex_wkt
    FROM parents
    GROUP BY parent"
    )
  ) |>
    st_as_sf(wkt = "hex_wkt", crs = 4326) |>
    st_set_geometry("geometry")
})
sf_hex <- bind_rows(hex_list)
st_write(sf_hex, hex_geo, delete_dsn = TRUE, quiet = TRUE)
cat("  hex.geojson:", nrow(sf_hex), "hexagons across 10 resolutions\n")

# step E: drop build-only objects (keep species/taxon/taxa_rank + bio_obs/env_obs) ----
cat("dropping build-only tables...\n")
drop_obj <- function(con, name) {
  # obs is a remote (partitioned) VIEW; sample_measurement is a local TABLE
  ok <- tryCatch(
    { dbExecute(con, glue("DROP TABLE IF EXISTS \"{name}\"")); TRUE },
    error = function(e) FALSE)
  if (!ok)
    tryCatch(dbExecute(con, glue("DROP VIEW IF EXISTS \"{name}\"")),
             error = function(e) NULL)
}
drop_obj(con, "obs")
drop_obj(con, "sample_measurement")
# `sample` and `spatial_attribute` are build inputs for sample_spatial; `spatial`
# STAYS, because the app needs the polygon geometry to draw and label a boundary
# summary. `sample` goes: bio_obs/env_obs carry everything the app reads from it.
drop_obj(con, "sample")
drop_obj(con, "spatial_attribute")
dbExecute(con, "DROP TABLE IF EXISTS hex_base")

# summary ----
final_tables <- dbListTables(con) |> sort()
cat("\nfinal tables:", paste(final_tables, collapse = ", "), "\n")
cat(
  "done. app database ready at:",
  file.path(db_dir, list.files(db_dir, "calcofi_.*\\.duckdb")),
  "\n"
)

dbDisconnect(con, shutdown = TRUE)

# repoint the `calcofi_latest.duckdb` symlink at the db we just built, so the app
# (global.R defaults to data/calcofi_latest.duckdb) serves this version without a
# manual step. relative target keeps the link valid regardless of mount path.
latest_link <- file.path(db_dir, "calcofi_latest.duckdb")
if (file.exists(latest_link) || !is.na(Sys.readlink(latest_link)))
  unlink(latest_link)
file.symlink(basename(db_file), latest_link)
cat("symlinked calcofi_latest.duckdb ->", basename(db_file), "\n")
