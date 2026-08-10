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

# Optional, because it is a nicety rather than a dependency: `dataset` is the
# release's own registry of what it contains (provider, dataset, dataset_name),
# which global.R uses to LABEL the Taxa / Environmental dataset pickers, so a
# newly ingested dataset names itself instead of waiting on a hand-edited
# lookup. Which datasets those pickers OFFER is measured from obs either way, so
# an older release without this table still preps and still works.
opt_tables <- c("dataset")

cat("fetching catalog for version:", db_version, "\n")
info       <- cc_db_info(version = db_version)
all_tables <- info$tables$name
missing    <- setdiff(keep_tables, all_tables)
if (length(missing) > 0)
  stop("release ", db_version, " is missing required tables: ",
       paste(missing, collapse = ", "))
skipped <- setdiff(opt_tables, all_tables)
if (length(skipped) > 0)
  cat("optional tables absent from this release:",
      paste(skipped, collapse = ", "), "\n")
keep_tables <- c(keep_tables, intersect(opt_tables, all_tables))
cat("tables to load:", paste(keep_tables, collapse = ", "), "\n")

# step A: download parquets + create local DuckDB tables ----
# delete stale DuckDB to avoid schema conflicts from prior materialized views
db_version_resolved <- if (db_version == "latest") {
  trimws(readLines(
    "https://storage.googleapis.com/calcofi-db/ducklake/releases/latest.txt",
    warn = FALSE)[1])
} else db_version
db_file <- file.path(db_dir, paste0("calcofi_", db_version_resolved, ".duckdb"))

# BUILD ASIDE, THEN SWAP. Never write the file the app is serving.
#
# This script rebuilds `calcofi_<version>.duckdb`, and `calcofi_latest.duckdb`
# points at it — so re-running against an UNCHANGED version used to delete and
# re-create the exact file the running app has open, then hold a read-write lock
# on it for the whole build. DuckDB's lock blocks even read-only connections, so
# every Shiny worker died at the `dbConnect` below (global.R). It took the live
# app down twice on 2026-08-04, the second time purely from this.
#
# Building into `data/.build/` and renaming at the end fixes it, because POSIX
# rename() only swaps the directory entry: workers already holding the old file
# keep reading a complete, valid database from the old inode, and workers started
# after the swap open the new one. There is no moment where the served path is
# missing, locked, or half-written. The app keeps serving the previous release
# right up to the swap, and prep_db.R becomes safe to run at any time.
#
# The parquet cache is symlinked in rather than re-downloaded — it is the
# expensive part, and the whole point of `cache_dir` being persistent.
build_dir <- file.path(db_dir, ".build")
dir.create(build_dir, showWarnings = FALSE, recursive = TRUE)
for (share in c("parquet", "latest.txt")) {
  src <- file.path(db_dir, share); lnk <- file.path(build_dir, share)
  if (file.exists(src) && !file.exists(lnk) && is.na(Sys.readlink(lnk)))
    file.symlink(src, lnk)
}
build_file <- file.path(build_dir, paste0("calcofi_", db_version_resolved, ".duckdb"))
for (f in c(build_file, paste0(build_file, ".wal"))) if (file.exists(f)) file.remove(f)
cat("building into:", build_file, "\n")

con <- cc_get_db(
  version = db_version,
  local_data = TRUE,
  cache_dir = build_dir,
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
# These settings help, but NOT the way they were first described here, and the
# difference matters: `memory_limit` bounds DuckDB's BUFFER MANAGER, and the
# spatial join operator is not spillable — it allocates straight past the limit.
# On the rebuild the R process still reached 9.6 GB RSS with 261 MB free and no
# spill directory was ever created. What actually kept the box alive was
# `threads = 3`, which held swap near 46% instead of 100%.
#
# So treat this as mitigation, not containment. The thing that actually bounds
# the spatial join is batching it — see sample_spatial below. Keep temp_directory
# on /ssd (where db_dir lives) rather than the 40 GB boot disk regardless, since
# other operators here DO spill.
dbExecute(con, "SET memory_limit = '8GB'")
# 3 -> 2 on 2026-08-10. Building v2026.08.10 was OOM-KILLED at 10.8 GB anon-RSS
# (`exit 137`, kernel `Out of memory: Killed process … (R)`), dying in the CDFW
# Regions layer — the hotspot the batching notes below already name. Nothing
# regressed; the release simply grew (obs 25.4M -> 26.3M rows) and ate the margin
# that let the previous build finish at 9.6 GB. Peak memory in the non-spillable
# spatial join scales with threads, so this is the cheapest lever that does not
# change what is computed. ERDDAP's java holds ~4.8 GB of the 16 GB throughout,
# which is most of why the margin is this thin.
dbExecute(con, "SET threads = 2")  # leave cores for the other containers
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
# hierarchy `taxon`, keyed on `taxon_key` — ONE tree per authority, each native.
#
# Was `WHERE taxon_key LIKE 'worms:%'`, i.e. a filter on the KEY AUTHORITY, so
# every seabird and marine mammal was absent from the tree entirely.
#
# The obvious repair — keep the filter off but key on `worms_id` — does not work,
# and the numbers say why. Of the release's 169 ITIS taxa:
#
#     rank      nodes   have a worms_id
#     Family       17        0
#     Order        10        0
#     Genus        41        3
#     Species      90       88
#
# WoRMS simply has no AphiaID for a bird family or order. Keying the tree on
# worms_id therefore keeps the bird SPECIES and drops every family and order
# above them, orphaning 72 species into roots — a tree that looks populated and
# is wrong. (It is easy to convince yourself otherwise: `Ardenna` is one of the
# three genera that DO have an AphiaID, so spot-checking that one genus passes.)
#
# So key on `taxon_key` and let each authority keep its own tree: birds walk the
# ITIS chain (Ardenna grisea -> Ardenna -> Procellariidae -> Procellariiformes ->
# Aves -> ... -> Animalia, all 12 ranks present in the release), everything else
# walks WoRMS. The chains are self-consistent by construction — a taxon_key's
# parent_taxon_key is always in the same authority — so the recursion needs no
# authority filter to stay in one tree. `authority` is kept for display and for
# callers that still want to scope explicitly.
dbExecute(con, "
  CREATE OR REPLACE TABLE taxon AS
  SELECT CASE WHEN t.taxon_key LIKE 'itis:%' THEN 'ITIS' ELSE 'WoRMS' END AS authority,
         t.taxon_key        AS taxonID,
         t.taxon_key        AS acceptedNameUsageID,
         t.parent_taxon_key AS parentNameUsageID,
         t.scientific_name  AS scientificName,
         t.rank             AS taxonRank,
         t.taxonomic_status AS taxonomicStatus,
         t.worms_id, t.itis_id
  FROM taxon_u t")
# legacy `taxa_rank` (rank -> order) folded into unified taxon.rank_order.
#
# MUST be one row per rank. `SELECT DISTINCT rank, rank_order` is not: taxa whose
# rank_order was never populated (every ITIS-keyed taxon — the release only
# assigns rank_order from the ichthyo `taxa_rank` lookup) contribute a second
# pair `(Family, NULL)` alongside `(Family, 140)`. get_taxon_children() ends with
# a LEFT JOIN on taxonRank, so each such rank DOUBLED every row of the taxa tree
# and its observation counts, silently.
dbExecute(con, "
  CREATE OR REPLACE TABLE taxa_rank AS
  SELECT rank AS taxonRank, max(rank_order) AS rank_order
  FROM taxon_u WHERE rank IS NOT NULL GROUP BY rank")

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
    o.taxon_key,
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
  -- parent taxon key, needed by taxa_tree_builder's (taxon_key, parent_id)
  -- grouping. Joined on taxon_key rather than worms_id so a bird resolves to its
  -- ITIS parent instead of NULL (no bird family or order has an AphiaID).
  LEFT JOIN taxon tx
    ON tx.taxonID = o.taxon_key
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
# BUILT IN BOUNDED PIECES, and that is not premature optimization: the
# single-statement version wedged the server on 2026-08-04. DuckDB's spatial join
# is NOT spillable — it allocates past `memory_limit` rather than spilling — so
# the cap above cannot contain it. The R process reached 9.6 GB RSS with 261 MB
# free on a 16 GB box running 11 containers, swap hit 100%, userspace stopped
# responding (the kernel still answered ICMP and TCP, so it *looked* down), and
# the VM needed a hard reset. `SET preserve_insertion_order=false` was tried and
# does NOT fix it either.
#
# Three things together bound it, each measured against the v2026.08.04 release
# under a deliberately tight 4 GB limit; all three are needed:
#
#   1. bbox pre-filter — plain arithmetic on lon/lat runs before ST_Intersects,
#      so geometry work only happens for candidates that can possibly match.
#      Alone: BOEM's 9,833 polygons went from the dominant cost to 0.1 min.
#   2. ST_Dump — DuckDB has no ST_Subdivide (that is PostGIS), but the expensive
#      layers are MULTIpolygons whose overall bbox spans the whole coast, so
#      per-feature bboxes filter nothing. Dumping to parts costs 0.2 s, turns
#      13,206 features into 13,655 parts and collapses MEOW from a hotspot to
#      0.03 min. Parts of a valid multipolygon are disjoint, so this cannot
#      duplicate a membership — verified: 0 duplicate (sample, polygon) pairs.
#   3. sample bucketing — neither of the above helps when a SINGLE part is huge
#      and complex (CDFW Regions has one with ~134k vertices), because its bbox
#      legitimately covers nearly every sample. Splitting the point side is the
#      only thing that bounds that case.
#
# Result: 2,786,030 memberships across 1,332,621 samples in ~7 min, identical to
# the unbounded version, inside 4 GB.
#
# Remaining hotspot: CDFW Regions is ~70% of the runtime. If that ever matters,
# the fix is true subdivision — clip each oversized part against a coarse grid so
# no piece exceeds N vertices — not a bigger machine.
# Defaults raised on 2026-08-10 after v2026.08.10 was OOM-killed inside CDFW
# Regions — the "remaining hotspot" named above, reached for the first time with
# no headroom left. Splitting the POINT side is the only thing that bounds a
# single huge part (a bbox that covers nearly every sample cannot be filtered),
# so buckets is the knob that matters here: 8 -> 24. Batch 500 -> 250 halves the
# polygon side of each piece too. Both stay env-overridable for a machine with
# more room: CC_SPATIAL_BUCKETS=8 CC_SPATIAL_BATCH=500 restores the old shape.
# More, smaller pieces cost wall-clock, not correctness — the loop is a UNION of
# disjoint work and the DISTINCT below already guards overlap.
SPATIAL_BATCH   <- as.integer(Sys.getenv("CC_SPATIAL_BATCH", "250"))
SPATIAL_BUCKETS <- as.integer(Sys.getenv("CC_SPATIAL_BUCKETS", "24"))
dbExecute(con, "SET preserve_insertion_order = false")

# Sample side, filtered ONCE and bucketed. Keeping raw lon/lat makes the bbox
# pre-filter pure arithmetic.
#
# `geom IS NOT NULL` is NOT sufficient. Release v2026.08.02 shipped 1,590 rows
# with NaN coordinates, and ST_Point(NaN, NaN) is a real non-NULL GEOMETRY, so
# they pass that check — and they do not merely add junk rows, they CORRUPT the
# result: with them present this join returns a different number of matches at
# different thread counts, dropping valid unrelated pairs. Measured on one county
# polygon:
#      NaN points in : 17,937 (1 thread) / 17,771 (2) / 20,070 (8)
#      NaN points out: 20,101 / 20,101 / 20,101
# The correct answer is higher than any corrupted one, so this silently
# UNDER-counted, differently on every machine — which is how it was found (laptop
# 2,300,433 memberships vs server 2,131,201 on identical inputs). calcofi4db
# 3.4.2 stops such geometries being minted; this keeps the join correct against
# releases already published without it.
#
# Relabel BOTH sides to the same CRS rather than coercing one to match the other.
# DuckDB refuses ST_Intersects across differing CRS tags, and which tag each side
# carries depends on the release:
#   up to v2026.08.02  sample.geom OGC:CRS84 (ST_Point) vs spatial.geom
#                      EPSG:4326 (ST_Read over GeoJSON) -> the join ERRORED
#   from v2026.08.03   both EPSG:4326, normalized at release time
# Forcing sp.geom to OGC:CRS84 fixed the first case and would BREAK the second.
# Setting both works against either release; ST_SetCRS relabels without
# transforming, and every one of these is WGS 84 lon/lat regardless of label.
cat("building sample_spatial...\n")
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE _sample_ok AS
  SELECT sample_key, longitude, latitude,
         ST_SetCRS(geom, 'EPSG:4326') AS geom,
         (hash(sample_key) % {SPATIAL_BUCKETS}) AS bkt
  FROM sample
  WHERE geom IS NOT NULL
    AND NOT isnan(latitude)  AND NOT isinf(latitude)
    AND NOT isnan(longitude) AND NOT isinf(longitude)"))

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE _part AS
  SELECT spatial_key, layer, name,
         -- REPAIR, and only where needed. 2 of CA Watersheds' 140 parts are
         -- invalid ('Ring edge missing at -123.171 37.772'), and ST_Intersects
         -- against an invalid ring silently UNDER-counts: that layer returned
         -- 52,730 memberships raw vs 58,255 repaired, a 9.5% loss that nothing
         -- reported. ST_Intersection additionally THROWS on them, so the
         -- subdivision below cannot run without this. Same family as the NaN
         -- coordinates above: geometry that is wrong rather than absent, and
         -- wrong quietly.
         CASE WHEN ST_IsValid(g) THEN g ELSE ST_MakeValid(g) END AS geom,
         ST_NPoints(g)::UINTEGER AS npts
  FROM (SELECT spatial_key, layer, name,
               UNNEST(ST_Dump(ST_SetCRS(geom, 'EPSG:4326'))).geom AS g
        FROM spatial)")

# ── subdivide oversized parts until every piece fits the vertex budget ────────
# THE fix for the hotspot the batching notes above describe. Batching bounds how
# many polygons and points meet at once, but it cannot bound a SINGLE part that
# is huge and complex: its bbox legitimately covers nearly every sample, so the
# pre-filter passes everything through to ST_Intersects. Building v2026.08.10 was
# OOM-killed exactly there (CDFW Regions, `exit 137` at 10.8 GB anon-RSS).
#
# Clip each oversized part against a 2x2 grid of its own bbox, recompute vertex
# counts, repeat. Iteration is what makes this work and a single pass is not
# enough — vertex density is uneven, so one cell of a coarse grid keeps most of a
# coastline. Sizing a grid in one shot as ceil(sqrt(npts/budget)) only got
# 105,387 -> 74,409; iterating reaches the budget in 5 rounds:
#
#     round 0: 24 parts over budget, max 105,387 vertices
#     round 1:  8                          48,668
#     round 2:  2                          34,703
#     round 3:  1                          31,362
#     round 4:  1                          23,220
#     round 5:  0                          19,585   <- under MAX_VERTS
#
# Cost is 99 extra pieces (13,655 -> 13,754, +0.7%), and every piece now has a
# tight bbox, which is what finally makes the pre-filter effective on the layers
# where it previously did nothing.
#
# MEMBERSHIP-PRESERVING, verified rather than assumed: against all 1,459,262
# release sample positions, all 8 layers containing an oversized part return
# identical (sample_key, spatial_key) counts before and after subdivision, once
# both sides use the repaired geometry. Grid cells tile disjointly so pieces
# partition the original, and a point on a shared edge matches both pieces — the
# DISTINCT below collapses that, exactly as it already does for ST_Dump parts.
MAX_VERTS <- as.integer(Sys.getenv("CC_MAX_PART_VERTS", "20000"))
MAX_SUBDIV_ITER <- 12L
sub_it <- 0L
repeat {
  n_big <- dbGetQuery(con, glue(
    "SELECT COUNT(*) AS n FROM _part WHERE npts > {MAX_VERTS}"))$n
  if (n_big == 0L || sub_it >= MAX_SUBDIV_ITER) break
  dbExecute(con, glue("
    CREATE OR REPLACE TEMP TABLE _part AS
    SELECT spatial_key, layer, name, geom, npts FROM _part WHERE npts <= {MAX_VERTS}
    UNION ALL
    SELECT spatial_key, layer, name, geom, ST_NPoints(geom)::UINTEGER AS npts FROM (
      SELECT b.spatial_key, b.layer, b.name,
             ST_CollectionExtract(ST_Intersection(b.geom, ST_MakeEnvelope(
               b.x0 + (b.x1-b.x0)*i/2.0,     b.y0 + (b.y1-b.y0)*j/2.0,
               b.x0 + (b.x1-b.x0)*(i+1)/2.0, b.y0 + (b.y1-b.y0)*(j+1)/2.0)), 3) AS geom
      FROM (SELECT *, ST_XMin(geom) x0, ST_XMax(geom) x1,
                      ST_YMin(geom) y0, ST_YMax(geom) y1
            FROM _part WHERE npts > {MAX_VERTS}) b,
           range(0,2) t(i), range(0,2) u(j))
    WHERE geom IS NOT NULL AND NOT ST_IsEmpty(geom)"))
  sub_it <- sub_it + 1L
}
sub_n <- dbGetQuery(con,
  "SELECT COUNT(*) AS pieces, MAX(npts) AS mx FROM _part")
cat(glue("  subdivided in {sub_it} round(s): {format(sub_n$pieces, big.mark=',')} ",
         "pieces, max {format(sub_n$mx, big.mark=',')} vertices\n"), "\n")
if (sub_n$mx > MAX_VERTS)
  warning(glue("a part still exceeds {MAX_VERTS} vertices after ",
               "{MAX_SUBDIV_ITER} rounds — the spatial join may run hot"))

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE _poly AS
  -- PARTITION BY layer: the batch loop's offsets run 0..n-1 WITHIN a layer, so
  -- a globally-numbered piece_id would select the wrong rows entirely.
  SELECT ROW_NUMBER() OVER (PARTITION BY layer) AS piece_id,
         spatial_key, layer, name, geom,
         ST_XMin(geom) AS xmin, ST_XMax(geom) AS xmax,
         ST_YMin(geom) AS ymin, ST_YMax(geom) AS ymax
  FROM _part")

dbExecute(con, "
  CREATE OR REPLACE TABLE sample_spatial (
    sample_key   VARCHAR,
    spatial_key  VARCHAR,
    layer        VARCHAR,
    spatial_name VARCHAR)")

sp_layers <- dbGetQuery(con, "SELECT layer, COUNT(*) AS n FROM _poly GROUP BY 1 ORDER BY 1")
for (i in seq_len(nrow(sp_layers))) {
  lyr <- sp_layers$layer[i]
  t_i <- Sys.time()
  for (off in seq(0, sp_layers$n[i] - 1, by = SPATIAL_BATCH)) {
    for (k in seq_len(SPATIAL_BUCKETS) - 1L) {
      # DISTINCT guards against a source layer whose multipolygon parts overlap;
      # valid ones do not, and a duplicated membership would double-count in the app.
      # Page on piece_id, NOT `ORDER BY spatial_key LIMIT/OFFSET`.
      #
      # spatial_key is NOT unique in _poly — ST_Dump already yields many parts
      # per key and the subdivision above yields more — so ORDER BY spatial_key
      # is not a TOTAL order and DuckDB may return tied rows in any order per
      # query. With LIMIT/OFFSET the pages then overlap and, worse, SKIP: a piece
      # that lands in no page contributes no memberships and nothing reports it.
      #
      # This silently cost CDFW Regions 18,700 of 58,269 memberships (-32%) the
      # first time subdivision multiplied its ties. It is a latent bug in the
      # pagination, not in the subdivision — any layer with more parts than
      # SPATIAL_BATCH and duplicate spatial_keys could always have been affected,
      # and BOEM (9,833 parts over 20 pages) is the obvious candidate to re-check.
      # It hid because the count that changes is a total nobody had a baseline
      # for.
      #
      # piece_id is a row number assigned once, so the ranges are disjoint and
      # cover every row exactly once by construction — no ordering required.
      dbExecute(con, glue("
        INSERT INTO sample_spatial
        SELECT DISTINCT s.sample_key, p.spatial_key, p.layer, p.name
        FROM (SELECT * FROM _poly WHERE layer = {dbQuoteString(con, lyr)}
              AND piece_id > {off} AND piece_id <= {off + SPATIAL_BATCH}) p
        JOIN _sample_ok s
          ON s.bkt = {k}
         AND s.longitude BETWEEN p.xmin AND p.xmax
         AND s.latitude  BETWEEN p.ymin AND p.ymax
         AND ST_Intersects(p.geom, s.geom)"))
    }
  }
  cat(sprintf("  %-32s %5d parts  %.2f min\n", lyr, sp_layers$n[i],
              as.numeric(difftime(Sys.time(), t_i, units = "mins"))))
}
dbExecute(con, "DROP TABLE IF EXISTS _sample_ok")
dbExecute(con, "DROP TABLE IF EXISTS _poly")
dbExecute(con, "DROP TABLE IF EXISTS _part")

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
# Same build-aside-and-swap as the database, for the same reason: the app reads
# this file, and `delete_dsn = TRUE` unlinks it and then writes ~430k features
# over several seconds. A worker starting in that window gets a missing or
# truncated GeoJSON. Write beside it and rename, which is atomic.
hex_tmp <- paste0(hex_geo, ".new")
if (file.exists(hex_tmp)) file.remove(hex_tmp)
# driver = "GeoJSON" is required, not decorative: st_write() infers the driver
# from the file EXTENSION, and the aside-and-swap name ends in `.new`, so it
# failed with "Could not guess driver for .../hex.geojson.new" and took the whole
# prep_db run down after the ~25 min spatial build had already succeeded.
st_write(sf_hex, hex_tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
if (!file.rename(hex_tmp, hex_geo))
  stop("could not swap ", hex_tmp, " -> ", hex_geo)
cat("  hex.geojson:", nrow(sf_hex), "hexagons across 10 resolutions\n")

# step E: drop build-only objects (keep species/taxon/taxa_rank + bio_obs/env_obs
#         + dataset) ----
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

# FORCE the WAL into the main file before disconnecting. `dbDisconnect(shutdown =
# TRUE)` is documented to checkpoint, but it does not reliably do so for a
# connection obtained from cc_get_db() — which owns its own driver handle — and
# the guard below then (correctly) refused to publish the build:
#   build left a WAL beside calcofi_v2026.08.07.duckdb — shutdown did not
#   checkpoint; refusing to swap an unflushed database
# after the full ~25 min build had succeeded. An explicit CHECKPOINT is
# deterministic and does not depend on driver ownership. We are the only writer,
# so FORCE has nothing to abort.
tryCatch(dbExecute(con, "FORCE CHECKPOINT"),
         error = function(e) message("  checkpoint: ", conditionMessage(e)))
dbDisconnect(con, shutdown = TRUE)

# swap the finished build into place ----
# Disconnect FIRST: the rename must move a cleanly-closed database, or the app
# inherits an unflushed WAL. A leftover .wal beside the build means the shutdown
# did not checkpoint, so refuse rather than publish a database that recovers on
# first open (in a read-only worker, that fails).
build_wal <- paste0(build_file, ".wal")

# If a WAL survived the disconnect, flush it rather than failing outright: reopen
# the file with a fresh driver and shut that driver down. Opening a database with
# a WAL replays it, and shutting the DRIVER down (not just the connection) is what
# forces the checkpoint — the distinction that makes this different from the
# dbDisconnect() above, which trusts a driver handle it does not own.
#
# The guard stays: if a WAL is STILL there afterwards, something is genuinely
# wrong and publishing would hand the app a database that recovers on first open,
# which fails in a read-only worker.
if (file.exists(build_wal)) {
  message("  WAL present after disconnect — reopening to force a checkpoint")
  flush_drv <- duckdb::duckdb(dbdir = build_file)
  flush_con <- DBI::dbConnect(flush_drv)
  tryCatch(DBI::dbExecute(flush_con, "FORCE CHECKPOINT"),
           error = function(e) message("  checkpoint: ", conditionMessage(e)))
  DBI::dbDisconnect(flush_con)
  duckdb::duckdb_shutdown(flush_drv)
}

if (file.exists(build_wal))
  stop("build left a WAL beside ", basename(build_file),
       " — shutdown did not checkpoint; refusing to swap an unflushed database")
if (!file.exists(build_file))
  stop("build produced no database at ", build_file)

# rename() within the same filesystem is atomic and does NOT disturb readers:
# workers holding the old inode keep serving a complete database until they exit.
if (!file.rename(build_file, db_file))
  stop("could not swap ", build_file, " -> ", db_file)
cat("swapped into place:", db_file,
    sprintf("(%.0f MB)\n", file.size(db_file) / 1024^2))

# repoint the `calcofi_latest.duckdb` symlink at the db we just built, so the app
# (global.R defaults to data/calcofi_latest.duckdb) serves this version without a
# manual step. relative target keeps the link valid regardless of mount path.
#
# Symlink replacement is NOT atomic via unlink+symlink — there is a window where
# the link is missing — so build the new link under a temp name and rename it
# over the old one, which is.
latest_link <- file.path(db_dir, "calcofi_latest.duckdb")
tmp_link    <- paste0(latest_link, ".swap")
if (file.exists(tmp_link) || !is.na(Sys.readlink(tmp_link))) unlink(tmp_link)
file.symlink(basename(db_file), tmp_link)
if (!file.rename(tmp_link, latest_link))
  stop("could not repoint calcofi_latest.duckdb -> ", basename(db_file))
cat("symlinked calcofi_latest.duckdb ->", basename(db_file), "\n")

# The h3t API holds this file OPEN and will keep serving the previous release
# until its container is restarted — that is not something this script can do,
# and it is silent (nothing errors, /h3t/health just reports the old db_mtime).
# scripts/deploy_consumers.sh in CalCOFI/workflows handles it.
cat("NOTE: restart the h3t API to pick this up ",
    "(CalCOFI/workflows: scripts/deploy_consumers.sh)\n", sep = "")

cat("\ndone. app database ready at:", db_file, "\n")
