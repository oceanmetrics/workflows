# eov_share.R ----
# rules of the AquaMaps EOV share product, as plain documented R functions.
# sourced by aquamaps_eov.qmd and tested by tests/testthat/test-eov_share.R, so the
# notebook calls the rules and the tests assert them (they cannot drift).
# needs: glue, jsonlite, terra, digest (loaded by the callers, not here).

# seeds ----

#' EOV taxonomic seeds
#'
#' The 33 WoRMS AphiaID seeds that define the 7 biology and ecosystems Essential
#' Ocean Variables (EOVs), transcribed exactly from `OBIS_EOV` (obisindicators
#' 0.8.0, `R/eov.R`), which in turn follows the IOOS Marine Life Data Network
#' `eov_taxonomy/IdentifierList.csv`
#' (https://github.com/ioos/marine_life_data_network). Each EOV is the union of the
#' WoRMS subtrees below its seeds, see [eov_tree_sql()].
#'
#' @return data.frame with columns eov, label, aphiaid, taxon, rank (33 rows)
eov_seeds <- function() {
  mk <- function(eov, label, aphiaid, taxon, rank) {
    data.frame(
      eov     = eov,
      label   = label,
      aphiaid = as.integer(aphiaid),
      taxon   = taxon,
      rank    = rank,
      stringsAsFactors = F)
  }
  rbind(
    mk("fish", "Fish",
       c(1829L, 1517375L, 152352L),
       c("Agnatha", "Chondrichthyes", "Osteichthyes"),
       c("Infraphylum", "Parvphylum", "Parvphylum")),
    mk("hardCorals", "Hard corals",
       1363L, "Scleractinia", "Order"),
    mk("mangroves", "Mangroves",
       c(235048L, 235033L, 234450L, 234495L, 235086L, 235089L, 235091L,
         235106L, 235056L, 235060L, 235045L, 235116L, 235063L, 235072L,
         235075L, 235077L, 235068L, 234488L, 235103L),
       c("Combretaceae", "Avicennia", "Nypa", "Bruguiera", "Ceriops",
         "Kandelia", "Rhizophora", "Sonneratia", "Excoecaria", "Pemphis",
         "Camptostemon", "Heritiera", "Xylocarpus", "Osbornia",
         "Pelliciera", "Aegialitis", "Aegiceras", "Acrostichum",
         "Scyphiphora"),
       c("Family", rep("Genus", 18L))),
    mk("marineMammals", "Marine mammals",
       # 477316 (Lutra felina) is the ACCEPTED id (IdentifierList.csv is correct)
       c(148736L, 2688L, 159502L, 242598L, 477316L, 159017L, 137085L),
       c("Pinnipedia", "Cetacea", "Sirenia", "Enhydra lutris",
         "Lutra felina", "Lontra canadensis", "Ursus maritimus"),
       c("Infraorder", "Infraorder", "Order", "Species", "Species",
         "Species", "Species")),
    mk("seabirds", "Seabirds",
       1836L, "Aves", "Class"),
    mk("seagrasses", "Seagrasses",
       153491L, "Alismatales", "Order"),
    mk("seaTurtles", "Sea turtles",
       987094L, "Chelonioidea", "Superfamily"))
}

#' EOV keys in petal order (without the composite)
eov_keys <- function() unique(eov_seeds()$eov)

#' EOV dimension values: composite `all` first, then the 7 EOVs
eov_levels <- function() c("all", eov_keys())

#' Human labels of the EOV dimension values (named character)
eov_labels <- function() {
  s <- eov_seeds()
  lbl <- s$label[match(eov_keys(), s$eov)]
  c(all = "All EOVs (mean)", stats::setNames(lbl, eov_keys()))
}

#' Band order of the score COGs: 1 all, 2-8 the EOVs, 9 n_eov
eov_bands <- function() c(eov_levels(), "n_eov")

#' EOVs that AquaMaps cannot populate (no birds, no seagrasses)
eov_absent <- function() c("seabirds", "seagrasses")

# taxa ----

#' SQL: EOV membership by WoRMS subtree
#'
#' Recursive CTE that expands every seed to its descendants through
#' `parentNameUsageID`, returning the union per EOV.
#'
#' @param seeds_tbl name of a table with columns eov, aphiaid (see [eov_seeds()])
#' @param worms_tbl name of a table with columns taxonID, parentNameUsageID
#' @return SQL text of a query returning distinct (eov, taxonID)
eov_tree_sql <- function(seeds_tbl = "eov_seed", worms_tbl = "worms") {
  glue::glue("
    WITH RECURSIVE tree(eov, taxonID) AS (
      SELECT s.eov, CAST(s.aphiaid AS BIGINT) AS taxonID
      FROM {seeds_tbl} s
      UNION
      SELECT t.eov, w.taxonID
      FROM tree t
      JOIN {worms_tbl} w ON w.parentNameUsageID = t.taxonID
    )
    SELECT DISTINCT eov, taxonID FROM tree")
}

#' Extinction-risk weight from an IUCN Red List category
#'
#' Equal-step weights of Butchart et al. (2004, 2007) as used by the Ocean Health
#' Index (Halpern et al. 2012): LC 0, NT 0.2, VU 0.4, EN 0.6, CR 0.8, EX and EW 1
#' (`scheme = "ohi"`). The shifted variant of MarineSensitivity v1 is LC 0.2, NT 0.4,
#' VU 0.6, EN 0.8, CR 1, EX/EW 1 (`"mst_v1"`). Legacy categories map LR/lc to LC and
#' LR/nt, LR/cd to NT. DD, NE and NA carry no weight (NA, excluded from `risk`).
#'
#' @param code character vector of Red List category codes
#' @param scheme "ohi" (default) or "mst_v1"
#' @return numeric vector, same length as `code`
redlist_weight <- function(code, scheme = c("ohi", "mst_v1")) {
  scheme <- match.arg(scheme)
  step   <- c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4)
  w      <- switch(
    scheme,
    ohi    = c(step * 0.2, EX = 1, EW = 1),
    mst_v1 = c((step + 1) * 0.2, EX = 1, EW = 1))
  code   <- trimws(as.character(code))
  key    <- toupper(code)
  key[key == "LR/LC"] <- "LC"
  key[key %in% c("LR/NT", "LR/CD")] <- "NT"
  no_wt  <- c("DD", "NE", "NA")
  key[key %in% no_wt] <- NA_character_
  known  <- names(w)
  bad    <- unique(code[!is.na(key) & !key %in% known])
  if (length(bad) > 0)
    stop("unknown Red List code(s): ", paste(bad, collapse = ", "),
         ". Known: ", paste(c(known, "LR/lc", "LR/nt", "LR/cd", no_wt), collapse = ", "))
  unname(w[key])
}

#' Summarise the taxon crosswalk per EOV
#'
#' @param taxa data.frame with columns eov, has_range, weight, excluded
#' @return data.frame with eov, n_taxa, n_with_range, n_weighted, n_excluded
eov_summarize_taxa <- function(taxa) {
  stopifnot(all(c("eov", "has_range", "weight", "excluded") %in% names(taxa)))
  one <- function(d) {
    data.frame(
      n_taxa       = nrow(d),
      n_with_range = sum(d$has_range),
      n_weighted   = sum(!is.na(d$weight) & !d$excluded),
      n_excluded   = sum(d$excluded))
  }
  by <- split(taxa, taxa$eov)
  out <- do.call(rbind, lapply(names(by), \(k) cbind(eov = k, one(by[[k]]))))
  rownames(out) <- NULL
  out
}

# share ----

#' Assign species to batches (one pass over the AquaMaps rows, restartable)
#'
#' Species are ordered by EOV then aphia_id and filled into batches whose summed
#' `n_rows` stays within `max_rows`. A batch never splits a species and never mixes
#' EOVs; a species bigger than the cap gets a batch of its own.
#'
#' @param species data.frame with eov, aphia_id, n_rows (rows to read for the species)
#' @param max_rows row cap per batch
#' @return `species` ordered, with an integer column `batch_id`
eov_make_batches <- function(species, max_rows = 80e6) {
  stopifnot(all(c("eov", "aphia_id", "n_rows") %in% names(species)))
  s    <- species[order(species$eov, species$aphia_id), , drop = F]
  id   <- integer(nrow(s))
  b    <- 0L
  cum  <- 0
  prev <- NA_character_
  for (i in seq_len(nrow(s))) {
    new <- i == 1L || s$eov[i] != prev || (cum > 0 && cum + s$n_rows[i] > max_rows)
    if (new) { b <- b + 1L; cum <- 0 }
    cum   <- cum + s$n_rows[i]
    id[i] <- b
    prev  <- s$eov[i]
  }
  s$batch_id <- id
  rownames(s) <- NULL
  s
}

.sql_files <- function(x) paste0("'", x, "'", collapse = ", ")

#' SQL: the statements that process one batch of species (DuckDB)
#'
#' Steps 1-5 of the method for the species of one batch, in this order:
#'
#' 1. `rng_b` (temp table): range footprint (aphia_id, cell_id) of the batch's range
#'    files, empty when there are none.
#' 2. `clip_b` (temp table): ua = u * area_km2 per species and cell, where v is the
#'    max over the species' models (grouped only when `has_multi`), clipped to the
#'    range footprint if the species has one, else the full distribution.
#' 3. `tot_b` (temp table): per species n_cells and total = sum(ua).
#' 4. `cell_eov`: COPY to `<dir_partials>/cell_eov/batch_<id>.parquet`, per cell and
#'    EOV share = sum(ua / total) and risk = sum(weight * ua / total); NULL weights add
#'    nothing to risk; species with total 0 are left out.
#' 5. `totals`: COPY of `tot_b` to `<dir_partials>/totals/batch_<id>.parquet`
#'    (written last, so a batch is complete when both files exist).
#' 6. `drop`: remove the temp tables.
#'
#' @param batch_id integer batch number
#' @param am_files AquaMaps partition files of the batch's models
#' @param rng_files IUCN range partition files of the batch's species (may be empty)
#' @param has_multi TRUE when a species in the batch has more than one model
#' @param map_tbl relation mdl_key, aphia_id, n_models, has_range (AquaMaps models)
#' @param area_tbl relation cell_id, area_km2
#' @param weight_tbl relation aphia_id, weight (NULL = no weight)
#' @param eov_tbl relation eov, aphia_id (membership)
#' @param dir_partials directory of the partial files
#' @param rng_map_tbl relation mdl_key, aphia_id (range models to species)
#' @param eov restrict the EOV aggregation to this EOV (the batch's EOV)
#' @return named list of SQL strings: rng_b, clip_b, tot_b, cell_eov, totals, drop
eov_batch_sql <- function(
    batch_id, am_files, rng_files = character(), has_multi = FALSE,
    map_tbl, area_tbl, weight_tbl, eov_tbl, dir_partials,
    rng_map_tbl = "rng_map", eov = NULL, share = TRUE, dir_cs = NULL) {
  id    <- sprintf("%05d", as.integer(batch_id))
  f_tot <- file.path(dir_partials, "totals",   paste0("batch_", id, ".parquet"))
  f_cel <- file.path(dir_partials, "cell_eov", paste0("batch_", id, ".parquet"))
  am    <- .sql_files(am_files)
  sp_rows <- if (has_multi) {
    glue::glue("
      SELECT m.aphia_id, a.cell_id, max(a.val) AS v
      FROM read_parquet([{am}], hive_partitioning = false) a
      JOIN {map_tbl} m ON m.mdl_key = a.mdl_key
      GROUP BY m.aphia_id, a.cell_id")
  } else {
    glue::glue("
      SELECT m.aphia_id, a.cell_id, a.val AS v
      FROM read_parquet([{am}], hive_partitioning = false) a
      JOIN {map_tbl} m ON m.mdl_key = a.mdl_key")
  }
  rng_b <- if (length(rng_files) > 0) {
    glue::glue("
      CREATE OR REPLACE TEMP TABLE rng_b AS
      SELECT DISTINCT m.aphia_id, r.cell_id
      FROM read_parquet([{.sql_files(rng_files)}], hive_partitioning = false) r
      JOIN {rng_map_tbl} m ON m.mdl_key = r.mdl_key")
  } else {
    "CREATE OR REPLACE TEMP TABLE rng_b (aphia_id BIGINT, cell_id INTEGER)"
  }
  only_eov <- if (is.null(eov)) "" else glue::glue("AND e.eov = '{eov}'")
  st <- list(
    rng_b = rng_b,
    clip_b = glue::glue("
      CREATE OR REPLACE TEMP TABLE clip_b AS
      SELECT s.aphia_id, s.cell_id, s.v, s.v * a.area_km2 AS ua
      FROM ({sp_rows}) s
      JOIN {area_tbl} a ON a.cell_id = s.cell_id
      JOIN (SELECT DISTINCT aphia_id, has_range FROM {map_tbl}) m ON m.aphia_id = s.aphia_id
      LEFT JOIN rng_b r ON r.aphia_id = s.aphia_id AND r.cell_id = s.cell_id
      WHERE NOT m.has_range OR r.cell_id IS NOT NULL"))
  if (share) {
    st$tot_b <- "
      CREATE OR REPLACE TEMP TABLE tot_b AS
      SELECT aphia_id, count(*)::BIGINT AS n_cells, sum(ua) AS total_suit_area
      FROM clip_b GROUP BY aphia_id"
    st$cell_eov <- glue::glue("
      COPY (
        SELECT c.cell_id, e.eov,
               sum(c.ua / t.total_suit_area)                        AS share,
               sum(coalesce(w.weight, 0.0) * c.ua / t.total_suit_area) AS risk
        FROM clip_b c
        JOIN tot_b t ON t.aphia_id = c.aphia_id
        JOIN {eov_tbl} e ON e.aphia_id = c.aphia_id
        LEFT JOIN {weight_tbl} w ON w.aphia_id = c.aphia_id
        WHERE t.total_suit_area > 0 {only_eov}
        GROUP BY c.cell_id, e.eov)
      TO '{f_cel}' (FORMAT parquet, COMPRESSION zstd)")
  }
  if (!is.null(dir_cs)) st$cell_species <- cell_species_stage1_sql(batch_id, dir_cs, "clip_b")
  if (share) {
    st$totals <- glue::glue("COPY tot_b TO '{f_tot}' (FORMAT parquet, COMPRESSION zstd)")
  }
  st$drop <- "DROP TABLE IF EXISTS clip_b; DROP TABLE IF EXISTS tot_b; DROP TABLE IF EXISTS rng_b"
  st
}

#' SQL: merge the partial files of all batches
#'
#' `eov_sp_total` (per species: n_models, has_range, n_cells, total_suit_area,
#' excluded; a species without a totals row or with total <= 0 is excluded with 0
#' cells) and `eov_cell_eov` (per cell and EOV: share and risk summed over batches).
#'
#' @param dir_partials directory with `totals/` and `cell_eov/`
#' @param map_tbl relation mdl_key, aphia_id, n_models, has_range
#' @param out_prefix prefix of the created tables
#' @return named list of SQL strings: sp_total, cell_eov
eov_merge_sql <- function(dir_partials, map_tbl, out_prefix = "eov_") {
  p <- out_prefix
  list(
    sp_total = glue::glue("
      CREATE OR REPLACE TABLE {p}sp_total AS
      SELECT m.aphia_id, m.n_models, m.has_range,
             coalesce(t.n_cells, 0)                AS n_cells,
             coalesce(t.total_suit_area, 0.0)      AS total_suit_area,
             coalesce(t.total_suit_area, 0.0) <= 0 AS excluded
      FROM (SELECT DISTINCT aphia_id, n_models, has_range FROM {map_tbl}) m
      LEFT JOIN (
        SELECT aphia_id, max(n_cells) AS n_cells, max(total_suit_area) AS total_suit_area
        FROM read_parquet('{dir_partials}/totals/*.parquet')
        GROUP BY aphia_id) t ON t.aphia_id = m.aphia_id"),
    cell_eov = glue::glue("
      CREATE OR REPLACE TABLE {p}cell_eov AS
      SELECT cell_id, eov, sum(share) AS share, sum(risk) AS risk
      FROM read_parquet('{dir_partials}/cell_eov/*.parquet')
      GROUP BY cell_id, eov"))
}

# cell -> species table ----

#' global05 grid math: cell_id <-> lon/lat <-> 5 degree tile
#'
#' cell_id is row-major from the top-left, 1-based: `row = (cell_id-1) div 7200`,
#' `col = (cell_id-1) mod 7200`; cell centres `lon = -180 + (col + 0.5) 0.05`,
#' `lat = 90 - (row + 0.5) 0.05`; a 5 degree tile is `(row div 100) * 72 + col div 100`
#' (0..2591), at most 100 x 100 = 10,000 cells.
#'
#' @param lon,lat degrees (vectors); points on the outer edge fall in the last cell
#' @param cell_id integer cell ids
#' @param ncol,nrow,res grid size and resolution (degrees)
#' @return `cell_of_lonlat`: integer cell ids; `lonlat_of_cell`: data.frame lon, lat of
#'   the cell centres; `tile_of_cell`: integer tile ids
cell_of_lonlat <- function(lon, lat, ncol = 7200L, nrow = 3600L, res = 0.05) {
  col <- pmin(pmax(floor((lon + 180) / res + 1e-9), 0), ncol - 1)
  row <- pmin(pmax(floor((90 - lat) / res + 1e-9), 0), nrow - 1)
  as.integer(row * ncol + col + 1)
}
#' @rdname cell_of_lonlat
lonlat_of_cell <- function(cell_id, ncol = 7200L, nrow = 3600L, res = 0.05) {
  i <- as.numeric(cell_id) - 1
  data.frame(
    lon = -180 + (i %% ncol + 0.5) * res,
    lat = 90 - (i %/% ncol + 0.5) * res)
}
#' @rdname cell_of_lonlat
tile_of_cell <- function(cell_id, ncol = 7200L, tile_cells = 100L) {
  i <- as.numeric(cell_id) - 1
  as.integer((i %/% ncol %/% tile_cells) * (ncol %/% tile_cells) + (i %% ncol) %/% tile_cells)
}

#' SQL: stage 1 of the cell -> species table, per species batch
#'
#' Writes the batch's (cell_id, aphia_id, suit) rows, partitioned by 5 degree row band
#' (`band = row div 100`, 0..35), to `<dir_cs>/batch_<id>/band=<b>/data_0.parquet`;
#' suit = round(v) as UTINYINT (v = max over the species' models, 1..100).
#'
#' @param batch_id integer batch number
#' @param dir_cs directory of the stage-1 partials
#' @param clip_tbl relation with aphia_id, cell_id, v (the clipped distribution)
cell_species_stage1_sql <- function(batch_id, dir_cs, clip_tbl = "clip_b") {
  d <- file.path(dir_cs, sprintf("batch_%05d", as.integer(batch_id)))
  glue::glue("
    COPY (
      SELECT CAST(cell_id AS INTEGER) AS cell_id,
             CAST(aphia_id AS INTEGER) AS aphia_id,
             CAST(round(v) AS UTINYINT) AS suit,
             ((cell_id - 1) // 7200) // 100 AS band
      FROM {clip_tbl})
    TO '{d}' (FORMAT parquet, COMPRESSION zstd, PARTITION_BY (band), OVERWRITE_OR_IGNORE true)")
}

#' Path of a tile file
cell_species_tile_path <- function(dir_out, tile) {
  file.path(dir_out, paste0("tile=", tile), "data_0.parquet")
}

#' SQL: stage 2 of the cell -> species table, per 5 degree band
#'
#' `load` reads the band's stage-1 files into the temp table `band_t` with the tile
#' column `c`; `cols` lists the non-empty tile columns; `tile(c)` writes one tile,
#' ordered by cell_id then suit DESC, to `<tile path>.tmp` (rename it with
#' [cell_species_commit()] so a half-written file is never taken for a finished one).
#'
#' @param band 0..35
#' @param dir_cs stage-1 directory
#' @param dir_out output directory (`cell_species/`)
#' @param row_group_size rows per row group (small row groups make cell_id statistics selective)
#' @return list(load, cols, tile = function(c), drop)
cell_species_stage2_sql <- function(band, dir_cs, dir_out, row_group_size = 32768L) {
  band <- as.integer(band)
  list(
    load = glue::glue("
      CREATE OR REPLACE TEMP TABLE band_t AS
      SELECT cell_id, aphia_id, suit, ((cell_id - 1) % 7200) // 100 AS c
      FROM read_parquet('{dir_cs}/batch_*/band={band}/*.parquet', hive_partitioning = false)"),
    cols = "SELECT DISTINCT c FROM band_t ORDER BY c",
    tile = function(c) {
      t   <- band * 72L + as.integer(c)
      tmp <- paste0(cell_species_tile_path(dir_out, t), ".tmp")
      glue::glue("
        COPY (
          SELECT cell_id, aphia_id, suit FROM band_t WHERE c = {as.integer(c)}
          ORDER BY cell_id, suit DESC, aphia_id)
        TO '{tmp}' (FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE {as.integer(row_group_size)})")
    },
    drop = "DROP TABLE IF EXISTS band_t")
}

#' Move a finished `.tmp` tile into place
cell_species_commit <- function(dir_out, tile) {
  f <- cell_species_tile_path(dir_out, tile)
  stopifnot(file.exists(paste0(f, ".tmp")))
  file.rename(paste0(f, ".tmp"), f)
  invisible(f)
}

#' Check that a tile's cell_id row-group statistics are sorted and non-overlapping
#'
#' Reads `parquet_metadata()` and asserts that every row group has cell_id min <= max
#' and that each row group starts at or after the previous one ended (the same cell may
#' straddle two row groups). This is what lets a reader prune to one row group.
#'
#' @param file tile parquet file
#' @param con a DuckDB connection (a temporary one when NULL)
#' @return invisibly, the row-group table; stops with a message when not monotone
cell_species_check <- function(file, con = NULL) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = T))
  }
  rg <- DBI::dbGetQuery(con, glue::glue("
    SELECT row_group_id, CAST(stats_min_value AS BIGINT) AS cell_min,
           CAST(stats_max_value AS BIGINT) AS cell_max
    FROM parquet_metadata('{file}')
    WHERE path_in_schema = 'cell_id'
    ORDER BY row_group_id"))
  if (nrow(rg) == 0 || anyNA(rg$cell_min) || anyNA(rg$cell_max))
    stop("no cell_id row-group statistics in ", file)
  if (any(rg$cell_min > rg$cell_max))
    stop("cell_id min > max in a row group of ", file)
  if (nrow(rg) > 1 && any(rg$cell_min[-1] < rg$cell_max[-nrow(rg)]))
    stop("cell_id row-group statistics overlap or are not sorted in ", file)
  invisible(rg)
}

#' SQL: the species lookup (eov_taxa plus names, ids and links)
#'
#' One row per taxon x EOV: all columns of `taxa_tbl` plus `common_name` (WoRMS
#' English, preferred name first), `sp_key` (the aquamaps_native registry's sp_key for
#' the AphiaID when present, else the first of `sp_keys`), `sis_taxon_id` and
#' `iucn_url` (latest Red List assessment matched by lower-case scientific name),
#' `worms_url` and `aquamaps_url`.
#'
#' @param taxa_tbl relation with aphia_id, scientific_name, sp_keys, ...
#' @param vern_tbl relation taxonID, vernacularName, language, isPreferredName
#' @param registry relation aphia_id, sp_key (the aquamaps_native registry)
#' @param redlist_tbl relation taxon_scientific_name, sis_taxon_id, url, latest
#' @param aquamaps_url template with `{sp_key}`
species_lookup_sql <- function(
    taxa_tbl = "eov_taxa", vern_tbl = "spp.worms_vernacular",
    registry = "read_parquet('https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/atlas/aquamaps_native/registry.parquet')",
    redlist_tbl = "spp.iucn_redlist",
    aquamaps_url = "https://www.aquamaps.org/preMap2.php?cache=1&SpecID={sp_key}") {
  url_sql <- gsub("\\{sp_key\\}", "' || sp_key || '", aquamaps_url)
  glue::glue("
    WITH vn AS (
      SELECT taxonID AS aphia_id, vernacularName AS common_name
      FROM {vern_tbl}
      WHERE language = 'ENG'
      QUALIFY row_number() OVER (
        PARTITION BY taxonID
        ORDER BY coalesce(CAST(isPreferredName AS INTEGER), 0) DESC, vernacularName) = 1),
    rg AS (
      SELECT CAST(aphia_id AS BIGINT) AS aphia_id, min(sp_key) AS sp_key
      FROM {registry} WHERE aphia_id IS NOT NULL GROUP BY 1),
    iu AS (
      SELECT lower(taxon_scientific_name) AS k, sis_taxon_id, url AS iucn_url
      FROM {redlist_tbl} WHERE latest
      QUALIFY row_number() OVER (
        PARTITION BY lower(taxon_scientific_name) ORDER BY year_published DESC) = 1)
    SELECT *, CASE WHEN sp_key IS NULL THEN NULL ELSE '{url_sql}' END AS aquamaps_url
    FROM (
      SELECT t.*,
             vn.common_name,
             coalesce(rg.sp_key, nullif(split_part(t.sp_keys, ';', 1), '')) AS sp_key,
             iu.sis_taxon_id,
             iu.iucn_url,
             'https://www.marinespecies.org/aphia.php?p=taxdetails&id=' || t.aphia_id AS worms_url
      FROM {taxa_tbl} t
      LEFT JOIN vn ON vn.aphia_id = t.aphia_id
      LEFT JOIN rg ON rg.aphia_id = t.aphia_id
      LEFT JOIN iu ON iu.k = lower(t.scientific_name))
    ORDER BY eov, aphia_id")
}

# score ----

#' Score 0-100 of a density
#'
#' score = 100 * min(1, d / q99), rounded to an integer, where q99 is the `q`
#' quantile of the reference densities (default: the strictly positive values of
#' `d`). Cells without a value stay NA.
#'
#' @param d numeric density (value per km2)
#' @param q quantile, default 0.99
#' @param ref reference densities for the quantile; default `d[d > 0]`
#' @return integer vector 0-100 (NA where `d` is NA); attribute `q` holds the
#'   quantile value
eov_score <- function(d, q = 0.99, ref = d[!is.na(d) & d > 0]) {
  ref <- ref[!is.na(ref)]
  if (length(ref) == 0) {
    out <- rep(NA_integer_, length(d))
    attr(out, "q") <- NA_real_
    return(out)
  }
  qv  <- as.numeric(stats::quantile(ref, q, names = F, type = 7))
  out <- if (qv > 0) {
    as.integer(round(100 * pmin(1, d / qv)))
  } else {
    ifelse(is.na(d), NA_integer_, 0L)
  }
  attr(out, "q") <- qv
  out
}

#' Composite score: row-wise mean of the non-NA EOV scores, rounded
#'
#' @param score_matrix numeric matrix, cells x EOVs, NA = not reportable
#' @return integer vector (NA where every EOV is NA)
eov_composite <- function(score_matrix) {
  m  <- as.matrix(score_matrix)
  n  <- rowSums(!is.na(m))
  s  <- rowSums(m, na.rm = T)
  out <- as.integer(round(s / n))
  out[n == 0] <- NA_integer_
  out
}

#' Number of reportable EOVs per row
eov_n <- function(score_matrix) as.integer(rowSums(!is.na(as.matrix(score_matrix))))

#' Composite density: sum of the EOV densities (NA when none is reportable)
eov_density_all <- function(density_matrix) {
  m   <- as.matrix(density_matrix)
  out <- rowSums(m, na.rm = T)
  out[rowSums(!is.na(m)) == 0] <- NA_real_
  out
}

# raster ----

#' Paint values by cell_id onto the global05 grid
#'
#' `cell_id` is the pixel value of `r_cellid_global.tif`, i.e. the row-major 1-based
#' cell number from the top-left, so cell_id indexes the raster directly. The
#' function asserts that convention on a sample of the template before painting.
#'
#' @param cell_id integer cell ids
#' @param val numeric vector, or matrix (cells x bands, colnames = layer names)
#' @param template_tif path of (or a SpatRaster with) the cell_id raster
#' @param nodata fill value for unpainted cells (255 for Byte, NA for Float32)
#' @param ncol,nrow expected grid dimensions (global05: 7200 x 3600)
#' @param ext expected extent c(xmin, xmax, ymin, ymax)
#' @param n_check number of sampled cells for the template check
#' @return SpatRaster with one layer per column of `val`
cells_to_raster <- function(
    cell_id, val, template_tif, nodata = NA_real_,
    ncol = 7200L, nrow = 3600L, ext = c(-180, 180, -90, 90), n_check = 1000L) {
  tpl <- if (inherits(template_tif, "SpatRaster")) template_tif else terra::rast(template_tif)
  stopifnot(
    terra::ncol(tpl) == ncol,
    terra::nrow(tpl) == nrow,
    all(abs(as.vector(terra::ext(tpl)) - ext) < 1e-4))   # the tif extent is off by ~6e-6 degrees
  val <- as.matrix(val)
  stopifnot(nrow(val) == length(cell_id), !anyNA(cell_id),
            all(cell_id >= 1L & cell_id <= ncol * nrow))
  # the check that makes cell_id == raster cell number
  set.seed(1)
  chk <- if (length(cell_id) > n_check) sample(cell_id, n_check) else cell_id
  got <- as.vector(terra::values(tpl, mat = F)[chk])
  stopifnot("template[cell_id] != cell_id: not the global05 cell_id raster" =
              identical(as.numeric(got), as.numeric(chk)))
  # exact global05 geometry (the template's own extent differs by ~6e-6 degrees)
  r  <- terra::rast(
    nrows = nrow, ncols = ncol, nlyrs = ncol(val), crs = "EPSG:4326",
    xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4])
  nm <- colnames(val)
  if (is.null(nm)) nm <- paste0("band", seq_len(ncol(val)))
  names(r) <- nm
  # NA values inside painted cells (EOV not reportable there) become the fill value
  val[is.na(val)] <- as.numeric(nodata)
  m <- matrix(as.numeric(nodata), nrow = ncol * nrow, ncol = ncol(val))
  m[cell_id, ] <- val
  terra::values(r) <- m
  r
}

# files, hashes ----

#' sha256 multihash (prefix 1220) of a file, as used by `file:checksum`
file_multihash <- function(path) {
  paste0("1220", digest::digest(path, algo = "sha256", file = T))
}

#' STAC asset for a local file, with `file:size` and `file:checksum`
#'
#' @param path local file
#' @param href published (relative) href
#' @param type media type
#' @param title asset title
#' @param roles character vector of roles
#' @param extra named list of further asset fields
stac_asset_file <- function(path, href, type, title, roles = "data", extra = list()) {
  c(list(
    href  = href,
    type  = type,
    title = title,
    roles = I(roles)),
    extra,
    list(
      `file:size`     = as.numeric(file.info(path)$size),
      `file:checksum` = file_multihash(path)))
}

# stac ----

eov_method_detail <- function(scheme = "ohi", q = 0.99) {
  paste0(
    "For each WoRMS species s and 0.05 degree cell c of the MarineSensitivity v9 global grid: ",
    "the AquaMaps suitability v (1-100, maximum over the species' models) is clipped to the IUCN range ",
    "footprint when the species has one (otherwise the full distribution is used); the share ",
    "share_s,c = u_s,c A_c / sum_c(u_s,c A_c) is the cell's fraction of the species' area-weighted ",
    "habitat (A_c = cell area, km2). Per EOV g (the union of the WoRMS subtrees of the MLDN seeds) ",
    "the cell value is share_c,g = sum_s share_s,c and the risk-weighted risk_c,g = sum_s w_s share_s,c ",
    "with Butchart/OHI extinction-risk weights w (scheme '", scheme, "'; Data Deficient and Not ",
    "Evaluated carry no weight). Densities (value per km2) are scored 0-100 as ",
    "100 * min(1, D / q), q the ", q, " quantile over reportable cells; the composite 'all' is the ",
    "mean of the reportable EOV scores. Seabirds and seagrasses are absent because AquaMaps models neither.")
}

#' Weights table of a scheme (code, weight)
eov_weights_table <- function(scheme = c("ohi", "mst_v1")) {
  scheme <- match.arg(scheme)
  codes  <- c("LC", "NT", "VU", "EN", "CR", "EX", "EW", "DD", "NE")
  data.frame(
    code   = codes,
    weight = redlist_weight(codes, scheme),
    stringsAsFactors = F)
}

.eov_dims <- function() {
  list(
    eov = list(
      type        = "other",
      description = "Essential Ocean Variable (taxonomic group); 'all' is the equal-weight composite",
      values      = I(eov_levels()),
      `atlas:labels` = as.list(eov_labels())),
    metric = list(
      type        = "other",
      description = "which share is scored",
      values      = I(c("risk", "share")),
      `atlas:labels` = list(
        risk  = "Risk-weighted share",
        share = "Share (unweighted)")))
}

.eov_sdm_core <- function(method_detail = NULL) {
  core <- list(
    `sdm:response_type`     = "mixed",
    `sdm:method`            = "derived_merge",
    `sdm:source_authority`  = "OceanMetrics",
    `sdm:temporal_interval` = "static",
    `sdm:value_unit`        = "score",
    `sdm:value_range`       = I(c(0, 100)))
  if (!is.null(method_detail)) core[["sdm:method_detail"]] <- method_detail
  core
}

.eov_cog_type <- "image/tiff; application=geotiff; profile=cloud-optimized"

#' STAC Collection of the AquaMaps EOV product
#'
#' Field names follow the plan D4 / the atlas conventions: `atlas:flower` on the
#' collection, `atlas:labels` inside each `cube:dimensions` entry.
#'
#' @param version product version (semver)
#' @param assets named list of assets, e.g. from [stac_asset_file()]
#' @param items character vector of item ids to link
#' @param scheme,q weight scheme and score quantile (for the method text)
#' @param updated ISO timestamp
#' @param base_url public prefix of the product
#' @param notebook_url rendered notebook (the method and the code)
#' @param cell_species TRUE adds the `atlas:cell_species` field ([eov_cell_species_field()])
#' @return list ready for [jsonlite::write_json()] with `auto_unbox = T`
stac_collection_eov <- function(
    version      = "1.0.0",
    assets       = list(),
    items        = character(),
    scheme       = "ohi",
    q            = 0.99,
    updated      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    base_url     = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/atlas/aquamaps_eov",
    notebook_url = "https://oceanmetrics.io/workflows/aquamaps_eov.html",
    cell_species = FALSE) {
  lnk <- function(rel, href, type = "application/json", title = NULL) {
    l <- list(rel = rel, href = href, type = type)
    if (!is.null(title)) l$title <- title
    l
  }
  links <- c(
    list(
      lnk("root",   "../catalog.json", title = "OceanMetrics Atlas"),
      lnk("parent", "../catalog.json", title = "OceanMetrics Atlas"),
      lnk("self",   paste0(base_url, "/collection.json")),
      lnk("describedby", "./README.md", "text/markdown", "Methods, files and credits"),
      lnk("describedby", notebook_url, "text/html", "Method notebook"),
      lnk("agents",  "./AGENTS.md", "text/markdown", "Agent/LLM usage guide"),
      lnk("preview", "./thumbnail.png", "image/png", "Thumbnail"),
      lnk("license", "https://creativecommons.org/licenses/by-nc/3.0/", "text/html",
          "CC BY-NC 3.0 Unported (inherited from AquaMaps)"),
      lnk("related", "https://file.marinesensitivity.org/stac/v9/am/collection.json",
          title = "MarineSensitivity v9 AquaMaps (msens-v9-am)"),
      lnk("related", "https://file.marinesensitivity.org/stac/v9/rng_iucn/collection.json",
          title = "MarineSensitivity v9 IUCN ranges (msens-v9-rng_iucn)"),
      lnk("via", "https://www.aquamaps.org", "text/html", "AquaMaps (producer)")),
    lapply(items, \(id) lnk("item", paste0("./items/", id, ".json"))))
  col <- c(
    list(
      type           = "Collection",
      stac_version   = "1.1.0",
      stac_extensions = I(c(
        "https://marinesensitivity.github.io/stac-sdm/v1.0.0/schema.json",
        "https://stac-extensions.github.io/scientific/v1.0.0/schema.json",
        "https://stac-extensions.github.io/raster/v1.1.0/schema.json",
        "https://stac-extensions.github.io/table/v1.2.0/schema.json",
        "https://stac-extensions.github.io/version/v1.2.0/schema.json",
        "https://stac-extensions.github.io/file/v2.1.0/schema.json",
        "https://stac-extensions.github.io/datacube/v2.2.0/schema.json")),
      id          = "aquamaps_eov",
      updated     = updated,
      title       = "AquaMaps EOV share: extinction-risk weighted habitat share by Essential Ocean Variable",
      description = paste(
        "Per 0.05 degree cell and Essential Ocean Variable (fish, hard corals, mangroves,",
        "marine mammals, sea turtles; seabirds and seagrasses are absent because AquaMaps",
        "has none), the share of the species' area-weighted AquaMaps habitat clipped to IUCN",
        "ranges, optionally weighted by IUCN Red List extinction risk (Butchart/OHI), scored",
        "0-100, with an equal-weight composite across EOVs. Derived from MarineSensitivity",
        "release v9. Non-commercial use only (CC BY-NC 3.0). Method and code: the rendered",
        "notebook."),
      license     = "CC-BY-NC-3.0",
      version     = version,
      deprecated  = FALSE,
      `atlas:note` = paste(
        "Non-commercial use only (CC BY-NC 3.0), inherited from AquaMaps.",
        "Link back to aquamaps.org when you reuse the numbers."),
      `sci:citation` = paste(
        "OceanMetrics (2026) AquaMaps EOV share, version", version,
        "https://oceanmetrics.io/workflows/aquamaps_eov.html.",
        "Built on Kaschner, K., Kesner-Reyes, K., Garilao, C., Segschneider, J., Rius-Barile, J.,",
        "Rees, T., Froese, R. (2019). AquaMaps: Predicted range maps for aquatic species.",
        "www.aquamaps.org, Version 10/2019."),
      `sci:publications` = list(
        list(doi = "10.1016/j.ecolmodel.2009.10.025", citation = "Ready et al. (2010) Predicting the distributions of marine organisms at the global scale. Ecological Modelling 221:467-478"),
        list(doi = "10.1371/journal.pbio.0020383", citation = "Butchart et al. (2004) Measuring global trends in the status of biodiversity: Red List Indices for birds. PLoS Biology 2(12):e383"),
        list(doi = "10.1371/journal.pone.0000140", citation = "Butchart et al. (2007) Improvements to the Red List Index. PLoS ONE 2(1):e140"),
        list(doi = "10.1038/nature11397", citation = "Halpern et al. (2012) An index to assess the health and benefits of the global ocean. Nature 488:615-620"),
        list(doi = "10.1111/gcb.14108", citation = "Miloslavich et al. (2018) Essential Ocean Variables for global sustained observations of biodiversity and ecosystem changes. Global Change Biology 24(6):2416-2433")),
      providers = list(
        list(name = "AquaMaps", roles = I(c("producer", "licensor")),
             url = "https://www.aquamaps.org",
             description = "Kaschner et al. 2019 relative environmental suitability models"),
        list(name = "IUCN", roles = I("licensor"), url = "https://www.iucnredlist.org",
             description = "Red List categories and species range polygons"),
        list(name = "World Register of Marine Species", roles = I("producer"),
             url = "https://www.marinespecies.org",
             description = "taxonomy and the AphiaID join key"),
        list(name = "MarineSensitivity", roles = I("processor"),
             url = "https://marinesensitivity.org",
             description = "v9 release: AquaMaps downscaled to 0.05 degree, IUCN ranges gridded; v10 share method"),
        list(name = "OceanMetrics", roles = I(c("processor", "host")),
             url = "https://oceanmetrics.io")),
      contacts = list(
        list(name = "Ben Best", emails = list(list(value = "ben@oceanmetrics.io")),
             roles = I(c("producer", "pointOfContact")))),
      `atlas:funders` = list(),
      `atlas:status` = "prototype",
      `atlas:source_release` = "MarineSensitivity v9"),
    .eov_sdm_core(eov_method_detail(scheme, q)),
    list(
      `sdm:derived_from` = I(c(
        "msens-v9-am", "msens-v9-rng_iucn", "worms-2026-07-01",
        "iucn-redlist-2025", "ioos-mldn-eov-taxonomy")),
      `atlas:flower` = list(dimension = "eov", composite = "all"),
      extent = list(
        spatial  = list(bbox = list(c(-180, -90, 180, 90))),
        temporal = list(interval = list(c("2019-10-01T00:00:00Z", "2019-10-01T00:00:00Z")))),
      `cube:dimensions` = .eov_dims(),
      assets = assets,
      links  = links))
  if (isTRUE(cell_species)) col[["atlas:cell_species"]] <- eov_cell_species_field()
  col

}

#' The `atlas:cell_species` collection field (the atlas hook for the cell -> species table)
#'
#' Tells a client where the tiled `cell_id, aphia_id, suit` Parquet files are, how to
#' find the tile of a cell (5 degree tiles of the global05 grid), the species lookup
#' asset and the links to show per species.
#'
#' @return list, serialised verbatim into the collection
eov_cell_species_field <- function() {
  list(
    asset        = "cell_species",
    lookup_asset = "species",
    layout       = "tiles",
    tile_deg     = 5L,
    href_template = "./cell_species/tile={tile}/data_0.parquet",
    grid = list(ncol = 7200L, nrow = 3600L, x0 = -180L, y0 = 90L, res = 0.05),
    key_column     = "cell_id",
    species_column = "aphia_id",
    lookup_key     = "aphia_id",
    value_column   = "suit",
    value_unit     = "percent",
    value_range    = I(c(1L, 100L)),
    value_label    = "Suitability",
    group_column    = "eov",
    group_dimension = "eov",
    share = list(
      total_column = "total_suit_area",
      formula      = "suit * area_km2(lat) / total_suit_area"),
    links = list(
      list(label = "Atlas", href_template = "?c=aquamaps_native&sp={sp_key}", same_app = TRUE),
      list(label = "WoRMS", column = "worms_url"),
      list(label = "IUCN", column = "iucn_url"),
      list(label = "AquaMaps", column = "aquamaps_url")))
}

#' STAC Item of one EOV x metric layer
#'
#' `atlas:bidx|rescale|colormap` live on the asset; `atlas:value_range`,
#' `atlas:dims` and `atlas:metric` on the properties.
#'
#' @param eov one of [eov_levels()]
#' @param metric "risk" or "share"
#' @param version product version
#' @param n_taxa number of taxa in the EOV (NA for `all`)
#' @param q99 density quantile used for the score (NA when absent)
#' @param base_url public prefix of the product
stac_item_eov <- function(
    eov, metric, version = "1.0.0", n_taxa = NA_integer_, q99 = NA_real_,
    base_url = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/atlas/aquamaps_eov") {
  stopifnot(eov %in% eov_levels(), metric %in% c("risk", "share"))
  id    <- paste0(eov, "_", metric)
  band  <- match(eov, eov_bands())
  absent <- eov %in% eov_absent()
  lab   <- eov_labels()[[eov]]
  mlab  <- c(risk = "Risk-weighted share", share = "Share (unweighted)")[[metric]]
  bands <- function(type, nodata, unit) {
    lapply(eov_bands(), \(b) list(
      name = b, description = unname(c(eov_labels(), n_eov = "Number of reportable EOVs")[b]),
      data_type = type, nodata = nodata, unit = unit))
  }
  props <- c(
    list(
      title          = paste0(lab, " \u00b7 ", mlab),
      datetime       = NULL,
      start_datetime = "2019-10-01T00:00:00Z",
      end_datetime   = "2019-10-01T00:00:00Z",
      version        = version,
      `sdm:env_datetime` = "2019-10"),
    .eov_sdm_core(),
    list(
      `atlas:dims`        = list(eov = eov, metric = metric),
      `atlas:metric`      = metric,
      `atlas:value_range` = I(c(0, 100)),
      `atlas:n_taxa`      = if (is.na(n_taxa)) NULL else as.integer(n_taxa),
      `atlas:q99`         = if (is.na(q99)) NULL else q99))
  # keep the explicit datetime null, drop the other unset (NULL) fields
  props <- props[!vapply(props, is.null, NA) | names(props) == "datetime"]
  if (absent) {
    props[["atlas:absent"]] <- TRUE
    props[["atlas:absent_note"]] <- paste0("AquaMaps models no ", tolower(lab))
    props[["description"]]  <- paste0(
      lab, " is absent from this product: AquaMaps models no ", tolower(lab),
      ", so the layer is empty (nodata) by construction.")
  }
  list(
    type            = "Feature",
    stac_version    = "1.1.0",
    stac_extensions = I(c(
      "https://marinesensitivity.github.io/stac-sdm/v1.0.0/schema.json",
      "https://stac-extensions.github.io/raster/v1.1.0/schema.json",
      "https://stac-extensions.github.io/version/v1.2.0/schema.json",
      "https://stac-extensions.github.io/datacube/v2.2.0/schema.json")),
    id         = id,
    collection = "aquamaps_eov",
    bbox       = I(c(-180, -90, 180, 90)),
    geometry   = list(
      type        = "Polygon",
      coordinates = list(list(
        c(-180, -90), c(180, -90), c(180, 90), c(-180, 90), c(-180, -90)))),
    properties = props,
    assets = list(
      cog = list(
        href  = paste0("../score_", metric, ".tif"),
        type  = .eov_cog_type,
        roles = I("data"),
        title = paste0(lab, ", ", mlab, " score 0-100 (band ", band, ")"),
        `atlas:bidx`     = band,
        `atlas:rescale`  = I(c(0, 100)),
        `atlas:colormap` = "viridis",
        `raster:bands`   = bands("uint8", 255L, "score")),
      density = list(
        href  = paste0("../density_", metric, ".tif"),
        type  = .eov_cog_type,
        roles = I("reference"),
        title = paste0(lab, ", ", mlab, " density (per km2, band ", band, ")"),
        `atlas:bidx`   = band,
        `raster:bands` = lapply(eov_levels(), \(b) list(
          name = b, data_type = "float32", nodata = "nan", unit = "1/km2")))),
    links = list(
      list(rel = "root",       href = "../../../catalog.json", type = "application/json"),
      list(rel = "parent",     href = "../collection.json",    type = "application/json"),
      list(rel = "collection", href = "../collection.json",    type = "application/json"),
      list(rel = "self",       href = paste0("./", id, ".json"), type = "application/json")))
}

#' Write a STAC list to JSON
write_stac_json <- function(x, path) {
  dir.create(dirname(path), showWarnings = F, recursive = T)
  jsonlite::write_json(
    x, path, auto_unbox = T, pretty = T, null = "null", na = "null", digits = NA)
  invisible(path)
}

# h3 ----

#' SQL: area-weighted mean of scores and densities per H3 cell
#'
#' Needs the DuckDB `h3` community extension; cell centroids (lon, lat) are
#' assigned to the H3 cell of resolution `res`.
#'
#' @param cell_tbl relation with cell_id, lon, lat, area_km2 and the value columns
#' @param cols value columns to average
#' @param res H3 resolution
eov_h3_sql <- function(cell_tbl, cols, res = 5L) {
  avg <- paste0("sum(", cols, " * area_km2) / sum(area_km2) FILTER (WHERE ", cols,
                " IS NOT NULL) AS ", cols, collapse = ",\n  ")
  glue::glue("
    SELECT
      h3_latlng_to_cell_string(lat, lon, {res}) AS h3,
      count(*) AS n_cells,
      {avg}
    FROM {cell_tbl}
    GROUP BY 1")
}
