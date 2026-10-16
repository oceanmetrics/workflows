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

#' SQL: the species-share and EOV-aggregation statements (DuckDB)
#'
#' Steps 1-5 of the method (see aquamaps_eov.qmd). Returns a named list of SQL
#' statements to run in order; the ones that create TABLEs are the checkpoints.
#'
#' 1. `sp_map` (table): model to species, with `n_models` and `has_range`.
#' 2. `sp_cell` (view): v = suitability per species and cell, the max over the
#'    species' models (single-model species avoid the group-by).
#' 3. `sp_clip` (view): u = v inside the IUCN range footprint when the species has
#'    one, else v over the full distribution.
#' 4. `sp_total` (table): per species, n_cells and total = sum(u * area_km2); a
#'    species with total 0 is `excluded`.
#' 5. `sp_share` (view): share = u * area_km2 / total.
#' 6. `cell_eov` (table): per cell and EOV, `share` = sum(share) over every taxon
#'    and `risk` = sum(weight * share); NULL weights add nothing to `risk`.
#'
#' @param am_tbl relation with mdl_key, cell_id, val (the AquaMaps partitions)
#' @param map_tbl relation with mdl_key, aphia_id (AquaMaps model to WoRMS species)
#' @param rng_tbl relation with aphia_id, cell_id (IUCN range footprint)
#' @param area_tbl relation with cell_id, area_km2
#' @param eov_tbl relation with eov, aphia_id (membership)
#' @param weight_tbl relation with aphia_id, weight (NULL weight = no weight)
#' @param out_prefix prefix of the created tables and views
#' @return named list of SQL strings: sp_map, sp_cell, sp_clip, sp_total,
#'   sp_share, cell_eov
eov_share_sql <- function(
    am_tbl, map_tbl, rng_tbl, area_tbl, eov_tbl, weight_tbl,
    out_prefix = "eov_") {
  p <- out_prefix
  list(
    sp_map = glue::glue("
      CREATE OR REPLACE TABLE {p}sp_map AS
      SELECT
        m.mdl_key,
        m.aphia_id,
        count(*) OVER (PARTITION BY m.aphia_id)               AS n_models,
        m.aphia_id IN (SELECT aphia_id FROM {rng_tbl})        AS has_range
      FROM (SELECT DISTINCT mdl_key, aphia_id FROM {map_tbl}) m"),
    sp_cell = glue::glue("
      CREATE OR REPLACE VIEW {p}sp_cell AS
      SELECT m.aphia_id, a.cell_id, a.val AS v
      FROM {am_tbl} a
      JOIN {p}sp_map m ON m.mdl_key = a.mdl_key
      WHERE m.n_models = 1
      UNION ALL
      SELECT m.aphia_id, a.cell_id, max(a.val) AS v
      FROM {am_tbl} a
      JOIN {p}sp_map m ON m.mdl_key = a.mdl_key
      WHERE m.n_models > 1
      GROUP BY m.aphia_id, a.cell_id"),
    sp_clip = glue::glue("
      CREATE OR REPLACE VIEW {p}sp_clip AS
      SELECT s.aphia_id, s.cell_id, s.v AS u
      FROM {p}sp_cell s
      WHERE s.aphia_id IN (SELECT aphia_id FROM {p}sp_map WHERE NOT has_range)
      UNION ALL
      SELECT s.aphia_id, s.cell_id, s.v AS u
      FROM {p}sp_cell s
      WHERE EXISTS (
        SELECT 1 FROM {rng_tbl} r
        WHERE r.aphia_id = s.aphia_id AND r.cell_id = s.cell_id)"),
    sp_total = glue::glue("
      CREATE OR REPLACE TABLE {p}sp_total AS
      SELECT
        m.aphia_id,
        m.n_models,
        m.has_range,
        coalesce(t.n_cells, 0)                AS n_cells,
        coalesce(t.total_suit_area, 0.0)      AS total_suit_area,
        coalesce(t.total_suit_area, 0.0) <= 0 AS excluded
      FROM (SELECT DISTINCT aphia_id, n_models, has_range FROM {p}sp_map) m
      LEFT JOIN (
        SELECT c.aphia_id, count(*) AS n_cells,
               sum(c.u * a.area_km2) AS total_suit_area
        FROM {p}sp_clip c
        JOIN {area_tbl} a ON a.cell_id = c.cell_id
        GROUP BY c.aphia_id) t
        ON t.aphia_id = m.aphia_id"),
    sp_share = glue::glue("
      CREATE OR REPLACE VIEW {p}sp_share AS
      SELECT c.aphia_id, c.cell_id, c.u * a.area_km2 / t.total_suit_area AS share
      FROM {p}sp_clip c
      JOIN {area_tbl} a ON a.cell_id = c.cell_id
      JOIN {p}sp_total t ON t.aphia_id = c.aphia_id
      WHERE NOT t.excluded"),
    cell_eov = glue::glue("
      CREATE OR REPLACE TABLE {p}cell_eov AS
      SELECT
        s.cell_id,
        e.eov,
        sum(s.share)                           AS share,
        coalesce(sum(w.weight * s.share), 0.0) AS risk
      FROM {p}sp_share s
      JOIN {eov_tbl} e ON e.aphia_id = s.aphia_id
      LEFT JOIN {weight_tbl} w ON w.aphia_id = s.aphia_id
      GROUP BY s.cell_id, e.eov"))
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
#' @return list ready for [jsonlite::write_json()] with `auto_unbox = T`
stac_collection_eov <- function(
    version      = "1.0.0",
    assets       = list(),
    items        = character(),
    scheme       = "ohi",
    q            = 0.99,
    updated      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    base_url     = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/atlas/aquamaps_eov",
    notebook_url = "https://oceanmetrics.io/workflows/aquamaps_eov.html") {
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
  c(
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
