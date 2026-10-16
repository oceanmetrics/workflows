# tests for R/eov_share.R: one test_that per rule, tiny synthetic fixtures ----

library(testthat)
library(DBI)

# locate and source the rules (works from the repo root or tests/testthat) ----
.src <- c("R/eov_share.R", "../../R/eov_share.R")
.src <- .src[file.exists(.src)][1]
source(.src)

new_con <- function() DBI::dbConnect(duckdb::duckdb())

# seeds ----
test_that("seeds: 33 seeds over 7 EOVs with the MLDN identifiers", {
  s <- eov_seeds()
  expect_equal(nrow(s), 33L)
  expect_equal(length(unique(s$eov)), 7L)
  expect_equal(
    unname(table(s$eov)[eov_keys()]),
    unname(table(factor(s$eov, eov_keys()))))
  expect_equal(
    as.list(table(s$eov))[c("fish", "hardCorals", "mangroves", "marineMammals",
                            "seabirds", "seagrasses", "seaTurtles")],
    list(fish = 3L, hardCorals = 1L, mangroves = 19L, marineMammals = 7L,
         seabirds = 1L, seagrasses = 1L, seaTurtles = 1L),
    ignore_attr = T)
  fish <- s[s$eov == "fish", ]
  expect_equal(fish$aphiaid, c(1829L, 1517375L, 152352L))
  expect_equal(fish$taxon, c("Agnatha", "Chondrichthyes", "Osteichthyes"))
  expect_equal(fish$rank, c("Infraphylum", "Parvphylum", "Parvphylum"))
  expect_equal(s$aphiaid[s$taxon == "Lutra felina"], 477316L)
  expect_equal(s$aphiaid[s$taxon == "Aves"], 1836L)
  expect_equal(s$aphiaid[s$taxon == "Chelonioidea"], 987094L)
  expect_false(anyDuplicated(s$aphiaid) > 0)
  expect_equal(eov_levels()[1], "all")
  expect_equal(unname(eov_labels()[c("all", "hardCorals", "seaTurtles")]),
               c("All EOVs (mean)", "Hard corals", "Sea turtles"))
  expect_equal(eov_bands(), c("all", "fish", "hardCorals", "mangroves", "marineMammals",
                              "seabirds", "seagrasses", "seaTurtles", "n_eov"))
})

# tree ----
test_that("tree CTE: union of subtrees via parentNameUsageID", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  # 1 -> 2 -> 4, 1 -> 3, 5 -> 6 ; 9 is an unrelated root
  worms <- data.frame(
    taxonID           = c(1L, 2L, 3L, 4L, 5L, 6L),
    parentNameUsageID = c(NA, 1L, 1L, 2L, NA, 5L))
  seeds <- data.frame(
    eov     = c("a", "a", "b", "c"),
    aphiaid = c(2L, 3L, 5L, 99L))     # 99 is not in worms: only itself
  DBI::dbWriteTable(con, "worms", worms)
  DBI::dbWriteTable(con, "seeds", seeds)
  d <- DBI::dbGetQuery(con, eov_tree_sql("seeds", "worms"))
  d <- d[order(d$eov, d$taxonID), ]
  expect_equal(
    paste(d$eov, d$taxonID),
    c("a 2", "a 3", "a 4", "b 5", "b 6", "c 99"))
  # overlapping seeds do not duplicate members
  seeds2 <- data.frame(eov = c("a", "a"), aphiaid = c(1L, 2L))
  DBI::dbWriteTable(con, "seeds2", seeds2)
  d2 <- DBI::dbGetQuery(con, eov_tree_sql("seeds2", "worms"))
  expect_equal(sort(d2$taxonID), 1:4)
})

# weights ----
test_that("weights: ohi scheme, every code", {
  codes <- c("LC", "NT", "VU", "EN", "CR", "EX", "EW")
  expect_equal(redlist_weight(codes, "ohi"), c(0, .2, .4, .6, .8, 1, 1))
  expect_equal(redlist_weight(codes), redlist_weight(codes, "ohi"))
})
test_that("weights: mst_v1 scheme, every code", {
  codes <- c("LC", "NT", "VU", "EN", "CR", "EX", "EW")
  expect_equal(redlist_weight(codes, "mst_v1"), c(.2, .4, .6, .8, 1, 1, 1))
})
test_that("weights: legacy codes map to LC / NT", {
  expect_equal(redlist_weight(c("LR/lc", "LR/nt", "LR/cd")), c(0, .2, .2))
  expect_equal(redlist_weight(c("LR/lc", "LR/nt", "LR/cd"), "mst_v1"), c(.2, .4, .4))
})
test_that("weights: DD, NE, NA carry no weight; unknown codes stop", {
  expect_equal(redlist_weight(c("DD", "NE", NA, "NA", "CR")), c(NA, NA, NA, NA, .8))
  expect_type(redlist_weight(NA_character_), "double")
  expect_error(redlist_weight(c("LC", "XX")), "unknown Red List code")
  expect_error(redlist_weight("LC", "other"))
  expect_length(redlist_weight(character()), 0)
})

# share ----
# fixture: 6 cells with unequal areas, 4 species, 5 models
#   cell      1  2  3  4  5  6
#   area      1  2  1  4  2  1
# A (101, fish, VU .4): one model, v 10 20 30 40 on c1-c4; range c2-c4 (c1 clipped out)
# B (102, fish, DD NULL): no range, v 50 50 100 on c1 c5 c6
# C (103, hardCorals, CR .8): two models, c1:10 c2:30 | c2:60 c3:20 -> max 10 60 20; no range
# D (104, fish, EN .6): v 40 40 on c5 c6, range c1 c2 only: zero overlap -> excluded
share_fixture <- function(con, dir = tempfile()) {
  dir.create(dir)
  am <- rbind(
    data.frame(mdl_key = "am|A", cell_id = 1:4,  val = c(10, 20, 30, 40)),
    data.frame(mdl_key = "am|B", cell_id = c(1L, 5L, 6L), val = c(50, 50, 100)),
    data.frame(mdl_key = "am|C1", cell_id = 1:2, val = c(10, 30)),
    data.frame(mdl_key = "am|C2", cell_id = 2:3, val = c(60, 20)),
    data.frame(mdl_key = "am|D", cell_id = 5:6, val = c(40, 40)))
  pq <- file.path(dir, "am.parquet")
  DBI::dbWriteTable(con, "am_raw", am)
  DBI::dbExecute(con, glue::glue("COPY am_raw TO '{pq}' (FORMAT parquet)"))
  DBI::dbWriteTable(con, "map", data.frame(
    mdl_key = c("am|A", "am|B", "am|C1", "am|C2", "am|D"),
    aphia_id = c(101L, 102L, 103L, 103L, 104L)))
  DBI::dbWriteTable(con, "rng", data.frame(
    aphia_id = c(101L, 101L, 101L, 101L, 104L, 104L),   # note the duplicate row
    cell_id  = c(2L, 3L, 4L, 4L, 1L, 2L)))
  DBI::dbWriteTable(con, "area", data.frame(
    cell_id = 1:6, area_km2 = c(1, 2, 1, 4, 2, 1)))
  DBI::dbWriteTable(con, "eov", data.frame(
    eov = c("fish", "fish", "hardCorals", "fish"),
    aphia_id = c(101L, 102L, 103L, 104L)))
  DBI::dbWriteTable(con, "wt", data.frame(
    aphia_id = 101:104,
    weight = c(0.4, NA, 0.8, 0.6)))
  sql <- eov_share_sql(
    am_tbl = glue::glue("read_parquet('{pq}')"), map_tbl = "map", rng_tbl = "rng",
    area_tbl = "area", eov_tbl = "eov", weight_tbl = "wt")
  for (s in sql) DBI::dbExecute(con, s)
  sql
}

test_that("share sql: statements are the documented checkpoints", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  sql <- share_fixture(con)
  expect_named(sql, c("sp_map", "sp_cell", "sp_clip", "sp_total", "sp_share", "cell_eov"))
})

test_that("share sql (a): range clip keeps only cells inside the range, exact shares", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  s <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_share WHERE aphia_id = 101 ORDER BY cell_id")
  expect_equal(s$cell_id, 2:4)                                # c1 outside the range dropped
  expect_equal(s$share, c(40, 30, 160) / 230)                 # u*A = 20*2, 30*1, 40*4
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total WHERE aphia_id = 101")
  expect_equal(t$total_suit_area, 230)
  expect_equal(t$n_cells, 3)
  expect_true(t$has_range); expect_false(t$excluded)
})

test_that("share sql (b): species without range uses the full distribution", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  s <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_share WHERE aphia_id = 102 ORDER BY cell_id")
  expect_equal(s$cell_id, c(1L, 5L, 6L))
  expect_equal(s$share, c(50, 100, 100) / 250)
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total WHERE aphia_id = 102")
  expect_false(t$has_range)
})

test_that("share sql (c): two models on an overlapping cell take the max", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  v <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_cell WHERE aphia_id = 103 ORDER BY cell_id")
  expect_equal(v$v, c(10, 60, 20))                            # c2: max(30, 60)
  s <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_share WHERE aphia_id = 103 ORDER BY cell_id")
  expect_equal(s$share, c(10, 120, 20) / 150)
  expect_equal(DBI::dbGetQuery(con, "SELECT n_models FROM eov_sp_total WHERE aphia_id = 103")$n_models, 2)
})

test_that("share sql (d): species whose cells never overlap its range is excluded", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total WHERE aphia_id = 104")
  expect_equal(t$total_suit_area, 0)
  expect_equal(t$n_cells, 0)
  expect_true(t$excluded)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM eov_sp_share WHERE aphia_id = 104")$n, 0)
  # every non-excluded species shares sum to 1
  sums <- DBI::dbGetQuery(con, "SELECT aphia_id, sum(share) s FROM eov_sp_share GROUP BY 1 ORDER BY 1")
  expect_equal(sums$aphia_id, 101:103)
  expect_equal(sums$s, rep(1, 3))
})

test_that("share sql: per cell x EOV share and risk, NULL weight excluded from risk only", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  ce <- DBI::dbGetQuery(con, "SELECT * FROM eov_cell_eov ORDER BY eov, cell_id")
  fish <- ce[ce$eov == "fish", ]
  hc   <- ce[ce$eov == "hardCorals", ]
  expect_equal(fish$cell_id, c(1L, 2L, 3L, 4L, 5L, 6L))
  # share: B (c1, c5, c6) + A (c2-c4); D contributes nothing
  expect_equal(fish$share, c(.2, 40/230, 30/230, 160/230, .4, .4))
  # risk: only A (VU .4) is weighted; B is DD (NULL) so c1, c5, c6 are 0, not NA
  expect_equal(fish$risk, c(0, .4 * 40/230, .4 * 30/230, .4 * 160/230, 0, 0))
  expect_false(anyNA(fish$risk))
  expect_equal(hc$cell_id, 1:3)
  expect_equal(hc$share, c(10, 120, 20) / 150)
  expect_equal(hc$risk, .8 * c(10, 120, 20) / 150)
  # the EOV totals: one unit of share per included species
  expect_equal(sum(fish$share), 2)
  expect_equal(sum(hc$share), 1)
  expect_equal(sum(fish$risk), 0.4)
})

# score ----
test_that("score: 100 * min(1, d / q99), q99 over the positive values", {
  d  <- c(0, 1:100, NA)
  sc <- eov_score(d)
  q  <- unname(quantile(1:100, 0.99))
  expect_equal(attr(sc, "q"), q)
  expect_equal(sc[1], 0L)                                      # d = 0 -> 0
  expect_equal(sc[length(d)], NA_integer_)                     # NA stays NA
  expect_equal(sc[101], 100L)                                    # d = 100 > q99: capped
  expect_equal(sc[51 + 0], as.integer(round(100 * 50 / q)))    # d = 50
  expect_true(all(sc[!is.na(sc)] >= 0 & sc[!is.na(sc)] <= 100))
  expect_type(sc, "integer")
})
test_that("score: other quantile, explicit reference, empty reference", {
  d <- c(0, 1:10)
  expect_equal(attr(eov_score(d, q = 1), "q"), 10)
  expect_equal(eov_score(d, q = 1)[11], 100L)
  # the reference may differ from d (reportable cells, plan D3.6)
  expect_equal(attr(eov_score(d, q = 1, ref = c(0, d)), "q"), 10)
  expect_equal(eov_score(c(5, 10), q = 1, ref = c(0, 20))[1], 25L)
  expect_true(all(is.na(eov_score(c(0, 0, NA)))))
})

test_that("composite: row mean of the non-NA scores, integer", {
  m <- rbind(c(10, 20, NA), c(NA, NA, NA), c(0, NA, 6), c(100, 100, 100))
  expect_equal(eov_composite(m), c(15L, NA, 3L, 100L))
  expect_equal(eov_n(m), c(2L, 0L, 2L, 3L))
  expect_type(eov_composite(m), "integer")
  d <- rbind(c(1, 2, NA), c(NA, NA, NA))
  expect_equal(eov_density_all(d), c(3, NA))
})

test_that("taxa summary counts per EOV", {
  taxa <- data.frame(
    eov = c("fish", "fish", "fish", "hardCorals"),
    has_range = c(T, F, T, F),
    weight = c(.2, NA, .4, 0),
    excluded = c(F, F, T, F))
  s <- eov_summarize_taxa(taxa)
  expect_equal(s$eov, c("fish", "hardCorals"))
  expect_equal(s$n_taxa, c(3L, 1L))
  expect_equal(s$n_with_range, c(2L, 0L))
  expect_equal(s$n_weighted, c(1L, 1L))        # NA weight and excluded taxa do not count
  expect_equal(s$n_excluded, c(1L, 0L))
})

# raster ----
tiny_template <- function(shuffle = F) {
  r <- terra::rast(ncols = 8, nrows = 4, xmin = -180, xmax = 180, ymin = -90, ymax = 90,
                   crs = "EPSG:4326")
  v <- 1:32
  if (shuffle) v <- rev(v)
  terra::values(r) <- v
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f, datatype = "INT4U")
  f
}
test_that("cells_to_raster paints values at cell_id and verifies the template", {
  tpl <- tiny_template()
  v   <- cbind(a = c(10, 20, 30), b = c(1, 2, 3))
  r   <- cells_to_raster(c(1L, 10L, 32L), v, tpl, nodata = 255, ncol = 8L, nrow = 4L)
  expect_s4_class(r, "SpatRaster")
  expect_equal(dim(r)[1:3], c(4, 8, 2))
  expect_equal(names(r), c("a", "b"))
  vals <- terra::values(r)
  expect_equal(vals[c(1, 10, 32), "a"], c(10, 20, 30))
  expect_equal(vals[c(1, 10, 32), "b"], c(1, 2, 3))
  expect_true(all(vals[-c(1, 10, 32), ] == 255))
  # cell 10 is row 2, column 2 (row-major from the top-left)
  expect_equal(unname(terra::cellFromRowCol(r, 2, 2)), 10)
  expect_equal(unname(terra::values(r)[10, "a"]), 20)
  # NA inside a painted cell takes the nodata value too
  rna <- cells_to_raster(c(1L, 2L), cbind(a = c(5, NA)), tpl, nodata = 255, ncol = 8L, nrow = 4L)
  expect_equal(unname(terra::values(rna)[1:2, "a"]), c(5, 255))
  # plain vector, NA fill
  r1 <- cells_to_raster(c(5L, 6L), c(1, 2), tpl, ncol = 8L, nrow = 4L)
  expect_equal(sum(!is.na(terra::values(r1))), 2)
})
test_that("cells_to_raster refuses a template that is not the cell_id raster", {
  bad <- tiny_template(shuffle = T)
  expect_error(cells_to_raster(1:3, c(1, 2, 3), bad, ncol = 8L, nrow = 4L),
               "template\\[cell_id\\]")
  good <- tiny_template()
  expect_error(cells_to_raster(1:3, c(1, 2, 3), good, ncol = 7200L, nrow = 3600L))
  expect_error(cells_to_raster(99L, 1, good, ncol = 8L, nrow = 4L))   # beyond the grid
})

# stac ----
test_that("stac collection: sdm, flower and dimension fields", {
  col <- stac_collection_eov(items = c("all_risk", "fish_share"))
  expect_equal(col$type, "Collection")
  expect_equal(col$id, "aquamaps_eov")
  expect_equal(col$stac_version, "1.1.0")
  expect_equal(col[["sdm:response_type"]], "mixed")
  expect_equal(col[["sdm:method"]], "derived_merge")
  expect_equal(col[["sdm:source_authority"]], "OceanMetrics")
  expect_equal(col[["sdm:temporal_interval"]], "static")
  expect_equal(col[["sdm:value_unit"]], "score")
  expect_equal(as.numeric(col[["sdm:value_range"]]), c(0, 100))
  expect_match(col[["sdm:method_detail"]], "Butchart")
  expect_equal(as.character(col[["sdm:derived_from"]]),
               c("msens-v9-am", "msens-v9-rng_iucn", "worms-2026-07-01",
                 "iucn-redlist-2025", "ioos-mldn-eov-taxonomy"))
  expect_equal(col$license, "CC-BY-NC-3.0")
  expect_true(nzchar(col[["atlas:note"]]))
  expect_equal(col[["atlas:flower"]], list(dimension = "eov", composite = "all"))
  expect_equal(col[["atlas:status"]], "prototype")
  expect_equal(col[["atlas:source_release"]], "MarineSensitivity v9")
  expect_equal(col[["atlas:funders"]], list())
  expect_true(all(c("sci:citation", "sci:publications", "providers", "contacts") %in% names(col)))
  expect_length(col[["sci:publications"]], 5)
  expect_equal(col$contacts[[1]]$name, "Ben Best")
  # dimensions with labels inside each entry
  dims <- col[["cube:dimensions"]]
  expect_equal(as.character(dims$eov$values), eov_levels())
  expect_equal(dims$eov[["atlas:labels"]][["hardCorals"]], "Hard corals")
  expect_equal(dims$eov[["atlas:labels"]][["all"]], "All EOVs (mean)")
  expect_equal(as.character(dims$metric$values), c("risk", "share"))
  expect_equal(dims$metric[["atlas:labels"]][["risk"]], "Risk-weighted share")
  # links
  rel <- vapply(col$links, `[[`, "", "rel")
  expect_true(all(c("root", "parent", "self", "describedby", "agents", "preview",
                    "license", "related", "via") %in% rel))
  expect_equal(sum(rel == "describedby"), 2)
  # README first (non-HTML), the rendered notebook second
  db <- col$links[rel == "describedby"]
  expect_equal(db[[1]]$href, "./README.md"); expect_equal(db[[1]]$type, "text/markdown")
  expect_equal(db[[2]]$type, "text/html"); expect_equal(db[[2]]$title, "Method notebook")
  hrefs <- vapply(col$links, `[[`, "", "href")
  expect_true("https://oceanmetrics.io/workflows/aquamaps_eov.html" %in% hrefs)
  expect_true("./README.md" %in% hrefs)
  expect_equal(sum(rel == "item"), 2)
})

test_that("stac items: 16 items, band index per EOV, labels and atlas keys", {
  grid <- expand.grid(eov = eov_levels(), metric = c("risk", "share"), stringsAsFactors = F)
  expect_equal(nrow(grid), 16L)
  items <- Map(stac_item_eov, grid$eov, grid$metric)
  ids <- vapply(items, `[[`, "", "id")
  expect_equal(length(unique(ids)), 16L)
  expect_true(all(c("all_risk", "seaTurtles_share", "seabirds_risk") %in% ids))
  band <- function(eov, metric = "risk") {
    stac_item_eov(eov, metric)$assets$cog[["atlas:bidx"]]
  }
  expect_equal(
    vapply(eov_levels(), band, 1L, USE.NAMES = F),
    1:8)
  it <- stac_item_eov("marineMammals", "share", n_taxa = 113L, q99 = 0.5)
  # atlas keys: bidx/rescale/colormap on the ASSET, value_range/dims/metric on the properties
  expect_equal(it$assets$cog[["atlas:bidx"]], 5L)
  expect_equal(as.numeric(it$assets$cog[["atlas:rescale"]]), c(0, 100))
  expect_equal(it$assets$cog[["atlas:colormap"]], "viridis")
  expect_equal(it$assets$cog$href, "../score_share.tif")
  expect_equal(it$assets$density$href, "../density_share.tif")
  expect_equal(as.character(it$assets$density$roles), "reference")
  expect_equal(as.character(it$assets$cog$roles), "data")
  expect_equal(it$assets$cog$type, "image/tiff; application=geotiff; profile=cloud-optimized")
  expect_length(it$assets$cog[["raster:bands"]], 9)
  expect_equal(it$assets$cog[["raster:bands"]][[9]]$name, "n_eov")
  for (b in it$assets$cog[["raster:bands"]]) {
    expect_equal(b$data_type, "uint8"); expect_equal(b$nodata, 255L); expect_equal(b$unit, "score")
    expect_true(nzchar(b$description))
  }
  expect_equal(it$assets$cog[["raster:bands"]][[5]]$description, "Marine mammals")
  expect_equal(it$assets$density[["raster:bands"]][[1]]$nodata, "nan")
  expect_equal(it$assets$density[["raster:bands"]][[1]]$data_type, "float32")
  expect_equal(as.numeric(it$bbox), c(-180, -90, 180, 90))
  expect_equal(it$geometry$type, "Polygon")
  expect_equal(it$properties$title, "Marine mammals \u00b7 Share (unweighted)")
  expect_equal(it$properties[["atlas:dims"]], list(eov = "marineMammals", metric = "share"))
  expect_equal(it$properties[["atlas:metric"]], "share")
  expect_equal(as.numeric(it$properties[["atlas:value_range"]]), c(0, 100))
  expect_equal(it$properties[["atlas:n_taxa"]], 113L)
  expect_equal(it$properties[["atlas:q99"]], 0.5)
  expect_null(it$properties$datetime)
  expect_true("datetime" %in% names(it$properties))
  expect_equal(it$properties[["sdm:response_type"]], "mixed")
  expect_equal(it$properties[["sdm:value_unit"]], "score")
  expect_equal(it$collection, "aquamaps_eov")
  # absent petals
  ab <- stac_item_eov("seabirds", "risk")
  expect_true(ab$properties[["atlas:absent"]])
  expect_match(ab$properties$description, "AquaMaps models no seabirds")
  expect_equal(ab$properties[["atlas:absent_note"]], "AquaMaps models no seabirds")
  expect_equal(stac_item_eov("seagrasses", "share")$properties[["atlas:absent_note"]],
               "AquaMaps models no seagrasses")
  expect_equal(as.numeric(ab$properties[["atlas:value_range"]]), c(0, 100))
  expect_null(it$properties[["atlas:absent"]])
  expect_error(stac_item_eov("whales", "risk"))
})

test_that("stac json: single-value arrays stay arrays, null datetime is explicit", {
  f  <- tempfile(fileext = ".json")
  write_stac_json(stac_item_eov("fish", "risk"), f)
  js <- jsonlite::read_json(f)
  expect_null(js$properties$datetime)
  expect_true("datetime" %in% names(js$properties))
  expect_equal(js$assets$cog$roles, list("data"))
  raw <- paste(readLines(f), collapse = "")
  expect_match(raw, '"datetime": null')
  expect_equal(js$assets$cog[["atlas:bidx"]], 2)
  fc <- tempfile(fileext = ".json")
  write_stac_json(stac_collection_eov(), fc)
  jc <- jsonlite::read_json(fc)
  expect_equal(jc[["atlas:flower"]]$composite, "all")
  expect_equal(jc[["atlas:funders"]], list())
})

test_that("stac asset: file size and sha256 multihash", {
  f <- tempfile(fileext = ".txt"); writeLines("hello", f)
  a <- stac_asset_file(f, "./x.txt", "text/plain", "x", "metadata")
  expect_equal(a[["file:size"]], 6)
  expect_match(a[["file:checksum"]], "^1220[0-9a-f]{64}$")
  expect_equal(a[["file:checksum"]], file_multihash(f))
  # sha256("hello\n")
  expect_equal(a[["file:checksum"]],
               "12205891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03")
})

test_that("weights table lists both schemes", {
  expect_equal(eov_weights_table("ohi")$weight, c(0, .2, .4, .6, .8, 1, 1, NA, NA))
  expect_equal(eov_weights_table("mst_v1")$weight, c(.2, .4, .6, .8, 1, 1, 1, NA, NA))
})

# h3 ----
test_that("h3 sql: area-weighted mean per H3 cell", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  ok <- tryCatch({DBI::dbExecute(con, "LOAD h3"); T}, error = \(e) F)
  skip_if(!ok, "h3 extension not installed")
  DBI::dbWriteTable(con, "cc", data.frame(
    cell_id = 1:3, lon = c(10.00, 10.01, 10.02), lat = c(10, 10, 10),
    area_km2 = c(1, 3, 2), score = c(0, 100, NA)))
  out <- DBI::dbGetQuery(con, eov_h3_sql("cc", "score", 5L))
  expect_equal(nrow(out), 1L)
  expect_equal(out$n_cells, 3)
  expect_equal(out$score, 75)       # (0*1 + 100*3) / (1 + 3); the NA cell is ignored
})
