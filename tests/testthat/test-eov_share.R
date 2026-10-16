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
# fixture: 6 cells with unequal areas, 4 species, 5 models, in two batches
#   cell      1  2  3  4  5  6
#   area      1  2  1  4  2  1
# A (101, fish, VU .4): one model, v 10 20 30 40 on c1-c4; range c2-c4 (c1 clipped out)
# B (102, fish, DD NULL): no range, v 50 50 100 on c1 c5 c6
# C (103, hardCorals, CR .8): two models, c1:10 c2:30 | c2:60 c3:20 -> max 10 60 20; no range
# D (104, fish, EN .6): v 40 40 on c5 c6, range c1 c2 only: zero overlap -> excluded
# batch 1 = C (has the multi-model species), batch 2 = A, B, D
share_fixture <- function(con, dir = tempfile()) {
  dir.create(dir)
  wr <- function(df, name) {
    DBI::dbWriteTable(con, "tmp_w", df, overwrite = T)
    f <- file.path(dir, paste0(name, ".parquet"))
    DBI::dbExecute(con, glue::glue("COPY tmp_w TO '{f}' (FORMAT parquet)"))
    f
  }
  am <- list(
    A  = wr(data.frame(mdl_key = "am|A", cell_id = 1:4, val = c(10, 20, 30, 40)), "A"),
    B  = wr(data.frame(mdl_key = "am|B", cell_id = c(1L, 5L, 6L), val = c(50, 50, 100)), "B"),
    C1 = wr(data.frame(mdl_key = "am|C1", cell_id = 1:2, val = c(10, 30)), "C1"),
    C2 = wr(data.frame(mdl_key = "am|C2", cell_id = 2:3, val = c(60, 20)), "C2"),
    D  = wr(data.frame(mdl_key = "am|D", cell_id = 5:6, val = c(40, 40)), "D"))
  rng <- list(
    A = wr(data.frame(mdl_key = "rng|1", cell_id = c(2L, 3L, 4L, 4L)), "rA"),   # duplicate row
    D = wr(data.frame(mdl_key = "rng|2", cell_id = c(1L, 2L)), "rD"))
  DBI::dbWriteTable(con, "map", data.frame(
    mdl_key   = c("am|A", "am|B", "am|C1", "am|C2", "am|D"),
    aphia_id  = c(101L, 102L, 103L, 103L, 104L),
    n_models  = c(1L, 1L, 2L, 2L, 1L),
    has_range = c(TRUE, FALSE, FALSE, FALSE, TRUE)))
  DBI::dbWriteTable(con, "rng_map", data.frame(
    mdl_key = c("rng|1", "rng|2"), aphia_id = c(101L, 104L)))
  DBI::dbWriteTable(con, "area", data.frame(cell_id = 1:6, area_km2 = c(1, 2, 1, 4, 2, 1)))
  DBI::dbWriteTable(con, "eov", data.frame(
    eov = c("fish", "fish", "hardCorals", "fish"), aphia_id = c(101L, 102L, 103L, 104L)))
  DBI::dbWriteTable(con, "wt", data.frame(aphia_id = 101:104, weight = c(0.4, NA, 0.8, 0.6)))
  part <- file.path(dir, "partials")
  dir.create(file.path(part, "totals"), recursive = T)
  dir.create(file.path(part, "cell_eov"), recursive = T)
  dir.create(file.path(dir, "cs"))                       # the notebook creates dir_cs up front
  batches <- list(
    list(id = 1L, eov = "hardCorals", am = c(am$C1, am$C2), rng = character(), multi = TRUE),
    list(id = 2L, eov = "fish", am = c(am$A, am$B, am$D), rng = c(rng$A, rng$D), multi = FALSE))
  sqls <- list()
  for (b in batches) {
    st <- eov_batch_sql(
      batch_id = b$id, am_files = b$am, rng_files = b$rng, has_multi = b$multi,
      map_tbl = "map", area_tbl = "area", weight_tbl = "wt", eov_tbl = "eov",
      dir_partials = part, rng_map_tbl = "rng_map", eov = b$eov, dir_cs = file.path(dir, "cs"))
    for (s in st) DBI::dbExecute(con, s)
    sqls[[b$id]] <- st
  }
  for (s in eov_merge_sql(part, "map")) DBI::dbExecute(con, s)
  list(sql = sqls, dir = part, cs = file.path(dir, "cs"), root = dir)
}

test_that("batch sql: statements, partial files and temp tables dropped", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  fx <- share_fixture(con)
  expect_named(fx$sql[[2]], c("rng_b", "clip_b", "tot_b", "cell_eov", "cell_species", "totals", "drop"))
  expect_true(all(file.exists(file.path(fx$dir, c("totals", "cell_eov"), "batch_00001.parquet"))))
  expect_true(all(file.exists(file.path(fx$dir, c("totals", "cell_eov"), "batch_00002.parquet"))))
  expect_false(any(c("clip_b", "tot_b", "rng_b") %in% DBI::dbListTables(con)))
})

test_that("batch sql (a): range clip keeps only cells inside the range, exact shares", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  fx <- share_fixture(con)
  # species A alone: fish partial restricted to A through the weights (B DD, D excluded)
  ce <- DBI::dbGetQuery(con, "SELECT * FROM eov_cell_eov WHERE eov = 'fish' ORDER BY cell_id")
  expect_equal(ce$cell_id[ce$cell_id %in% 2:4], 2:4)              # c1 of A clipped out
  a_share <- ce$share[ce$cell_id %in% 2:4]
  expect_equal(a_share, c(40, 30, 160) / 230)                     # u*A = 20*2, 30*1, 40*4
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total WHERE aphia_id = 101")
  expect_equal(t$total_suit_area, 230)
  expect_equal(t$n_cells, 3)
  expect_true(t$has_range); expect_false(t$excluded)
})

test_that("batch sql (b): species without range uses the full distribution", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total WHERE aphia_id = 102")
  expect_equal(t$total_suit_area, 250)                            # 50*1 + 50*2 + 100*1
  expect_equal(t$n_cells, 3)
  expect_false(t$has_range)
  ce <- DBI::dbGetQuery(con, "SELECT * FROM eov_cell_eov WHERE eov = 'fish' ORDER BY cell_id")
  expect_equal(ce$share[ce$cell_id %in% c(5L, 6L)], c(.4, .4))
})

test_that("batch sql (c): two models on an overlapping cell take the max", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total WHERE aphia_id = 103")
  expect_equal(t$total_suit_area, 150)                            # max 10, 60, 20 times 1, 2, 1
  expect_equal(t$n_models, 2)
  ce <- DBI::dbGetQuery(con, "SELECT * FROM eov_cell_eov WHERE eov = 'hardCorals' ORDER BY cell_id")
  expect_equal(ce$share, c(10, 120, 20) / 150)                    # c2: max(30, 60)
})

test_that("batch sql (d): species whose cells never overlap its range is excluded", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total WHERE aphia_id = 104")
  expect_equal(t$total_suit_area, 0)
  expect_equal(t$n_cells, 0)
  expect_true(t$excluded)
  # excluded species adds nothing: fish cells 5, 6 hold B only
  ce <- DBI::dbGetQuery(con, "SELECT * FROM eov_cell_eov WHERE eov = 'fish' ORDER BY cell_id")
  expect_equal(ce$share[ce$cell_id %in% 5:6], c(.4, .4))
})

test_that("batch sql: per cell x EOV share and risk, NULL weight excluded from risk only", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  ce <- DBI::dbGetQuery(con, "SELECT * FROM eov_cell_eov ORDER BY eov, cell_id")
  fish <- ce[ce$eov == "fish", ]
  hc   <- ce[ce$eov == "hardCorals", ]
  expect_equal(fish$cell_id, c(1L, 2L, 3L, 4L, 5L, 6L))
  expect_equal(fish$share, c(.2, 40/230, 30/230, 160/230, .4, .4))
  expect_equal(fish$risk, c(0, .4 * 40/230, .4 * 30/230, .4 * 160/230, 0, 0))
  expect_false(anyNA(fish$risk))
  expect_equal(hc$cell_id, 1:3)
  expect_equal(hc$share, c(10, 120, 20) / 150)
  expect_equal(hc$risk, .8 * c(10, 120, 20) / 150)
  expect_equal(sum(fish$share), 2)
  expect_equal(sum(hc$share), 1)
  expect_equal(sum(fish$risk), 0.4)
  tot <- DBI::dbGetQuery(con, "SELECT aphia_id, excluded FROM eov_sp_total ORDER BY aphia_id")
  expect_equal(tot$excluded, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("batch sql: a species in two EOVs counts once per EOV batch; no totals row means excluded", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  share_fixture(con)
  # species 102 also in hardCorals: the eov argument keeps each batch to its own EOV
  DBI::dbWriteTable(con, "eov2", data.frame(eov = c("fish", "other"), aphia_id = c(102L, 102L)))
  dir <- tempfile(); dir.create(file.path(dir, "totals"), recursive = T)
  dir.create(file.path(dir, "cell_eov"))
  am <- tempfile(fileext = ".parquet")
  DBI::dbExecute(con, glue::glue("COPY (SELECT 'am|B' AS mdl_key, 1 AS cell_id, 50.0 AS val) TO '{am}' (FORMAT parquet)"))
  st <- eov_batch_sql(1L, am, character(), FALSE, "map", "area", "wt", "eov2", dir, "rng_map", eov = "fish")
  for (s in st) DBI::dbExecute(con, s)
  out <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM read_parquet('{dir}/cell_eov/*.parquet')"))
  expect_equal(out$eov, "fish")
  expect_equal(out$share, 1)
  # a species of the map that no batch processed has no totals row
  m <- eov_merge_sql(dir, "map")
  DBI::dbExecute(con, m$sp_total)
  t <- DBI::dbGetQuery(con, "SELECT * FROM eov_sp_total ORDER BY aphia_id")
  expect_equal(t$excluded, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(t$n_cells, c(0, 1, 0, 0))
})

test_that("batches: never split a species, never mix EOVs, respect the cap, oversize alone", {
  sp <- data.frame(
    eov      = c("fish", "fish", "fish", "fish", "fish", "corals", "corals", "corals"),
    aphia_id = c(5L, 1L, 2L, 3L, 4L, 11L, 10L, 12L),
    n_rows   = c(30, 40, 50, 500, 20, 10, 10, 10))
  b <- eov_make_batches(sp, max_rows = 100)
  expect_equal(nrow(b), nrow(sp))
  expect_equal(b$eov, c(rep("corals", 3), rep("fish", 5)))          # ordered by eov, aphia_id
  expect_equal(b$aphia_id[b$eov == "fish"], 1:5)
  # no species is split across batches; each batch has a single EOV
  expect_false(anyDuplicated(b$aphia_id[b$eov == "fish"]) > 0)
  expect_true(all(tapply(b$eov, b$batch_id, \(x) length(unique(x))) == 1))
  tot <- tapply(b$n_rows, b$batch_id, sum)
  big <- b$batch_id[b$aphia_id == 3 & b$eov == "fish"]
  expect_equal(unname(tot[as.character(big)]), 500)                  # oversized species alone
  expect_true(all(tot[names(tot) != as.character(big)] <= 100))
  expect_equal(b$batch_id, c(1, 1, 1, 2, 2, 3, 4, 4))                # corals | 40+50 | 500 | 20+30
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

# cell -> species table ----
test_that("grid math: lon/lat <-> cell_id <-> tile", {
  expect_equal(cell_of_lonlat(-86.73, 21.62), 9844266L)
  expect_equal(tile_of_cell(9844266), 954L)
  ll <- lonlat_of_cell(9844266)
  expect_equal(ll$lon, -86.725); expect_equal(ll$lat, 21.625)
  expect_equal(cell_of_lonlat(ll$lon, ll$lat), 9844266L)
  # corners and the outer edge
  expect_equal(cell_of_lonlat(-180, 90), 1L)
  expect_equal(cell_of_lonlat(180, -90), 25920000L)
  expect_equal(tile_of_cell(1), 0L)
  expect_equal(tile_of_cell(25920000), 2591L)
  # round trip of random cells, and a tile holds at most 100 x 100 cells
  set.seed(3)
  id <- sample.int(25920000, 5000)
  ll <- lonlat_of_cell(id)
  expect_equal(cell_of_lonlat(ll$lon, ll$lat), as.integer(id))
  expect_true(all(table(tile_of_cell(1:25920000)) == 10000))
  expect_equal(unname(tile_of_cell(c(1, 100, 101, 720001))), c(0L, 0L, 1L, 72L))
})

read_tiles <- function(con, dir_out) {
  DBI::dbGetQuery(con, glue::glue("SELECT * FROM read_parquet('{dir_out}/tile=*/data_0.parquet', hive_partitioning = false)"))
}
run_stage2 <- function(con, dir_cs, dir_out, bands, row_group_size = 32768L) {
  for (b in bands) {
    st <- cell_species_stage2_sql(b, dir_cs, dir_out, row_group_size)
    dir.create(dirname(dirname(cell_species_tile_path(dir_out, b * 72L))), showWarnings = F)
    DBI::dbExecute(con, st$load)
    for (cc in DBI::dbGetQuery(con, st$cols)$c) {
      t <- b * 72L + cc
      dir.create(dirname(cell_species_tile_path(dir_out, t)), recursive = T, showWarnings = F)
      DBI::dbExecute(con, st$tile(cc))
      cell_species_commit(dir_out, t)
    }
    DBI::dbExecute(con, st$drop)
  }
}

test_that("cell species: stage 1 + 2 on the fixture give sorted tiles, suit = round(max v), clipped cells absent", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  fx  <- share_fixture(con)
  expect_true(file.exists(file.path(fx$cs, "batch_00001", "band=0", "data_0.parquet")))
  out <- file.path(fx$root, "cell_species"); dir.create(out)
  run_stage2(con, fx$cs, out, 0L)
  d <- read_tiles(con, out)
  expect_equal(names(d), c("cell_id", "aphia_id", "suit"))
  # tile 0 holds all 6 cells; no tmp files left over
  expect_equal(list.files(out, recursive = T), "tile=0/data_0.parquet")
  expect_equal(nrow(d), 9L)                               # A 3 + B 3 + C 3; D excluded
  # sorted by cell_id, then suit DESC
  expect_equal(d$cell_id, c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 5L, 6L))
  expect_equal(d$aphia_id, c(102L, 103L, 103L, 101L, 101L, 103L, 101L, 102L, 102L))
  expect_equal(d$suit, c(50L, 10L, 60L, 20L, 30L, 20L, 40L, 50L, 100L))   # C c2 = max(30, 60)
  # A (range c2-c4) has no row in the clipped-out cell 1; D has none at all
  expect_false(any(d$cell_id == 1L & d$aphia_id == 101L))
  expect_false(any(d$aphia_id == 104L))
  types <- DBI::dbGetQuery(con, glue::glue("DESCRIBE SELECT * FROM read_parquet('{out}/tile=0/data_0.parquet')"))
  expect_equal(types$column_type, c("INTEGER", "INTEGER", "UTINYINT"))
  expect_silent(cell_species_check(cell_species_tile_path(out, 0L), con))
})

test_that("cell species: rounding, several bands and tiles, one row group per cell range", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  dir <- tempfile(); dir.create(dir)
  # cells in band 0 tile 0, band 0 tile 1, band 1 tile 72
  DBI::dbWriteTable(con, "clip_t", data.frame(
    aphia_id = c(1L, 2L, 1L, 3L), cell_id = c(1L, 1L, 105L, 720001L),
    v = c(10.4, 10.6, 99.5, 1.0)))
  dir.create(file.path(dir, "cs"))
  DBI::dbExecute(con, cell_species_stage1_sql(7L, file.path(dir, "cs"), "clip_t"))
  expect_setequal(list.files(file.path(dir, "cs", "batch_00007")), c("band=0", "band=1"))
  out <- file.path(dir, "cell_species"); dir.create(out)
  run_stage2(con, file.path(dir, "cs"), out, 0:1)
  expect_setequal(list.files(out), c("tile=0", "tile=1", "tile=72"))
  d <- read_tiles(con, out)
  expect_equal(d$suit[order(d$cell_id, d$aphia_id)], c(10L, 11L, 100L, 1L))   # round(10.4), round(10.6), round(99.5)
  expect_equal(tile_of_cell(sort(unique(d$cell_id))), c(0L, 1L, 72L))
})

test_that("cell species check: passes on a sorted file, fails on a shuffled one", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  DBI::dbWriteTable(con, "tt", data.frame(
    cell_id = rep(1:2000, each = 3), aphia_id = rep(1:3, 2000), suit = 50L))
  good <- tempfile(fileext = ".parquet"); bad <- tempfile(fileext = ".parquet")
  DBI::dbExecute(con, glue::glue("COPY (SELECT * FROM tt ORDER BY cell_id, aphia_id) TO '{good}' (FORMAT parquet, ROW_GROUP_SIZE 2048)"))
  DBI::dbExecute(con, glue::glue("COPY (SELECT * FROM tt ORDER BY hash(cell_id * 7 + aphia_id)) TO '{bad}' (FORMAT parquet, ROW_GROUP_SIZE 2048)"))
  rg <- cell_species_check(good, con)
  expect_equal(nrow(rg), 3L)
  expect_true(all(rg$cell_min <= rg$cell_max))
  expect_error(cell_species_check(bad, con), "overlap or are not sorted")
  # a cell straddling two row groups (2048 is not a multiple of 3) is fine
  expect_true(rg$cell_max[1] == rg$cell_min[2])
})

test_that("species lookup: preferred English name, registry sp_key, fallback, links", {
  con <- new_con(); on.exit(DBI::dbDisconnect(con, shutdown = T))
  DBI::dbWriteTable(con, "tx", data.frame(
    aphia_id = c(1L, 2L, 3L), scientific_name = c("Alpha beta", "Gamma delta", "Eps zeta"),
    eov = c("fish", "fish", "seaTurtles"), sp_keys = c("Fis-1;Fis-2", "Fis-3", "Rep-9"),
    total_suit_area = c(10, 20, 30)))
  DBI::dbWriteTable(con, "vern", data.frame(
    taxonID = c(1L, 1L, 1L, 2L, 3L),
    vernacularName = c("zzz name", "Preferred name", "Nom", "Gamma fish", "Turtle"),
    language = c("ENG", "ENG", "FRA", "ENG", "ENG"),
    isPreferredName = c(0, 1, 1, 0, NA)))
  DBI::dbWriteTable(con, "reg", data.frame(aphia_id = c(1L, 1L, 2L), sp_key = c("Fis-2", "Fis-1", NA)))
  DBI::dbWriteTable(con, "iu", data.frame(
    taxon_scientific_name = c("ALPHA BETA", "Alpha beta", "Eps zeta"), sis_taxon_id = c(10L, 11L, 12L),
    url = c("u-old", "u-new", "u-eps"), latest = c(TRUE, TRUE, FALSE), year_published = c(2020L, 2024L, 2020L)))
  d <- DBI::dbGetQuery(con, species_lookup_sql("tx", "vern", "reg", "iu"))
  expect_equal(nrow(d), 3L)
  expect_equal(d$common_name, c("Preferred name", "Gamma fish", "Turtle"))
  expect_equal(d$sp_key, c("Fis-1", "Fis-3", "Rep-9"))     # registry (min) / first of sp_keys / first of sp_keys
  expect_equal(d$sis_taxon_id, c(11L, NA, NA))             # latest assessment by name only
  expect_equal(d$iucn_url, c("u-new", NA, NA))
  expect_equal(d$worms_url[1], "https://www.marinespecies.org/aphia.php?p=taxdetails&id=1")
  expect_equal(d$aquamaps_url[3], "https://www.aquamaps.org/preMap2.php?cache=1&SpecID=Rep-9")
  expect_true(all(c("total_suit_area", "eov", "sp_keys") %in% names(d)))
})
