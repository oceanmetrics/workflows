# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Quarto notebooks (R) for OceanMetrics.io data processing. There is no R package and no build step beyond rendering
notebooks. R tests exist only for the `aquamaps_eov` rules (`tests/testthat/test-eov_share.R`); the `om:` block
logic below is tested in `../extract` (pytest). Sibling repos live in `../` (notably `../apps`, the Shiny apps that consume
the data produced here, e.g. `../apps/indicators/app.R`).

## Commands

- Render one notebook: `quarto render ingest_indicators.qmd` (from the shell) or
  `quarto::quarto_render("ingest_indicators.qmd")` (from R). Use the shell form to iterate on render errors.
- Render the whole site: `quarto render`.
- Output goes to `_output/` (set in `_quarto.yml`). **`_output/` is committed**: the GitHub Actions workflow
  (`.github/workflows/jekyll-gh-pages.yml`) builds GitHub Pages with Jekyll straight from `_output/` on every
  push to `main`, so a notebook change is not published until its re-rendered HTML (and `*_files/` folder) is
  committed too. `_output/README.md` is a Jekyll index that lists the rendered pages.
- `_environment` sets `home_directory='/tmp'` and is git-ignored.

## The `om:` block, run order, DAG and index

Every notebook carries an `om:` block in its YAML front matter (schema:
`../platform/packages/catalog/schemas/om-notebook.schema.json`; example in `../extract/README.md`): `target_name`,
`workflow_type` (ingest, extract, publish, release, reference, explore), `dependency` (target_names that run first),
`output` (the manifest that marks the target done), `products`, `schedule`, `status` (active, archived, draft) and a
one-sentence `description`. No `[auto]` dependencies. The run order, the DAG and the index page are generated from the
blocks, never edited by hand. After adding or changing a block (not a render), from this directory:

```sh
uv run --project ../extract om-extract notebooks check .                                   # exit 1 on schema, duplicate, unknown-dependency or cycle errors
uv run --project ../extract om-extract notebooks index . --title "Ocean Metrics workflows"  # writes index.qmd, _om/order.json, _om/dag.mmd
```

Commit `index.qmd` and `_om/` with the notebooks. `index.qmd` is rendered with the site (`quarto render`) into
`_output/index.html`; `_om/order.json` is the run order (archived notebooks excluded). An active notebook may not depend
on an archived one.

## Notebooks

- `ingest_indicators.qmd`: the main pipeline. Reads 36 indicator shapefiles (named
  `{indicator}_{type}_{season}_{group}.shp`) from Google Drive
  (`~/My Drive/projects/oceanmetrics/data/raw/...`), pivots them to long format keyed by `FID`, builds layer
  keys and descriptions from a hand-edited lookup CSV (`layer_lookup.csv`, written once and then edited
  manually, so it is not regenerated if present), and writes:
  - PostGIS table `oceanmetrics.ds_indicators` plus `ds_indicators_lyrs` in database `msens` (host `localhost`
    on the laptop, `postgis` on the Linux server; password read from `~/My Drive/private/` or
    `/share/private/`). Writes are skipped if the table already exists.
  - `indicators.pmtiles` via `tippecanoe` then `pmtiles convert` (both from Homebrew), uploaded with `aws.s3`
    to the public bucket `oceanmetrics.io-public` (us-east-1). AWS keys come from a CSV in
    `~/My Drive/private/`.

  Intermediate files (`fid.geojson`, `data_long.csv`, `data_wide.csv`, `layers.csv`, the `.mbtiles` and
  `.pmtiles`) are written next to the raw data on Google Drive, not into this repo. A `stop_eval` chunk
  (`knitr::opts_chunk$set(eval = F)`) sits just after the `mapgl` chunk, so a normal render stops before the
  tippecanoe, PMTiles and S3 steps (and the WDPA section at the end). To rebuild tiles, run those chunks by
  hand. The S3 upload is skipped when the object already exists, so replacing `indicators.pmtiles` means
  deleting or overwriting it explicitly.
- `explore_geoarrow.qmd`: experiments writing/reading `sf` data as GeoParquet locally and on S3, including the
  bucket's public-read policy and CORS config. `data/geoarrow/` holds its small sample files.

## Tile serving: current vs. legacy

The apps now read `https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/indicators.pmtiles` through
`mapgl::add_pmtiles_source()`. The older path, `pg_tileserv` at
`https://api.marinesensitivity.org/tilejson?table=oceanmetrics.ds_indicators`, is still referenced in the
`mapgl` chunk of `ingest_indicators.qmd`. The two differ: in the PMTiles the source layer is `indicators`, the
id field is `FID`, and columns use long names like `Ind_WeightedRichShape_summer_All_ecrg_rc` rather than the
short layer keys (e.g. `al_er_su_wr`) stored in `ds_indicators_lyrs` (see `dev/claude_prompts.md`,
2026-03-27).

## History worth knowing

Observable JS (`{ojs}`) chunks that read GeoParquet from S3 with DuckDB/geoarrow-js were tried and abandoned
for spatial data (commits `ff972c7`, `fb42495`); prefer R chunks for mapping. `dev/claude_prompts.md` records
earlier prompts and outcomes for this repo.

## aquamaps_eov.qmd (AquaMaps EOV share product)

- Pipeline and documentation in one notebook. Reads the MarineSensitivity v9 files in `MSENS_DERIVED`
  (default `~/_big/msens/derived`), computes the per-cell, per-EOV share and risk (Butchart/OHI weights), scores,
  composite and an H3 twin, and stages the product in `~/_big/oceanmetrics/aquamaps_eov/<ver>/` (never in git):
  COGs `score_*.tif`/`density_*.tif`, `cells/eov=<g>/data.parquet`, `h3_r5.parquet`, `eov_taxa.parquet`,
  `eov_summary.csv`, `method.json`, `provenance.json`, README/AGENTS, STAC `collection.json` + 16 `items/`.
  The DuckDB work file and checkpoints are in `~/_big/oceanmetrics/aquamaps_eov/work_<ver>/`.
- Flags (environment): `EOV_TEST_N=<n>` first n species per EOV, writes to `.../test`, never uploads;
  `EOV_REDO=1` recompute checkpoints; `EOV_S3=1` upload through `../platform/scripts/om-publish.sh` (both stores, an explicit file list, never a delete; `EOV_S3=dry` lists only; the only side effect);
  `EOV_BATCH_ROWS=80e6` rows per batch of species (the pass over the AquaMaps rows is batched; each batch writes
  `work_<ver>/partials_*/totals|cell_eov/batch_<id>.parquet`, finished batches are skipped on a re-render, `EOV_REDO=1` deletes them);
  `EOV_CELL_SPECIES=1|0` the cell -> species table (5 degree Parquet tiles `cell_species/tile=<t>/data_0.parquet` of
  cell_id, aphia_id, suit, plus `species.parquet`; default on in full mode, off in test mode; stage 1 runs inside the
  batch loop, stage 2 per band in the `cell_species` chunk, markers/partials in `work_<ver>/cs_partials_*`);
  `EOV_WEIGHTS=ohi|mst_v1`; `EOV_Q=0.99`; `EOV_VERIFY_N=20` COG spot-check against the published
  `aquamaps_native` COGs (0 skips); `BUILD_MEMORY_GB`, `BUILD_THREADS`, `BUILD_TMP_GB` (spill cap, default 100).
- The `stac` chunk writes the Items, then runs `eov_cog_assets()` per score COG (needs `uv` and `../atlas`, or `ATLAS_DIR`)
  BEFORE the collection reads the COGs' `file:size`/`file:checksum`: the command rewrites `score_<metric>.tif` in place.
- Every scientific rule is a function in `R/eov_share.R` (seeds, WoRMS subtree CTE, Red List weights, share SQL,
  score, composite, `cells_to_raster`, STAC builders, `eov_cog_write` = the one COG writer, nearest overviews for every GeoTIFF, `eov_cog_assets` = the atlas pipeline's `cog-assets` step); `tests/testthat/test-eov_share.R` asserts them on synthetic
  fixtures. Change a rule there and in its test in the same commit; run
  `Rscript -e 'testthat::test_dir("tests/testthat")'` before rendering.
- Diagrams: edit `diagrams/*.mmd`, run `libs/render_diagrams.sh` (uses `node_modules/.bin/mmdc` or `mmdc` on the
  PATH), commit the `.svg`. The zoom viewer is `libs/diagram.js`, included by `libs/diagram-head.html`.
- Quick check: `EOV_TEST_N=40 EOV_VERIFY_N=3 quarto render aquamaps_eov.qmd`. Commit the re-rendered
  `_output/aquamaps_eov.html` and `_output/aquamaps_eov_files/` (Pages builds from `_output/`).
