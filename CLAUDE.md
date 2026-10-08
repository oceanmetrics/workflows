# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Quarto notebooks (R) for OceanMetrics.io data processing. There is no R package, no test suite, and no build
step beyond rendering notebooks. Sibling repos live in `../` (notably `../apps`, the Shiny apps that consume
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
