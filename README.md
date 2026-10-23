# workflows

Notebooks and scripts for OceanMetrics.io. Rendered pages: https://oceanmetrics.io/workflows/

## Notebooks

- `ingest_indicators.qmd`: reads the 36 BioTrack indicator shapefiles, loads them to PostGIS (`oceanmetrics.ds_indicators`) and builds `indicators.pmtiles` for the apps.
- `explore_geoarrow.qmd`: experiments writing and reading `sf` data as GeoParquet locally and on S3.
- `aquamaps_eov.qmd`: AquaMaps EOV share. Extinction-risk weighted share of AquaMaps habitat (clipped to IUCN ranges) per Essential Ocean Variable on the 0.05 degree grid; computes the product, writes it as a STAC collection (COGs, Parquet, method and provenance) and, with `EOV_S3=1`, uploads it. It also writes a cell to species table (`EOV_CELL_SPECIES`, default on in full runs): 5 degree Parquet tiles of the species present in each 0.05 degree cell, plus a species lookup. The rules are in `R/eov_share.R`, tested by `tests/testthat`. After the score COGs and Items are written, the notebook calls the atlas pipeline (`eov_cog_assets()`, i.e. `uv run --project ../atlas/pipeline ../atlas/pipeline/build.py cog-assets <items> score_<metric>.tif`) so the product itself has nearest overviews, the `score_<metric>_3857.tif` copy for the browser and per-band statistics and histograms in the Items; it needs `uv`, GDAL and the `oceanmetrics/atlas` checkout beside this repo (or `ATLAS_DIR`).

## Tests and diagrams

- `Rscript -e 'testthat::test_dir("tests/testthat")'` runs the unit tests of `R/eov_share.R`.
- `libs/render_diagrams.sh` renders `diagrams/*.mmd` to `diagrams/*.svg` (mermaid-cli, `npm install` optional).
