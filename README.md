# workflows

Notebooks and scripts for OceanMetrics.io. Rendered pages: https://oceanmetrics.io/workflows/

## Notebooks

- `ingest_indicators.qmd`: reads the 36 BioTrack indicator shapefiles, loads them to PostGIS (`oceanmetrics.ds_indicators`) and builds `indicators.pmtiles` for the apps.
- `explore_geoarrow.qmd`: experiments writing and reading `sf` data as GeoParquet locally and on S3.
- `aquamaps_eov.qmd`: AquaMaps EOV share. Extinction-risk weighted share of AquaMaps habitat (clipped to IUCN ranges) per Essential Ocean Variable on the 0.05 degree grid; computes the product, writes it as a STAC collection (COGs, Parquet, method and provenance) and, with `EOV_S3=1`, uploads it. The rules are in `R/eov_share.R`, tested by `tests/testthat`.

## Tests and diagrams

- `Rscript -e 'testthat::test_dir("tests/testthat")'` runs the unit tests of `R/eov_share.R`.
- `libs/render_diagrams.sh` renders `diagrams/*.mmd` to `diagrams/*.svg` (mermaid-cli, `npm install` optional).
