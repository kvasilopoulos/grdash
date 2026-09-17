# grdash

Greek housing price dashboard. `analysis.R` downloads the Bank of Greece house price index workbook and processes it; `prices.xls` is a cached copy of the source data.

## Environment

Dependencies are pinned with `renv` (installer backend: `pak`, enabled via `.Rprofile`).

- Restore: `Rscript -e 'renv::restore()'`
- Add a dependency: `Rscript -e 'renv::install("pkg"); renv::snapshot()'`
- After changing dependencies: `Rscript -e 'renv::snapshot()'`

## Commands

- Run the analysis: `Rscript analysis.R`
