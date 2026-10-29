---
title: grdash
type: project
status: active
created: 2026-10-27
updated: 2026-10-29
sources: []
scope: housing-observatory
tags: [project, backend]
verified_commit: e4d0a62b24b92fbf790bcf8253d91f1fade4a6d1
files: [analysis.R, README.Rmd, prices.xls, rproject.toml]
---
# grdash

Greek house price indices: downloads the Bank of Greece property price workbook and reshapes it into tidy data, the base for a Greek dashboard.

Part of [[concepts/housing-observatory]].

## Stack
R. Source: the Bank of Greece residential and commercial property price indices workbook. `prices.xls` is a cached copy. R 4.6.1 via `rig`; dependencies with `rv` (`rproject.toml` + `rv.lock`), never `install.packages()` or `renv`; format with `air format .`, lint with `jarl check .` before committing (repo `CLAUDE.md`).

## Run and test
`Rscript analysis.R`. In R: `source("analysis.R"); get_hp()` returns a tibble with columns `date`, `name` (for example "Urban"), `value` and `base`, quarterly from 1997 (550 rows when the README was knitted).

## Layout
`analysis.R` (download and tidy), `README.Rmd` (generates `README.md`), `prices.xls`, `rproject.toml`, `air.toml`, `jarl.toml`.

## Unknowns
- No Shiny app or deployment exists in the repo yet, despite the "dashboard" name (unverified whether one is planned).

Related: [[concepts/housing-observatory]], [[conventions]], [[inbox/grdash]].
