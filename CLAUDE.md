# grdash

Greek housing price dashboard. `analysis.R` downloads the Bank of Greece house price index workbook and processes it; `prices.xls` is a cached copy of the source data.

## Environment

- R version: managed with `rig` (currently 4.6.1 - `rig default 4.6.1`)
- Dependencies: managed with `rv` (`rproject.toml` + lockfile), not renv
  - Setup: `rv sync`
  - Add a dependency: `rv add <pkg>`
- Format: `air format .` (check only: `air format --check .`)
- Lint: `jarl check .` (autofix: `jarl check . --fix`)

### Rules
- Never use `install.packages()` or `renv::*` - all dependency changes go through `rv add`/`rv remove`.
- Run `air format .` then `jarl check .` before committing.
- No `# nolint` comments - jarl uses `# jarl-ignore <rule>: <reason>` on the line before the flagged code instead.

## Commands

- Run the analysis: `Rscript analysis.R`
