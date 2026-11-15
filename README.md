# institute_shiny

Platform Engagement Dashboard (R Shiny), deployed on Posit Connect from GitHub.

## Adding or updating an R package

Connect restarts the app from `manifest.json`. If a package is used in the code but missing from `renv.lock` / `manifest.json`, the restart fails. After adding a package:

1. Install it (e.g. `install.packages("pkg")`) and use it in the code.
2. Update the lock file:
   ```r
   renv::status()                      # should list only the new package
   renv::snapshot(force = TRUE)        # force = TRUE for now, see note below
   ```
3. Rebuild the manifest (lists only the app's own files, so `.env` is left out):
   ```r
   files <- c("app.R", "global.R", list.files("R", pattern = "[.]R$", full.names = TRUE))
   rsconnect::writeManifest(appPrimaryDoc = "app.R", appFiles = files)
   ```
4. Check `git diff renv.lock manifest.json`. Only the new package (and its new dependencies) should be added, with no unrelated version changes.
5. Commit and push. Connect restarts the app.

Rebuild the manifest (step 3) after adding or renaming any `.R` file too, since it records file checksums.

**Why `force = TRUE`:** `furrr` and `anytime` (pulled in by `tidyquant`) require newer versions of `purrr`, `rlang`, `vctrs` and `Rcpp` than are locked, so a plain `renv::snapshot()` aborts. This mismatch predates the pool work and could go away if `tidyquant` is dropped.

**Careful:** installing packages can upgrade core packages (e.g. installing `rsconnect` once upgraded `rlang`). Run `renv::status()` afterwards; if it reports a version mismatch, restore the locked version with `renv::restore(packages = "pkg")`.

## Configuration

Set in `.env` (not committed) locally, or as environment variables on Connect:
`DB_NAME`, `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`, `APP_PASSWORD`, and optionally `AUTH_SECRET` (signs "remember me" login cookies; defaults to `APP_PASSWORD`).
