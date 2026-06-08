# ShinyFruit 0.1.2

## Bug fixes

- Removed `purrr::map_dfr()` usage that caused `vctrs` version conflicts and dplyr install prompts during size analysis.
- Fixed spinner hanging when trait analysis errors by debouncing stats and isolating errors from the plot renderer.

## Improvements

- Drupelet counting now uses **boundary/texture segmentation** (color-independent) instead of Lab color thresholds.
- Preview resolution increased to 1200px max for sharper interactive display.
- Drupelet UI simplified to boundary sensitivity, smoothing, and minimum size sliders.

# ShinyFruit 0.1.1

## Improvements

- Redesigned UI with bslib sidebar layout, accordion workflow, and cleaner results panel.
- Faster interactive preview using downscaled images (800px) while keeping full-resolution batch analysis.
- Improved drupelet counting: per-berry assignment, tunable Lab thresholds, minimum size filter, and corrected 0-1 colorspace scale.
- Increased slider debounce (600ms) to reduce lag on large images.

# ShinyFruit 0.1.0

## Bug fixes

- Fixed `fileUploaded` output wiring so Step 1 wizard gating works reliably.
- Fixed `RunBatch` drupelet calculation when size analysis is not selected.
- Replaced Unix-only path regex with `basename()` for Windows compatibility.
- Batch processing now filters to JPEG and PNG files only.
- Added missing `ggplot2` and `shinycssloaders` package dependencies.

## Improvements

- Restored packaged datasets (`cs_labs`, `rgb2rhs`, `variable_desc`) and static assets.
- Refactored monolithic server into step modules with a unified image plot pipeline.
- Added error notifications for image read failures and empty batch folders.
- Restored "Drupelet Count" trait in the analysis UI.
- Added loading spinner on the main image panel.
- Added automated tests and GitHub Actions CI.
- Bumped minimum R version to 4.1.0.
