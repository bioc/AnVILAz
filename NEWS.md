# AnVILAz 1.6.0

## Bug fixes and minor improvements

* Sanitize error messages in `.az_do()` to prevent sensitive information in
  URLs.
* Added unit tests for utility functions.

# AnVILAz 1.4.0

* Minimum R version requirement updated to 4.5.0.

# AnVILAz 1.2.0

* Maintenance release following Bioconductor release cycle.

# AnVILAz 1.0.0

* Initial release to Bioconductor.
* Added `has_avworkspace()` method for `azure` platform to check if the AnVIL
  environment is set up correctly.
* `az_health_check()` now checks if required apps (e.g., CBAS, WDS) are running in
  the workspace.
* `avcopy()` now includes a `dry` parameter (default `TRUE`) for dry runs.
* Un-exported internal `az_copy_*_storage` helpers.
* Improved `azcopyStatus` handling and error reporting.

# AnVILAz 0.99.7

* Initial Bioconductor submission.
