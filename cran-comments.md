## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for CRAN Maintainers

This is a release containing bug fixes and minor feature improvements to the `wrappedtools` package. All updates maintain backward compatibility with existing functionality.

Key changes in this version (0.9.10):
- Updated `compare_n_qualvars` to dplyr 1.1.0 syntax
- Updated `formatP` and `markSign` to use `readr::parse_number`
- Added CI options for mean and median in `compare_n_numvars`
- Bug fixes in string handling and numeric formatting

The package has been tested on R version 4.6.1 and passes R CMD check with no errors, warnings, or notes.
