## Notes

* we changed the URLs in the README to the canonical form

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## Package changes

 * removed unnecessary example data
 * backwards compatible improvement to `EventStudy()` to streamline first-differencing and allow for gaps in panel
 * increased the tolerance to 1e-4 in the solver for finding the smoothest path in order to fix non-convergence error
