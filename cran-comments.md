## Notes

Version 1.2.1 is a patch release for compatibility with estimatr 2.0.0. No estimation code changed.

## R CMD check results

Duration: 1m 16s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Package changes

 * Preserved the matrix-valued smoothest path when estimatr 2.0.0 returns a tibble from `tidy()`.
 * Updated the one-way fixed-effects tests to work with the `felevels` names from both old estimatr versions and the new estimatr 2.0.0.
