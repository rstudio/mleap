## Resubmission

This is a resubmission. Changes:

- `install_mleap()` now instructs Maven to use a temporary directory to cache download files, so that no persistent folders are created in the user's home directory as a side effect.
- Remove logic from unit tests to remove Ivy and Maven cache directories.

## Test environments
* local OS X install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
