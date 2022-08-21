##  Resubmission
This is a resubmission. In this version I have:

* Fixed an old url link to the sportyR package
* Fixed an issue with get_player_stats_hr() that was causing an error in the hockey-ref-scrapers.Rmd vignette
* Added checks to ensure urls work before scraping
* Switched to the magrittr pipe and lowered the R verison dependency from 4.1 to 3.5

## Resubmission
This is a resubmission. In this version I have:

* Updated the DESCRIPTION with proper links in angle brackets to the data sources

* Removed unnecessary print() messages to the console in get_game_ids.R, get_rosters.R, and load_pbp.R


## Previous cran-comments

## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
