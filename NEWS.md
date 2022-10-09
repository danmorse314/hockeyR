# hockeyR 1.3.0

* fixed `calculate_individual` to ensure players with 0 shot events recorded still show up in final stats data (affects individual game stats mostly)
* removed `polite` dependency, shifted to entirely `rvest` scraping
* added checks to fix failing [hockey-reference](https://www.hockey-reference.com) scrapers
* removed lingering native pipe from readme and `scrape_game`
* fixed incorrect color code in `team_logos_colors` ([#6](https://github.com/danmorse314/hockeyR/issues/6))

# hockeyR 1.2.0

## New functions added
* `calculate_toi` calculates the time on ice for all skaters in a supplied play-by-play set
* `calculate_on_ice` calculates player on-ice statistics for all skaters in a supplied play-by-play set
* `calculate_individual` calculates player individual statistics for all skaters in a supplied play-by-play set
* `get_standings` pulls standings and team stats for given seasons from [NHL.com](https://www.nhl.com/)


## Function updates
* `get_game_ids` now returns column for scheduled start time for each game
* `%not_in%` operator now defined globally within package

## Bug fixes
* `get_draft_class` now works for classes prior to 1983 (first year player IDs existed)

# hockeyR 1.1.0

## New addition:
* Play-by-play data loaded through `load_pbp` includes new column for expected goals
* Details on & code to create the hockeyR expected goals model can be found [here](https://github.com/danmorse314/hockeyR-models)
* The `scrape_game` function has been adjusted to automatically add expected goals to the output

## New function:
* `calculate_xg` adds expected goals column to pbp data (used inside `scrape_game`, not necessary to use this to get expected goal values)

## Fixes:
* Changed the `player_id` column in `get_draft_class` to `prospect_id` - proper NHL `player_id` column only returns with `player_details` set to `TRUE`

# hockeyR 1.0.0
* The `scrape_game()` function runs about 4x faster now

Three new functions have been added:

* `get_team_roster()` fetches the current roster for a single team from [nhl.com](https://www.nhl.com/)
* `get_current_rosters()` fetches current rosters for all 32 teams from [nhl.com](https://www.nhl.com/)
* `get_draft_class()` fetches all draft selections for a single draft year from [nhl.com](https://www.nhl.com/)

# hockeyR 0.1.2

* Updated `get_game_info()` & `get_game_rosters()` functions to require `game_id` as the argument rather than raw json data
* Updated `scrape_game()` function to accommodate above changes
* Added `get_goalie_stats()` function to scrape season goalie stats from [hockey-reference.com](https://www.hockey-reference.com)
* Added `get_skater_stats()` function to scrape season skater stats from [hockey-reference.com](https://www.hockey-reference.com)

# hockeyR 0.1.1

* Fixed issue in hockey-reference scraper where page exists but no stats recorded throws error (issue was breaking the hockey-ref-scrapers vignette)

# hockeyR 0.1.0.9000

* Updated to use the [magrittr](https://magrittr.tidyverse.org/reference/pipe.html) pipe operator `%>%` rather than the native R `|>`
* Walked back R dependency from 4.1.0 to 3.5.0 by switching pipes

# hockeyR 0.1.0

* Updated DESCRIPTION to include R 4.1.0 dependency (#1)
* Added httr dependency for `load_pbp()` function
* Moved hockey-ref examples from README to a vignette
* Submitted to CRAN

# hockeyR 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
