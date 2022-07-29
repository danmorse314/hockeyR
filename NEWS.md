# hockeyR 0.1.2

* Updated 'get_game_info()' & 'get_game_rosters()' functions to require 'game_id' as the argument rather than raw json data
* Updated 'scrape_game()' function to accommodate above changes
* Added 'get_goalie_stats()' function to scrape season goalie stats from [hockey-reference.com](https://hockey-reference.com)
* Added 'get_skater_stats()' function to scrape season skater stats from [hockey-reference.com](https://hockey-reference.com)

# hockeyR 0.1.1

* Fixed issue in hockey-reference scraper where page exists but no stats recorded throws error (issue was breaking the hockey-ref-scrapers vignette)

# hockeyR 0.1.0.9000

* Updated to use the [magrittr](https://magrittr.tidyverse.org/reference/pipe.html) pipe operator `%>%` rather than the native R `|>`
* Walked back R dependency from 4.1.0 to 3.5.0 by switching pipes

# hockeyR 0.1.0

* Updated DESCRIPTION to include R 4.1.0 dependency (#1)
* Added httr dependency for 'load_pbp()' function
* Moved hockey-ref examples from README to a vignette
* Submitted to CRAN

# hockeyR 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
