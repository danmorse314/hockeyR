
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hockeyR

<!-- badges: start -->
<!-- badges: end -->

This package contains various functions to help scrape hockey data from
hockey-reference.com, including standings, player stats, and jersey
number history.

## Installation

You can install the released version of hockeyR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hockeyR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("danmorse314/hockeyR")
```

## Usage

Grab every team’s win-loss record in any season going back to 1918 with
the `get_team_records` function

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.1.2     v dplyr   1.0.6
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(hockeyR)
get_team_records(1924)
#> # A tibble: 4 x 10
#>   team_name     team_abbr season_short season overall     w     l   otl overtime
#>   <chr>         <chr>            <dbl> <chr>  <chr>   <int> <int> <int> <chr>   
#> 1 Hamilton Tig~ HAM               1924 1923-~ 9-15-0      9    15     0 0-0     
#> 2 Montreal Can~ MTL               1924 1923-~ 13-11-0    13    11     0 0-0     
#> 3 Ottawa Senat~ OTS               1924 1923-~ 16-8-0     16     8     0 0-0     
#> 4 Toronto St. ~ TRS               1924 1923-~ 10-14-0    10    14     0 0-0     
#> # ... with 1 more variable: st_points <dbl>
```

You can also get stats down to the player-level with `get_player_stats`.
This function defaults to the player’s career statistics, but you can
enter a specific season or range of seasons as well. Note that the
season references the year the specific season ended (ie the 2021-22
season should be entered as 2022)

``` r
get_player_stats(player_name = "Wayne Gretzky", season = 1982)
#> # A tibble: 1 x 25
#>   player  link  season_full   age tm    lg       gp     g     a   pts plus_minus
#>   <chr>   <glu> <chr>       <int> <chr> <chr> <int> <int> <int> <int>      <int>
#> 1 Wayne ~ http~ 1981-82        21 EDM   NHL      80    92   120   212         80
#> # ... with 14 more variables: pim <int>, ev <int>, pp <int>, sh <int>,
#> #   gw <int>, ev_a <int>, pp_a <int>, sh_a <int>, s <int>, s_percent <dbl>,
#> #   toi <lgl>, atoi <lgl>, awards <chr>, season_short <int>
```

Ever wonder who the most prolific goal-scorer was to wear a specific
number? Use `get_jersey_players` in conjunction with `get_player_stats`
to find out:

``` r
# get every player to wear the desired number
df <- get_jersey_players(69)

# get their statistics from the year they wore that sweater
df2 <- purrr::map2_dfr(
  .x = df$player,
  .y = df$season,
  ~get_player_stats(player_name = .x, season = .y)
  )

# who had the most goals?
arrange(df2, desc(g))
#> # A tibble: 3 x 33
#>   player  link  season_full   age tm    lg       gp     g     a   pts plus_minus
#>   <chr>   <glu> <chr>       <int> <chr> <chr> <int> <int> <int> <int>      <int>
#> 1 Andrew~ http~ 2011-12        25 SJS   NHL      76     4    13    17          4
#> 2 Andrew~ http~ 2010-11        24 SJS   NHL      17     1     2     3         -1
#> 3 Mel An~ http~ 2003-04        31 WSH   NHL       2     0     0     0          0
#> # ... with 22 more variables: pim <int>, ev <int>, pp <int>, sh <int>,
#> #   gw <int>, ev_a <int>, pp_a <int>, sh_a <int>, s <int>, s_percent <dbl>,
#> #   toi <int>, atoi <chr>, awards <lgl>, season_short <int>, tsa <int>,
#> #   fow <int>, fol <int>, fo_percent <dbl>, blk <int>, hit <int>, tk <int>,
#> #   gv <int>
```

You can use the data to make plots with actual team colors and logos as
well using the `team_logos_colors` file included with the package.

``` r
# add colors & logos
df3 <- df2 |>
  group_by(player, season_full) |>
  # this part is just to get both of Mete's seasons into one row
  summarize(
    gp = sum(gp),
    pts = sum(pts),
    pts_gm = pts/gp,
    tm = tail(tm, n=1),
    .groups = "drop"
  ) |>
  mutate(player_season = glue::glue("{player}\n{season_full}")) |>
  left_join(team_logos_colors, by = c("tm" = "team_abbr"))

df3 |>
  ggplot(aes(reorder(player_season, -pts_gm), pts_gm)) +
  geom_col(fill = df3$team_color1, color = df3$team_color2) +
  ggimage::geom_image(
    aes(y = pts_gm + .025, image = team_logo_espn),
    size = .05, asp = 1.5
  ) +
  geom_text(aes(y = 0.01, label = player_season),
            color = "white", angle = 90, hjust = 0) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = "white"),
    title = element_text(color = "white")
  ) +
  labs(x = NULL, y = "Points per game",
       title = "Most productive NHL seasons wearing #98",
       caption = "data pulled from hockey-reference.com using hockeyR")
```

<img src="man/figures/README-plot example-1.png" width="100%" />
