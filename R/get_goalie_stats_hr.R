#' Scrape goalie stats
#'
#' @param season Integer value denoting the end year of the season to scrape
#'
#' @description A function to scrape all goalie stats from a single season via
#' hockey-reference.com
#'
#' @return A tibble containing all goalie stats found on hockey-reference.com
#' for the given season
#' @export
#'
#' @examples
#' \dontrun{
#' get_goalie_stats_hr(2022)
#' }
get_goalie_stats_hr <- function(season = as.numeric(format(Sys.Date()+81, "%Y"))){
  # season: end year for hockey season, eg 2022 = 2021-22

  if(season == 2005){
    stop("No stats available for 2004-05; season cancelled by Gary Bettman")
  } else if(season < 1918){
    stop("Stats only available back to 1917-18 season")
  }

  url <- paste0("https://www.hockey-reference.com/leagues/NHL_",season,"_goalies.html")

  site <- tryCatch(
    rvest::read_html(url),
    warning = function(cond){
      message(paste0("Problem fetching goalie stats\n\n",cond))
      return(NULL)
    },
    error = function(cond){
      message(paste0("Problem fetching goalie stats\n\n",cond))
      return(NULL)
    }
  )

  if(is.null(site)){
    stop(paste("Could not get goalie stats for",season))
  }

  player_links <- dplyr::tibble(
    link = site %>%
      rvest::html_element("#stats") %>%
      rvest::html_elements("a[href^='/players/']") %>%
      rvest::html_attr("href")
  ) %>%
    dplyr::mutate(
      player_id = stringr::str_remove_all(link,"/players/[a-z]/"),
      player_id = stringr::str_remove(player_id, ".html")
    ) %>%
    dplyr::select(link, player_id)

  df <- site %>%
    rvest::html_element("#stats") %>%
    rvest::html_table() %>%
    janitor::row_to_names(row_number = 1) %>%
    janitor::clean_names() %>%
    # remove rows of column names in middle of data
    dplyr::filter(rk != "Rk")

  df <- df %>%
    dplyr::bind_cols(player_links) %>%
    # remove total stats from goalies to play for multiple teams
    dplyr::filter(tm != "TOT") %>%
    dplyr::select(-rk) %>%
    dplyr::mutate(
      season = paste0(season-1,"-",substr(season,3,4))
      ) %>%
    # clean up names
    dplyr::rename(
      team_abbr = tm,
      games_played = gp,
      games_started = gs,
      wins = w,
      losses = l,
      ot_losses = t_o,
      goals_against = ga,
      shots_against = sa,
      saves = sv,
      save_percent = sv_percent,
      goals_against_average = gaa,
      shutouts = so,
      minutes = min,
      quality_starts = qs,
      quality_start_percent = qs_percent,
      really_bad_starts = rbs,
      hr_point_shares = gps,
      goals_against_percent = ga_percent,
      goals_saved_above_average = gsaa,
      goals = g,
      assists = a,
      points = pts,
      penalty_minutes = pim
    ) %>%
    dplyr::select(player, team_abbr, season, tidyselect::everything())

  # convert numerical columns to numeric
  df <- utils::type.convert(df, as.is = TRUE)

  return(df)
}
