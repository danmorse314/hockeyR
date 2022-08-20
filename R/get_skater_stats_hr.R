#' Scrape skater stats
#'
#' @param season Integer value denoting the end year of the season to scrape
#'
#' @description A function to scrape all skater stats from a single season via
#' hockey-reference.com
#'
#' @return A tibble containing all skater stats found on hockey-reference.com
#' for the given season
#' @export
#'
#' @examples
#' \dontrun{
#' get_skater_stats_hr(2022)
#' }
get_skater_stats_hr <- function(season = as.numeric(format(Sys.Date()+81, "%Y"))){

  if(season == 2005){
    stop("No stats available for 2004-05; season cancelled by Gary Bettman")
  } else if(season < 1918){
    stop("Stats only available back to 1917-18 season")
  }

  url <- paste0("https://www.hockey-reference.com/leagues/NHL_",season,"_skaters.html")

  site <- tryCatch(
    rvest::read_html(url),
    warning = function(cond){
      message(paste0("Problem fetching skater stats\n\n",cond))
      return(NULL)
    },
    error = function(cond){
      message(paste0("Problem fetching skater stats\n\n",cond))
      return(NULL)
    }
  )

  if(is.null(site)){
    stop(paste("Could not get skater stats for",season))
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

  suppressWarnings({
    df <- site %>%
      rvest::html_element("#stats") %>%
      rvest::html_table() %>%
      janitor::row_to_names(row_number = 1) %>%
      janitor::clean_names() %>%
      # remove rows of column names in middle of data
      dplyr::filter(rk != "Rk")
  })

  df <- df %>%
    dplyr::bind_cols(player_links) %>%
    # remove total stats from goalies to play for multiple teams
    dplyr::filter(tm != "TOT") %>%
    dplyr::select(-rk) %>%
    dplyr::mutate(
      season = paste0(season-1,"-",substr(season,3,4)),
      s_percent = as.numeric(g) / as.numeric(s)
    )

  if(length(unique(df$atoi)) == 1){
    # checking to see if atoi is available
    df <- df %>%
      dplyr::mutate(
        atoi = lubridate::ms(atoi) %>%
          lubridate::period_to_seconds()/60
      )
  }

  if("fow" %in% names(df)){
    df <- df %>%
      dplyr::mutate(fo_percent = as.numeric(fow) / (as.numeric(fow)+as.numeric(fol))) %>%
      dplyr::rename(
        team_abbr = tm,
        position = pos,
        games_played = gp,
        goals = g,
        assists = a,
        points = pts,
        plus_minus = x,
        penalty_minutes = pim,
        hr_point_shares = ps,
        goals_even_strength = ev,
        goals_powerplay = pp,
        goals_shorthanded = sh,
        goals_game_winning = gw,
        assists_even_strength = ev_2,
        assists_powerplay = pp_2,
        assists_shorthanded = sh_2,
        shots_on_goal = s,
        shooting_percent = s_percent,
        time_on_ice = toi,
        mean_time_on_ice = atoi,
        blocks = blk,
        hits = hit,
        faceoff_wins = fow,
        faceoff_losses = fol,
        faceoff_win_percent = fo_percent
      ) %>%
      dplyr::select(player, team_abbr, season, tidyselect::everything())
  } else {
    df <- df %>%
      dplyr::rename(
        team_abbr = tm,
        position = pos,
        games_played = gp,
        goals = g,
        assists = a,
        points = pts,
        plus_minus = x,
        penalty_minutes = pim,
        hr_point_shares = ps,
        goals_even_strength = ev,
        goals_powerplay = pp,
        goals_shorthanded = sh,
        goals_game_winning = gw,
        assists_even_strength = ev_2,
        assists_powerplay = pp_2,
        assists_shorthanded = sh_2,
        shots_on_goal = s,
        shooting_percent = s_percent,
        time_on_ice = toi,
        mean_time_on_ice = atoi
      ) %>%
      dplyr::select(player, team_abbr, season, tidyselect::everything())

  }

  # convert numerical columns to numeric
  df <- utils::type.convert(df, as.is = TRUE)

  return(df)
}
