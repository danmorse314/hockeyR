#' Gather basic game information
#'
#' @param game_id Game ID to scrape (Can be found using get_game_ids function)
#'
#' @description Scrapes basic game info like date, venue, & information about
#' the home and away teams for a given game
#'
#' @return A 1xN tibble containing N pieces of information about the specified game
#' @export
#'
get_game_info <- function(game_id){

  # get game url
  url <- glue::glue("https://api-web.nhle.com/v1/gamecenter/{game_id}/play-by-play")

  # get raw json pbp data

  site <- tryCatch(
    jsonlite::read_json(url),
    warning = function(cond){
      message(paste0("There was a problem with game ID ",game_id,"\n\n",cond))
      return(NULL)
    },
    error = function(cond){
      message(paste0("There was a problem with game ID ",game_id,"\n\n",cond))
      return(NULL)
    }
  )

  if(is.null(site)){
    stop(paste("Could not get game info for game ID",game_id))
  }

  game_info <- dplyr::tibble(
    game_id = site$id,
    season = site$season,
    season_type = site$gameType,
    game_date = site$gameDate,
    game_start = site$startTimeUTC %>%
      lubridate::parse_date_time("ymd_HMS") %>%
      lubridate::with_tz("US/Eastern"),
    game_state = site$gameState,
    venue = site$venue$default,
    home_abbr = site$homeTeam$abbrev,
    away_abbr = site$awayTeam$abbrev,
    home_id = site$homeTeam$id,
    away_id = site$awayTeam$id
  )

  return(game_info)

}
