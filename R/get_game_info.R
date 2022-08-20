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
  url <- glue::glue("http://statsapi.web.nhl.com/api/v1/game/{game_id}/feed/live")

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

  gd <- site$gameData

  game <- gd$game %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      game_id = pk,
      season_type = type
    )

  datetime <- gd$datetime %>%
    dplyr::bind_rows()

  # older seasons don't include end time of game

  if("endDateTime" %in% names(datetime)){
    datetime <- datetime %>%
      dplyr::mutate(
        game_start = dateTime %>%
          lubridate::parse_date_time("ymd_HMS") %>%
          lubridate::with_tz("US/Eastern"),
        game_end = endDateTime %>%
          lubridate::parse_date_time("ymd_HMS") %>%
          lubridate::with_tz("US/Eastern"),
        game_date = lubridate::date(game_start),
        game_length = lubridate::as.period(game_end - game_start)
      ) %>%
      dplyr::select(game_date, game_start, game_end, game_length)
  } else {
    datetime <- datetime %>%
      dplyr::mutate(
        game_start = dateTime %>%
          lubridate::parse_date_time("ymd_HMS") %>%
          lubridate::with_tz("US/Eastern"),
        game_date = lubridate::date(game_start),
      ) %>%
      dplyr::select(game_date, game_start)
  }

  status <- gd[["status"]] %>%
    dplyr::bind_rows() %>%
    dplyr::select(game_state = abstractGameState, detailed_state = detailedState)

  venue <- gd[["venue"]] %>%
    dplyr::bind_rows()

  colnames(venue) <- glue::glue("venue_{names(venue)}")

  team_names_keep <- c(
    "home_name","home_abbreviation","home_division_name","home_conference_name","home_id",
    "away_name","away_abbreviation","away_division_name","away_conference_name","away_id"
  )

  teams <- gd$teams %>%
    unlist() %>%
    dplyr::bind_rows() %>%
    janitor::clean_names() %>%
    dplyr::select(dplyr::matches(team_names_keep))

  game_info <- dplyr::bind_cols(
    game, datetime, status, venue, teams
  )

  return(game_info)

}
