#' Gather basic game information
#'
#' @param site Raw JSON data from NHL API
#'
#' @description A helper function used in game scraping. Adds columns like date,
#' venue, & information about the home and away teams
#'
#' @return A 1xN tibble containing N pieces of information about the specified game
#' @export
#'
get_game_info <- function(site){

  # for testing
  #site <- jsonlite::read_json(glue::glue("http://statsapi.web.nhl.com/api/v1/game/{game_id}/feed/live"))

  gd <- site$gameData

  game <- gd$game |>
    dplyr::bind_rows() |>
    dplyr::rename(
      game_id = pk,
      season_type = type
    )

  datetime <- gd$datetime |>
    dplyr::bind_rows()

  # older seasons don't include end time of game

  if("endDateTime" %in% names(datetime)){
    datetime <- datetime |>
      dplyr::mutate(
        game_start = dateTime |>
          lubridate::parse_date_time("ymd_HMS") |>
          lubridate::with_tz("US/Eastern"),
        game_end = endDateTime |>
          lubridate::parse_date_time("ymd_HMS") |>
          lubridate::with_tz("US/Eastern"),
        game_date = lubridate::date(game_start),
        game_length = lubridate::as.period(game_end - game_start)
      ) |>
      dplyr::select(game_date, game_start, game_end, game_length)
  } else {
    datetime <- datetime |>
      dplyr::mutate(
        game_start = dateTime |>
          lubridate::parse_date_time("ymd_HMS") |>
          lubridate::with_tz("US/Eastern"),
        game_date = lubridate::date(game_start),
      ) |>
      dplyr::select(game_date, game_start)
  }

  status <- gd[["status"]] |>
    dplyr::bind_rows() |>
    dplyr::select(game_state = abstractGameState, detailed_state = detailedState)

  venue <- gd[["venue"]] |>
    dplyr::bind_rows()

  colnames(venue) <- glue::glue("venue_{names(venue)}")

  team_names_keep <- c(
    "home_name","home_abbreviation","home_division_name","home_conference_name","home_id",
    "away_name","away_abbreviation","away_division_name","away_conference_name","away_id"
  )

  teams <- gd$teams |>
    unlist() |>
    dplyr::bind_rows() |>
    janitor::clean_names() |>
    dplyr::select(dplyr::matches(team_names_keep))

  game_info <- dplyr::bind_cols(
    game, datetime, status, venue, teams
  )

  return(game_info)

}
