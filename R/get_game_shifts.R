#' Fetch game shift data
#'
#' @param game_id Game ID to scrape
#'
#' @description A function to gather shift data from a given game. Shifts are
#' turned into events to match the style of events in standard game pbp.\cr\cr
#' Portions of this code are modified versions of code from the NHL scraper by
#' Evolving-Hockey, which in turn were modified from the NHL scraper by Manny Perry.
#'
#' @return A tibble containing each player change as an event
#' @export
#'
#' @examples
#' \donttest{
#' try({
#' get_game_shifts(game_id = 2023020201)
#' })
#' }
get_game_shifts <- function(game_id){

  url <- glue::glue("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={game_id}")

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
    message(paste("There was a problem getting shift data from the NHL for game ID",game_id,", please try again later."))
    return(NULL)
  } else if(length(site$data) < 10){
    message(paste0("No shift data recorded for game ID ",game_id,", play-by-play data will be returned without on-ice skaters."))
    shifts <- NULL
  } else {

    shifts_raw <- site$data %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      janitor::clean_names() %>%
      tidyr::unite(player_name, c(first_name, last_name), sep = " ") %>%
      dplyr::select(game_id, player_id, player_name, team_abbrev, team_id,
                    team_name, period, start_time, end_time, duration) %>%
      dplyr::filter(!is.na(duration)) %>%
      dplyr::mutate(
        start_time_ms = lubridate::ms(start_time),
        start_seconds = lubridate::period_to_seconds(start_time_ms),
        start_game_seconds = start_seconds + (1200 * (period-1)),
        end_time_ms = lubridate::ms(end_time),
        end_seconds = lubridate::period_to_seconds(end_time_ms),
        end_game_seconds = end_seconds + (1200 * (period-1)),
        duration = lubridate::ms(duration),
        duration_seconds = lubridate::period_to_seconds(duration)
      ) %>%
      dplyr::filter(duration_seconds > 0)

    shifts_on <- shifts_raw %>%
      dplyr::group_by(
        event_team_abbr = team_abbrev, period, start_time, start_seconds, start_game_seconds
      ) %>%
      dplyr::summarize(
        num_on = dplyr::n(),
        players_on = paste(player_name, collapse = ", "),
        ids_on = paste(player_id, collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::rename(
        period_time = start_time,
        period_seconds = start_seconds,
        game_seconds = start_game_seconds
      )

    shifts_off <- shifts_raw %>%
      dplyr::group_by(
        event_team_abbr = team_abbrev, period, end_time, end_seconds, end_game_seconds
      ) %>%
      dplyr::summarize(
        num_off = dplyr::n(),
        players_off = paste(player_name, collapse = ", "),
        ids_off = paste(player_id, collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::rename(
        period_time = end_time,
        period_seconds = end_seconds,
        game_seconds = end_game_seconds
      )

    shifts <- dplyr::full_join(
      shifts_on, shifts_off,
      by = c("game_seconds", "event_team_abbr", "period", "period_time", "period_seconds")
    ) %>%
      dplyr::arrange(game_seconds) %>%
      dplyr::mutate(
        event = "Change",
        event_type = "CHANGE",
        game_seconds_remaining = 3600 - game_seconds
      ) %>%
      # removing NA values at start and end of periods
      dplyr::mutate(
        players_on = ifelse(is.na(players_on), "None", players_on),
        players_off = ifelse(is.na(players_off), "None", players_off),
        ids_on = ifelse(is.na(ids_on), 0, ids_on),
        ids_off = ifelse(is.na(ids_off), 0, ids_off)
      )
  }

  return(shifts)
}
