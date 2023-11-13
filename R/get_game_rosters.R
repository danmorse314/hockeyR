#' Scrape game day rosters for individual game
#'
#' @param game_id Game ID to scrape (Can be found using get_game_ids function)
#'
#' @description Scrapes the game-day rosters for both teams in the given game ID
#'
#' @return A tibble containing player names, ids, and positions for both team rosters
#' in a given game.
#' @export
get_game_rosters <- function(game_id){

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
    stop(paste("Could not get rosters for game ID",game_id))
  }

  rosters <- site$rosterSpots %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_wider(firstName) %>%
    dplyr::select(teamId, playerId, first_name = default, lastName:positionCode) %>%
    tidyr::unnest_wider(lastName) %>%
    dplyr::mutate(player_name = paste(first_name, default)) %>%
    dplyr::mutate(
      position = positionCode,
      priority = ifelse(position == "G", 2, 1)
    ) %>%
    dplyr::arrange(priority) %>%
    dplyr::mutate(
      position = ifelse(position %in% c("L","R"), paste0(position,"W"), position),
      position_type = dplyr::case_when(
        position %in% c("LW","RW","C") ~ "F",
        position %in% c("LD","RD","D") ~ "D",
        position == "G" ~ "G",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      team_id = teamId, player_id = playerId,
      player_name, position, position_type
    )

  return(rosters)

}
