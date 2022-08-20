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
    stop(paste("Could not get rosters for game ID",game_id))
  }

  rosters <- site$gameData$players %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::select(id, fullName, primaryPosition) %>%
    tidyr::unnest_wider(3) %>%
    dplyr::mutate(
      priority = ifelse(abbreviation == "G", 2, 1)
    ) %>%
    dplyr::arrange(priority) %>%
    dplyr::select(
      "player_id" = id,
      "player_name" = fullName,
      "position_type" = type,
      "position" = abbreviation
    )

  return(rosters)

}
