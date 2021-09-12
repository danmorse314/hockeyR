#' Scrape game day rosters for individual game
#'
#' @param site Raw JSON data from NHL API
#'
#' @description A helper function for game scraping. Fetches the game-day rosters
#' for both teams in the given game.
#'
#' @return A tibble containing player names, ids, and positions for both team rosters
#' in a given game.
#' @export
get_game_rosters <- function(site){

  rosters <- site$gameData$players |>
    dplyr::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(id, fullName, primaryPosition) |>
    tidyr::unnest_wider(3) |>
    dplyr::mutate(
      priority = ifelse(abbreviation == "G", 2, 1)
    ) |>
    dplyr::arrange(priority) |>
    dplyr::select(
      "player_id" = id,
      "player_name" = fullName,
      "position_type" = type,
      "position" = abbreviation
    )

  return(rosters)

}
