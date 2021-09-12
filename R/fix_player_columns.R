#' Fix event player columns
#'
#' @param players A tibble of event players from JSON pbp data
#' @param col Integer between 1 & 4 for which event player is being evaluated
#'
#' @description Helper function to parse player columns from JSON pbp data &
#' clean up the column names
#'
#' @return A tibble of an individual event player with cleaned column names
#' @export
fix_player_columns <- function(players, col){

  # for parsing event players in pbp data

  colt <- "event_player_"

  player <- players |>
    tidyr::unnest_wider(col) |>
    tidyr::unnest_wider(player)

  if("seasonTotal" %in% names(player)) {
    player <- player |>
      dplyr::select(id:seasonTotal)
  } else {
    player <- player |>
      dplyr::select(id:playerType)
  }

  colnames(player) <- glue::glue("{colt}{col}_{c('id','name','link','type','season_total')}")

  return(player)
}
