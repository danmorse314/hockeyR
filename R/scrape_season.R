#' Scrape full season play-by-play
#'
#' @param season An integer value denoting the end year of the season to scrape
#' @param type A character vector of the game types to include: REG, POST, or "all"
#'
#' @return A tibble containing all play-by-play data for a given season in
#' the same format as the output of \code{\link{scrape_game}}
#' @export
#'
#' @examples
#' \dontrun{
#' # scrape all regular season & postseason games for the 2016-2017 season
#' pbp_2016_2017 <- scrape_season(2017, type = "REG")
#' }
scrape_season <- function(season, type = "all"){

  games <- get_game_ids(season = season)

  if(tolower(type) == "all") {
    type <- c("REG","POST")
  }

  games <- games |>
    dplyr::filter(game_type %in% type) |>
    dplyr::arrange(desc(game_type)) #  puts regular season first, helps with column ordering

  # for testing
  # don't stop in case of bad game
  scrape_game_safe <- purrr::possibly(scrape_game, otherwise = NULL, quiet = FALSE)

  pbp <- purrr::map_df(
    .x = games |> dplyr::pull(game_id),
    ~scrape_game_safe(.x)
  )

  return(pbp)
}
