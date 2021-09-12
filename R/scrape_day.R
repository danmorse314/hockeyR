#' Scrape play-by-play for single day's games
#'
#' @param day A day in the format of 'YYYY-MM-DD'
#'
#' @return A tibble containing all play-by-play data for a given day in
#' the same format as the output of \code{\link{scrape_game}}
#' @export
#'
#' @examples
#' \dontrun{
#' pbp_day <- scrape_day(day = "2015-01-06")
#' }
scrape_day <- function(day){

  # for testing
  #day <- "2017-10-17"

  games <- get_game_ids(day = day)

  game_ids <- games |> dplyr::pull(game_id)

  scrape_game_safe <- purrr::possibly(scrape_game, otherwise = NULL, quiet = FALSE)

  pbp <- purrr::map_df(
    game_ids,
    ~scrape_game_safe(.x)
  )

  #pbp <- pbp |>
  #  utils::type.convert(as.is = TRUE)

  return(pbp)
}
