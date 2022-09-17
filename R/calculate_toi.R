#' Calculate player time on ice
#'
#' @description A function to calculate individual skater time on ice for a provided
#' play-by-play data set
#'
#' @param pbp A tibble of play-by-play data, typically returned from either
#' \code{\link{load_pbp}} or \code{\link{scrape_game}}
#'
#' @return A tibble containing time on ice information for every skater in supplied pbp data
#' \describe{
#' \item{player_name}{String identifying player name}
#' \item{player_id}{Integer value of the NHL player ID}
#' \item{gp}{Games Played}
#' \item{toi}{String description of total time on ice in 'minutes:seconds'}
#' \item{mean_toi}{String description of average time on ice over all supplied games, in 'minutes:seconds'}
#' \item{toi_minutes}{Numeric total time on ice, in minutes}
#' \item{mean_toi_minutes}{Numeric average time on ice over all supplied games, in minutes}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' pbp_2022 <- load_pbp(2022)
#' skater_toi <- calculate_toi(pbp_2022)
#' }
calculate_toi <- function(pbp){

  ids <- pbp %>%
    dplyr::select(player_name = event_player_1_name, player_id = event_player_1_id) %>%
    dplyr::distinct()

  ids$player_name <- stringr::str_replace_all(ids$player_name,"'",".")

  pbp_changes <- pbp %>%
    dplyr::filter(event_type == "CHANGE") %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(
      on_ice = paste(
        home_on_1,home_on_2,home_on_3,home_on_4,home_on_5,home_on_6,
        away_on_1,away_on_2,away_on_3,away_on_4,away_on_5,away_on_6,
        sep = ";"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_id, game_seconds, on_ice)

  # remove NA players
  pbp_changes$on_ice <- stringr::str_remove_all(pbp_changes$on_ice,";NA")
  pbp_changes$on_ice <- stringr::str_remove_all(pbp_changes$on_ice,"NA;")

  pbp_changes <- pbp_changes %>%
    dplyr::filter(on_ice != "NA")

  full_shifts <- pbp_changes %>%
    # remove repeats of time stamp, happens at the start of each period
    dplyr::group_by(game_id, game_seconds) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    padr::pad_int(
      by = "game_seconds",
      group = "game_id"
    ) %>%
    tidyr::fill(
      on_ice
    ) %>%
    tidyr::separate_rows(on_ice, sep =";") %>%
    # 0 is start time, don't want to add it to toi
    dplyr::filter(game_seconds > 0)

  toi_df <- full_shifts %>%
    dplyr::group_by(player_name = on_ice) %>%
    dplyr::summarize(
      gp = length(unique(game_id)),
      toi_seconds = dplyr::n(),
      # decimal toi
      toi_minutes = toi_seconds / 60,
      # readable toi
      toi = paste(
        floor(toi_minutes),
        formatC(
          round(toi_seconds - 60*floor(toi_minutes)),
          width = 2, flag = "0"
          ),
        sep = ":"
      ),
      # for more than 1 GP, calculate mean_toi
      # decimal toi
      mean_toi_minutes = toi_minutes / gp,
      # readable toi
      mean_toi = paste(
        floor(mean_toi_minutes),
        formatC(
          round(60*mean_toi_minutes - 60*floor(mean_toi_minutes)),
          width = 2, flag = "0", digits = 2
          ),
        sep = ":"
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(-toi_seconds) %>%
    dplyr::left_join(ids, by = "player_name") %>%
    dplyr::select(player_name, player_id, gp, toi, mean_toi, dplyr::everything())

  return(toi_df)
}
