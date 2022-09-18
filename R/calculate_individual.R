#' Calculate individual player stats
#'
#' @param pbp A tibble of play-by-play data, typically returned from either
#' \code{\link{load_pbp}} or \code{\link{scrape_game}}
#' @param type Season type to filter by; "R" for regular season and/or "P" for postseason
#' @param game_strength String or vector of strings defining strength state to
#' filter by; ex c("3v5","4v5","3v4") returns stats for shorthanded strength
#'
#' @return A tibble containing individual shooting stats for all players in supplied pbp data.
#' **These stats are individual player stats, not player on-ice stats.**
#' For help with on-ice stats, please see \code{\link{calculate_on_ice}}.

#' \describe{
#' \item{player_name}{String identifying player name}
#' \item{player_id}{Integer value of the NHL player ID}
#' \item{team}{String identifying player's most recent team}
#' \item{gp}{Games Played}
#' \item{ixg}{Numeric expected goals}
#' \item{goals}{Numeric goals scored}
#' \item{assists}{Numeric total assists}
#' \item{points}{Numeric points scored}
#' \item{assists_primary}{Numeric primary assists}
#' \item{assists_secondary}{Numeric secondary assists}
#' \item{points_primary}{Numeric primary points}m
#' \item{gax}{Numeric goals scored over expected}
#' \item{icf}{Numeric shot attempts (Corsi)}
#' \item{iff}{Numeric unblocked shot attempts (Fenwick)}
#' \item{isog}{Numeric shots on goal}
#' \item{sh_perc}{Numeric shooting percentage}
#' }
#' If supplied play-by-play data includes shift change events (the default for \code{\link{scrape_game}};
#' if using \code{\link{load_pbp}} user must set `shift_events` argument to `TRUE`) then
#' the following rate stats will also be calculated:
#' \describe{
#' \item{toi}{String description of total time on ice in 'minutes:seconds'}
#' \item{mean_toi}{String description of average time on ice over all supplied games, in 'minutes:seconds'}
#' \item{toi_minutes}{Numeric total time on ice, in minutes}
#' \item{mean_toi_minutes}{Numeric average time on ice over all supplied games, in minutes}
#' \item{ixg_per60}{Numeric expected goals per 60 minutes}
#' \item{goals_per60}{Numeric goals scored per 60 minutes}
#' \item{assists_per60}{Numeric total assists per 60 minutes}
#' \item{points_per60}{Numeric points scored per 60 minutes}
#' \item{assists_primary_per60}{Numeric primary assists per 60 minutes}
#' \item{assists_secondary_per60}{Numeric secondary assists per 60 minutes}
#' \item{points_primary_per60}{Numeric primary points per 60 minutes}
#' \item{gax_per60}{Numeric goals scored over expected per 60 minutes}
#' \item{icf_per60}{Numeric shot attempts (Corsi) per 60 minutes}
#' \item{iff_per60}{Numeric unblocked shot attempts (Fenwick) per 60 minutes}
#' \item{isog_per60}{Numeric shots on goal per 60 minutes}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' #load pbp
#' pbp_2022 <- load_pbp(2022, shift_events = TRUE)
#' player_stats <- calculate_individual(pbp_2022, type = "R", game_strength = "5v5")
#' }
calculate_individual <- function(pbp, type = c("R","P"), game_strength = "all"){

  pbp <- pbp %>%
    dplyr::filter(season_type %in% type & period_type != "SHOOTOUT")

  if(game_strength != "all"){
    pbp <- pbp %>%
      dplyr::filter(strength_state %in% game_strength)
  }

  goals <- pbp %>%
    dplyr::filter(event_type %in% c("GOAL","SHOT","MISSED_SHOT","BLOCKED_SHOT")) %>%
    dplyr::group_by(player_name = event_player_1_name, player_id = event_player_1_id) %>%
    dplyr::summarize(
      gp = length(unique(game_id)),
      team = dplyr::last(event_team),
      ixg = sum(xg, na.rm = TRUE),
      goals = sum(event_type == "GOAL"),
      gax = goals - ixg,
      icf = dplyr::n(),
      iff = sum(event_type != "BLOCKED_SHOT"),
      isog = sum(event_type %in% c("SHOT","GOAL")),
      sh_perc = goals / isog,
      .groups = "drop"
    )

  a1 <- pbp %>%
    dplyr::filter(event_type == "GOAL" & !is.na(event_player_2_name)) %>%
    dplyr::group_by(player_name = event_player_2_name, player_id = event_player_2_id) %>%
    dplyr::summarize(
      assists_primary = dplyr::n(),
      .groups = "drop"
    )

  a2 <- pbp %>%
    dplyr::filter(event_type == "GOAL" & !is.na(event_player_3_name)) %>%
    dplyr::group_by(player_name = event_player_3_name, player_id = event_player_3_id) %>%
    dplyr::summarize(
      assists_secondary = dplyr::n(),
      .groups = "drop"
    )

  ind_stats <- goals %>%
    dplyr::full_join(a1, by = c("player_name", "player_id")) %>%
    dplyr::full_join(a2, by = c("player_name", "player_id")) %>%
    dplyr::mutate(
      goals = ifelse(is.na(goals), 0, goals),
      assists_primary = ifelse(is.na(assists_primary), 0, assists_primary),
      assists_secondary = ifelse(is.na(assists_secondary), 0, assists_secondary),
      assists = assists_primary + assists_secondary,
      points = goals + assists,
      points_primary = goals + assists_primary
    ) %>%
    dplyr::select(
      player_name, player_id, team,
      gp, ixg, goals, assists, points, assists_primary, assists_secondary,
      points_primary, gax, icf, iff, isog, sh_perc
    )

  if("CHANGE" %in% unique(pbp$event_type)){
    # get player time on ice
    toi_df <- calculate_toi(pbp) %>%
      # no need to duplicate games played column
      dplyr::select(-gp)

    ind_stats <- ind_stats %>%
      dplyr::left_join(toi_df, by = c("player_name", "player_id")) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = ixg:isog,
          ~ .x / toi_minutes * 60,
          .names =  "{.col}_per60"
        )
      )
  }

  return(ind_stats)
}
