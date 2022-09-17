#' Calculate player on-ice stats
#'
#' @param pbp A tibble of play-by-play data, typically returned from either
#' \code{\link{load_pbp}} or \code{\link{scrape_game}}
#' @param type Season type to filter by; "R" for regular season and/or "P" for postseason
#' @param game_strength String or vector of strings defining strength state to
#' filter by; ex c("3v5","4v5","3v4") returns stats for shorthanded strength
#'
#' @return A tibble containing on-ice shot stats for all players in supplied pbp data.
#' **These stats are for all players in which a player was on the ice, not individual stats.**
#' For help with individual stats, please see \code{\link{calculate_individual}}.
#' On-ice stats include:
#' \describe{
#' \item{player_name}{String identifying player name}
#' \item{player_id}{Integer value of the NHL player ID}
#' \item{team}{String identifying player's most recent team}
#' \item{gp}{Games Played}
#' \item{cf}{Numeric shot attempts (Corsi) for}
#' \item{ca}{Numeric shot attempts (Corsi) against}
#' \item{cf_perc}{Numeric Corsi For % (CF%)}
#' \item{ff}{Numeric unblocked shot attempts (Fenwick) for}
#' \item{fa}{Numeric unblocked shot attempts (Fenwick) against}
#' \item{ff_perc}{Numeric Fenwick For % (FF%)}
#' \item{gf}{Numeric goals for}
#' \item{ga}{Numeric goals against}
#' \item{gf_perc}{Numeric Goals For % (GF%)}
#' \item{xgf}{Numeric expected goals for}
#' \item{xga}{Numeric expected goals against}
#' \item{xgf_perc}{Numeric Expected Goals For % (xGF%)}
#' }
#' If supplied play-by-play data includes shift change events (the default for \code{\link{scrape_game}};
#' if using \code{\link{load_pbp}} user must set `shift_events` argument to `TRUE`) then
#' the following rate stats will also be calculated:
#' \describe{
#' \item{toi}{String description of total time on ice in 'minutes:seconds'}
#' \item{mean_toi}{String description of average time on ice over all supplied games, in 'minutes:seconds'}
#' \item{toi_minutes}{Numeric total time on ice, in minutes}
#' \item{mean_toi_minutes}{Numeric average time on ice over all supplied games, in minutes}
#' \item{cf_per60}{Numeric shot attempts (Corsi) for per 60 minutes}
#' \item{ca_per60}{Numeric shot attempts (Corsi) against per 60 minutes}
#' \item{ff_per60}{Numeric unblocked shot attempts (Fenwick) for per 60 minutes}
#' \item{fa_per60}{Numeric unblocked shot attempts (Fenwick) against per 60 minutes}
#' \item{gf_per60}{Numeric goals for per 60 minutes}
#' \item{ga_per60}{Numeric goals against per 60 minutes}
#' \item{xgf_per60}{Numeric expected goals for per 60 minutes}
#' \item{xga_per60}{Numeric expected goals against per 60 minutes}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' #load pbp
#' pbp_2022 <- load_pbp(2022, shift_events = TRUE)
#' player_stats <- calculate_on_ice(pbp_2022, type = "R", game_strength = "5v5")
#' }
calculate_on_ice <- function(pbp, type = c("R","P"), game_strength = "all"){

  ids <- pbp %>%
    dplyr::select(player_name = event_player_1_name, player_id = event_player_1_id) %>%
    dplyr::distinct()

  ids$player_name <- stringr::str_replace_all(ids$player_name,"'",".")

  pbp <- pbp %>%
    dplyr::filter(season_type %in% type & period_type != "SHOOTOUT")

  if(game_strength != "all"){
    pbp <- pbp %>%
      dplyr::filter(strength_state %in% game_strength)
  }

  pbp_new <- pbp %>%
    dplyr::filter(event_type %in% c("SHOT","MISSED_SHOT","BLOCKED_SHOT","GOAL"))

  # convert player on-ice cols to vector cols
  pbp_new <- pbp_new %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(
      home_on_ice = paste(
        home_on_1,home_on_2,home_on_3,home_on_4,home_on_5,home_on_6,
        sep = ";"
      ),
      away_on_ice = paste(
        away_on_1,away_on_2,away_on_3,away_on_4,away_on_5,away_on_6,
        sep = ";"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_id, event_id, home_name, away_name,
           event_type, xg, event_team, home_on_ice, away_on_ice)

  # remove NA players
  pbp_new$home_on_ice <- stringr::str_remove_all(pbp_new$home_on_ice,";NA")
  pbp_new$away_on_ice <- stringr::str_remove_all(pbp_new$away_on_ice,";NA")

  # split by home and away
  home_stats <- pbp_new %>%
    dplyr::select(-away_on_ice) %>%
    # individual rows per on-ice player
    tidyr::separate_rows(
      home_on_ice,
      sep = ";"
    ) %>%
    dplyr::mutate(
      xg_for = xg * (event_team == home_name),
      xg_against = xg * (event_team == away_name),
      gf = ifelse(event_type == "GOAL" & event_team == home_name, 1, 0),
      ga = ifelse(event_type == "GOAL" & event_team == away_name, 1, 0),
      ff = ifelse(event_type != "BLOCKED_SHOT" & event_team == home_name, 1, 0),
      fa = ifelse(event_type != "BLOCKED_SHOT" & event_team == away_name, 1, 0)
    ) %>%
    dplyr::group_by(player_name = home_on_ice) %>%
    dplyr::summarise(
      team = paste(unique(home_name), collapse = ";"),
      gp = length(unique(game_id)),
      cf = sum(event_team == home_name),
      ca = sum(event_team == away_name),
      ff = sum(ff),
      fa = sum(fa),
      gf = sum(gf),
      ga = sum(ga),
      xgf = sum(xg_for, na.rm = TRUE),
      xga = sum(xg_against, na.rm = TRUE),
      .groups = "drop"
    )

  away_stats <- pbp_new %>%
    dplyr::select(-home_on_ice) %>%
    # individual rows per on-ice player
    tidyr::separate_rows(
      away_on_ice,
      sep = ";"
    ) %>%
    dplyr::mutate(
      xg_for = xg * (event_team == away_name),
      xg_against = xg * (event_team == home_name),
      gf = ifelse(event_type == "GOAL" & event_team == away_name, 1, 0),
      ga = ifelse(event_type == "GOAL" & event_team == home_name, 1, 0),
      ff = ifelse(event_type != "BLOCKED_SHOT" & event_team == away_name, 1, 0),
      fa = ifelse(event_type != "BLOCKED_SHOT" & event_team == home_name, 1, 0)
    ) %>%
    dplyr::group_by(player_name = away_on_ice) %>%
    dplyr::summarise(
      team = paste(unique(away_name), collapse = ";"),
      gp = length(unique(game_id)),
      cf = sum(event_team == away_name),
      ca = sum(event_team == home_name),
      ff = sum(ff),
      fa = sum(fa),
      gf = sum(gf),
      ga = sum(ga),
      xgf = sum(xg_for, na.rm = TRUE),
      xga = sum(xg_against, na.rm = TRUE),
      .groups = "drop"
    )

  # combine
  onice_stats <- dplyr::bind_rows(home_stats, away_stats) %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarize(
      team = paste(unique(team), collapse = ";"),
      gp = sum(gp),
      cf = sum(cf),
      ca = sum(ca),
      cf_perc = 100 * cf / (cf + ca),
      ff = sum(ff),
      fa = sum(fa),
      ff_perc = 100 * ff / (ff + fa),
      gf = sum(gf),
      ga = sum(ga),
      gf_perc = 100 * gf / (gf + ga),
      xgf = sum(xgf),
      xga = sum(xga),
      xgf_perc = 100 * xgf / (xgf + xga),
      .groups = "drop"
    ) %>%
    # fix team to show last team played for
    tidyr::separate_rows(team, sep = ";") %>%
    dplyr::group_by(player_name) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    # default sort to xgf_perc
    dplyr::arrange(-xgf_perc) %>%
    # add player ids
    dplyr::left_join(ids, by = "player_name") %>%
    dplyr::select(player_name, player_id, dplyr::everything())

  if("CHANGE" %in% unique(pbp$event_type)){
    # get player time on ice
    toi_df <- calculate_toi(pbp) %>%
      # no need to duplicate games played column
      dplyr::select(-gp)

    onice_stats <- dplyr::left_join(
      onice_stats, toi_df, by = c("player_name","player_id")
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c(cf,ca,ff,fa,gf,ga,xgf,xga),
          .names = "{col}_per60",
          ~ .x / toi_minutes * 60
          )
      )
  }

  return(onice_stats)

}
