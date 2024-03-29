#' Prepare xG data
#'
#' @description Helper function to prepare hockeyR pbp data for xG calculations
#'
#' @param x A play-by-play data frame generated by hockeyR before xG is calculated
#'
#' @return A tibble; pbp data with xG model mutations along with identifiers for game and strength state
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' pbp <- load_pbp(2022) %>% dplyr::select(-xg)
#' model_data <- prepare_xg_data(pbp)
#' }
prepare_xg_data <- function(x){

  model_df <- x %>%
    # filter out shootouts
    dplyr::filter(period_type != "SHOOTOUT") %>%
    # remove penalty shots
    dplyr::filter(secondary_type != "Penalty Shot" | is.na(secondary_type)) %>%
    # remove shift change events, which were excluded from model
    dplyr::filter(event_type != "CHANGE") %>%
    # add model feature variables
    dplyr::group_by(game_id, period) %>%
    dplyr::mutate(
      last_event_type = dplyr::lag(event_type),
      last_event_team = dplyr::lag(event_team),
      time_since_last = game_seconds - dplyr::lag(game_seconds),
      last_x = dplyr::lag(x),
      last_y = dplyr::lag(y),
      distance_from_last = round(sqrt(((y - last_y)^2) + ((x - last_x)^2)),1),
      event_zone = dplyr::case_when(
        x >= -25 & x <= 25 ~ "NZ",
        (x_fixed < -25 & event_team_abbr == home_abbr) |
          (x_fixed > 25 & event_team_abbr == away_abbr) ~ "DZ",
        (x_fixed > 25 & event_team_abbr == home_abbr) |
          (x_fixed < -25 & event_team_abbr == away_abbr) ~ "OZ"
      ),
      last_event_zone = dplyr::lag(event_zone)
    ) %>%
    dplyr::ungroup() %>%
    # filter to only unblocked shots
    dplyr::filter(event_type %in% c("SHOT","MISSED_SHOT","GOAL")) %>%
    # get rid off oddball last_events
    #   ie "EARLY_INTERMISSION_START"
    dplyr::filter(
      last_event_type %in% c(
        "FACEOFF","GIVEAWAY","TAKEAWAY","BLOCKED_SHOT","HIT",
        "MISSED_SHOT","SHOT","STOP","PENALTY","GOAL"
      )
    ) %>%
    # add more feature variables
    dplyr::mutate(
      era_2011_2013 = ifelse(
        season %in% c("20102011","20112012","20122013"),
        1, 0
      ),
      era_2014_2018 = ifelse(
        season %in% c("20132014","20142015","20152016","20162017","20172018"),
        1, 0
      ),
      era_2019_2021 = ifelse(
        season %in% c("20182019","20192020","20202021"),
        1, 0
      ),
      era_2022_on = ifelse(
        as.numeric(season) > 20202021, 1, 0
      ),
      # these are only for the ST model
      event_team_skaters = ifelse(event_team_abbr == home_abbr, home_skaters, away_skaters),
      opponent_team_skaters = ifelse(event_team_abbr == home_abbr, away_skaters, home_skaters),
      total_skaters_on = event_team_skaters + opponent_team_skaters,
      event_team_advantage = event_team_skaters - opponent_team_skaters,
      # these are in 5v5 model
      rebound = ifelse(last_event_type %in% c("SHOT","MISSED_SHOT","GOAL") & time_since_last <= 2, 1, 0),
      rush = ifelse(last_event_zone %in% c("NZ","DZ") & time_since_last <= 4, 1, 0),
      cross_ice_event = ifelse(
        # indicates goalie had to move from one post to the other
        last_event_zone == "OZ" &
          ((last_y >  3 & y < -3) | (last_y < -3 & y > 3)) &
          # need some sort of time frame here to indicate shot was quick after goalie had to move
          time_since_last <= 2, 1, 0
      ),
      # fix missing empty net vars
      empty_net = ifelse(is.na(empty_net) | empty_net == FALSE, FALSE, TRUE),
      shot_type = secondary_type,
      goal = ifelse(event_type == "GOAL", 1, 0)
    ) %>%
    dplyr::select(season, game_id, event_id, strength_state, shot_distance, shot_angle, empty_net, last_event_type:goal) %>%
    # one-hot encode some categorical vars
    dplyr::mutate(type_value = 1, last_value = 1) %>%
    tidyr::pivot_wider(names_from = shot_type, values_from = type_value, values_fill = 0) %>%
    tidyr::pivot_wider(
      names_from = last_event_type, values_from = last_value, values_fill = 0, names_prefix = "last_"
    ) %>%
    janitor::clean_names() %>%
    dplyr::select(
      -last_event_team, -event_zone, -last_event_zone, -event_team_skaters, -opponent_team_skaters
    )

  if("na" %in% names(model_df)){
    model_df <- dplyr::select(model_df, -na)
  }

  missing_feats <- dplyr::tibble(
    feature = xg_model_5v5$feature_names
  ) %>%
    dplyr::filter(feature %not_in% names(model_df)) %>%
    dplyr::mutate(val = 0) %>%
    tidyr::pivot_wider(names_from = feature, values_from = val)

  if(length(missing_feats) > 0){
    model_df <- dplyr::bind_cols(model_df, missing_feats)
  }

  return(model_df)

}
