#' Scrape game play-by-play
#'
#' @param game_id Game ID to scrape

#' @description Scrapes play-by-play data for a specified game ID.
#'
#' @return A tibble containing event-based play-by-play data for an individual
#' NHL game. The resulting data will have columns for:
#' \describe{
#' \item{xg}{Numeric expected goal value for unblocked shot events}
#' \item{event}{String defining the event}
#' \item{event_type}{String with alternate event definition; in all caps}
#' \item{secondary_type}{String defining secondary event type}
#' \item{event_team}{String defining the primary team involved in the event}
#' \item{event_team_type}{String indicator of event team type: home or away}
#' \item{description}{String detailed description of event}
#' \item{period}{Integer value of the game period}
#' \item{period_seconds}{Numeric value of the seconds into the period of the event}
#' \item{period_seconds_remaining}{Numeric value of the seconds remaining in the period}
#' \item{game_seconds}{Numeric value of the seconds into the game of the event}
#' \item{game_seconds_remaining}{Numeric value of the seconds remaining in the game; negative for overtime periods}
#' \item{home_score}{Integer value of the home team score after the event}
#' \item{away_score}{Integer value of the away team score after the event}
#' \item{event_player_1_name}{String name of the primary event player}
#' \item{event_player_1_type}{String indicator for the role of event_player_1}
#' \item{event_player_2_name}{String name of the secondary event player}
#' \item{event_player_2_type}{String indicator for the role of event_player_2}
#' \item{event_player_3_name}{String name of the tertiary event player}
#' \item{event_player_3_type}{String indicator for the role of event_player_3}
#' \item{event_goalie_name}{String name of the goalie involved in the event}
#' \item{strength_code}{String indicator for game strength: EV, SH, or PP}
#' \item{strength}{String name for game strength: Even, Shorthanded, or Power Play}
#' \item{strength_state}{String name for detailed game strength in the form of '(event team skaters)v(opponent skaters)'}
#' \item{penalty_minutes}{Integer value of the penalty minutes on penalty events}
#' \item{penalty_severity}{String name for penalty severity: Minor or Major}
#' \item{num_on}{Integer value of the number of skaters substituted on during a shift change event}
#' \item{players_on}{String of player names substituted on during a shift change event}
#' \item{num_off}{Integer value of the number of skaters substituted off during a shift change event}
#' \item{players_off}{String of player names substituted off during a shift change event}
#' \item{extra_attacker}{Logical indicator of whether or not the event team had their goalie pulled}
#' \item{x}{Numeric x-coordinate of event in feet, with origin at center ice}
#' \item{y}{Numeric y-coordinate of event in feet, with origin at center ice}
#' \item{x_fixed}{Numeric transformed x-coordinate of event in feet, where the home team always shoots to the right, away team to the left}
#' \item{y_fixed}{Numeric transformed y-coordinate of event in feet, where the home team always shoots to the right, away team to the left}
#' \item{shot_distance}{Numeric distance (in feet) to center of net for unblocked shot events}
#' \item{shot_angle}{Numeric angle (in degrees) to center of net for unlocked shot events}
#' \item{home_skaters}{Numeric value for number of skaters on the ice for the home team, excluding the goalie}
#' \item{away_skaters}{Numeric value for number of skaters on the ice for the away team, excluding the goalie}
#' \item{home_on_1}{String name of home team player on ice}
#' \item{home_on_2}{String name of home team player on ice}
#' \item{home_on_3}{String name of home team player on ice}
#' \item{home_on_4}{String name of home team player on ice}
#' \item{home_on_5}{String name of home team player on ice}
#' \item{home_on_6}{String name of home team player on ice}
#' \item{home_on_7}{String name of home team player on ice}
#' \item{away_on_1}{String name of away team player on ice}
#' \item{away_on_2}{String name of away team player on ice}
#' \item{away_on_3}{String name of away team player on ice}
#' \item{away_on_4}{String name of away team player on ice}
#' \item{away_on_5}{String name of away team player on ice}
#' \item{away_on_6}{String name of away team player on ice}
#' \item{away_on_7}{String name of away team player on ice}
#' \item{home_goalie}{String name of home goalie on ice}
#' \item{away_goalie}{String name of away goalie on ice}
#' \item{game_id}{Integer value of assigned game ID}
#' \item{event_idx}{Numeric index for event}
#' \item{event_id}{Numeric id for event -- more specified than event_idx}
#' \item{event_player_1_id}{Integer value of the player ID for the primary event player}
#' \item{event_player_1_link}{String value of the NHL.com player link for the primary event player}
#' \item{event_player_1_season_total}{Integer value for the total events for the primary event player this season}
#' \item{event_player_2_id}{Integer value of the player ID for the secondary event player}
#' \item{event_player_2_link}{String value of the NHL.com player link for the secondary event player}
#' \item{event_player_2_season_total}{Integer value for the total events for the secondary event player this season}
#' \item{event_player_3_id}{Integer value of the player ID for the tertiary event player}
#' \item{event_player_3_link}{String value of the NHL.com player link for the tertiary event player}
#' \item{event_player_3_season_total}{Integer value for the total events for the tertiary event player this season}
#' \item{event_goalie_id}{Integer value of the player ID for the event goalie}
#' \item{event_goalie_link}{String value of the NHL.com player link for the event goalie}
#' \item{event_goalie_type}{String indicator for the role of the event_goalie}
#' \item{game_winning_goal}{Logical indicator of whether or not the goal scored was the game-winning goal}
#' \item{empty_net}{Logical indicator of whether or not the goal scored was on an empty net}
#' \item{period_type}{String name of period type: REGULAR, OVERTIME, or SHOOTOUT}
#' \item{ordinal_num}{String name of the ordinal period: 1st, 2nd, 3rd, 4th...}
#' \item{period_time}{String value of the time into the period of the event}
#' \item{period_time_remaining}{String value of the time remaining in the period}
#' \item{date_time}{String value of the real-world timestamp of the event}
#' \item{event_team_id}{Integer value of the NHL ID of \code{event_team}}
#' \item{event_team_link}{String value of the NHL.com team link for the \code{event_team}}
#' \item{event_team_abbr}{String value of the 3-letter NHL abbreviation for the \code{event_team}}
#' \item{home_final}{Integer value of the final score for the home team}
#' \item{away_final}{Integer value of the final score for the away team}
#' \item{season}{String value of the official NHL season}
#' \item{season_type}{String indicator of season type: R, or P}
#' \item{game_date}{Date of game}
#' \item{game_start}{Date time of start of game in US/Eastern time zone}
#' \item{game_end}{Date time of end of game in US/Eastern time zone}
#' \item{game_length}{Period value of length of game, in hours:minutes}
#' \item{game_state}{String indicator of state of game}
#' \item{detailed_state}{String indicator of detailed game state}
#' \item{venue_id}{Integer value of the NHL ID for the venue}
#' \item{venue_name}{String name of the game venue}
#' \item{venue_link}{String value of the NHL.com link for the venue}
#' \item{home_name}{String name of the home team}
#' \item{home_abbreviation}{String value of the 3-letter NHL abbreviation of the home team}
#' \item{home_division_name}{String value of the name of the NHL division of the home team}
#' \item{home_conference_name}{String value of the name of the NHL conference of the home team}
#' \item{home_id}{Integer value of the NHL ID of the home team}
#' \item{away_name}{String name of the away team}
#' \item{away_abbreviation}{String value of the 3-letter NHL abbreviation of the away team}
#' \item{away_division_name}{String value of the name of the NHL division of the away team}
#' \item{away_conference_name}{String value of the name of the NHL conference of the away team}
#' \item{away_id}{Integer value of the NHL ID of the away team}
#'}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pbp <- scrape_game(2020020420)
#' }
scrape_game <- function(game_id){

  # load team abbreviations
  team_info <- team_logos_colors

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
    stop(paste("Could not get play-by-play for game ID",game_id))
  }

  game_info <- get_game_info(game_id)

  teams <- game_info %>%
    dplyr::select(event_team = home_abbr, event_team_id = home_id) %>%
    dplyr::mutate(team_type = "home") %>%
    dplyr::bind_rows(
      game_info %>%
        dplyr::select(event_team = away_abbr, event_team_id = away_id) %>%
        dplyr::mutate(team_type = "away")
    )

  rosters <- get_game_rosters(game_id)

  corsi_events <- c("MISSED_SHOT","SHOT","GOAL","BLOCKED_SHOT")
  fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")

  # unnest game plays
  plays <- site$plays %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::select(-c(typeCode, periodDescriptor)) %>%
    tidyr::unnest_wider(details)

  # check for missing columns, ie 'servedByPlayerId` not always there
  # especially common for live games
  int_columns <- c(
    "eventOwnerTeamId", "period", "homeScore", "awayScore",
    "scoringPlayerId", "shootingPlayerId", "hittingPlayerId", "winningPlayerId",
    "committedByPlayerId", "playerId", "assist1PlayerId", "blockingPlayerId",
    "goalieInNetId", "hitteePlayerId", "losingPlayerId", "drawnByPlayerId",
    "assist2PlayerId", "goalieInNetId", "servedByPlayerId",
    "xCoord", "yCoord"
  )

  char_columns <- c(
    "typeDescKey", "shotType", "descKey", "timeInPeriod", "timeRemaining",
    "homeTeamDefendingSide", "typeCode", "situationCode"
  )

  for(i in int_columns){
    if(i %not_in% names(plays)){
      plays[, i] <- NA_integer_
    }
  }

  for(i in char_columns){
    if(i %not_in% names(plays)){
      plays[, i] <- NA_character_
    }
  }

  # clean it up
  pbp <- plays %>%
    dplyr::bind_cols(game_info) %>%
    dplyr::mutate(
      # change event team on blocked shots to shooting team
      eventOwnerTeamId = dplyr::case_when(
        typeDescKey == "blocked-shot" & eventOwnerTeamId == home_id ~ away_id,
        typeDescKey == "blocked-shot" & eventOwnerTeamId == away_id ~ home_id,
        TRUE ~ eventOwnerTeamId
      ),
      # trying to match the old pbp output for event_type
      event_type = toupper(gsub("-","_",typeDescKey)),
      event_type = dplyr::case_when(
        event_type == "SHOT_ON_GOAL" ~ "SHOT",
        event_type == "STOPPAGE" ~ "STOP",
        TRUE ~ event_type
      ),
      secondary_type = dplyr::case_when(
        !is.na(shotType) ~ shotType,
        !is.na(descKey) ~ descKey,
        TRUE ~ NA_character_
      ),
      # event team
      event_team_id = eventOwnerTeamId,
      event_team_type = dplyr::case_when(
        event_team_id == game_info$home_id ~ "home",
        event_team_id == game_info$away_id ~ "away",
        TRUE ~ NA_character_
      ),
      # clean up the times
      period_seconds = lubridate::period_to_seconds(lubridate::ms(timeInPeriod)),
      game_seconds = period_seconds + (1200 * (period-1)),
      period_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(timeRemaining)),
      game_seconds_remaining = ifelse(
        period < 4,
        ((3-period) * 1200) + period_seconds_remaining,
        0 - period_seconds
      ),
      # final scores
      home_final = dplyr::last(homeScore, na_rm = TRUE),
      away_final = dplyr::last(awayScore, na_rm = TRUE),
      # event players
      event_player_1_id = dplyr::coalesce(
        scoringPlayerId, #  scorer
        shootingPlayerId, # shooter
        hittingPlayerId, #  hitter
        winningPlayerId, #  faceoff winner
        committedByPlayerId, # penalty on
        playerId #          takeaway/giveaway player
      ),
      event_player_2_id = dplyr::coalesce(
        assist1PlayerId, #  primary assist
        blockingPlayerId, # shot blocker
        goalieInNetId, #    goalie for shot
        hitteePlayerId, #   got hit
        losingPlayerId, #   faceoff loser
        drawnByPlayerId #   penalty drawer
      ),
      event_player_3_id = dplyr::case_when(
        # assign as secondary assist player or goalie on goals
        # served by on bench minors
        #   NA otherwise
        !is.na(assist1PlayerId) & is.na(assist2PlayerId) ~ goalieInNetId,
        typeDescKey == "penalty" & is.na(committedByPlayerId) ~ servedByPlayerId,
        TRUE ~ assist2PlayerId
      ),
      event_player_4_id = ifelse(
        # goalie for goals with 2 assists
        !is.na(assist2PlayerId),
        goalieInNetId,
        NA_integer_
      ),
      event_player_1_type = dplyr::case_when(
        typeDescKey == "goal" ~ "Scorer",
        typeDescKey %in% c("shot-on-goal","missed-shot","blocked-shot") ~ "Shooter",
        typeDescKey == "faceoff" ~ "Winner",
        typeDescKey == "hit" ~ "Hitter",
        typeDescKey == "penalty" ~ "PenaltyOn",
        TRUE ~ NA_character_
      ),
      event_player_2_type = dplyr::case_when(
        typeDescKey == "goal" & !is.na(assist1PlayerId) ~ "Assist",
        typeDescKey %in% c("shot-on-goal","missed-shot") ~ "Goalie",
        typeDescKey == "blocked-shot" ~ "Blocker",
        typeDescKey == "faceoff" ~ "Loser",
        typeDescKey == "hit" ~ "Hittee",
        typeDescKey == "penalty" ~ "DrewBy",
        TRUE ~ NA_character_
      ),
      event_player_3_type = dplyr::case_when(
        typeDescKey == "goal" & !is.na(assist2PlayerId) ~ "Assist",
        typeDescKey == "goal" & is.na(assist2PlayerId) & !is.na(assist1PlayerId) ~ "Goalie",
        typeDescKey == "penalty" & is.na(committedByPlayerId) ~ "ServedBy",
        TRUE ~ NA_character_
      ),
      event_player_4_type = ifelse(!is.na(event_player_4_id), "Goalie", NA_character_),
      # event goalie column
      event_goalie_id = dplyr::case_when(
        event_player_2_type == "Goalie" ~ event_player_2_id,
        event_player_3_type == "Goalie" ~ event_player_3_id,
        event_player_4_type == "Goalie" ~ event_player_4_id,
        TRUE ~ NA_integer_
      ),
      # fixed coordinates so home team always shoots right
      x_fixed = dplyr::case_when(
        event_team_type == "home" & homeTeamDefendingSide == "right" ~ 0 - xCoord,
        event_team_type == "away" & homeTeamDefendingSide == "right" ~ 0 - xCoord,
        TRUE ~ xCoord
      ),
      y_fixed = dplyr::case_when(
        event_team_type == "home" & homeTeamDefendingSide == "right" ~ 0 - yCoord,
        event_team_type == "away" & homeTeamDefendingSide == "right" ~ 0 - yCoord,
        TRUE ~ yCoord
      ),
      # add shot distance/angle
      shot_distance = dplyr::case_when(
        event_team_type == "home" & event_type %in% fenwick_events ~
          round(abs(sqrt((x_fixed - 89)^2 + (y_fixed)^2)),1),
        event_team_type == "away" & event_type %in% fenwick_events ~
          round(abs(sqrt((x_fixed - (-89))^2 + (y_fixed)^2)),1),
        TRUE ~ NA_real_
      ),
      shot_angle = dplyr::case_when(
        event_team_type == "home" & event_type %in% fenwick_events ~
          round(abs(atan((0-y_fixed) / (89-x_fixed)) * (180 / pi)),1),
        event_team_type == "away" & event_type %in% fenwick_events ~
          round(abs(atan((0-y_fixed) / (-89-x_fixed)) * (180 / pi)),1),
        TRUE ~ NA_real_
      ),
      # fix behind the net angles
      shot_angle = ifelse(
        (event_team_type == "home" & x_fixed > 89) |
          (event_team_type == "away" & x_fixed < -89),
        180 - shot_angle,
        shot_angle
      ),
      penalty_severity = dplyr::case_when(
        typeCode == "MIN" ~ "Minor",
        typeCode == "MAJ" ~ "Major",
        typeCode == "BEN" ~ "Bench Minor",
        TRUE ~ NA_character_
      ),
      home_skaters = as.integer(substr(situationCode, 3, 3)),
      away_skaters = as.integer(substr(situationCode, 2, 2)),
      home_goalie_in = as.integer(substr(situationCode, 4, 4)),
      away_goalie_in = as.integer(substr(situationCode, 1, 1)),
      # does event team have their goalie pulled?
      extra_attacker = dplyr::case_when(
        event_team_id == home_id & home_goalie_in == 0 ~ TRUE,
        event_team_id == away_id & away_goalie_in == 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      # is the shooting team shooting at an empty net?
      empty_net = dplyr::case_when(
        event_team_id == home_id & away_goalie_in == 0 & event_type %in% corsi_events ~ TRUE,
        event_team_id == away_id & home_goalie_in == 0 & event_type %in% corsi_events ~ TRUE,
        TRUE ~ FALSE
      ),
      penalty_minutes = duration
    ) %>%
    # add event team abbreviation
    dplyr::left_join(
      teams %>%
        dplyr::select(event_team_abbr = event_team,
                      event_team_id),
      by = "event_team_id"
    ) %>%
    # add event team name
    dplyr::left_join(
      team_logos_colors %>%
        dplyr::select(event_team = full_team_name,
                      event_team_abbr = team_abbr),
      by = "event_team_abbr"
    ) %>%
    # add event player names
    dplyr::left_join(
      rosters %>%
        dplyr::select(event_player_1_id = player_id, event_player_1_name = player_name),
      by = "event_player_1_id"
    ) %>%
    dplyr::left_join(
      rosters %>%
        dplyr::select(event_player_2_id = player_id, event_player_2_name = player_name),
      by = "event_player_2_id"
    ) %>%
    dplyr::left_join(
      rosters %>%
        dplyr::select(event_player_3_id = player_id, event_player_3_name = player_name),
      by = "event_player_3_id"
    ) %>%
    dplyr::left_join(
      rosters %>%
        dplyr::select(event_player_4_id = player_id, event_player_4_name = player_name),
      by = "event_player_4_id"
    ) %>%
    dplyr::left_join(
      rosters %>%
        dplyr::select(event_goalie_id = player_id, event_goalie_name = player_name),
      by = "event_goalie_id"
    ) %>%
    dplyr::select(
      event_id = eventId, event_type, event = typeDescKey, secondary_type,
      event_team, event_team_type, period, period_seconds, period_seconds_remaining,
      game_seconds, game_seconds_remaining,
      home_score = homeScore, away_score = awayScore,
      event_player_1_name, event_player_1_type, event_player_2_name,
      event_player_2_type, event_player_3_name, event_player_3_type,
      event_goalie_name,
      penalty_severity, penalty_minutes,
      x = xCoord, y = yCoord, x_fixed, y_fixed,
      shot_distance, shot_angle,
      home_skaters, away_skaters, extra_attacker,
      dplyr::everything()
    )

  # add shift events

  shifts <- get_game_shifts(game_id)

  # if shift data exists
  if(!is.null(shifts)) {
    # MODIFIED FROM THE SCRAPER BY EVOLVING HOCKEY
    # WHO MODIFIED ORIGINAL CODE BY MANNY PERRY

    # combine shift events with pbp events
    pbp_full <- pbp %>%
      dplyr::bind_rows(shifts) %>%
      dplyr::mutate(
        # arrange all events so shift changes are in the proper spot
        priority =
          1 * (event_type %in% c("TAKEAWAY", "GIVEAWAY", "MISSED_SHOT", "HIT", "SHOT", "BLOCKED_SHOT") & !(period == 5 & season_type == "R")) +
          2 * (event_type == "GOAL" & !(period == 5 & season_type == "R")) +
          3 * (event_type == "STOP" & !(period == 5 & season_type == "R")) +
          4 * (event_type == "PENALTY" & !(period == 5 & season_type == "R")) +
          5 * (event_type == "CHANGE" & !(period == 5 & season_type == "R")) +
          6 * (event_type == "PERIOD_END" & !(period == 5 & season_type == "R")) +
          7 * (event_type == "GAME_END" & !(period == 5 & season_type == "R")) +
          8 * (event_type == "FACEOFF" &  !(period == 5 & season_type == "R"))
      ) %>%
      dplyr::arrange(period,game_seconds, priority) %>%
      dplyr::mutate(
        home_index = as.numeric(cumsum(event_type == "CHANGE" &
                                         event_team_abbr == unique(pbp$home_abbr))),
        away_index = as.numeric(cumsum(event_type == "CHANGE" &
                                         event_team_abbr == unique(pbp$away_abbr))),
      ) %>%
      dplyr::select(-priority)

    # construct a matrix of player names as columns & event row as rows
    home_skaters <- NULL

    for(i in 1:nrow(rosters)){

      player <- as.character(rosters$player_id[i])

      skaters_i <- dplyr::tibble(
        on_ice = cumsum(
          1 * stringr::str_detect(
            dplyr::filter(pbp_full,
                          event_type == "CHANGE" &
                            event_team_abbr == unique(pbp$home_abbr))$ids_on,
            player) -
            1 * stringr::str_detect(
              dplyr::filter(pbp_full,
                            event_type == "CHANGE" &
                              event_team_abbr == unique(pbp$home_abbr))$ids_off,
              player)
        )
      )

      suppressMessages({
        home_skaters <- dplyr::bind_cols(home_skaters, skaters_i)
      })

      rm(skaters_i, player)
    }

    colnames(home_skaters) <- rosters$player_id

    home_skaters <- data.frame(home_skaters)

    # transform into matrix of only players on the ice
    on_home <- which(home_skaters == 1, arr.ind = TRUE) %>%
      data.frame() %>%
      dplyr::group_by(row) %>%
      dplyr::summarize(
        home_on_1 = colnames(home_skaters)[unique(col)[1]],
        home_on_2 = colnames(home_skaters)[unique(col)[2]],
        home_on_3 = colnames(home_skaters)[unique(col)[3]],
        home_on_4 = colnames(home_skaters)[unique(col)[4]],
        home_on_5 = colnames(home_skaters)[unique(col)[5]],
        home_on_6 = colnames(home_skaters)[unique(col)[6]],
        home_on_7 = colnames(home_skaters)[unique(col)[7]]
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = home_on_1:home_on_7,
          ~as.integer(gsub("X","",.x))
        )
      )

    away_skaters <- NULL

    for(i in 1:nrow(rosters)){

      player <- as.character(rosters$player_id[i])

      skaters_i <- dplyr::tibble(
        on_ice = cumsum(
          1 * stringr::str_detect(
            dplyr::filter(pbp_full,
                          event_type == "CHANGE" &
                            event_team_abbr == unique(pbp$away_abbr))$ids_on,
            player) -
            1 * stringr::str_detect(
              dplyr::filter(pbp_full,
                            event_type == "CHANGE" &
                              event_team_abbr == unique(pbp$away_abbr))$ids_off,
              player)
        )
      )

      suppressMessages({
        away_skaters <- dplyr::bind_cols(away_skaters, skaters_i)
      })

      rm(skaters_i, player)
    }

    colnames(away_skaters) <- rosters$player_id

    away_skaters <- data.frame(away_skaters)

    # transform into matrix of only players on the ice
    on_away <- which(away_skaters == 1, arr.ind = TRUE) %>%
      data.frame() %>%
      dplyr::group_by(row) %>%
      dplyr::summarize(
        away_on_1 = colnames(away_skaters)[unique(col)[1]],
        away_on_2 = colnames(away_skaters)[unique(col)[2]],
        away_on_3 = colnames(away_skaters)[unique(col)[3]],
        away_on_4 = colnames(away_skaters)[unique(col)[4]],
        away_on_5 = colnames(away_skaters)[unique(col)[5]],
        away_on_6 = colnames(away_skaters)[unique(col)[6]],
        away_on_7 = colnames(away_skaters)[unique(col)[7]]
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = away_on_1:away_on_7,
          ~as.integer(gsub("X","",.x))
        )
      )

    # define goalies
    goalies <- rosters %>%
      dplyr::filter(position == "G") %>%
      #dplyr::mutate(
      #  player_name = stringr::str_replace(player_name, " ", ".")
      #) %>%
      #dplyr::mutate(
      #  player_name = stringr::str_replace(player_name, "-", ".")
      #) %>%
      dplyr::pull(player_id)

    non_plays <- c("PERIOD_START","PERIOD_END","GAME_END")

    pbp_full <- pbp_full %>%
      dplyr::left_join(on_home, by = c("home_index" = "row")) %>%
      dplyr::left_join(on_away, by = c("away_index" = "row")) %>%
      # adding game info to shift change events and moving info to the end
      dplyr::select(!dplyr::all_of(names(game_info))) %>%
      dplyr::bind_cols(game_info) %>%
      dplyr::mutate(
        # create goalie on-ice columns
        home_goalie = dplyr::case_when(
          home_on_1 %in% goalies ~ home_on_1,
          home_on_2 %in% goalies ~ home_on_2,
          home_on_3 %in% goalies ~ home_on_3,
          home_on_4 %in% goalies ~ home_on_4,
          home_on_5 %in% goalies ~ home_on_5,
          home_on_6 %in% goalies ~ home_on_6,
          home_on_7 %in% goalies ~ home_on_7
        ),
        away_goalie = dplyr::case_when(
          away_on_1 %in% goalies ~ away_on_1,
          away_on_2 %in% goalies ~ away_on_2,
          away_on_3 %in% goalies ~ away_on_3,
          away_on_4 %in% goalies ~ away_on_4,
          away_on_5 %in% goalies ~ away_on_5,
          away_on_6 %in% goalies ~ away_on_6,
          away_on_7 %in% goalies ~ away_on_7
        ),
        # include only skaters in on-ice columns
        # rosters are ordered such that goalies should always be
        # last on-ice skater
        home_on_1 = ifelse(home_on_1 == home_goalie & !is.na(home_goalie), NA, home_on_1),
        home_on_2 = ifelse(home_on_2 == home_goalie & !is.na(home_goalie), NA, home_on_2),
        home_on_3 = ifelse(home_on_3 == home_goalie & !is.na(home_goalie), NA, home_on_3),
        home_on_4 = ifelse(home_on_4 == home_goalie & !is.na(home_goalie), NA, home_on_4),
        home_on_5 = ifelse(home_on_5 == home_goalie & !is.na(home_goalie), NA, home_on_5),
        home_on_6 = ifelse(home_on_6 == home_goalie & !is.na(home_goalie), NA, home_on_6),
        home_on_7 = ifelse(home_on_7 == home_goalie & !is.na(home_goalie), NA, home_on_7),
        away_on_1 = ifelse(away_on_1 == away_goalie & !is.na(away_goalie), NA, away_on_1),
        away_on_2 = ifelse(away_on_2 == away_goalie & !is.na(away_goalie), NA, away_on_2),
        away_on_3 = ifelse(away_on_3 == away_goalie & !is.na(away_goalie), NA, away_on_3),
        away_on_4 = ifelse(away_on_4 == away_goalie & !is.na(away_goalie), NA, away_on_4),
        away_on_5 = ifelse(away_on_5 == away_goalie & !is.na(away_goalie), NA, away_on_5),
        away_on_6 = ifelse(away_on_6 == away_goalie & !is.na(away_goalie), NA, away_on_6),
        away_on_7 = ifelse(away_on_7 == away_goalie & !is.na(away_goalie), NA, away_on_7)
      ) %>%
      # rename to remark that we're using IDs there not names
      dplyr::rename_with(
        .cols = c(dplyr::starts_with("home_on"),dplyr::starts_with("away_on"),home_goalie,away_goalie),
        ~paste0(.x,"_id")
      ) %>%
      # add player names to on ice columns
      dplyr::left_join(
        dplyr::select(rosters, home_on_1 = player_name, home_on_1_id = player_id),
        by = "home_on_1_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, home_on_2 = player_name, home_on_2_id = player_id),
        by = "home_on_2_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, home_on_3 = player_name, home_on_3_id = player_id),
        by = "home_on_3_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, home_on_4 = player_name, home_on_4_id = player_id),
        by = "home_on_4_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, home_on_5 = player_name, home_on_5_id = player_id),
        by = "home_on_5_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, home_on_6 = player_name, home_on_6_id = player_id),
        by = "home_on_6_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, home_on_7 = player_name, home_on_7_id = player_id),
        by = "home_on_7_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_on_1 = player_name, away_on_1_id = player_id),
        by = "away_on_1_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_on_2 = player_name, away_on_2_id = player_id),
        by = "away_on_2_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_on_3 = player_name, away_on_3_id = player_id),
        by = "away_on_3_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_on_4 = player_name, away_on_4_id = player_id),
        by = "away_on_4_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_on_5 = player_name, away_on_5_id = player_id),
        by = "away_on_5_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_on_6 = player_name, away_on_6_id = player_id),
        by = "away_on_6_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_on_7 = player_name, away_on_7_id = player_id),
        by = "away_on_7_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, home_goalie = player_name, home_goalie_id = player_id),
        by = "home_goalie_id"
      ) %>%
      dplyr::left_join(
        dplyr::select(rosters, away_goalie = player_name, away_goalie_id = player_id),
        by = "away_goalie_id"
      ) %>%
      tidyr::fill(
        c(home_skaters, away_skaters), .direction = "updown"
      ) %>%
      dplyr::mutate(
        # create strength states
        #home_skaters =
        #  1 * (!is.na(home_on_1)) + 1 * (!is.na(home_on_2)) +
        #  1 * (!is.na(home_on_3)) + 1 * (!is.na(home_on_4)) +
        #  1 * (!is.na(home_on_5)) + 1 * (!is.na(home_on_6)) +
        #  1 * (!is.na(home_on_7)),
        #away_skaters =
        #  1 * (!is.na(away_on_1)) + 1 * (!is.na(away_on_2)) +
        #  1 * (!is.na(away_on_3)) + 1 * (!is.na(away_on_4)) +
        #  1 * (!is.na(away_on_5)) + 1 * (!is.na(away_on_6)) +
        #  1 * (!is.na(away_on_7)),
        strength_state = dplyr::case_when(
          event_team_abbr == home_abbr ~ glue::glue("{home_skaters}v{away_skaters}"),
          event_team_abbr == away_abbr ~ glue::glue("{away_skaters}v{home_skaters}"),
          TRUE ~ glue::glue("{home_skaters}v{away_skaters}")
        ),
        strength_code = dplyr::case_when(
          home_skaters == away_skaters ~ "EV",
          (home_skaters < away_skaters & event_team_abbr == home_abbr) |
            (away_skaters < home_skaters & event_team_abbr == away_abbr) ~ "SH",
          (home_skaters < away_skaters & event_team_abbr == away_abbr) |
            (away_skaters < home_skaters & event_team_abbr == home_abbr) ~ "PP"
        ),
        # fixing the change events at start and end of periods
        strength_code = ifelse(
          event_type %in% non_plays |
            dplyr::lead(event_type) %in% non_plays |
            dplyr::lead(dplyr::lead(event_type)) %in% non_plays |
            dplyr::lag(event_type) %in% non_plays |
            dplyr::lag(dplyr::lag(event_type)) %in% non_plays,
          NA_character_, strength_code
        ),
        strength = dplyr::case_when(
          strength_code == "EV" ~ "Even",
          strength_code == "SH" ~ "Shorthanded",
          strength_code == "PP" ~ "Power Play"
        )
      )

    pbp_full <- pbp_full %>%
      dplyr::mutate(
        event_idx = dplyr::row_number()-1,
        description = dplyr::case_when(
          event_type == "PERIOD_START" ~ glue::glue("Start of Period {period}"),
          event_type == "PERIOD_END" ~ glue::glue("End of Period {period}"),
          event_type == "GAME_END" ~ "Game End",
          event_type == "FACEOFF" ~ glue::glue("{event_player_1_name} faceoff won against {event_player_2_name}"),
          event_type == "BLOCKED_SHOT" ~ glue::glue("{event_player_1_name} shot blocked by {event_player_2_name}"),
          event_type == "CHANGE" ~ glue::glue("ON: {players_on}; OFF: {players_off}"),
          event_type == "GIVEAWAY" ~ glue::glue("Giveaway by {event_player_1_name}"),
          event_type == "TAKEAWAY" ~ glue::glue("Takeaway by {event_player_1_name}"),
          event_type == "HIT" ~ glue::glue("{event_player_1_name} hit {event_player_2_name}"),
          event_type == "MISSED_SHOT" ~ glue::glue("{event_player_1_name} shot missed wide of net"),
          event_type == "PENALTY" & typeCode != "BEN" ~ glue::glue("{event_player_1_name} {secondary_type} against {event_player_2_name}"),
          event_type == "PENALTY" & typeCode == "BEN" ~ glue::glue("{event_team_abbr} bench minor for {secondary_type}"),
          event_type == "SHOT" ~ glue::glue("{event_player_1_name} shot on goal saved by {event_goalie_name}"),
          event_type == "STOP" & secondaryReason == "tv-timeout" ~ glue::glue("Stoppage in play ({reason}) - TV timeout"),
          event_type == "STOP" & secondaryReason == "player-injury" ~ "Stoppage in play (player injury)",
          event_type == "STOP" & secondaryReason %not_in% c("tv-timeout","player-injury") ~ glue::glue("Stoppage in play ({reason})"),
          event_type == "GOAL" & !is.na(event_player_4_id) ~ glue::glue("{event_player_1_name} {secondary_type}, assists: {event_player_2_name}, {event_player_3_name}"),
          event_type == "GOAL" & is.na(event_player_4_id) & !is.na(event_player_3_id) ~ glue::glue("{event_player_1_name} {secondary_type}, assists: {event_player_2_name}"),
          event_type == "GOAL" & is.na(event_player_3_id) ~ glue::glue("{event_player_1_name} {secondary_type}, unassisted")
        ),
        secondary_type = ifelse(
          event_type == "CHANGE",
          "On the fly",
          secondary_type
        ),
        secondary_type = ifelse(
          event_type == "CHANGE" &
            (dplyr::lead(event_type) == "FACEOFF" |
               dplyr::lag(event_type) == "STOP" |
               dplyr::lag(event_type) == "GOAL" |
               dplyr::lead(event_type) == "PERIOD_END" |
               dplyr::lag(event_type) == "PERIOD_END" |
               dplyr::lead(dplyr::lead(event_type)) == "PERIOD_END" |
               dplyr::lag(dplyr::lag(event_type)) == "PERIOD_END"),
          "Line change",
          secondary_type
        )
      ) %>%
      # remove unnecessary columns
      dplyr::select(-event_id,-home_index,-away_index)

    # reorder the columns
    if("event_player_3_name" %in% names(pbp_full)){
      pbp_full <- pbp_full %>%
        # change event player names to match on-ice player name conventions
        #dplyr::mutate_at(c("event_player_1_name","event_player_2_name",
        #                   "event_player_3_name","event_goalie_name"),
        #                 ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))) %>%
        dplyr::select(
          event_type, event, secondary_type, event_team, event_team_type,
          description, period, period_seconds, period_seconds_remaining,
          game_seconds, game_seconds_remaining, home_score, away_score,
          event_player_1_name, event_player_1_type, event_player_2_name,
          event_player_2_type, event_player_3_name, event_player_3_type,
          event_goalie_name, strength_state, strength_code:event_idx,
          num_on, players_on, num_off, players_off, extra_attacker,
          x, y, x_fixed, y_fixed, shot_distance, shot_angle,
          home_skaters, away_skaters, home_on_1:away_on_7,
          home_goalie, away_goalie, game_id, event_idx,
          tidyselect::everything()
        )
    } else {
      pbp_full <- pbp_full %>%
        #dplyr::mutate_at(c("event_player_1_name","event_player_2_name",
        #                   "event_goalie_name"),
        #                 ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))) %>%
        dplyr::select(
          event_type, event, secondary_type, event_team, event_team_type,
          description, period, period_seconds, period_seconds_remaining,
          game_seconds, game_seconds_remaining, home_score, away_score,
          event_player_1_name, event_player_1_type, event_player_2_name,
          event_player_2_type,
          event_goalie_name, strength_state, strength_code:event_idx,
          num_on, players_on, num_off, players_off, extra_attacker,
          x, y, x_fixed, y_fixed, shot_distance, shot_angle,
          home_skaters, away_skaters, home_on_1:away_on_7,
          home_goalie, away_goalie, game_id, event_idx,
          tidyselect::everything()
        )
    }


    # fill in current score for shift events
    #   first assign score of 0 to start of game to fix issue with
    #   game ID 2018020965, which is missing the "GAME_SCHEDULED" event
    pbp_full$home_score[1] <- 0
    pbp_full$away_score[1] <- 0
    pbp_full$home_score <- zoo::na.locf(pbp_full$home_score)
    pbp_full$away_score <- zoo::na.locf(pbp_full$away_score)

    # fix period type for shift events
    pbp_full <- pbp_full %>%
      dplyr::mutate(
        period_type = dplyr::case_when(
          period < 4 ~ "REGULAR",
          season_type %in% c("R","PR") & period == 4 ~ "OVERTIME",
          season_type %in% c("R","PR") & period == 5 ~ "SHOOTOUT",
          season_type == "P" & period > 3 ~ "OVERTIME"
        ),
        ordinal_num = dplyr::case_when(
          period == 1 ~ glue::glue("{period}st"),
          period == 2 ~ glue::glue("{period}nd"),
          period == 3 ~ glue::glue("{period}rd"),
          TRUE ~ glue::glue("{period}th")
        )
      )

  } else if(is.null(shifts) & "x" %in% names(pbp)){

    # no shift data available but shot location available
    # ie preseason

    pbp_full <- pbp %>%
      dplyr::select(-event_id,-event_code) %>%
      # add fixed x & y coordinates so home team shoots right, away shoots left
      dplyr::group_by(event_team, period, game_id) %>%
      # find median x shot coordinate to tell us which side teams are shooting on
      dplyr::mutate(med_x = stats::median(x[event_type %in% fenwick_events], na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        x_fixed = dplyr::case_when(
          event_team == home_name & med_x > 0 ~ x,
          event_team == home_name & med_x < 0 ~ 0 - x,
          event_team == away_name & med_x > 0 ~ 0 - x,
          event_team == away_name & med_x < 0 ~ x
        ),
        y_fixed = dplyr::case_when(
          event_team == home_name & med_x > 0 ~ y,
          event_team == home_name & med_x < 0 ~ 0 - y,
          event_team == away_name & med_x > 0 ~ 0 - y,
          event_team == away_name & med_x < 0 ~ y
        ),
        # add shot distance/angle
        shot_distance = dplyr::case_when(
          event_team == home_name & event_type %in% fenwick_events ~
            round(abs(sqrt((x_fixed - 89)^2 + (y_fixed)^2)),1),
          event_team == away_name & event_type %in% fenwick_events ~
            round(abs(sqrt((x_fixed - (-89))^2 + (y_fixed)^2)),1)
        ),
        shot_angle = dplyr::case_when(
          event_team == home_name & event_type %in% fenwick_events ~
            round(abs(atan((0-y_fixed) / (89-x_fixed)) * (180 / pi)),1),
          event_team == away_name & event_type %in% fenwick_events ~
            round(abs(atan((0-y_fixed) / (-89-x_fixed)) * (180 / pi)),1)
        ),
        # fix behind the net angles
        shot_angle = ifelse(
          (event_team == home_name & x_fixed > 89) |
            (event_team == away_name & x_fixed < -89),
          180 - shot_angle,
          shot_angle
        ),
        event_team_type =  dplyr::case_when(
          event_team == home_name ~ "home",
          event_team == away_name ~ "away"
        )
      ) %>%
      dplyr::select(-med_x)

    if("event_player_3_name" %in% names(pbp_full)) {
      pbp_full <- pbp_full %>%
        #dplyr::mutate_at(
        #  c("event_player_1_name","event_player_2_name",
        #      "event_player_3_name","event_goalie_name"),
        #  ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
        #  ) %>%
        dplyr::select(
          event_type, event, secondary_type, event_team, event_team_type,
          description, period, period_seconds, period_seconds_remaining,
          game_seconds, game_seconds_remaining, home_score, away_score,
          event_player_1_name, event_player_1_type, event_player_2_name,
          event_player_2_type, event_player_3_name, event_player_3_type,
          event_goalie_name, penalty_severity:strength, x, y, x_fixed, y_fixed,
          shot_distance, shot_angle,
          game_id, event_idx,
          tidyselect::everything()
        )
    } else {
      pbp_full <- pbp_full %>%
        #dplyr::mutate_at(
        #  c("event_player_1_name","event_player_2_name",
        #      "event_goalie_name"),
        #  ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
        #  ) %>%
        dplyr::select(
          event_type, event, secondary_type, event_team, event_team_type,
          description, period, period_seconds, period_seconds_remaining,
          game_seconds, game_seconds_remaining, home_score, away_score,
          event_player_1_name, event_player_1_type, event_player_2_name,
          event_player_2_type,
          event_goalie_name, penalty_severity:strength, x, y, x_fixed, y_fixed,
          shot_distance, shot_angle,
          game_id, event_idx,
          tidyselect::everything()
        )
    }

  } else {
    # no shift data or shot location
    if("event_player_3_name" %in% names(pbp)){
      pbp_full <- pbp %>%
        #dplyr::mutate_at(
        #  c("event_player_1_name","event_player_2_name",
        #      "event_player_3_name","event_goalie_name"),
        #  ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
        #) %>%
        dplyr::select(-event_id,-event_code) %>%
        dplyr::mutate(
          event_team_type =  dplyr::case_when(
            event_team == home_name ~ "home",
            event_team == away_name ~ "away"
          )
        ) %>%
        dplyr::select(
          event_type, event, secondary_type, event_team, event_team_type,
          description, period, period_time, period_time_remaining,
          event_player_1_name, event_player_1_type, event_player_2_name,
          event_player_2_type, event_player_3_name, event_player_3_type,
          event_goalie_name, game_id, event_idx, tidyselect::everything()
        )
    } else {
      pbp_full <- pbp %>%
        #dplyr::mutate_at(
        #  c("event_player_1_name","event_player_2_name",
        #      "event_goalie_name"),
        #  ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
        #) %>%
        dplyr::select(-event_id,-event_code) %>%
        dplyr::mutate(
          event_team_type =  dplyr::case_when(
            event_team == home_name ~ "home",
            event_team == away_name ~ "away"
          )
        ) %>%
        dplyr::select(
          event_type, event, secondary_type, event_team, event_team_type,
          description, period, period_time, period_time_remaining,
          event_player_1_name, event_player_1_type, event_player_2_name,
          event_player_2_type,
          event_goalie_name, game_id, event_idx, tidyselect::everything()
        )
    }

  }

  # add event_id
  pbp_full <- pbp_full %>%
    dplyr::mutate(
      event_idx = stringr::str_pad(event_idx, width = 4, side = "left", pad = 0),
      event_id = as.numeric(paste0(game_id,event_idx)),
      secondary_type = ifelse(
        stringr::str_detect(dplyr::lead(description), "PS -") &
          event_type %in% c("SHOT","MISSED_SHOT","GOAL"),
        "Penalty Shot", secondary_type
      )
    ) %>%
    #add home and away names
    dplyr::left_join(
      dplyr::select(team_info, home_name = full_team_name, home_abbr = team_abbr),
      by = "home_abbr"
    ) %>%
    dplyr::left_join(
      dplyr::select(team_info, away_name = full_team_name, away_abbr = team_abbr),
      by = "away_abbr"
    )

  # add xg
  # depends on strength state, needs shift data to be there
  pbp_return <- tryCatch(
    calculate_xg(pbp_full),
    warning = function(cond){
      message(paste0("There was a problem calulating xG for game id ",game_id,"\n\n",cond))
      return(NULL)
    },
    error = function(cond){
      message(paste0("There was a problem calulating xG for game id ",game_id,"\n\n",cond))
      return(NULL)
    }
  )

  if(is.null(pbp_return)){
    pbp_return <- pbp_full
  }

  return(pbp_full)

}
