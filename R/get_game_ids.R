#' Fetch game IDs for a single day or a full season
#'
#' @param season An integer value denoting the end year of the season to scrape
#' @param day A day in the format of 'YYYY-MM-DD'
#'
#' @return A tibble containing game IDs and basic info for specified time frame
#'
#' \describe{
#' \item{game_id}{Integer value of NHL game ID used in \code{\link{scrape_game}}}
#' \item{season_full}{String defining NHL season}
#' \item{date}{Date of game, as a string}
#' \item{game_time}{Scheduled start time (US/Eastern) of game, as a string}
#' \item{home_name}{Home team name, as a string}
#' \item{away_name}{Away team name, as a string}
#' \item{home_final_score}{Numeric final score for home team - will return 0 for games that haven't started}
#' \item{away_final_score}{Numeric final score for away team - will return 0 for games that haven't started}
#' \item{game_type}{String denoting type of game: "REG" or "POST"}
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
#' get_game_ids(season = 2020)
#' get_game_ids(day = "2015-03-12")
#' })
#' }
get_game_ids <- function(season = NULL, day = as.Date(Sys.Date(), "%Y-%m-%d")){

  # load team abbreviations
  team_info <- team_logos_colors

  if(is.null(season)){

    # scrape day's games

    url <- glue::glue("https://api-web.nhle.com/v1/schedule/{day}")

    # check that url isn't broken
    site <- tryCatch(
      jsonlite::read_json(url),
      warning = function(cond){
        message(paste0("There was a problem fetching games:\n\n",cond))
        return(NULL)
      },
      error = function(cond){
        message(paste0("There was a problem fetching games:\n\n",cond))
        return(NULL)
      }
    )

    if(is.null(site)){
      stop()
    } else if(site$gameWeek[[1]]$numberOfGames == 0){
      stop(glue::glue("No NHL games found on {day}"))
    } else {
      game_id_list <- site$gameWeek[[1]]$games %>%
        dplyr::tibble()
    }

  } else {

    # scrape season's games

    game_id_list <- NULL
    for(i in unique(team_info$team_abbr)){
      url <- glue::glue("https://api-web.nhle.com/v1/club-schedule-season/{i}/{season-1}{season}")

      site <- tryCatch(
        jsonlite::read_json(url),
        warning = function(cond){
          message(paste0("There was a problem fetching games:\n\n",cond))
          return(NULL)
        },
        error = function(cond){
          message(paste0("There was a problem fetching games:\n\n",cond))
          return(NULL)
        }
      )

      if(!is.null(site)){
        team_game_list <- site$games %>%
          dplyr::tibble()
        game_id_list <- dplyr::bind_rows(game_id_list, team_game_list)
      } else {
        next()
      }
    }

  }

  # check for days with no games
  if(is.null(season)){
    if(site$gameWeek[[1]]$numberOfGames == 0) {
      game_id_list <- NULL
    }
  }

  if(!is.null(game_id_list)){
    game_id_list <- game_id_list %>%
      tidyr::unnest_wider(1) %>%
      dplyr::mutate(
        gameDate = lubridate::with_tz(lubridate::ymd_hms(startTimeUTC),"US/Eastern"),
        game_time = format(gameDate, "%I:%M %p"),
        date = lubridate::date(gameDate)
      ) %>%
      dplyr::select(
        game_id = id, season_full = season, game_time, gameType,
        homeTeam, awayTeam, date
      )

    # check if game has a score attached to the teams
    if("score" %in% names(dplyr::tibble(game_id_list$homeTeam)%>%tidyr::unnest_wider(1))){
      # game either over or in progress with scores
      game_id_list <- game_id_list %>%
        tidyr::unnest_wider(homeTeam) %>%
        dplyr::select(game_id:gameType, date, home_abbr = abbrev, home_final_score = score, awayTeam) %>%
        tidyr::unnest_wider(awayTeam) %>%
        dplyr::select(game_id:home_final_score, date,
                      away_abbr = abbrev, away_final_score = score)
    } else {
      game_id_list <- game_id_list %>%
        tidyr::unnest_wider(homeTeam) %>%
        dplyr::select(game_id:gameType, date, home_abbr = abbrev, awayTeam) %>%
        tidyr::unnest_wider(awayTeam) %>%
        dplyr::select(game_id:home_abbr, date,
                      away_abbr = abbrev) %>%
        dplyr::mutate(home_final_score = NA_integer_,
                      away_final_score = NA_integer_)
    }

    game_id_list <- game_id_list  %>%
      dplyr::select(game_id, season_full, date, game_time,
                    home_abbr, away_abbr,
                    game_type = gameType,
                    home_final_score, away_final_score) %>%
      dplyr::mutate(
        game_type = dplyr::case_when(
          game_type == 1 ~ "PRE",
          game_type == 2 ~ "REG",
          game_type == 3 ~ "POST",
          game_type == 4 ~ "ALLSTAR"
        )
      ) %>%
      # add team names because that's what it used to pull from old API
      dplyr::left_join(
        team_info %>%
          dplyr::select(home_abbr = team_abbr, home_name = full_team_name),
        by = c("home_abbr")
      ) %>%
      dplyr::left_join(
        team_info %>%
          dplyr::select(away_abbr = team_abbr, away_name = full_team_name),
        by = "away_abbr"
      ) %>%
      dplyr::select(
        game_id, season_full, game_type, date, game_time, home_abbr, away_abbr,
        home_name, away_name, home_final_score, away_final_score
      )

    game_id_list <- dplyr::filter(game_id_list,
                                  game_type == "REG" | game_type == "POST")

    # make sure we're only pulling for correct season by using
    # the season code in the game_id

    if(!is.null(season)) {
      game_id_list <- game_id_list %>%
        dplyr::filter(
          substr(game_id, 1, 4) == (as.numeric(season)-1)
        ) %>%
        dplyr::arrange(date)
    }
  }

  return(game_id_list)

}
