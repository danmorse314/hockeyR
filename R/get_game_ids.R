#' Fetch game IDs for a single day or a full season
#'
#' @param season An integer value denoting the end year of the season to scrape
#' @param day A day in the format of 'YYYY-MM-DD'
#'
#' @return A tibble containing game IDs for specified time frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_game_ids(season = 2020)
#' get_game_ids(day = "2015-03-12")
#' }
get_game_ids <- function(season = NULL, day = as.Date(Sys.Date(), "%Y-%m-%d")){

  if(is.null(season)){

    # scrape day's games

    url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?date={day}")

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
    } else if(site$totalGames == 0){
      stop(glue::glue("No NHL games found on {day}"))
    }

  } else {

    # scrape season's games

    if(season == 2020){
      # searching the nhl api for games between Sep 1 2019 & Sep 30th 2020
      # what a stupid season
      url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?startDate={season-1}-09-01&endDate={season}-09-30")
    } else {
      # searching the nhl api for games between Sep 1 & July 5
      url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?startDate={season-1}-09-01&endDate={season}-07-05")
    }

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

  }

  if(is.null(site)){
    stop("check the season or day argument and try again")
  } else if(site$totalGames == 0) {
    game_id_list <- NULL
  } else {
    game_id_list <- site$dates %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::select(date, games) %>%
      tidyr::unnest_longer(games) %>%
      tidyr::unnest_wider(games) %>%
      dplyr::select(date, gamePk, season, teams) %>%
      tidyr::unnest_wider(teams) %>%
      tidyr::unnest_wider(away) %>%
      tidyr::unnest_wider(team) %>%
      dplyr::rename(
        game_id = gamePk,
        season_full = season,
        away_name = name,
        away_final_score = score
      ) %>%
      dplyr::select(-leagueRecord, -id, -link) %>%
      tidyr::unnest_wider(home) %>%
      tidyr::unnest_wider(team) %>%
      dplyr::rename(
        home_name = name,
        home_final_score = score
      ) %>%
      dplyr::select(game_id, season_full, date, home_name, away_name, home_final_score, away_final_score)

    game_id_list$game_type <- dplyr::case_when(
      substr(game_id_list$game_id, 6, 6) == 1 ~ "PRE",
      substr(game_id_list$game_id, 6, 6) == 2 ~ "REG",
      substr(game_id_list$game_id, 6, 6) == 3 ~ "POST",
      substr(game_id_list$game_id, 6, 6) == 4 ~ "ALLSTAR"
    )

    game_id_list <- dplyr::filter(game_id_list,
                                  game_type == "REG" | game_type == "POST")

    # make sure we're only pulling for correct season by using
    # the season code in the game_id

    if(!is.null(season)) {
      game_id_list <- game_id_list %>%
        dplyr::filter(
          substr(game_id, 1, 4) == (as.numeric(season) - 1)
        )
    }

  }

  return(game_id_list)

}
