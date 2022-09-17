#' Load team standings
#'
#' @param seasons End year of seasons to pull
#'
#' @description Get full regular season standings for given year(s), including win-loss record and
#' goals for and against
#'
#' @return A tibble containing team records and stats for given year
#' @export
#'
#' @examples
#' \dontrun{
#' get_standings(2022)
#' }
get_standings <- function(seasons = as.numeric(format(Sys.Date()+78, "%Y"))){

  standings <- NULL

  for(i in seasons){

    if(i == 2005){
      next("Could not get standings for 2004-05; season was cancelled by Gary Bettman")
    }

    url <- glue::glue("https://statsapi.web.nhl.com/api/v1/standings?season={i-1}{i}")

    site <- tryCatch(
      jsonlite::read_json(url),
      warning = function(cond){
        message(paste0("There was a problem fetching standings for ",i,"\n\n",cond))
        return(NULL)
      },
      error = function(cond){
        message(paste0("There was a problem fetching standings for ",i,"\n\n",cond))
        return(NULL)
      }
    )

    if(length(site$records) == 0){
      next(glue::glue("Could not get standings for {i-1}-{substr(i,3,4)} season"))
    }

    season_standings <- site$records %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      tidyr::unnest_wider(league, names_sep = "_") %>%
      tidyr::unnest_wider(division, names_sep = "_") %>%
      tidyr::unnest_wider(conference, names_sep = "_") %>%
      tidyr::unnest_longer(teamRecords) %>%
      tidyr::unnest_wider(teamRecords, names_sep = "_") %>%
      tidyr::unnest_wider(teamRecords_team, names_sep = "_") %>%
      tidyr::unnest_wider(teamRecords_leagueRecord, names_sep = "_") %>%
      dplyr::rename_with(
        .cols = dplyr::starts_with("teamRecords_"),
        ~stringr::str_remove(.x, "teamRecords_")
      ) %>%
      dplyr::rename_with(
        .cols = dplyr::starts_with("leagueRecord_"),
        ~stringr::str_remove(.x, "leagueRecord_")
      ) %>%
      tidyr::unnest_wider(streak, names_sep = "_") %>%
      janitor::clean_names()

    standings <- dplyr::bind_rows(standings, season_standings)
  }

  return(standings)
}
