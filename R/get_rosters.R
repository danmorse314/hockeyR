#' Get team rosters from hockey-reference.com
#'
#' @param team A character vector of team names or abbreviations
#' @param season An integer value denoting the end year of the season to scrape
#' @param include_stats Set to TRUE to return player counting stats for the season
#'
#' @description Get the latest roster for any team from hockey-reference.com.
#' You may enter either the team abbreviation or the full team name. Seasons
#' must be 4-digit integers denoting the end-year of the regular season
#' desired (ie 2021-22 season should be 2022)\cr\cr
#' Please note that this uses the hockey-reference.com team abbreviations,
#' the oddest of which is Vegas being 'VEG' instead of 'VGK'. If you are
#' unsure of the team abbreviation, enter the full team name instead, or
#' check the full team abbreviations data and filter to your desired season:\cr\cr
#' \code{filter(team_abbr_yearly, season_short == {season})}
#'
#' @return A tibble containing the latest roster for the specified team(s)
#' in the specified season
#' @export
#'
#' @examples
#' \dontrun{
#' get_rosters("SEA", 2022)
#' }
get_rosters <- function(
  team = "all",
  season = as.numeric(format(Sys.Date()+184, "%Y")),
  include_stats = FALSE
  ){

  if(length(team) == 1 && team == "all"){
    to_pull <- team_abbr_yearly |>
      dplyr::select(-season) |>
      dplyr::filter(season_short %in% season) |>
      dplyr::pull(team_abbr)
  } else {
    to_pull <- team_abbr_yearly |>
      dplyr::select(-season) |>
      dplyr::filter(season_short %in% season) |>
      dplyr::filter(
        toupper(team_name) %in% toupper(team) |
          team_abbr %in% toupper(team)
      ) |>
      dplyr::pull(team_abbr)
  }

  session <- polite::bow("https://www.hockey-reference.com/teams/")
  rosters <- NULL

  for(i in to_pull){

    team_url <- glue::glue("https://www.hockey-reference.com/teams/{i}/{season}.html")
    session <- polite::nod(session, team_url)

    team_roster <- session |>
      polite::scrape() |>
      rvest::html_element("#roster")

    suppressWarnings({
      urls <- team_roster |>
        rvest::html_elements("a[href^='/players/']") |>
        stringr::str_extract_all("/players/.+.html") |>
        unlist() |>
        dplyr::as_tibble() |>
        dplyr::mutate(
          link = glue::glue("https://www.hockey-reference.com{value}")
        ) |>
        dplyr::select(link)
    })

    team_roster <- team_roster |>
      rvest::html_table() |>
      janitor::clean_names() |>
      tidyr::separate(s_c, into = c("shoots","catches"), sep = "/", remove = TRUE) |>
      tidyr::separate(ht, into = c("ft","inches"), sep = "-", remove = FALSE) |>
      dplyr::mutate(
        height_ft = as.numeric(ft) + (as.numeric(inches)/12),
        height_cm = height_ft * 30.48,
        team_abbr = i,
        season_short = season,
        exp = ifelse(exp == "R", 0, exp),
        exp = as.numeric(exp)
        ) |>
      dplyr::rename(
        number = no,
        nationality = flag,
        position = pos,
        height = ht,
        weight = wt,
        experience = exp
      ) |>
      dplyr::bind_cols(urls) |>
      dplyr::select(-ft, -inches) |>
      dplyr::left_join(team_abbr_yearly, by = c("team_abbr", "season_short")) |>
      dplyr::select(number, player, team_name, team_abbr, season, position,
                    everything())

    team_roster$player <- stringr::str_remove_all(team_roster$player, "\\s\\(C\\)")
    team_roster$player <- stringr::str_remove_all(team_roster$player, "\\s\\(A\\)")

    if(include_stats == TRUE){

      test <- polite::scrape(session) |>
        rvest::html_element("#skaters")

      if(length(test) == 0){
        message(glue::glue("Stats not available for {season} yet, check back later"))
      } else {
        skater_stats <- session |>
          polite::scrape() |>
          rvest::html_element("#skaters") |>
          rvest::html_table() |>
          janitor::clean_names() |>
          dplyr::mutate(
            assists = ifelse(x == "Rk", "ev_a", assists),
            assists_2 = ifelse(x == "Rk", "pp_a", assists_2),
            assists_3 = ifelse(x == "Rk", "sh_a", assists_3)
          ) |>
          janitor::row_to_names(row_number = 1) |>
          janitor::clean_names() |>
          dplyr::rename(plus_minus = x) |>
          dplyr::select(-rk) |>
          dplyr::filter(player != "Team Total" & pos != "G")

        goalie_stats <- session |>
          polite::scrape() |>
          rvest::html_element("#goalies") |>
          rvest::html_table() |>
          janitor::clean_names() |>
          janitor::row_to_names(row_number = 1) |>
          janitor::clean_names() |>
          dplyr::select(-rk) |>
          dplyr::filter(player != "Team Total")

        team_stats <- dplyr::bind_rows(skater_stats, goalie_stats) |>
          dplyr::select(-age, -pos)

        team_stats$player <- stringr::str_remove_all(team_stats$player, "\\*")
        team_stats$player <- stringr::str_remove_all(team_stats$player, "\\s\\(C\\)")
        team_stats$player <- stringr::str_remove_all(team_stats$player, "\\s\\(A\\)")

        team_roster <- team_roster |>
          dplyr::left_join(team_stats, by = "player")
      }

    }

    team_roster <- team_roster |>
      dplyr::mutate_all(type.convert, as.is = TRUE) |>
      dplyr::mutate(number = as.character(number))

    rosters <- dplyr::bind_rows(rosters, team_roster)
  }

  return(rosters)
}
