#' Get team rosters from hockey-reference.com
#'
#' @param team A character vector of team names or abbreviations
#' @param season An integer value denoting the end year of the season to scrape
#'
#' @return A tibble containing the latest roster for the specified team(s)
#' in the specified season
#' @export
#'
#' @examples
#' \dontrun{
#' get_rosters("SEA", 2022)
#' }
get_rosters <- function(team = "all", season = as.numeric(format(Sys.Date()+184, "%Y"))){

  if(length(team) == 1 & team == "all"){
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
      rvest::html_element("#roster") |>
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
      # add hockey-ref player page links
      tidyr::separate(
        player, into = c("first","last"), sep = " ",
        extra = "merge", remove = FALSE
      ) |>
      dplyr::mutate(
        last = gsub("\\ ","",last),
        last = tolower(last),
        first = tolower(first),
        link = glue::glue("https://www.hockey-reference.com/players/{substr(last,1,1)}/{substr(last,1,5)}{substr(first,1,2)}01.html")
      ) |>
      dplyr::select(-ft, -inches, -first, -last) |>
      dplyr::left_join(team_abbr_yearly, by = c("team_abbr", "season_short")) |>
      dplyr::select(number, player, team_name, team_abbr, season, position,
                    everything())

    rosters <- dplyr::bind_rows(rosters, team_roster)
  }

  return(rosters)
}
