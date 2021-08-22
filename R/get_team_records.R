#' Get team records from hockey-reference.com
#'
#' @param season An integer value denoting the end year of the season(s) to
#' scrape
#' @param league The league stats to scrape, either 'NHL' or 'WHA' or get both
#' with c('NHL','WHA')
#' @param include_records Option to exclude records from the function, used to
#' gather full team names & abbreviations for every season since 1918
#'
#' @return A tibble containing full team names & win-loss records for teams
#' in all desired seasons
#' @export
#'
#' @examples
#' \dontrun{
#' get_team_records(2021)
#' }
get_team_records <- function(season = as.numeric(format(Sys.Date()+184, "%Y")), league = "NHL", include_records = TRUE){

  # gathers all full team names & abbreviations for specified years
  # from hockey-reference
  # defaults to current season, using July 1st as start of league year

  if(league != "NHL" & league != "WHA"){
    stop("League not recognized; must be either 'NHL' or 'WHA'")
  } else if(!is.integer(type.convert(season, as.is = TRUE))){
    stop("'season' argument must be single 4-digit year;
  ex: 2010-11 season should be written as '2011'")
  } else if (league == "WHA" & (season < 1973 | season > 1979)){
    stop("WHA only existed between 1973-1979 seasons")
  } else if(league == "NHL" & season < 1918){
    stop(glue::glue("The NHL didn't exist in {season}; please enter a season of 1918 or later"))
  } else if (league == "NHL" & season > as.numeric(format(Sys.Date()+184, "%Y"))) {
    stop(glue::glue("Data not available for {season} yet, please check back closer to the start of the {season-1}-{season} season"))
  } else if (league == "NHL" & season == 2005) {
    stop(glue::glue("Can't get data for 2005; season cancelled due to lockout"))
  }

  team_list <- NULL

  session <- polite::bow("https://www.hockey-reference.com/")

  for(yr in season){

    hr_url <- glue::glue("https://www.hockey-reference.com/leagues/{league}_{yr}_standings.html")

    session <- polite::nod(session, hr_url)

    records <- session |>
      polite::scrape() |>
      rvest::html_element("#team_vs_team") |>
      rvest::html_table()

    colnames(records) <- c("rk","team_name",names(records)[3:length(records)])

    teams <- dplyr::tibble(
      team_name = records$team_name,
      team_abbr = names(records |> dplyr::select(-team_name, -rk)),
      season_short = yr,
      season = glue::glue("{yr-1}-{substr(season_short,3,4)}") |> as.character()
    )

    team_list <- dplyr::bind_rows(team_list, teams)
  }

  if(include_records == TRUE){

    session <- polite::bow("https://www.hockey-reference.com/")

    for(yr in season){

      hr_url <- glue::glue("https://www.hockey-reference.com/leagues/{league}_{yr}_standings.html")

      session <- polite::nod(session, hr_url)

      records <- session |>
        polite::scrape() |>
        rvest::html_element("#expanded_standings") |>
        rvest::html_table() |>
        janitor::clean_names()

      colnames(records) <- c("rk","team_name",names(records)[3:length(records)])

      records <- records |>
        dplyr::select(team_name:overtime) |>
        tidyr::separate(overall, into = c("w","l","otl"), remove = FALSE) |>
        utils::type.convert(as.is = TRUE) |>
        dplyr::mutate(
          st_points = (2*w)+otl,
          season_short = yr
        )

      team_list <- team_list |>
        dplyr::left_join(
          records, by = c("team_name", "season_short")
        )
    }

    return(team_list)
  }

  return(team_list)
}
