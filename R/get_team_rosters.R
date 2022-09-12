#' Get current team roster
#'
#' @param team A team name or abbreviation, as a string - or the NHL integer team ID
#'
#' @return A tibble containing the current official team roster per NHL.com
#' @export
#'
#' @examples
#' \dontrun{
#' get_team_rosters("SEA")
#' }
get_team_rosters <- function(team){

  # if looking for a single team roster with team name or abbreviation
  if(is.character(team)){
    # get all team names & ids
    team_list <- jsonlite::read_json("https://statsapi.web.nhl.com/api/v1/teams")$teams %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::select(team_id = id, full_team_name = name, team_abbr = abbreviation)

    # find team id from supplied team name
    team_id <- team_list %>%
      dplyr::filter(full_team_name == team |
                      team_abbr == team) %>%
      dplyr::pull(team_id)
  } else {
    team_id <- team
  }

  # access NHL API
  url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/",team_id,"/roster")

  site <- tryCatch(
    jsonlite::read_json(url),
    warning = function(cond){
      message(paste0("There was a problem fetching rosters\n\n",cond))
      return(NULL)
    },
    error = function(cond){
      message(paste0("There was a problem fetching rosters\n\n",cond))
      return(NULL)
    }
  )

  if(is.null(site)){
    stop("Could not get current rosters, please try again later")
  }

  # parse json roster data
  roster <- site$roster %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_wider(person) %>%
    tidyr::unnest_wider(position) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      jersey_number = as.integer(jersey_number),
      position_type = dplyr::case_when(
        type == "Forward" ~ "F",
        type == "Goalie" ~ "G",
        type == "Defenseman" ~ "D"
      ),
      team_id = team_id,
    ) %>%
    # remove excessive position vars
    dplyr::select(-code, -name, -link, -type) %>%
    dplyr::rename(
      player_id = id,
      player = full_name,
      position = abbreviation
    )

  return(roster)
}
