#' Get current NHL rosters
#'
#' @return A tibble containing the current rosters for every team per the NHL
#' \describe{
#' \item{player_id}{NHL player ID, as an integer}
#' \item{player}{Player name as a string}
#' \item{jersey_number}{Player jersey number, as an integer}
#' \item{position}{Abbreviated official player position, as a string}
#' \item{position_type}{Abbreviated position group: F, D, or G}
#' \item{team_id}{NHL integer team ID}
#' \item{full_team_name}{Full team name as a string}
#' \item{team_abbr}{Team abbreviation, as a string}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' current_rosters <- get_current_rosters()
#' }
get_current_rosters <- function(){

  site <- tryCatch(
    jsonlite::read_json("https://statsapi.web.nhl.com/api/v1/teams"),
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

  # get all team ids
  team_list <- site$teams %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::select(team_id = id, full_team_name = name, team_abbr = abbreviation)

  # get rosters via NHL API
  rosters <- purrr::map_dfr(
    .x = team_list$team_id,
    ~get_team_rosters(.x)
  ) %>%
    # add team name & abbreviation
    dplyr::left_join(team_list, by = "team_id")

  return(rosters)
}
