#' Get draft classes
#'
#' @param draft_year Draft year to scrape
#' @param player_details If true, returns more detailed data on each prospect
#'
#' @description Get all selections for any single NHL draft class back to 1963
#'
#' @return A tibble containing all selections for the supplied draft year.
#' ## Basic draft class variables
#' * Draft year
#' * Round
#' * Overall Pick #
#' * Round Pick #
#' * Drafting team
#' * Player ID
#' * Player name
#' * Player link
#' ## Detailed draft class variables
#' * Player birthdate
#' * Player birthplace & nationality
#' * Player height & weight
#' * Player shoots/catches
#' * Player position
#' * Player amateur team & league
#' @export
#'
#' @examples
#' \dontrun{
#' draft_2022 <- get_draft_class(draft_year = 2022, player_details = TRUE)
#' }
get_draft_class <- function(draft_year = as.numeric(format(Sys.Date()-181, "%Y")), player_details = FALSE){

  if(draft_year %not_in% 1963:as.numeric(format(Sys.Date()-181, "%Y"))){
    stop(paste("No NHL Entry Draft data found for the year",draft_year))
  }

  url <- paste0("https://statsapi.web.nhl.com/api/v1/draft/",draft_year)

  site <- tryCatch(
    jsonlite::read_json(url),
    warning = function(cond){
      message(paste0("There was a problem fetching the draft class for ",draft_year,"\n\n",cond))
      return(NULL)
    },
    error = function(cond){
      message(paste0("There was a problem fetching the draft class for ",draft_year,"\n\n",cond))
      return(NULL)
    }
  )

  if(is.null(site)){
    stop(paste("Could not get draft class for",draft_year))
  }

  # get selections
  df <- site$drafts %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_longer(rounds) %>%
    tidyr::unnest_wider(rounds) %>%
    dplyr::select(-round, -roundNumber) %>%
    tidyr::unnest_longer(picks) %>%
    tidyr::unnest_wider(picks) %>%
    tidyr::unnest_wider(team) %>%
    dplyr::select(
      -year,-id, -link
    ) %>%
    tidyr::unnest_wider(prospect) %>%
    janitor::clean_names()

  if("id" %in% names(df)){
    df <- df %>%
      dplyr::rename(
        prospect_id = id, player = full_name, player_link = link,
        full_team_name = name
      )
  }

  if(player_details == TRUE & draft_year > 1982) {
    # player ids didn't exist til 1983
    # no details can be retrieved before then

    # add player details
    details <- NULL
    for(i in unique(df$player_link)){

      url <- paste0("https://statsapi.web.nhl.com/",i)

      # for voided picks, a la AZ 2021 1st rounder
      if(i != "/api/v1/draft/prospects/null") {
        details_df <- jsonlite::read_json(url)$prospects %>%
          dplyr::tibble() %>%
          tidyr::unnest_wider(1) %>%
          dplyr::select(-fullName, -link) %>%
          dplyr::rename(prospect_id = id) %>%
          tidyr::unnest_wider(primaryPosition) %>%
          dplyr::rename(
            position = abbreviation
          ) %>%
          dplyr::mutate(
            position_type = dplyr::case_when(
              type == "Forward" ~ "F",
              type == "Goalie" ~ "G",
              type == "Defenseman" ~ "D"
            )
          ) %>%
          dplyr::select(-code, -name, -type) %>%
          tidyr::unnest_wider(prospectCategory) %>%
          dplyr::select(-id,-shortName) %>%
          dplyr::rename(prospect_category = name) %>%
          tidyr::unnest_wider(amateurTeam) %>%
          dplyr::select(-link) %>%
          dplyr::rename(amateur_team = name) %>%
          tidyr::unnest_wider(amateurLeague) %>%
          dplyr::select(-link) %>%
          dplyr::rename(amateur_league = name) %>%
          # central scouting ranks included for 2022 but not earlier
          # potential to update to include them in the future here
          dplyr::select(-ranks)
      } else {
        details_df <- NULL
      }

      details <- dplyr::bind_rows(details, details_df)
    }

    details <- janitor::clean_names(details) %>%
      dplyr::rename(player_id = nhl_player_id)

    df <- dplyr::left_join(df, details, by = "prospect_id")
  }

  return(df)

}
