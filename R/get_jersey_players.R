#' Get Players by Jersey Number
#'
#' Get the name of every player to wear a specific jersey number in
#' the NHL and the season in which they wore it.
#'
#' @param jersey An integer or a vector of integers between 0 & 99
#'
#' @importFrom utils type.convert
#' @importFrom rlang .data
#'
#' @return A tibble containing each player-season where a player wore
#' the specified number
#'
#' @export
#'
#' @examples
#' get_jersey_players(69)
#'
#' get_jersey_players(c(99, 66))
get_jersey_players <- function(jersey){

  sweaters <- NULL

  for(i in jersey){

    if(!is.integer(utils::type.convert(i, as.is = TRUE)) |
       !dplyr::between(i, 0, 99)){
      next
    }

    num_url <- glue::glue("https://www.hockey-reference.com/friv/numbers.cgi?number={i}")

    session <- polite::bow(num_url)

    # get the data
    num_list <- session |>
      polite::scrape() |>
      rvest::html_table()

    num_table <- num_list[[1]] |>
      janitor::clean_names()

    number_table <- dplyr::tibble(
      player = .data$num_table$player,
      team_season = stringr::str_split(.data$num_table$team_s, "\\(|\\)")
    ) |>
      tidyr::unnest(cols = c(team_season)) |>
      dplyr::filter(.data$team_season != "")

    names_table <- number_table |>
      dplyr::left_join(
        number_table |>
          dplyr::count(player),
        by = "player"
      ) |>
      dplyr::select(-team_season) |>
      dplyr::distinct()

    final_table <- dplyr::tibble(
      player = rep(
        .data$names_table$player,
        .data$names_table$n/2
      ),
      team = .data$number_table$team_season[c(TRUE, FALSE)],
      season = .data$number_table$team_season[c(FALSE, TRUE)]
    ) |>
      tidyr::separate_rows(.data$season, sep = ", ") |>
      dplyr::mutate(
        season = ifelse(
          substr(.data$season,1,1)=="0" | substr(.data$season,1,1)=="1" | substr(.data$season,1,1)=="2",
          glue::glue("20{.data$season}"),
          glue::glue("19{.data$season}")
        )
      ) |>
      dplyr::mutate(
        player = gsub("\\*","",.data$player),
        season = as.numeric(.data$season),
        team = substr(.data$team, 1, nchar(.data$team)-1),
        jersey_number = i
      )

    sweaters <- dplyr::bind_rows(sweaters, final_table)
  }

  return(sweaters)
}
