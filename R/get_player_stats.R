#' Get player counting stats from hockey-reference.com
#'
#' @param player A player name or vector of player names
#' @param season An integer value denoting the end year of a season
#' @param league The league stats to scrape, either 'NHL' or 'WHA'
#'
#' @importFrom utils type.convert
#'
#' @return A tibble containing goals, assists, and various other stats for
#' the specified player(s) from hockey-reference.com
#' @export
#'
#' @examples
#' get_player_stats("Wayne Gretzky")
#'
#' get_player_stats(c("Wayne Gretzky","Mario Lemieux"))
get_player_stats <- function(player_name, season = "career", league = "NHL"){

  player_table <- tibble(player = player_name) |>
    tidyr::separate(
      player, into = c("first","last"), sep = " ",
      extra = "merge", remove = FALSE
      ) |>
    dplyr::mutate(
      #season_short = as.numeric(season),
      last = gsub("\\ ","",last),
      last = tolower(last),
      first = tolower(first),
      link = glue::glue("https://www.hockey-reference.com/players/{substr(last,1,1)}/{substr(last,1,5)}{substr(first,1,2)}01.html")
    ) #|>
    #dplyr::select(-season)

  links <- player_table |>
    dplyr::select(player, link) |>
    dplyr::distinct()

  stats <- NULL
  session <- polite::bow("https://www.hockey-reference.com/players/")

  for(i in 1:nrow(links)){

    session <- polite::nod(session, links$link[i])

    # pull player stats table
    player <- session |>
      polite::scrape() |>
      rvest::html_element("table") |>
      rvest::html_table() |>
      janitor::row_to_names(row_number = 1) |>
      janitor::clean_names() |>
      dplyr::filter(stringr::str_detect(season, "yr", negate = TRUE)) |>
      dplyr::filter(tm != "TOT" & lg == league) |>
      dplyr::mutate(
        player = links$player[i],
        season_short = case_when(
          season == "1999-00" ~ "2000",
          season == "Career" ~ "0",
          season != "Career" & season != "1999-00" ~ as.character(glue::glue("{substr(season,1,2)}{substr(season,6,7)}"))
        ),
        season_short = as.numeric(season_short)
      ) |>
      dplyr::rename(
        season_full = season
        )

    # filtering to selected season, or career totals by default
    if(!is.numeric(season)){
      player <- player |>
        dplyr::filter(season_short == 0)
    } else {
      player <- player |>
        dplyr::filter(season_short %in% season)
    }

    # change stat names that only show up for skaters, not goalies
    if("ev_2" %in% names(player)){
      player <- player |>
        dplyr::rename(
          ev_g = ev,
          pp_g = pp,
          sh_g = sh,
          ev_a = ev_2,
          pp_a = pp_2,
          sh_a = sh_2,
          plus_minus = x
          )
    }

    stats <- dplyr::bind_rows(stats, player)

    # rest before pulling more players
    if(i < nrow(links)){
      Sys.sleep(2)
    }
  }

  # convert numbers to...well, numbers
  stats <- stats |>
    dplyr::mutate_all(type.convert, as.is = TRUE)

  new_table <- player_table |>
    dplyr::select(-first, -last) |>
    dplyr::left_join(
      stats,
      by = "player"
    )

  return(new_table)
}
