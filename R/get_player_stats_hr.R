#' Get player counting stats from hockey-reference.com
#'
#' @param player_name A player name or vector of player names
#' @param season An integer value denoting the end year of the season(s) to scrape
#' @param league The league stats to scrape, either 'NHL' or 'WHA'
#'
#' @importFrom utils type.convert
#'
#' @return A tibble containing goals, assists, and various other stats for
#' the specified player(s) from hockey-reference.com
#' @export
#'
#' @examples
#' \dontrun{
#' #' get_player_stats("Wayne Gretzky")
#' get_player_stats_hr(c("Wayne Gretzky","Mario Lemieux"))
#' }
get_player_stats_hr <- function(player_name, season = "career", league = "NHL"){

  player_table <- dplyr::tibble(player = player_name) %>%
    tidyr::separate(
      player, into = c("first","last"), sep = " ",
      extra = "merge", remove = FALSE
      ) %>%
    dplyr::mutate(
      last = gsub("\\ ","",last),
      last = tolower(last),
      first = tolower(first),
      link = glue::glue("https://www.hockey-reference.com/players/{substr(last,1,1)}/{substr(last,1,5)}{substr(first,1,2)}01.html")
    )

  links <- player_table %>%
    dplyr::select(player, link) %>%
    dplyr::distinct()

  stats <- NULL
  session <- rvest::session("https://www.hockey-reference.com/players/")

  for(i in 1:nrow(links)){

    session <- rvest::session_jump_to(session, links$link[i])

    # skip failed links
    if(is.null(session)){
      next
    }

    player <- session %>%
      rvest::html_element("table")

    # skip players with no stats
    # likely linking to wrong player
    if(length(player) ==0){
      next
    }

    player <- player %>%
      rvest::html_table() %>%
      janitor::clean_names()

    # avoid duplicate row names warnings
    if("assists" %in% names(player)){
      player <- player %>%
        dplyr::mutate(
          assists = ifelse(x == "Season", "ev_a", assists),
          assists_2 = ifelse(x == "Season", "pp_a", assists_2),
          assists_3 = ifelse(x == "Season", "sh_a", assists_3)
        )
    }

    player <- player %>%
      janitor::row_to_names(row_number = 1) %>%
      janitor::clean_names() %>%
      dplyr::filter(stringr::str_detect(season, "yr", negate = TRUE)) %>%
      dplyr::filter(tm != "TOT" & lg == league) %>%
      dplyr::mutate(
        player = links$player[i],
        season_short = dplyr::case_when(
          season == "1999-00" ~ "2000",
          season == "Career" ~ "0",
          season != "Career" & season != "1999-00" ~ as.character(glue::glue("{substr(season,1,2)}{substr(season,6,7)}"))
        ),
        season_short = as.numeric(season_short)
      ) %>%
      dplyr::rename(
        season_full = season
        )

    # filtering to selected season, or career totals by default
    if(!is.numeric(season)){
      player <- player %>%
        dplyr::filter(season_short == 0)
    } else {
      player <- player %>%
        dplyr::filter(season_short %in% season)
    }

    # change stat names that only show up for skaters, not goalies
    if("ev_a" %in% names(player)){
      player <- player %>%
        dplyr::rename(
          plus_minus = x
          )
    }

    stats <- dplyr::bind_rows(stats, player)

  }

  # convert numbers to...well, numbers
  stats <- stats %>%
    dplyr::mutate_all(type.convert, as.is = TRUE)

  # fix issue where player page exists but no stats yet recorded
  if(nrow(stats) > 0){
    new_table <- player_table %>%
      dplyr::select(-first, -last) %>%
      dplyr::left_join(
        stats,
        by = "player"
      )
  } else {
    new_table <- NULL
  }


  return(new_table)
}
