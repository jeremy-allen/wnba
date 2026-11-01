# The rules that decide who won, and the checks that stop a wrong answer before
# it is published. They live here rather than in dashboard.qmd so that
# tests/test-checks.R can run them against small made-up games: a chunk in a
# Quarto document can only be exercised by rendering the whole dashboard.
#
# update_data.R and dashboard.qmd both source this file, so every check runs on
# every `just update`, and a failure stops the update before it publishes.

library(dplyr)

# One player, two careers: ESPN gave these four a new athlete id along with a new
# name, so nothing in the box scores connects the two. Each maps to the name the
# league lists her career under.
same_player <- c(
  "Katie Feenstra" = "Katie Mattera",
  "Erin Buescher" = "Erin Perperoglou",
  "Shanna Crossley" = "Shanna Zolman",
  "Tamika Williams-Jeter" = "Tamika Raymond"
)

# The name a career goes by. ESPN writes many names from the 2000s with two
# spaces, which shows as one space on the page and fails an exact match.
career_name <- function(name) {
  clean <- trimws(gsub("\\s+", " ", name))
  coalesce(unname(same_player[clean]), clean)
}

# Best of three in 2004, best of five through 2024, best of seven since.
finals_wins_needed <- function(season) {
  case_when(season >= 2025 ~ 4L, season >= 2005 ~ 3L, .default = 2L)
}

# One row per season whose Finals are decided. The winner of the last game
# played is the champion only once the series is over. Mid-series it is just
# whoever won last: a Finals tied 3-3 would crown the team that tied it.
crown_champions <- function(finals) {
  finals |>
    arrange(season, game_date) |>
    summarise(
      champion = last(winner),
      wins = sum(winner == last(winner)),
      .by = season
    ) |>
    filter(wins >= finals_wins_needed(season)) |>
    select(-wins)
}

# Every game with a box score has to be finished in the schedule too, with the
# same winner. The champions list reads the schedule and the Playoffs page reads
# the box scores, so a game only one of them has finished puts two different
# answers on the same dashboard.
check_schedule_settled <- function(dat, sched) {
  box_winners <- dat |>
    filter(team_winner %in% TRUE) |>
    distinct(game_id, box_winner = team_display_name)

  boxed <- sched |>
    as_tibble() |>
    filter(game_id %in% dat$game_id) |>
    left_join(box_winners, by = "game_id") |>
    mutate(
      schedule_winner = case_when(
        home_winner %in% TRUE ~ home_display_name,
        away_winner %in% TRUE ~ away_display_name
      )
    )

  unsettled <- boxed |>
    filter(!status_type_completed %in% TRUE | is.na(schedule_winner))
  conflicted <- boxed |>
    filter(!is.na(schedule_winner), schedule_winner != box_winner)

  if (nrow(unsettled) > 0) {
    stop(
      "schedule has not settled games that have box scores: ",
      toString(unsettled$game_id),
      call. = FALSE
    )
  }
  if (nrow(conflicted) > 0) {
    stop(
      "schedule and box scores name different winners for games: ",
      toString(conflicted$game_id),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# The champions list and the bracket are worked out from different data, so
# their agreement means something. `bracket_winner` is NA until the Finals are
# decided, and the list must then have no champion for that season at all.
check_champion_matches_bracket <- function(champions, season, bracket_winner) {
  listed <- champions$champion[champions$season == season]
  expected <- bracket_winner[!is.na(bracket_winner)]

  if (!identical(as.character(listed), as.character(expected))) {
    stop(
      "the ", season, " champions list says [", toString(listed),
      "] but the Finals bracket says [", toString(expected), "]",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# A title drawn inside a crest cannot be seen. `titles` needs row and star_x;
# `crests` needs row and end. The tolerance lets a star touch a crest, which is
# where the star of a title won in the latest season is placed on purpose.
check_stars_clear_crests <- function(titles, crests, crest_seasons, star_seasons) {
  hidden <- titles |>
    inner_join(crests, by = "row") |>
    filter(abs(star_x - end) < crest_seasons / 2 + star_seasons - 1e-9)

  if (nrow(hidden) > 0) {
    stop(
      "a championship star is hidden behind a crest in rows: ",
      toString(unique(hidden$row)),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
