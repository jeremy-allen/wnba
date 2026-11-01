# Offline tests for checks.R, run from the repository root with `just test`.
# Every case uses made-up games, so these need no network and no data caches.
# Most of them replay the 2026 Finals, where Game 7 reached the box scores but
# stayed "Scheduled" in the schedule and Atlanta was listed as champion.

source("checks.R")

failures <- 0L

report <- function(ok, label) {
  message(if (ok) "ok    " else "FAIL  ", label)
  if (!ok) failures <<- failures + 1L
}

passes <- function(label, code) {
  report(!inherits(try(code, silent = TRUE), "try-error"), label)
}

stops_with <- function(label, pattern, code) {
  result <- try(code, silent = TRUE)
  report(inherits(result, "try-error") && grepl(pattern, result), label)
}

# A Finals whose games were won by `winners`, in order.
finals_of <- function(winners, season = 2026L) {
  tibble::tibble(
    season = season,
    game_date = as.Date("2026-10-17") + seq_along(winners),
    winner = winners
  )
}

gsv <- "Golden State Valkyries"
atl <- "Atlanta Dream"
tied_series <- c(gsv, atl, atl, gsv, gsv, atl)

# ---- crown_champions --------------------------------------------------------

report(
  nrow(crown_champions(finals_of(tied_series))) == 0,
  "a Finals tied 3-3 has no champion, whoever won last"
)
report(
  identical(crown_champions(finals_of(c(tied_series, gsv)))$champion, gsv),
  "the team that reaches four wins is the champion"
)
report(
  nrow(crown_champions(finals_of(c(gsv, gsv, gsv)))) == 0,
  "three wins is not enough in a best of seven"
)
report(
  identical(crown_champions(finals_of(c(atl, gsv, gsv, atl, gsv), 2024L))$champion, gsv),
  "three wins take a best of five"
)
report(
  identical(crown_champions(finals_of(c(gsv, atl, gsv), 2004L))$champion, gsv),
  "two wins take the best of three in 2004"
)

# ---- check_schedule_settled -------------------------------------------------

box <- tibble::tibble(
  game_id = c(1L, 1L),
  team_display_name = c(gsv, atl),
  team_winner = c(TRUE, FALSE)
)

settled <- tibble::tibble(
  game_id = 1L,
  home_display_name = gsv, away_display_name = atl,
  status_type_completed = TRUE, home_winner = TRUE, away_winner = FALSE
)

passes("a finished game that agrees with its box score", {
  check_schedule_settled(box, settled)
})
passes("an unplayed game with no box score", {
  upcoming <- settled |>
    mutate(game_id = 2L, status_type_completed = FALSE, home_winner = NA, away_winner = NA)
  check_schedule_settled(box, bind_rows(settled, upcoming))
})
stops_with("a box score against a game still marked scheduled", "not settled", {
  stale <- mutate(settled, status_type_completed = FALSE, home_winner = NA, away_winner = NA)
  check_schedule_settled(box, stale)
})
stops_with("a finished game with no winner", "not settled", {
  check_schedule_settled(box, mutate(settled, home_winner = NA, away_winner = NA))
})
stops_with("a winner that contradicts the box score", "different winners", {
  check_schedule_settled(box, mutate(settled, home_winner = FALSE, away_winner = TRUE))
})

# ---- check_champion_matches_bracket -----------------------------------------

champions <- tibble::tibble(season = c(2026L, 2025L), champion = c(gsv, "Las Vegas Aces"))
last_year_only <- filter(champions, season == 2025L)

passes("the list and the bracket name the same champion", {
  check_champion_matches_bracket(champions, 2026L, gsv)
})
passes("neither has a champion while the Finals are open", {
  check_champion_matches_bracket(last_year_only, 2026L, NA_character_)
})
stops_with("the list names a different champion", "bracket says", {
  check_champion_matches_bracket(mutate(champions, champion = atl), 2026L, gsv)
})
stops_with("the list names a champion before the bracket has one", "bracket says", {
  check_champion_matches_bracket(champions, 2026L, NA_character_)
})
stops_with("the bracket has a champion and the list has none", "bracket says", {
  check_champion_matches_bracket(last_year_only, 2026L, gsv)
})

# ---- check_stars_clear_crests -----------------------------------------------

crest <- tibble::tibble(row = 3, end = 2026)

passes("a star against the crest", {
  beside <- tibble::tibble(row = 3, star_x = 2026 - 0.3 - 0.1)
  check_stars_clear_crests(beside, crest, 0.6, 0.1)
})
passes("a star at the crest's season on another line", {
  check_stars_clear_crests(tibble::tibble(row = 4, star_x = 2026), crest, 0.6, 0.1)
})
stops_with("a star under the crest", "hidden behind a crest", {
  check_stars_clear_crests(tibble::tibble(row = 3, star_x = 2026), crest, 0.6, 0.1)
})

if (failures > 0) stop(failures, " test(s) failed", call. = FALSE)
message("all tests passed")
