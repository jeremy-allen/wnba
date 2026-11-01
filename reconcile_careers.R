# Compare this dashboard's regular-season career totals with the league's.
#
# data/leaders-table.csv is a copy of stats.wnba.com/alltime-leaders, pasted by
# hand because the page has no export and the API does not answer. Run from the
# repository root:  Rscript reconcile_careers.R
#
# Every league career lands in exactly one cause, so the causes can be counted
# rather than argued about. Nothing here feeds the dashboard.

library(dplyr)
library(readr)
library(stringr)
source("checks.R")

# The league table stopped updating partway through 2026. This is the date at
# which the most careers match it exactly; a later copy of the table needs a
# later date, or none. Comparing past it reads as this dashboard counting too
# many games.
league_through <- as.Date("2026-08-16")

# The same career under another spelling. Each entry maps the name used here to
# the name the league uses, and only matters to this comparison. Careers that are
# actually split in two are merged for the dashboard by `same_player` in checks.R.
league_names <- c(
  "Camille Little" = "Camille Smith",
  "Iziane Castro" = "Iziane Castro Marques",
  "DeMya Chakheia Walker" = "DeMya Walker",
  "Nia Brodie" = "Nia Coffey",
  "Tiffany Jackson-Jones" = "Tiffany Jackson",
  "Mistie McCray Bass" = "Mistie Bass",
  "Jayne Appel-Marinelli" = "Jayne Appel Marinelli",
  "Vanessa Hayden-Johnson" = "Vanessa Hayden",
  "Aneika Henry-Morello" = "Aneika Morello",
  "AD Durr" = "Asia (AD) Durr",
  "Suzy Batkovic" = "Suzy Batkovic-Brown",
  "Amanda Zahui B." = "Amanda Zahui B",
  "Ivana Dojkic" = "Ivana Dojkić",
  "Clarissa dos Santos" = "Clarissa Dos Santos",
  "Marie Gulich" = "Marie Gülich",
  "Ji-Su Park" = "JiSu Park",
  "Nikki McCray-Penson" = "Nikki McCray"
)

# Two schedule rows with no box score that are not missing games: an exhibition
# and a game listed 0-0.
not_real_games <- c(240805096L, 401018942L)

league <- read_csv("data/leaders-table.csv", show_col_types = FALSE) |>
  transmute(
    player,
    lg_gp = GP, lg_pts = PTS, lg_reb = REB, lg_ast = AST
  )

sched <- readRDS("sched.rds") |>
  as_tibble() |>
  mutate(
    game_kind = case_when(
      type_id == 4 ~ "allstar",
      type_id == 39 ~ "cup",
      season_type == 3 ~ "post",
      season_type == 2 & type_id == 1 ~ "regular",
      .default = "other"
    )
  )

# Games where the players listed add up to fewer points than their team scored,
# so at least one player's line is absent. Read before any row is filtered out.
raw_box <- readRDS("dat.rds") |>
  as_tibble() |>
  left_join(select(sched, game_id, game_kind), by = "game_id")

partial_games <- raw_box |>
  filter(game_kind == "regular", !is.na(team_score)) |>
  summarise(
    listed = sum(points, na.rm = TRUE),
    scored = first(team_score),
    .by = c(game_id, season, team_display_name)
  ) |>
  filter(listed < scored) |>
  count(season, team_display_name, name = "team_partial")

# The same filters dashboard.qmd applies, in the same order. About 200 rows that
# pass them have no minutes recorded, and no rule sorts those out: dropping them
# fixes careers that show a game too many and breaks ones that match exactly
# (Angel McCoughtry falls to 309 games against the league's 311). The league
# counts some of them, so they get their own cause below.
box <- raw_box |>
  filter(game_kind != "allstar", !(did_not_play %in% TRUE), !is.na(points)) |>
  mutate(
    athlete_display_name = if (is.na(athlete_id[1])) {
      athlete_display_name
    } else {
      athlete_display_name[which.max(game_date)]
    },
    .by = athlete_id
  ) |>
  filter(game_kind == "regular", game_date <= league_through) |>
  mutate(
    athlete_display_name = career_name(athlete_display_name),
    plain = stringi::stri_trans_general(athlete_display_name, "Latin-ASCII"),
    player = coalesce(unname(league_names[athlete_display_name]), plain)
  )

ours <- box |>
  summarise(
    gp = n_distinct(game_id),
    pts = sum(points),
    reb = sum(rebounds, na.rm = TRUE),
    ast = sum(assists, na.rm = TRUE),
    first_seen = min(season),
    names_here = n_distinct(athlete_display_name),
    .by = player
  )

# The most games a career could have lost to missing or partial box scores:
# every such game played by a team she was on that season. An upper bound, since
# she may have sat some of them out.
missing_games <- sched |>
  filter(
    game_kind == "regular",
    status_type_completed %in% TRUE,
    !game_id %in% box$game_id,
    !game_id %in% not_real_games,
    game_date <= league_through
  ) |>
  select(game_id, season, home_display_name, away_display_name) |>
  tidyr::pivot_longer(
    c(home_display_name, away_display_name),
    values_to = "team_display_name"
  ) |>
  count(season, team_display_name, name = "team_missing")

exposure <- box |>
  distinct(player, season, team_display_name) |>
  left_join(missing_games, by = c("season", "team_display_name")) |>
  left_join(partial_games, by = c("season", "team_display_name")) |>
  summarise(
    could_miss = sum(coalesce(team_missing, 0L) + coalesce(team_partial, 0L)),
    .by = player
  )

compared <- league |>
  left_join(ours, by = "player") |>
  left_join(exposure, by = "player") |>
  mutate(
    could_miss = coalesce(could_miss, 0L),
    short_gp = lg_gp - gp,
    short_pts = lg_pts - pts,
    cause = case_when(
      is.na(gp) ~ "career ended before 2004",
      short_gp == 0 & short_pts == 0 & lg_reb == reb & lg_ast == ast ~ "exact",
      short_pts == 0 & lg_reb == reb & lg_ast == ast ~ "same stats, game count differs",
      short_gp == 0 & short_pts > 0 & could_miss > 0 ~ "missing or partial box scores",
      short_gp == 0 ~ "same games, stats differ",
      short_gp < 0 ~ "more games here than the league",
      short_gp <= could_miss ~ "missing or partial box scores",
      # more than a stray game or two: a season the box scores do not reach
      short_gp - could_miss >= 15 ~ "seasons before 2004",
      .default = "unexplained"
    )
  )

summary_by_cause <- compared |>
  summarise(
    careers = n(),
    games_short = sum(coalesce(short_gp, lg_gp)),
    points_short = sum(coalesce(short_pts, lg_pts)),
    .by = cause
  ) |>
  arrange(desc(careers))

print(summary_by_cause)

# the claim METHODOLOGY.md makes about the top twenty careers
top_twenty <- compared |>
  arrange(desc(lg_pts)) |>
  slice_head(n = 20) |>
  select(player, cause, gp, lg_gp, pts, lg_pts, short_pts)
print(top_twenty, n = 20)

# careers here with enough games to be on the league's table, but not on it
absent <- ours |>
  filter(!player %in% league$player, gp >= min(league$lg_gp)) |>
  arrange(desc(pts))
message("careers here that the league table lacks: ", nrow(absent))
print(absent, n = 20)

for (one_cause in c(
  "same games, stats differ", "more games here than the league", "unexplained"
)) {
  message("\n", one_cause)
  compared |>
    filter(cause == one_cause) |>
    arrange(desc(abs(short_pts))) |>
    select(player, gp, lg_gp, pts, lg_pts, short_pts, reb, lg_reb, ast, lg_ast) |>
    print(n = 25)
}
