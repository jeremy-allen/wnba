library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(wehoop)
library(ggrepel)
library(reactable)

pbp <- wehoop::load_wnba_pbp()
dat <- load_wnba_player_box(seasons = c(2004:2024))

athletes <- dat |> select(athlete_id, athlete_display_name)

cc <- athletes |>
  filter(athlete_display_name == "Caitlin Clark") |> 
  pull(athlete_id) |> 
  unique()
ar <- athletes |>
  filter(athlete_display_name == "Angel Reese") |> 
  pull(athlete_id) |> 
  unique()

dat |> 
  #filter(athlete_id %in% c(cc, ar)) |> 
  filter(season_type == 2) |> 
  summarise(
    position = first(athlete_position_name),
    games_played = n_distinct(game_id),
    total_points = sum(points, na.rm = TRUE),
    goals_made = sum(field_goals_made, na.rm = TRUE),
    threes_made = sum(three_point_field_goals_made, na.rm = TRUE),
    frees_made = sum(free_throws_made, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_rebounds = sum(rebounds, na.rm = TRUE),
    total_steals = sum(steals, na.rm = TRUE),
    total_blocks = sum(blocks, na.rm = TRUE),
    total_turnovers = sum(turnovers, na.rm = TRUE),
    total_fouls = sum(fouls, na.rm = TRUE),
    total_ejections = sum(ejected, na.rm = TRUE),
    .by = athlete_display_name
  ) |> 
  rename(
    athlete = athlete_display_name
  ) |> 
  reactable(
    filterable = TRUE
  )

pbp |> names()

sched <- load_wnba_schedule(2024)

###---------------------------------------------------------------


years <- c(2004:2023)
dat <- dat |> 
  mutate(
    game_year = year(game_date),
    game_month = month(game_date, label = TRUE, abbr = TRUE)
  ) |>
  mutate(
    game_year = factor(game_year, levels = years, labels = years,
    ordered = TRUE)
  )

rookie_season <- dat |> 
  summarise(
    games_played = length(unique(game_id)),
    .by = c(athlete_display_name, season)
  ) |> 
  arrange(athlete_display_name, season) |> 
  summarise(
    rookie_season = first(season),
    rookie_games = first(games_played),
    seasons = length(unique(season)),
    .by = athlete_display_name
  )

stats <- dat |>  
  arrange(game_date) |> 
  summarise(
    position = first(athlete_position_name),
    years_played = length(unique(as.character(game_year))),
    first_year = first(game_year),
    latest_year = last(game_year),
    fg_made = sum(field_goals_made, na.rm = TRUE),
    threes_made = sum(three_point_field_goals_made, na.rm = TRUE),
    points_made = sum(points, na.rm = TRUE),
    .by = athlete_display_name
  ) |> 
  arrange(desc(points_made))

guard_stats <- stats |> 
  filter(position == "Guard") |> 
  arrange(desc(points_made))


pick_guards <- function(data, min, max, sample) {
  data |> 
  filter(between(years_played, min, max)) |> 
  #slice(1:100) |> 
  slice_sample(n = sample, replace = FALSE) |> 
  pull(athlete_display_name)
}


draw <- function(data, players) {
  dat <- data |>
  arrange(athlete_display_name, game_year) |>
  filter(athlete_display_name %in% players) |>  
  summarise(
    points_made = sum(points, na.rm = TRUE),
    .by = c(athlete_display_name, game_year)
  ) |> 
  ungroup() |> 
  group_by(athlete_display_name) |> 
  mutate(
    #points_delta = points_made - first(points_made),
    points_delta =  ((points_made - first(points_made))/ first(points_made))*100,
    year_order = row_number(),
    final_label = if_else(
      game_year == last(game_year),
      paste(first(game_year),
      "-",
      last(game_year),
      ": ",
      sum(points_made),
      " total points"),
      ""
    )
  ) |> 
  ungroup() |> 
  arrange(athlete_display_name, game_year)

  final_year <- max(dat$year_order)

  ggplot(
    data = dat,
    aes(label = final_label)
  ) +
  scale_x_continuous(
    expand = expansion(add = c(.5, 2)),
    breaks = seq(1, final_year, 1)
  ) +
  geom_point(
    data = dat |> filter(as.character(game_year) == "2020"),
    aes(
      x = year_order,
      y = points_delta,
      group = athlete_display_name
    ),
    color = "black",
    alpha = 1,
    size = 4,
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = dat |> filter(as.character(game_year) == "2020"),
    aes(
      x = year_order,
      y = points_delta,
      group = athlete_display_name
    ),
    direction = "x",
    size = 2,
    label = "2020\ncovid",
    show.legend = FALSE
  ) +
  geom_hline(
      yintercept = 0,
      color = "black",
      show.legend = FALSE
  ) +
  geom_line(
    data = dat,
    aes(
      x = year_order,
      y = points_delta,
      color = athlete_display_name,
      group = athlete_display_name
    ),
    alpha = .7,
    linewidth = 1.25
  ) +
  geom_point(
    data = dat,
    aes(
      x = year_order,
      y = points_delta,
      color = athlete_display_name
    ),
    alpha = 1,
    size = 2,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = dat,
    aes(
      x = year_order,
      y = points_delta,
      group = athlete_display_name,
      fill = athlete_display_name,
      segment.colour = "black"
    ),
    size = 3,
    nudge_x = .7,
    nudge_y = 0,
    colour = "white",
    min.segment.length = 1,
    max.overlaps = 40,
    show.legend = FALSE
  ) +
  labs(
    title = "How It Started How It's Going - WNBA Points Over A Career",
    subtitle = "What happens after year one? After a player's first year,\ndo they score fewer points or more points than their rookie year?",
    caption = "\ndata from the wehoop R package,which\nsources from ESPN and WNBA",
    x = "Career Year",
    y = "Percent difference from year one",
    color = "Player: "
  ) +
  theme(
    legend.position = "top",
    aspect.ratio=2/3,
    plot.title = element_text(margin = margin(20, 1, 9, 1, "pt")),
    plot.subtitle = element_text(margin = margin(1, 1, 18, 1, "pt")),
    plot.caption = element_text(margin = margin(1, 1, 20, 1, "pt")),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e7e7f0"),
    panel.background = element_rect(fill = "white", color = "white")
)
}
  
guards <- pick_guards(guard_stats, 3, 19, 3)
draw(dat, guards)

