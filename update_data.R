# Regenerate the local data caches (dat.rds, sched.rds) used by dashboard.qmd
# when config.yml selects data_source: "local".
#
# Run with:  Rscript update_data.R
#
# Two things this script guards against:
#
# 1. Silent corruption. wehoop pulls one .rds per season from the
#    sportsdataverse-data GitHub releases, and the pre-2024 files store their
#    columns as arrow ALTREP vectors. Without the arrow package loaded, readRDS
#    returns those as length-zero vectors and rbindlist(fill=TRUE) quietly pads
#    them to NA -- producing a cache with the right row count and date range but
#    no actual data. The validation gates below turn that into a hard failure.
#
# 2. Mirror lag. Those release files are a mirror that can run a few days behind
#    ESPN, and the release asset is re-uploaded on a schedule whether or not new
#    games were ingested -- so a fresh timestamp does not mean fresh data. The
#    top-up step fills the tail straight from ESPN, and the upcoming-games step
#    refreshes the games not yet played, which the mirror holds as placeholders
#    (teams TBD, no tip-off time) long after ESPN has filled them in.

library(wehoop)
library(arrow) # required: see note 1 above
library(dplyr)
library(lubridate)

seasons <- c(2004:as.integer(year(now())))

message("fetching seasons ", min(seasons), "-", max(seasons), " from the bulk mirror ...")

dat <- load_wnba_player_box(seasons = seasons)
sched <- load_wnba_schedule(seasons = seasons)

# gate 1: the bulk pull is sound (catches the ALTREP/arrow failure)
box_seasons <- sort(unique(dat$season[!is.na(dat$season)]))
sched_seasons <- sort(unique(sched$season[!is.na(sched$season)]))

stopifnot(
  "player box is missing seasons" =
    length(setdiff(seasons, box_seasons)) == 0,
  "player box has NA game_id" =
    !any(is.na(dat$game_id)),
  "player box has NA athlete_display_name" =
    mean(is.na(dat$athlete_display_name)) < 0.01,
  "schedule is missing seasons" =
    length(setdiff(seasons, sched_seasons)) == 0,
  "schedule has NA game_id" =
    !any(is.na(sched$game_id)),
  "schedule has NA type_id" =
    !any(is.na(sched$type_id))
)


# ---- ESPN top-up ------------------------------------------------------------

# The events ESPN's scoreboard lists for `dates`: one day as "YYYYMMDD" or one
# month as "YYYYMM". A range ("YYYYMMDD-YYYYMMDD") is answered with a 400.
espn_events <- function(dates) {
  # request via httr, the way wehoop itself does. Reading the URL straight into
  # jsonlite uses base R's url() connection, whose user agent ESPN answers with
  # a 403.
  url <- sprintf(
    paste0(
      "http://site.api.espn.com/apis/site/v2/sports/basketball/wnba",
      "/scoreboard?limit=1000&dates=%s"
    ),
    dates
  )

  resp <- try(httr::RETRY("GET", url, times = 3, quiet = TRUE), silent = TRUE)
  if (inherits(resp, "try-error") || httr::status_code(resp) != 200) {
    warning("could not reach ESPN for ", dates, call. = FALSE)
    return(NULL)
  }

  payload <- try(
    jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    ),
    silent = TRUE
  )
  if (inherits(payload, "try-error") || length(payload$events) == 0) {
    return(NULL)
  }

  payload$events
}

# Completed games ESPN has for a single date, as schedule rows matching the
# columns dashboard.qmd reads out of sched.
#
# Read from the raw ESPN endpoint rather than wehoop::espn_wnba_scoreboard()
# because that helper drops competitions[].type, which carries the authoritative
# type_id. type_id cannot be derived from season_type: season_type 2 covers
# regular season (1), All-Star (4) and Commissioner's Cup (39) alike, and the
# dashboard filters on type_id == 1, so guessing would pollute the leaderboards.
espn_final_games <- function(on_date) {
  events <- espn_events(format(on_date, "%Y%m%d"))

  rows <- lapply(events, function(event) {
    comp <- event$competitions[[1]]

    # only completed games; anything in progress would land as a partial box
    if (!identical(comp$status$type$name, "STATUS_FINAL")) {
      return(NULL)
    }

    sides <- comp$competitors
    names(sides) <- vapply(sides, function(x) x$homeAway, character(1))
    home <- sides[["home"]]
    away <- sides[["away"]]

    tibble::tibble(
      game_id = as.integer(event$id),
      # the queried date, not event$date -- the latter is UTC, which rolls an
      # evening Eastern tip-off onto the following day
      game_date = on_date,
      season = as.integer(event$season$year),
      season_type = as.integer(event$season$type),
      type_id = as.integer(comp$type$id),
      type_abbreviation = as.character(comp$type$abbreviation),
      home_display_name = as.character(home$team$displayName),
      home_score = as.integer(home$score %||% NA),
      away_display_name = as.character(away$team$displayName),
      away_score = as.integer(away$score %||% NA),
      attendance = as.numeric(comp$attendance %||% NA)
    )
  })

  bind_rows(rows)
}

last_bulk_date <- max(dat$game_date, na.rm = TRUE)
gap_dates <- if (last_bulk_date < Sys.Date()) {
  seq(as.Date(last_bulk_date) + 1, Sys.Date(), by = "day")
} else {
  as.Date(character(0))
}

if (length(gap_dates) > 0) {
  message(
    "bulk mirror ends ", last_bulk_date, "; checking ESPN for ",
    length(gap_dates), " day(s) through ", Sys.Date(), " ..."
  )

  new_games <- bind_rows(lapply(gap_dates, espn_final_games))

  # every day may come back empty or unreachable, leaving a 0-column frame
  if (!"game_id" %in% names(new_games)) {
    new_games <- new_games[0, ]
  } else {
    # dedupe against the player box, not the schedule. The mirror's schedule
    # already lists these games -- it just still has them as upcoming, with no
    # score -- so filtering on sched$game_id would discard every one of them.
    new_games <- new_games |>
      filter(!game_id %in% dat$game_id)
  }

  if (nrow(new_games) > 0) {
    # one call per game; the tail is only ever a handful of days
    new_box <- bind_rows(lapply(new_games$game_id, function(g) {
      out <- try(espn_wnba_player_box(game_id = g), silent = TRUE)
      if (inherits(out, "try-error")) {
        warning("could not fetch box score for game ", g, call. = FALSE)
        return(NULL)
      }

      # ESPN returns team_winner as a boolean for most games and as a "true" /
      # "false" string for the occasional one, and a single string-typed game
      # makes bind_rows refuse the whole list. Coercing is the right move
      # regardless of the bind: the bulk mirror stores the column as logical,
      # and dashboard.qmd counts a win with `team_winner %in% TRUE`, which
      # reads a character "TRUE" as a loss.
      out$team_winner <- as.logical(out$team_winner)
      out
    }))

    # only keep games whose box scores actually arrived, so the left_join in
    # dashboard.qmd never yields a game with no player rows
    new_games <- new_games |>
      filter(game_id %in% unique(new_box$game_id))

    dat <- bind_rows(dat, new_box)

    # refresh the stale scheduled rows in place (final score, confirmed type)
    # and append anything the mirror's schedule does not know about at all
    sched <- sched |>
      rows_update(
        new_games |> filter(game_id %in% sched$game_id),
        by = "game_id",
        unmatched = "ignore"
      ) |>
      bind_rows(new_games |> filter(!game_id %in% sched$game_id))

    message(
      "topped up ", nrow(new_games), " game(s) from ESPN, through ",
      max(new_games$game_date)
    )
  } else {
    message("ESPN has no completed games the mirror is missing")
  }
} else {
  message("bulk mirror is current through ", last_bulk_date)
}


# ---- ESPN upcoming games ----------------------------------------------------

# Games ESPN lists for a month that are not final yet, as schedule rows in the
# mirror's own column names and types.
espn_upcoming_games <- function(month) {
  events <- espn_events(format(month, "%Y%m"))

  rows <- lapply(events, function(event) {
    comp <- event$competitions[[1]]

    # final games belong to the top-up above, which also brings their box score
    if (identical(comp$status$type$name, "STATUS_FINAL")) {
      return(NULL)
    }

    sides <- comp$competitors
    names(sides) <- vapply(sides, function(x) x$homeAway, character(1))
    home <- sides[["home"]]$team
    away <- sides[["away"]]$team

    # Eastern, as the mirror stores it. A slot with no tip-off time yet comes
    # back as midnight Eastern, which dashboard.qmd reads as "Time TBD".
    tip_off <- with_tz(ymd_hm(comp$date, tz = "UTC"), "America/New_York")

    tibble::tibble(
      game_id = as.integer(event$id),
      date = as.character(comp$date),
      start_date = as.character(comp$startDate %||% comp$date),
      time_valid = as.logical(comp$timeValid %||% NA),
      notes_headline = as.character(comp$notes[[1]]$headline %||% NA),
      type_id = as.integer(comp$type$id),
      type_abbreviation = as.character(comp$type$abbreviation),
      venue_id = as.integer(comp$venue$id %||% NA),
      venue_full_name = as.character(comp$venue$fullName %||% NA),
      venue_address_city = as.character(comp$venue$address$city %||% NA),
      venue_address_state = as.character(comp$venue$address$state %||% NA),
      status_type_name = as.character(comp$status$type$name),
      home_id = as.integer(home$id),
      home_location = as.character(home$location),
      home_name = as.character(home$name),
      home_abbreviation = as.character(home$abbreviation),
      home_display_name = as.character(home$displayName),
      home_short_display_name = as.character(home$shortDisplayName),
      home_logo = as.character(home$logo %||% NA),
      away_id = as.integer(away$id),
      away_location = as.character(away$location),
      away_name = as.character(away$name),
      away_abbreviation = as.character(away$abbreviation),
      away_display_name = as.character(away$displayName),
      away_short_display_name = as.character(away$shortDisplayName),
      away_logo = as.character(away$logo %||% NA),
      season = as.integer(event$season$year),
      season_type = as.integer(event$season$type),
      game_date_time = tip_off,
      game_date = as.Date(tip_off, tz = "America/New_York"),
      # the mirror keeps the first outlet's name and market here, and every
      # outlet in `broadcast`
      broadcast_market = as.character(
        if (length(comp$broadcasts) > 0) comp$broadcasts[[1]]$market else ""
      ),
      broadcast_name = as.character(
        if (length(comp$broadcasts) > 0) comp$broadcasts[[1]]$names[[1]] else ""
      ),
      broadcast = as.character(comp$broadcast %||% "")
    )
  })

  bind_rows(rows)
}

# from this month through the last month the mirror's schedule reaches
last_sched_date <- max(sched$game_date[sched$season == max(seasons)], na.rm = TRUE)
upcoming_months <- if (last_sched_date >= Sys.Date()) {
  seq(floor_date(Sys.Date(), "month"), floor_date(last_sched_date, "month"),
      by = "month")
} else {
  as.Date(character(0))
}

upcoming <- bind_rows(lapply(upcoming_months, espn_upcoming_games))

if ("game_id" %in% names(upcoming)) {
  # a game with a box score is finished, whatever ESPN's status flag says
  upcoming <- upcoming |>
    filter(!game_id %in% dat$game_id)

  sched <- sched |>
    rows_update(
      upcoming |> filter(game_id %in% sched$game_id),
      by = "game_id",
      unmatched = "ignore"
    ) |>
    bind_rows(upcoming |> filter(!game_id %in% sched$game_id))

  message("refreshed ", nrow(upcoming), " upcoming game(s) from ESPN")
} else {
  message("ESPN lists no upcoming games")
}


# gate 2: the combined result is still coherent
stopifnot(
  "duplicate game_id in schedule" =
    !any(duplicated(sched$game_id)),
  "player box has NA game_id after top-up" =
    !any(is.na(dat$game_id)),
  "schedule has NA type_id after top-up" =
    !any(is.na(sched$type_id)),
  "player box has games missing from the schedule" =
    length(setdiff(unique(dat$game_id), unique(sched$game_id))) == 0
)

saveRDS(dat, "dat.rds")
saveRDS(sched, "sched.rds")

message(
  "wrote dat.rds (", nrow(dat), " rows, ",
  n_distinct(dat$athlete_display_name), " athletes) and sched.rds (",
  nrow(sched), " rows) for seasons ",
  min(box_seasons), "-", max(box_seasons),
  "; data through ", max(dat$game_date, na.rm = TRUE)
)
