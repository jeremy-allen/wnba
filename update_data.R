# Regenerate the local data caches (dat.rds, sched.rds) used by dashboard.qmd
# when config.yml selects data_source: "local".
#
# Run with:  Rscript update_data.R
#
# The validation gate below is the point of this script. wehoop pulls one .rds
# per season from the sportsdataverse-data GitHub releases, and the pre-2024
# files store their columns as arrow ALTREP vectors. Without the arrow package
# loaded, readRDS returns those as length-zero vectors and rbindlist(fill=TRUE)
# quietly pads them out to NA -- producing a cache that has the right number of
# rows and the right date range but no actual data. Assert, then write.

library(wehoop)
library(arrow) # required: see note above
library(dplyr)
library(lubridate)

seasons <- c(2004:as.integer(year(now())))

message("fetching seasons ", min(seasons), "-", max(seasons), " ...")

dat <- load_wnba_player_box(seasons = seasons)
sched <- load_wnba_schedule(seasons = seasons)

# validation gate: fail loudly rather than cache garbage
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

saveRDS(dat, "dat.rds")
saveRDS(sched, "sched.rds")

message(
  "wrote dat.rds (", nrow(dat), " rows, ",
  n_distinct(dat$athlete_display_name), " athletes) and sched.rds (",
  nrow(sched), " rows) for seasons ",
  min(box_seasons), "-", max(box_seasons)
)
