## Where the numbers come from

`wehoop::load_wnba_player_box()` and `load_wnba_schedule()` read pre-built files
from the **sportsdataverse-data** GitHub releases. That is a mirror of ESPN,
rebuilt on its own schedule, and it usually runs a day or two behind. The
release asset is re-uploaded whether or not new games were ingested, so a fresh
file timestamp is not evidence of fresh data.

`update_data.R` closes that gap. After the bulk pull it asks ESPN directly for
any completed game the mirror is missing and merges it in. It then asserts full
season coverage and non-NA keys before writing the cache, so a broken fetch
fails loudly instead of quietly caching a hollow file.

The navbar carries two dates, and they answer different questions:

- **Data through** — the most recent game in the data
- **Rendered** — when the page was last built

They drift apart whenever the page is rebuilt without refreshing the data.
Rendering does not fetch anything.

> The `arrow` package must stay installed. Season files before 2024 store their
> columns as arrow ALTREP vectors; without it `readRDS` silently returns
> zero-length vectors that get padded to NA. The result looks like a complete
> dataset — right row count, right date range — with no data in it.

## Which games count

Every figure on this dashboard uses **regular-season games only**
(`type_id == 1`).

| Excluded | `type_id` |
|---|---|
| All-Star exhibitions | 4 |
| Commissioner's Cup | 39 |
| Playoffs (all rounds) | 14, 15, 16, 17 |

All-Star rosters are dropped as the data loads, since they are not real teams.
That test used to be `team_id < 90`, which worked when only exhibition rosters
had large ESPN ids — until ESPN began issuing six-digit ids to new franchises
too, and it started silently deleting Golden State, Toronto and Portland.
Excluding by game type is what was always meant.

**Playoff performances therefore appear nowhere**, career totals included.

## What counts as a game played

A row in the box score is not the same thing as a game played. ESPN carries one
row per rostered player per game, whether or not that player took the floor. A
row is counted only when **both** hold:

1. `did_not_play` is not `TRUE`
2. the row has a stat line — `points` is not `NA`

The second condition exists because `did_not_play` only became reliable in
2013. Before that, 10–22% of rows per season are inactive players it never
flagged, appearing as a row with no minutes and every stat column `NA`. There
are 7,654 such rows. The test is safe: of the 102,076 rows that do carry
minutes, not one is blank across all thirteen stat columns.

This is the rule that matters most, and the easiest to get wrong. Counting
roster appearances rather than games played pushes every per-game average
below ESPN's — Caitlin Clark, who had missed three games, read 19.3 points per
game instead of 21.5, which was enough to drop her out of the comparison table
altogether.

## Points and per-game averages

- **Totals** — the sum of `points` across counted games.
- **Per game** — total points ÷ games played, using the same definition of
  "played" as above. Games missed never enter the denominator.
- Sparklines under *Per Season* and *Per Game* are the same numbers, one bar
  per season or per game, on a scale shared across all players in the table.

## Known differences from ESPN

- **No minimum-games qualifier.** ESPN requires a minimum before ranking a
  player on a per-game leaderboard; this dashboard does not. Kelsey Plum's
  23.6 points per game over 13 games ranks third here and is omitted there.
  The stat leaderboards rank by season totals, so they are unaffected.
- **The shooting-efficiency scatter applies its own floor** of 50 field-goal
  attempts, to keep small samples from dominating the percentage axis.
- **Playoffs are excluded**, so career totals here run below any source that
  includes postseason play.

## Refreshing the dashboard

```bash
Rscript update_data.R                          # fetch, validate, cache
quarto render dashboard.qmd                    # rebuild from the cache
quarto publish posit-connect-cloud dashboard.qmd --no-render
```

The data caches (`dat.rds`, `sched.rds`) are deliberately untracked, as is the
rendered `dashboard.html` — it ran to roughly 10 MB per render and git cannot
delta-compress minified HTML. Publishing sends the rendered file straight from
disk, so `--no-render` ships exactly the artifact you inspected.
