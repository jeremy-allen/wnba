# Plan: 2026 playoff page

Status: approved for implementation on September 25, 2026. Publication remains
subject to a separate review.

## What the dashboard already tracks

The **2026** page shows all 15 teams' regular-season records, points scored and
allowed per game, and recent results. Its player table ranks total points and
counts double-doubles and triple-doubles. Expanded rows show a player's totals,
per-game rates, shooting percentages, and points for each scheduled team game.
A comparison table puts players' per-game rates and shooting percentages beside
one another. Three interactive charts show cumulative points, points against
games played, and true shooting percentage against shot volume. Eight tabbed
tables rank points, assists, rebounds, steals, blocks, field goals, three-point
field goals, and turnovers. Player selection links the tables and charts through
crosstalk. Team cards filter those player views.

The dashboard is one static Quarto HTML document. `dashboard.qmd` prepares data
with R and lays out its pages. `styles.scss` defines the visual rules. The
dashboard uses `wehoop` data, with an ESPN top-up in `update_data.R`, and reads
the local `dat.rds` and `sched.rds` caches when rendered locally. Existing
`reactable`, `ggplot2`, `plotly`, `crosstalk`, and `htmltools` components can serve
the playoff page. The plan requires no new framework, service, or dependency.

## Source findings that determine the design

- The data preparation already classifies every `season_type == 3` game as
  `game_kind == "post"`. Every existing player and team statistic explicitly
  filters to `game_kind == "regular"`, so a playoff page can use the same caches
  without changing regular-season totals.
- On September 25, 2026, the local player-box cache has **no 2026 playoff
  games**. The local schedule has 29 playoff slots, and both team names are
  `TBD` in every slot. Twelve slots are for the first round, ten are for the
  semifinals, and seven are for the finals. Some slots are conditional games.
- Every one of those 29 slots has a national `broadcast_name` in `sched.rds`,
  including channel names and streaming services. That field identifies one
  listed outlet per slot. It does not establish that the outlet is the only
  way to watch the game.
- The same schedule still marks five September 24 regular-season games as
  scheduled, although their box scores and final scores are present. The
  existing `season_complete` calculation therefore reads `FALSE`. A playoff
  page must determine whether a game was played from reconciled game IDs and
  scores, rather than trusting the schedule's completion flag alone.
- The 2026 semifinal slots have `type_id == 1`, while their
  `notes_headline` values say `Semifinals`. A playoff round cannot be inferred
  from `type_id` alone. Classify 2026 rounds from the schedule headline, use
  known type IDs as a cross-check, and reject an unrecognized round.
- The [WNBA's 2026 playoff page](https://www.wnba.com/playoffs/2026) now names
  the first-round pairings even though the local schedule does not. The
  [WNBA's 2026 postseason FAQ](https://www.wnba.com/news/2026-wnba-postseason-faq)
  says eight teams qualify. The first round is best of three, the semifinals
  are best of five, and the finals are best of seven. Use those rules to check
  the bracket. Do not count unplayed conditional games as scheduled results.

## Proposed page

Add a **2026 Playoffs** page immediately after the **2026** page in
`dashboard.qmd`. Keep the year fixed at `2026L` so the playoff record remains a
2026 page when the current-season page advances to 2027. Use the existing dark
palette, typography, cards, tables, and chart theme.

1. **Bracket and series status.** Show four first-round series, two semifinal
   series, and one finals series. Each series card names the teams and seeds,
   shows wins in that series, and identifies the advancing team only after it
   has the required wins. Give each team its playoff game record and points
   scored and allowed per game once games exist. Derive first-round pairings
   from the final regular-season seed order, then compare them with the WNBA
   bracket before rendering. Advance teams through the published bracket path
   as each series ends. Until a later pairing is known, show `TBD` for that
   pairing. Do not assign the schedule's generic `TBD` game slots to a series.
2. **Games and where to watch.** Show a chronological `reactable` of actual
   games and upcoming schedule slots. Give each row a prominent **Watch**
   field beside its Eastern date and time. Include round, series when known,
   game number, teams or `Teams TBD`, final score or scheduled status, and the
   watch outlet. Show the cached `broadcast_name` when present. If an official
   WNBA listing confirms additional ways to watch the same game, list each
   channel or streaming service by name. Distinguish a national outlet from a
   local outlet when the source specifies a market. When no outlet is known,
   say `Broadcast not listed` rather than guessing. Keep `TBD` slots in the
   schedule so readers can see their watch outlets before matchups are named,
   but do not assign those slots to a series. Label conditional games as
   conditional and omit them from the default upcoming list after a series is
   decided. Identify a final game by matching its schedule row to a complete
   box score and valid scores. Flag a source mismatch before publication if a
   reported final lacks its box score.
3. **Players.** Add a playoff-only `reactable` of points, games played,
   points per game, double-doubles, and triple-doubles. Its expanded row uses
   the existing dossier style for playoff totals, shooting percentages, and a
   game-by-game points log with opponent and round. Add a comparison table for
   playoff per-game rates and percentages. Start its default columns with the
   leading total scorers, and show games played beside rate statistics so a
   one-game average is identifiable. Do not apply the regular-season 70% or
   525-point qualification rule to playoff players.
4. **Charts and leaders.** Adapt the current cumulative-points chart and the
   points-versus-games chart to playoff games. Show the efficiency chart only
   after players meet a stated, playoff-appropriate field-goal-attempt floor.
   Add the same eight tabbed stat leaderboards, computed from playoff games
   only. Reuse `ggplot2` plus `plotly` with the existing basic bundle and hover
   helpers, and reuse `stat_card()` where its input contract fits.

Before the first playoff box score arrives, show the bracket and a clear
`No playoff games completed in the current data` message in every statistics
area. Do not call `max()` on an empty player frame, render empty plots, or show
zeroes for games nobody has played. Keep the dashboard's existing data-through
and render dates visible.

## Data and interaction work after approval

1. Prepare `post_schedule` from 2026 schedule rows classified as `post` and
   `post_box` from 2026 player rows classified as `post`. Join by `game_id`.
   Deduplicate team-game and player-game rows before counting wins or games
   played. Use stable team IDs and the canonical player names already prepared
   in `dashboard.qmd`.
2. Reconcile schedule status with complete box scores. Use that reconciliation
   for playoff results and for the final regular-season seed order. Check that
   all 15 teams have 44 completed regular-season games after excluding the
   postponed duplicate. Compare the eight seeds and four first-round pairings
   with the official WNBA bracket. Stop the render if a mismatch could put a
   team in the wrong series.
3. Group completed games by round and the two team IDs. Count series wins from
   completed games only. Parse the round and game number from
   `notes_headline`, because the 2026 semifinal `type_id` is unreliable.
   Preserve the schedule's `TBD` and `If Necessary` meanings. Do not infer a
   result from a future row whose placeholder score is `0-0`.
   Keep each schedule slot's `game_id` attached to its watch information so a
   later team update does not pair a broadcast with the wrong game. Compare
   the cache's outlet with the WNBA's game listing and record any additional
   confirmed channel or streaming service without inventing one.
4. Reuse the existing `experience` crosstalk group for playoff player widgets
   so player selections work across pages and the navbar Clear control still
   works. Check the team-card roster map against postseason participants,
   including players absent from regular-season box scores, so a team filter
   does not silently exclude a playoff player.
5. Add only page-specific SCSS to `styles.scss`. Update `README.md` and
   `METHODOLOGY.md` after the page exists, because both documents currently say
   that every player and team figure is regular-season only.

## Validation after implementation

- Render with the current caches and inspect the empty-state page at desktop
  and phone widths. Verify that the four first-round pairings match the
  [WNBA bracket](https://www.wnba.com/playoffs/2026), that no `TBD` schedule
  slot becomes a result, and that regular-season statistics remain unchanged.
  Check that all 29 schedule slots display their cached watch outlet, including
  slots whose teams are still `TBD`.
- Exercise the series calculations against completed 2025 playoff games in
  the same caches, including an unused conditional game. Check round labels,
  game order, wins, advancement, and all player denominators by hand.
- After 2026 playoff games enter the caches, run `just data` and `just render`.
  Compare each displayed series score and several player totals with the WNBA
  game records. Compare each game's **Watch** field with the
  [WNBA's published schedule](https://www.wnba.com/news/2026-wnba-postseason-faq),
  including any separately listed streaming service. In the rendered browser
  page, test selection, team filtering, expanded rows, tabsets, keyboard
  access, and responsive layout. Check the single-file HTML size before
  considering publication.

Publication is a separate step after review of the implemented page and its
rendered output. The implementation approval did not authorize publication.
