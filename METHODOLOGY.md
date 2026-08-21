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

Every figure on this dashboard uses **regular-season games only**. Each game is
sorted once as the data loads, into a `game_kind` the rest of the document
reads:

| `game_kind` | what it is | how it is recognised |
|---|---|---|
| `regular` | regular season | `season_type == 2` **and** `type_id == 1` |
| `post` | playoffs, every round | `season_type == 3` |
| `allstar` | All-Star exhibition | `type_id == 4` |
| `cup` | Commissioner's Cup final | `type_id == 39` |

**`type_id` names a round, not a part of the season, and it cannot be asked
which part a game belongs to.** The playoff rounds — RD16 14, QTR 15, SEMI 16,
FINAL 17 — only exist from 2019 on. Every playoff game from 2004 through 2018,
and six of 2019's, carries `type_id == 1` under `season_type == 3`, which is
what a regular-season game looks like from that column alone.

This dashboard read `type_id == 1` as "regular season" in twelve places, and so
counted 282 playoff games as regular-season ones: 43,041 points across 374 of
934 careers. Diana Taurasi showed 11,753 career points against the league's
10,646, Tina Charles 8,661 against 8,396. The two columns are only ever read
together now, in one place, and `game_kind` is the answer everywhere else.

`type_id == 15` is the other half of the same trap. The old exclusion list
named rounds 14, 16 and 17 and missed the quarter-finals, so four games from
2020 and 2021 appeared on neither the regular-season pages nor the postseason
one. Recognising the playoffs by `season_type` cannot miss a round.

All-Star rosters are dropped as the data loads, since they are not real teams.
That test used to be `team_id < 90`, which worked when only exhibition rosters
had large ESPN ids — until ESPN began issuing six-digit ids to new franchises
too, and it started silently deleting Golden State, Toronto and Portland.
Excluding by game type is what was always meant.

**Playoff performances therefore appear nowhere**, career totals included.

## One career, one name

Every aggregation groups players by display name, so a player who changed hers
came out as two careers. Skylar Diggins-Smith held 4,255 points and Skylar
Diggins 1,541, where the league lists one Skylar Diggins with 5,758. Six
careers were split this way; Cheyenne Parker-Tyus, Betnijah Laney-Hamilton,
Megan DiLeo, Haley Peters and Maria Kliundikova were the others.

The fix canonicalises the **name** — every row an `athlete_id` owns takes the
most recent name that id used — rather than regrouping on the id itself.
Grouping by id would break three careers that matching on name gets right,
because ESPN issued two ids to one player: Li Yueru (4422426 and 4336633),
Wendy Palmer (317 and 599) and Isabel Sanchez (548 and 596). Canonicalising the
name merges the splits and leaves those three merged.

## How far back the data goes

**The box scores start in 2004. The league started in 1997.** A career that
opened before 2004 is counted here only from 2004 on, and there is no way to
extend it: `wehoop` reaches back to 2002, but the 2002 file holds 322 box-score
rows where a season is about 4,500, and 1997–2001 does not exist at all. A
half-loaded season would read as coverage and would not be, so the floor stays
at 2004 and is stated instead.

Sue Bird and Tamika Catchings are each short exactly 74 games — their 2002 and
2003 seasons. Lisa Leslie, Sheryl Swoopes, Tina Thompson, Katie Smith and
Lauren Jackson lose more.

## What is left against the league's own figures

Checked against
[stats.wnba.com/alltime-leaders](https://stats.wnba.com/alltime-leaders/),
regular season, totals: of the top twenty careers, eighteen agree to within 30
points — a few tenths of one percent — and Brittney Griner, Angel McCoughtry
and Skylar Diggins agree exactly, games included. The two that do not are Bird
and Catchings, for the reason above.

The remaining fraction is the mirror itself. 47 games from 2004 to 2018 have a
schedule row and no box score at all, 23 of them in 2004, so the totals run
slightly light: Taurasi 10,525 here against 10,646, Cappie Pondexter 6,741
against 6,811. That is a limit of the source, not a rule of this dashboard, and
it is not worked around.

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

## Who qualifies for a ranking

The league publishes its own bar at
[stats.wnba.com/help/statminimums](https://stats.wnba.com/help/statminimums/),
under the heading *Statistical Minimums to Qualify For WNBA League Leaders*.
Every threshold there is written per season — "70% of team games played (31 in
a 44-game season)", "525 Points", "100 field goals made" — and every one of
them gates a **season** leaders list.

The comparison table ranks this season by points per game, so it uses the
league's scoring rule: **70% of the games her team has played, or 525 points.**
A player traded mid-season is measured against the team she is on now, which is
the nearest single reading of a rule written for a whole season. Nobody is
removed from the table by this — the columns it opens on are the twenty best
per game among those who qualified, and anyone else is still one click away in
the tables beside it.

**Career totals take no minimum, and the league applies none to its all-time
lists.** A career total is already its own answer to the question a minimum
asks. The eight All Time leaderboards rank totals, so the rule has nothing to
do there.

The two shooting-efficiency scatters keep a floor of their own — 50 field-goal
attempts for the season chart, 500 for the career one — rather than the
league's 100 made field goals. They draw a distribution rather than a ranking,
and the league's rule would leave 64 of this season's 228 players on the page.
If a percentage ever gets ranked here, the rule for it is 100 field goals made,
50 free throws made, 25 threes made.

## Known differences from ESPN

- **Playoffs are excluded**, so career totals here run below any source that
  includes postseason play.
- **Seasons before 2004 are absent**, so careers that began earlier run below
  any source that starts at 1997.
