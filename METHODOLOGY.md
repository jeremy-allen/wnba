## Where the numbers come from

`wehoop::load_wnba_player_box()` and `load_wnba_schedule()` read pre-built files
from the **sportsdataverse-data** GitHub releases, a mirror of ESPN that rebuilds
on its own schedule and usually runs a day or two behind. The mirror re-uploads
its release assets whether or not it ingested new games, so a fresh file
timestamp is no evidence of fresh data.

`update_data.R` closes that gap. After the bulk pull it asks ESPN directly for
every completed game the mirror is missing and merges those games in. It then
asserts full season coverage and non-NA keys before it writes the cache, so a
broken fetch fails loudly instead of caching a hollow file.

The navbar carries two dates, and each answers a different question:

- **Data through:** the date of the most recent game in the data
- **Rendered:** the date the page was last built

The two drift apart whenever someone rebuilds the page without refreshing the
data, because rendering fetches nothing.

> The `arrow` package must stay installed. Season files before 2024 store their
> columns as arrow ALTREP vectors, and without `arrow` loaded, `readRDS` returns
> zero-length vectors that get padded to NA. The result carries the right row
> count and the right date range and holds no data at all.

## Which games count

Every player and team figure on this dashboard counts **regular-season games
only**. The champions list is the single exception, because it reads the playoff
schedule to find who won each season's final.

The data load sorts each game once, into a `game_kind` that every later filter
reads:

| `game_kind` | what it is | how the load recognises it |
|---|---|---|
| `regular` | regular season | `season_type == 2` and `type_id == 1` |
| `post` | playoffs, every round | `season_type == 3` |
| `allstar` | All-Star exhibition | `type_id == 4` |
| `cup` | Commissioner's Cup final | `type_id == 39` |

Two columns settle the question together, because neither settles it alone.
`type_id` names a **round**, not a part of the season, and the playoff rounds
RD16 (14), QTR (15), SEMI (16) and FINAL (17) only exist from 2019 on. Every
playoff game from 2004 through 2018 carries `type_id == 1` under
`season_type == 3`, which is what a regular-season game looks like from
`type_id` alone. `season_type` separates the playoffs from the regular season in
every year, so `game_kind` reads it first, reaching for `type_id` only to tell
the exhibitions and the Cup final apart from the regular-season games that sit
beside them under `season_type == 2`.

Reading `season_type` also makes it impossible to lose a round. A list of
playoff `type_id`s has to name each round it excludes, and the quarter-finals
ran in 2020 and 2021 alone, so a list that omits 15 drops those four games out
of the playoffs without putting them anywhere else.

The load drops All-Star rosters by game type, since an All-Star roster is not a
real team. A threshold on `team_id` cannot do this job, because ESPN issues
six-digit ids to new franchises as well as to exhibition rosters, so the
threshold would delete Golden State, Toronto and Portland along with the
exhibitions.

**Playoff performances therefore appear in no player or team figure**, career
totals included.

## What counts as a game played

A box-score row is not the same thing as a game played. ESPN carries one row per
rostered player per game, whether or not that player took the floor. A row
counts only when **both** conditions hold:

1. `did_not_play` is not `TRUE`
2. the row carries a stat line, meaning `points` is not `NA`

The second condition exists because `did_not_play` only became reliable in 2013.
Before then, inactive players ESPN never flagged make up 10–22% of the rows in
each season, each appearing as a row with no minutes and every stat column `NA`.
The cache holds 7,654 of them. The test costs nothing elsewhere: of the 102,076
rows that do carry minutes, not one is blank across all thirteen stat columns.

This rule governs every per-game average on the dashboard. Counting roster
appearances instead of games played puts each game a player missed into her
denominator, which pulls her average down by the fraction of appearances she sat
out. Caitlin Clark has played 32 of her team's 36 games this season and averages
22.1 points in them, where counting the four she missed would read 20.2.

## One career, one name

Every aggregation groups players by display name, so a player who has changed
her name would otherwise hold two careers rather than one. The load prevents
that: every box-score row an `athlete_id` owns takes the most recent name that
id used. Six players have changed names mid-career, and each comes out as a
single row, Skylar Diggins carrying all 5,758 of her points instead of 4,255
under one name and 1,541 under another.

Canonicalising the **name** does this job, where regrouping on `athlete_id`
would not. ESPN has issued two ids to one player three times over, for Li Yueru
(4422426 and 4336633), Wendy Palmer (317 and 599) and Isabel Sanchez (548 and
596). Grouping by id splits those three careers in half, whereas taking the most
recent name each id used merges the six name changes and leaves the three
duplicate ids merged as well.

## Points and per-game averages

- **Totals:** the sum of `points` across counted games.
- **Per game:** total points divided by games played, on the same definition of
  "played" as above. Games missed never enter the denominator.
- Sparklines under *Per Season* and *Per Game* are those same numbers, one bar
  per season or per game, on a scale shared across every player in the table.

## Who qualifies for a ranking

The league publishes its minimums at
[stats.wnba.com/help/statminimums](https://stats.wnba.com/help/statminimums/),
under the heading *Statistical Minimums to Qualify For WNBA League Leaders*.
Every threshold there is written per season, "70% of team games played (31 in a
44-game season)" and "525 Points" and "100 field goals made", and every one of
them gates a **season** leaders list.

The comparison table ranks this season by points per game, so it applies the
league's scoring minimum: **70% of the games her team has played, or 525
points.** A player traded mid-season is measured against the team she is on now,
which is the nearest single reading of a rule written for a whole season. The
minimum removes nobody from the table. It chooses which columns the table opens
on, the twenty best per game among the players who qualified, and anyone else
stays one click away in the tables beside it.

**Career totals take no minimum, and the league applies none to its all-time
lists.** A minimum exists to stop a short run of games from ranking above a
season's work, and a career total already carries the volume that a minimum
tests for. All eight All Time leaderboards rank totals, so the rule has no work
to do on that page.

The two shooting-efficiency scatters keep a floor of their own, 50 field-goal
attempts for the season chart and 500 for the career one, rather than the
league's 100 made field goals. Each scatter draws a distribution rather than a
ranking, and the league's minimum would leave 64 of this season's 228 players on
the page. Should a percentage ever get ranked here, the minimums for it are 100
field goals made, 50 free throws made and 25 threes made.

## How far back the data goes

**The box scores start in 2004, and the league started in 1997.** A career that
opened before 2004 is counted here only from 2004 on, and no source available to
this dashboard can extend it. `wehoop` reaches back to 2002, but the 2002 file
holds 322 box-score rows where a full season runs to roughly 4,500, and
1997–2001 does not exist at all. Loading a season that thin would read as
coverage without being coverage, so the floor stays at 2004 and this page states
it.

Sue Bird and Tamika Catchings are each short exactly 74 games, their 2002 and
2003 seasons. Lisa Leslie, Sheryl Swoopes, Tina Thompson, Katie Smith and Lauren
Jackson lose more.

## How these totals compare with the league's

Measured against
[stats.wnba.com/alltime-leaders](https://stats.wnba.com/alltime-leaders/) at
regular season and totals, eighteen of the top twenty careers agree to within 30
points, a few tenths of one percent, and Brittney Griner, Angel McCoughtry and
Skylar Diggins agree exactly, games included. The two that do not are Bird and
Catchings, for the reason above.

The remaining fraction is the mirror itself. Of the games played between 2004
and 2018, 47 have a schedule row and no box score at all, 23 of them in 2004, so
the totals here run slightly light: Diana Taurasi at 10,525 against the league's
10,646, Cappie Pondexter at 6,741 against 6,811. That gap belongs to the source
rather than to any rule on this page, and nothing here works around it.

Two rules of this dashboard put its career totals below other sources by
design. Playoff performances are excluded, so every total runs below a source
that counts postseason play. Seasons before 2004 are absent, so a career that
began earlier runs below a source that starts at 1997.
