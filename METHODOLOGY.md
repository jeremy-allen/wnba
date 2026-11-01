## Where the numbers come from

Every statistic, score, schedule, team crest and player photo on this dashboard
comes from [ESPN](https://www.espn.com/wnba/), through the
[wehoop](https://wehoop.sportsdataverse.org/) R package and the
[sportsdataverse](https://www.sportsdataverse.org/) project. This is an
independent, non-commercial project, not affiliated with or endorsed by ESPN or
the WNBA.

`wehoop::load_wnba_player_box()` and `load_wnba_schedule()` read pre-built files
from the **sportsdataverse-data** GitHub releases, a mirror of ESPN that usually
runs a day or two behind. The mirror re-uploads its files whether or not it added
new games, so a fresh file timestamp does not prove the data is fresh.

`update_data.R` closes that gap. After the bulk pull, it requests from ESPN every
completed game the mirror lacks and merges those games in. It then checks full
season coverage and non-NA keys before it writes the cache, so a broken fetch
stops the script instead of caching an incomplete file.

The navbar shows two dates:

- **Data through:** the date of the most recent game in the data
- **Rendered:** the date the page was last built

Rendering fetches nothing, so the two dates differ whenever someone rebuilds the
page without refreshing the data.

> The `arrow` package must stay installed. Season files before 2024 store their
> columns as arrow ALTREP vectors. Without `arrow` loaded, `readRDS` returns
> zero-length vectors that get padded to NA, and the result has the right row
> count and date range but no data.

## Which games count

The **2026** and **All Time** player and team figures count regular-season games
only, and career totals exclude playoff games. The **2026 Playoffs** page counts
playoff games only. The champions list reads the playoff schedule to identify
each season's champion.

The data load sorts each game once into a `game_kind`, which every later filter
reads:

| `game_kind` | what it is | how the load recognises it |
|---|---|---|
| `regular` | regular season | `season_type == 2` and `type_id == 1` |
| `post` | playoffs, every round | `season_type == 3` |
| `allstar` | All-Star exhibition | `type_id == 4` |
| `cup` | Commissioner's Cup final | `type_id == 39` |

Neither column settles a game's kind alone. `type_id` names a **round**, not a
part of the season, and the playoff rounds RD16 (14), QTR (15), SEMI (16) and
FINAL (17) exist only from 2019 on. Every playoff game from 2004 through 2018 has
`type_id == 1` under `season_type == 3`, so `type_id` alone makes it look like a
regular-season game. `game_kind` therefore reads `season_type` first, and it uses
`type_id` only to separate the exhibitions and the Cup final from the
regular-season games under `season_type == 2`.

Reading `season_type` also means no round can be lost. A list of playoff
`type_id`s must name every round, and the quarter-finals ran only in 2020 and
2021, so a list that omits 15 drops those four games from the playoffs.

The load drops All-Star rosters by game type, because an All-Star roster is not a
real team. A threshold on `team_id` would fail, because ESPN gives six-digit ids
to new franchises as well as to exhibition rosters. That threshold would delete
Golden State, Toronto and Portland too.

## How the 2026 playoff page works

The 2026 playoff page stays fixed on 2026 when the current-season page advances.
It checks that the 2026 regular-season box scores contain 330 games and that each
of the 15 teams played 44 games. It also checks the eight playoff seeds against
the final standings and the [WNBA's published 2026
bracket](https://www.wnba.com/playoffs/2026). The first round is best of three,
the semifinals best of five, and the finals best of seven.

The page groups completed games by playoff round and team pair. The box score
supplies a completed game's home and away teams and their points, even if the
schedule still lists undecided teams. The schedule supplies the date and broadcast
outlet. A series win counts only when both teams have box-score points and one
team scored more.

The playoff round and game number come from the schedule headline, because the
2026 semifinal schedule rows have `type_id == 1`. Future games with undecided
teams stay unassigned to a series. The page removes an unplayed conditional game
from the schedule when its series is decided or every series in that round is
decided.

Each game's **Watch** listing starts with the schedule cache's `broadcast_name`.
The page adds channels or streaming services that the
[WNBA's 2026 postseason FAQ](https://www.wnba.com/news/2026-wnba-postseason-faq)
confirms for the same game. A missing outlet appears as `Broadcast not listed`.
Times are Eastern, and a game without a confirmed tipoff says `Time TBD`. Outlets
and times can change after a render, so the navbar's **Rendered** date shows the
age of the listing.

The playoff player tables and charts count playoff games under the box-score rule
below and apply no regular-season minimum. The playoff comparison table shows
games played beside per-game rates. The playoff efficiency chart requires 20
playoff field-goal attempts, a display floor for this chart and not an official
minimum. Before a playoff box score exists, each playoff statistics area says that
no playoff games have been completed.

## What counts as a game played

ESPN has one box-score row per rostered player per game, whether or not the player
took the floor. A row counts as a game played only when **both** conditions hold:

1. `did_not_play` is not `TRUE`
2. `points` is not `NA`

The second condition exists because `did_not_play` became reliable only in 2013.
Before then, inactive players ESPN never flagged make up 10–22% of each season's
rows, each with no minutes and every stat column `NA`. The cache holds 7,654 of
these rows. The test removes nothing else: of the 102,076 rows with minutes, none
is blank across all thirteen stat columns.

This rule governs every per-game average on the dashboard. Counting roster rows
would put each missed game in the player's denominator. A player who averages 22.0
points and plays nine of every ten team games would read 19.8, which is 10% low.

## One career, one name

Every aggregation groups players by display name, so a player who changed names
would otherwise hold two careers. The load prevents that: every box-score row an
`athlete_id` owns takes the most recent name that id used. Six players changed
names mid-career, and each holds one career here, including Skylar Diggins.

The load changes the **name** because regrouping on `athlete_id` would fail. ESPN
issued two ids to each of three players: Li Yueru (4422426 and 4336633), Wendy
Palmer (317 and 599) and Isabel Sanchez (548 and 596). Grouping by id would split
those three careers, whereas grouping by name keeps them whole.

Four players received a new id along with a new name, so nothing in the box scores
connects their two careers: Katie Feenstra and Katie Mattera, Erin Buescher and
Erin Perperoglou, Shanna Zolman and Shanna Crossley, and Tamika Williams-Jeter and
Tamika Raymond. A short hand-kept list merges those four under the name the league
uses.

## Points and per-game averages

- **Totals:** the sum of `points` across counted games.
- **Per game:** total points divided by games played, as defined above.
- Sparklines under *Per Season* and *Per Game* show those same numbers, one bar per
  season or game, on a scale shared across every player in the table.

## Who qualifies for a ranking

The league publishes its minimums at
[stats.wnba.com/help/statminimums](https://stats.wnba.com/help/statminimums/),
under the heading *Statistical Minimums to Qualify For WNBA League Leaders*. Every
threshold there, such as "70% of team games played (31 in a 44-game season)", "525
Points" and "100 field goals made", applies to a **season** leaders list.

The comparison table ranks this season by points per game, so it applies the
league's scoring minimum: **70% of the player's team games, or 525 points.** A
player traded mid-season is measured against her current team, the closest reading
of a rule written for a whole season. The minimum removes nobody from the table.
It only chooses the columns the table opens on, the twenty best per game among
qualified players, and every other player can be selected from the tables beside
it.

**Career totals have no minimum, because the league applies none to its all-time
lists.** A minimum stops a short run of games from outranking a full season, and
a career total already reflects volume. All eight All Time leaderboards rank
totals, so the rule has no effect on that page.

The regular-season and career shooting-efficiency scatters use their own floors of
50 and 500 field-goal attempts, not the league's 100 made field goals. Each
scatter shows a distribution, not a ranking, and the league's minimum would leave
65 of this season's 228 players on the chart. If this dashboard ever ranks a
percentage, the minimums are 100 field goals made, 50 free throws made and 25
threes made.

## How the standings break a tie

The standings cards follow the league's published
[tiebreak procedure](https://www.wnba.com/standings), which ranks teams level on
winning percentage by four criteria in order:

1. the better record in head-to-head games
2. the better winning percentage against every team finishing .500 or better
3. the better head-to-head point differential, total points scored less total
   points allowed
4. the better point differential against all opponents

When a criterion separates any teams, the procedure restarts at criterion 1 for
each remaining tied group, so head-to-head records are recomputed among those
teams.

Two criteria work differently during the season. Criterion 2 is defined on records
"at the end of the season", so it applies only once the schedule is complete.
Criterion 4 is written as a net total, which suits a finished season of 44 games
for every team. Before then a net total would reward teams that have played more
games, so the dashboard uses point differential per game until the season ends.
The two readings order a finished season identically.

Criterion 1 does not apply when a tied team has yet to meet the others, which can
happen only mid-season. The unplaced team could belong anywhere among the teams
that have met, so ranking those teams would break no tie. Criterion 3 has no such
gap, because point differential is zero over no games.

Teams level on all four criteria are ordered alphabetically, so every render
returns the same order.

## How far back the data goes

**The box scores start in 2004, and the league started in 1997.** A career that
began before 2004 is counted only from 2004 on. `wehoop` reaches back to 2002, but
the 2002 file holds 322 box-score rows where a full season has about 4,500, and
1997–2001 does not exist. A season that incomplete would look like coverage
without being coverage, so the floor stays at 2004.

Sue Bird and Tamika Catchings are each short exactly 74 games, their 2002 and 2003
seasons. Lisa Leslie, Sheryl Swoopes, Tina Thompson, Katie Smith and Lauren
Jackson are short more.

## How these totals compare with the league's

On November 1, 2026, this table compared regular-season totals with a copy of
[stats.wnba.com/alltime-leaders](https://stats.wnba.com/alltime-leaders/), sorting
the 758 careers it lists by cause. The league's table requires 31 games, and that
copy stopped at August 16, 2026, so the comparison stops there too.

| Cause | Careers | Points short |
|---|---|---|
| Exact on games, points, rebounds and assists | 280 | 0 |
| Some seasons before 2004 | 139 | 134,189 |
| Entire career before 2004 | 138 | 65,080 |
| Missing or partial box scores | 137 | 2,232 |
| Same stats, game count differs | 32 | 0 |
| Not explained | 21 | 640 |
| Same games, small stat differences | 8 | -2 |
| More games here than the league | 3 | -37 |

Seasons before 2004 account for 98.6% of the points short. The causes are inferred
from the size of each gap, so a career that began in 2003 can land under "Not
explained" if it lost few games. `reconcile_careers.R` reproduces the table.

Of the league's top twenty careers, six lost seasons before 2004. Of the other
fourteen, twelve are within 30 points and four are exact: Nneka Ogwumike, A'ja
Wilson, Brittney Griner and Kayla McBride. The widest gaps are Diana Taurasi at
10,525 against the league's 10,646 and Cappie Pondexter at 6,741 against 6,811.

Those gaps come from the source. From 2004 to 2018, 45 regular-season games have a
schedule row and no box score, 21 of them in 2004. ESPN's game pages list the
players for most of those games with blank stats, and only two have a complete set
of points. Another 153 games from 2004 to 2012 have a box score whose players add
up to fewer points than the final score, 953 points in all.

About 200 rows have no minutes recorded, and the league counts some as games
played but not others. Dropping them all corrects careers with a game too many but
breaks careers that are exact, so the rows stay in.

Two rules put these career totals below other sources by design. Career totals
exclude playoff games, so they run below any source that counts them. Seasons
before 2004 are absent, so a career that began earlier runs below any source that
starts at 1997.
