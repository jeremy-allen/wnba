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

The **2026** and **All Time** player and team figures count regular-season games
only. The **2026 Playoffs** page counts playoff games only. The champions list
reads the playoff schedule to identify each season's champion.

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

Playoff performances appear only on the 2026 Playoffs page. Career totals
exclude playoff games.

## How the 2026 playoff page works

The 2026 playoff page keeps its year fixed at 2026 when the current-season page
advances. It checks that the 2026 regular-season box scores contain 330 games
and that each of the 15 teams played 44 games. It checks the eight playoff
seeds against the final standings and the [WNBA's published 2026
bracket](https://www.wnba.com/playoffs/2026). The first round is best of three,
the semifinals are best of five, and the finals are best of seven.

The page groups completed games by playoff round and team pair. For a completed
game, the box score supplies the home and away teams and their points, even if
the schedule still lists undecided teams. The schedule supplies the game's date
and broadcast outlet. The page counts a series win only when both teams have
box-score points and one team scored more.
The playoff round and game number come from the schedule headline. The 2026
semifinal schedule slots have `type_id == 1`, so that field alone would
misclassify the round. Future schedule slots with undecided teams remain
unassigned to a series. The page removes an unplayed conditional slot from the
game schedule when its decided series can be identified, or when every series
in that round is decided.

Each game's **Watch** listing starts with the schedule cache's
`broadcast_name`. The page adds channels or streaming services that the
[WNBA's 2026 postseason FAQ](https://www.wnba.com/news/2026-wnba-postseason-faq)
confirms for the same slot. A missing outlet appears as `Broadcast not listed`.
Dates and times use Eastern time, and a slot without a confirmed tipoff says
`Time TBD`. The outlet and time can change after a dashboard render, so the
navbar's **Rendered** date identifies the age of the listing.

The playoff player tables and charts count playoff games played under the
box-score rule below. They apply no regular-season qualification minimum. The
playoff comparison table shows games played beside per-game rates. The playoff
efficiency chart requires 20 playoff field-goal attempts, a display floor for
this chart rather than an official statistical minimum. Before a playoff box
score exists, each playoff statistics area says that no playoff games have been
completed in the current data.

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
denominator, which pulls her average down by exactly the fraction of her
appearances she sat out. A player who dressed for every one of her team's games
and played nine in ten of them would read 10% below what she has actually
averaged, 19.8 points against 22.0.

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

The regular-season and career shooting-efficiency scatters keep floors of their
own, 50 field-goal attempts for the season chart and 500 for the career one, rather than the
league's 100 made field goals. Each scatter draws a distribution rather than a
ranking, and the league's minimum would leave 65 of this season's 228 players on
the page. Should a percentage ever get ranked here, the minimums for it are 100
field goals made, 50 free throws made and 25 threes made.

## How the standings break a tie

The standings cards follow the league's published
[tiebreak procedure](https://www.wnba.com/standings), which ranks teams level on
winning percentage by four criteria in order:

1. the better record in head-to-head games
2. the better winning percentage against every team finishing .500 or better
3. the better point differential head-to-head, total points scored less total
   points allowed
4. the better point differential against all opponents, on the same net

As soon as a criterion separates anyone, the procedure begins again at the first
criterion on each group it leaves behind, so a smaller tie is settled by
head-to-head games recomputed among the teams that remain.

Two criteria read differently while the season runs. Criterion 2 is defined on
records "at the end of the season", so it has no answer before the season has an
end, and it joins the procedure on the day the schedule runs out. Criterion 4 is
written as a net total, which is the right reading of a finished season where
every team has played the same 44 games. Before then teams have played different
numbers of games and a net total would reward whoever has played more, so it is
read per game until the season ends. The two readings order a finished season
identically.

Criterion 1 stands aside when a tied team has yet to meet the others, which can
happen only in mid-season. Ranking the teams that have met would eliminate
nobody from the tie, because the unplaced team could belong anywhere among them.
Criterion 3 has no such gap, since total points scored less total points allowed
comes to zero over no games.

Teams that all four criteria leave level are ordered alphabetically, which at
least returns the same answer on every render.

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
design. Career totals exclude playoff performances, so every career total runs
below a source that counts postseason play. Seasons before 2004 are absent, so a career that
began earlier runs below a source that starts at 1997.
