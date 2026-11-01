# Meet the WNBA

A cross-filtering dashboard of WNBA player and franchise history, built with
[Quarto](https://quarto.org), [reactable](https://glin.github.io/reactable/),
[plotly](https://plotly-r.com) and
[crosstalk](https://rstudio.github.io/crosstalk/), using data from the
[wehoop](https://wehoop.sportsdataverse.org/) R package.

**[View the dashboard →](https://connect.posit.cloud/jeremyallen/content/019fce5e-f2d4-6c4b-7bde-bcc8564b62e3)**

![The dashboard's current-season page: a column of team standings cards down the left, a sortable player table in the middle, a comparison table on the right showing the top ten scorers side by side, and a points leaderboard and champions list below.](images/dashboard.jpg)

The dashboard is one self-contained HTML file of about 8.6 MB. No server or
Shiny process runs behind it. After one download, the reader's browser does all
of this:

- Click a player in any table to update every other table and chart.
  Leaderboards narrow to that player, charts dim the other players, the player's
  team card gains a highlight, and the comparison table replaces its twenty
  default columns with the selected players.
- Click a team card to filter every table and chart to that roster.
- Expand any row for a portrait, splits, and a game-by-game bar chart.
- Sort, filter, and page through the tables, and hover over charts for tooltips.
- On the **2026 Playoffs** page, check the bracket and the **Watch** listing for
  each scheduled game, including games whose teams are undecided.

Standard packages produce almost all of this behavior. The
[second half of this README](#interactivity-without-a-server) shows what they do
alone, where they stop, and what the small amount of custom JavaScript does.

---

## The dashboard

Five pages:

| Page | What's on it |
|---|---|
| **2026** | Standings cards, a player table with expandable panels, a comparison table, scoring-race, efficiency and points charts, eight stat leaderboards, and the champions list |
| **2026 Playoffs** | A seeded bracket, a game schedule with a **Watch** listing for each game, playoff player totals and game logs, comparison views, charts, and eight playoff leaderboards. Before the first playoff game, the player views say that no playoff games have been completed. |
| **All Time** | Career leaders with expandable career panels; a tabset of time on the court, The Chase (every career with 1,500+ points), career efficiency, and three-point and attendance trends; and eight career leaderboards |
| **Franchises** | A timeline of every franchise rename and relocation |
| **Methods** | The rules below, stated on the page |

### Where the data comes from

`wehoop::load_wnba_player_box()` and `load_wnba_schedule()` read pre-built files
from the **sportsdataverse-data** GitHub releases. That repository mirrors ESPN
and usually runs a day or two behind. It re-uploads its files whether or not it
added new games, so a fresh file timestamp does not prove the data is fresh.

`update_data.R` closes that gap. After the bulk pull, it requests from ESPN every
completed game the mirror lacks and merges those games in. It then checks full
season coverage and non-NA keys before it writes `dat.rds` and `sched.rds`, so a
broken fetch stops the script instead of caching an incomplete file.

The navbar shows a **Data through** date and a **Rendered** date. They differ
whenever the page is rebuilt without a data refresh.

> The `arrow` package must stay installed. Without it, season files before 2024
> load with the right row count and date range but no data.

### How games and stats are computed

The **2026** and **All Time** pages count regular-season games only
(`game_kind == "regular"`). The **2026 Playoffs** page counts playoff games only
(`game_kind == "post"`). Player totals exclude All-Star exhibitions and the
Commissioner's Cup final, and career totals exclude playoff games.

The data load sorts each game once, because neither schedule column settles the
game's kind alone. `type_id` names a *round*, and playoff rounds exist only from
2019 on, so `type_id` makes every playoff game from 2004 through 2018 look like a
regular-season game. `season_type` separates the two in every year. The load
drops All-Star rosters by game type, not by team id, because ESPN gives six-digit
ids to new franchises as well as to exhibition rosters. A threshold on the id
would delete Golden State, Toronto and Portland too.

**One career, one name.** Every box-score row an `athlete_id` owns takes the most
recent name that id used, so a player who changed names, such as Skylar Diggins,
holds one career instead of two. Four players received a new id along with a new
name, and a short list in `checks.R` merges their careers.

**The box scores start in 2004**, and the league started in 1997, so a career
that began earlier is counted only from 2004 on.

**A box-score row is not a game played.** ESPN has one row per rostered player
per game, whether or not the player took the floor. A row counts only when
`did_not_play` is not `TRUE` *and* `points` is not `NA`. The second condition
exists because `did_not_play` became reliable only in 2013. Before then, inactive
players ESPN never flagged make up 10–22% of each season's rows. Counting those
rows would pull every per-game average below ESPN's.

**Totals** sum `points` across counted games, and **per game** divides that sum by
the same counted games, so missed games never enter the denominator.

**Who qualifies for a ranking.** The 2026 regular-season comparison table applies
the league's scoring minimum: 70% of the player's team games, or 525 points. The
league writes those
[minimums](https://stats.wnba.com/help/statminimums/) per season for season
leaders. Career totals have no minimum, because the league applies none to its
all-time lists. The playoff comparison table applies no minimum and shows playoff
games played beside each per-game rate.

The playoff page reads game dates and broadcast outlets from the schedule cache
and adds confirmed simulcasts from the [WNBA's 2026 postseason
FAQ](https://www.wnba.com/news/2026-wnba-postseason-faq).

📄 **[METHODOLOGY.md](METHODOLOGY.md)** states the full rules, what the 2004 floor
costs each pre-2004 career, and the cause of each gap between these totals and
the 758 careers the league lists.

---

## Interactivity without a server

Every interaction runs in the browser. Posit Connect Cloud serves the published
page as a static asset.

### The whole mechanism, in one line

```r
SharedData$new(df, key = ~athlete, group = "experience")
```

`crosstalk::SharedData` wraps a data frame and declares a **key** column and a
**group** name. Widgets built from a `SharedData` with the same group name share
one selection, keyed on athlete name. Selecting a row in one widget publishes a
set of keys. Every other widget in the group then updates: reactable hides rows
whose keys are absent, and plotly dims unselected traces.

No other code connects the widgets. This repository has no event bus, observer,
or callback registry, and no code that reads a click in one widget and updates
another.

The player tables and interactive charts on the 2026, 2026 Playoffs, and All Time
pages join that group. The comparison tables need custom JavaScript because their
players are columns, as described below.

### What the standard packages do on their own

| Behavior | What provides it | Written here |
|---|---|---|
| Row selection broadcasts to the whole page | `reactable(selection = "multiple", onClick = "select")` on a `SharedData` | 2 arguments |
| Every other table narrows to the selection | crosstalk and reactable, built in | nothing |
| Charts highlight the selection and dim the rest | `ggplotly() \|> highlight(color =, opacityDim =, selected = attrs_selected())` | 1 pipe per chart |
| Selection **and** filter applied together | reactable intersects the two crosstalk channels per table | nothing |
| Sort, per-column filter, pagination, striping, row highlight | `reactable()` arguments | 1 argument each |
| Expandable row showing a portrait, splits, and game-by-game bars | `details = colDef(html = TRUE, details = function(i) ...)` | an htmltools builder |
| Bar-in-cell leaderboards | `colDef(cell = ...)` returning markup | a 15-line helper |
| Chart tooltips | `hovertemplate` | a 10-line helper |
| Tabsets, cards, scrolling, layout, navbar | Quarto `format: dashboard` | markdown headings |

The fourth row is the most useful, because readers usually assume it needs Shiny.
Selection and filtering use **separate crosstalk channels**, and reactable
intersects them per widget. A team filter therefore narrows every table while a
row selection still highlights rows *within* that narrowed set, with no
coordination code.

Three charts are deliberately static SVG: the franchise lineage, the three-point
trend, and the attendance trend. No athlete belongs to any of them, so a
selection has nothing to change. `ggplotly()` would also damage them, because it
silently drops `plot.subtitle` and ignores a mapped `hjust`, which centred every
franchise label and ran the long labels off the panel.

### Where the standard packages stop

![The same dashboard with two players selected: their two team cards are outlined and highlighted, the comparison table has narrowed from ten columns to two, the points leaderboard below shows only those two rows, and a Clear button has appeared in the player table's header.](images/dashboard-selection.jpg)

Crosstalk cannot express four behaviors on this page, and those four account for
all of the JavaScript.

**1. The team cards are a filter source, but not a widget.** (`standings_js`, 111
lines.) The standings column is hand-built `htmltools` divs, and crosstalk
connects only to htmlwidgets. The script therefore constructs a
`crosstalk.FilterHandle` directly and sets it from an R-generated team-to-roster
map, so clicking a card filters the group to that roster. The only standard
alternative, `crosstalk::filter_select()`, would replace the standings with a
dropdown. The remaining lines handle the visual response: the selected card's
highlight, the bounce when a second player from a highlighted team is added, and
the scroll that brings a highlighted card into view.

**2. The comparison table has players as columns.** (`compare_js`, 77 lines.)
Crosstalk filters *rows* and cannot change which **columns** are visible. The
script listens on a `SelectionHandle` and calls `Reactable.setHiddenColumns()`
with the narrowest available set: the selected players, or failing that the
filtered roster, or failing that the default twenty.

**3. The Clear button clears a filter that another script owns.** A crosstalk
handle clears only its own contribution to the group, and the team filter belongs
to the standings script. The Clear button therefore requests the removal through
a custom DOM event, `wnba:clear-filter`. The button sits outside the tables
because a selection can leave the visible table with no row to untick.

**4. The champions table is keyed by season.** (4 lines.) It is not keyed by
athlete, so it cannot join the group. The standings script passes the team name
to it through `Reactable.setMeta()`, and a `rowClass = JS(...)` callback reads
that metadata on every render. The highlight therefore survives a sort, which a
class written onto the rows once would not.

These scripts compute no statistics, read no data, build no table markup, and
define no event system. Every line instantiates a documented crosstalk handle,
calls a documented `Reactable.*` method, or toggles a CSS class. `styles.scss`
defines the visual states: `.is-selected`, `.is-filtering`, `.is-bounce` with its
`@keyframes` and a `prefers-reduced-motion` guard, and `.champ-lit`.

Two costs:

- **The scripts depend on rendered element ids**: `main-table`, `all-time-table`,
  `compare-table`, `champions`, and `#standings`. Renaming an `elementId` breaks
  the JavaScript silently.
- **One line is a workaround.** Deselecting the last row broadcasts `[]`, which
  plotly ignores. `TraceManager.prototype.updateSelection` restores `origOpacity`
  only when `keys === null` and adds highlight traces only when
  `keys.length >= 1`, so an empty array matches neither condition and every chart
  stays dimmed. Broadcasting a strict `null` restores the charts.

### What this approach cannot do

Everything the page can show must be **in** the page. The 933 career panels and
226 season panels behind the expandable rows are all pre-rendered, whether or not
a reader opens one. The file can be copied or emailed, and it computes nothing
new after the render. A model fit at request time, a database, user accounts, or
any write still requires Shiny.

### Keeping the file size down

The page carries all of its data and markup, so its size needs watching. Before
the playoff page was added, five changes took a render from **14.7 MB to 7.6 MB
(−48%; 2.65 MB → 1.43 MB gzipped)** on identical data:

| Saved | Measure | Where |
|---:|---|---|
| 2.58 MB | plotly's **basic** bundle instead of the full one | `partial_bundle(type = "basic")` |
| 2.48 MB | reactable given **markup**, not htmltools tags | `colDef(html = TRUE)` |
| 1.38 MB | duplicate Bootstrap deleted after render | `slim_dashboard.R` |
| 0.39 MB | one `hovertemplate` per trace instead of per-point tooltip text | `hovertemplate` + `customdata` |
| 0.35 MB | svglite figures instead of 2× PNG | `fig-format: svg`, `dev: svglite` |

Three of those apply to any Quarto page using these widgets:

- **reactable serialises htmltools tags as a React tag tree**, writing each
  element as `{"name":...,"attribs":...,"children":[...]}`, roughly twice the
  length of the equivalent markup. Passing a markup *string* with `html = TRUE`
  on the column stores the markup instead. That took 5.81 MB to 3.20 MB here, and
  R shows no sign of the difference.
- **`partial_bundle()` must wrap every chart.** It renames the html dependency
  from `plotly-main` to `plotly-basic`, so one chart left on the full bundle makes
  the page embed both. Name the bundle, because `type = "auto"` silently falls
  back to the full one.
- **Quarto 1.10 embeds every stylesheet three times, and no setting prevents it.**
  A dashboard containing only the word "Hello." renders at 2.72 MB.
  `slim_dashboard.R` groups the `<link>`s by `href`, keeps the first of each, and
  reports how many it dropped. When that report reaches zero, Quarto has fixed the
  defect and the script can go.

📄 **[FILESIZE.md](FILESIZE.md)** records every measurement from that earlier
render. The current render is larger because it includes the playoff page.

---

## Building it

[`just`](https://github.com/casey/just) drives the build:

```sh
just test      # run the offline tests of checks.R
just data      # refresh dat.rds / sched.rds from wehoop, topped up from ESPN
just render    # quarto render + slim_dashboard.R
just publish   # upload dashboard.html to Posit Connect Cloud, --no-render
just update    # all four, in order
```

`update` needs its render step. `quarto publish` runs with `--no-render` and
uploads `dashboard.html` as it sits on disk, so publishing straight after new data
would upload the previous build and report success.

[renv](https://rstudio.github.io/renv/) pins the R packages, and `renv::restore()`
installs them. The `library(svglite)` call in the setup chunk exists for the
*snapshot*, because implicit snapshots read `library()` calls and not the YAML
that names the device.

## Repository layout

| | |
|---|---|
| `dashboard.qmd` | the whole dashboard: data prep, helpers, widgets, JavaScript |
| `styles.scss` | theme tokens and the CSS for every interactive state |
| `update_data.R` | wehoop pull, ESPN top-up, assertions, cache write |
| `checks.R` | champion rule, career names, and the checks that stop a bad update |
| `tests/test-checks.R` | offline tests of `checks.R`, run with `just test` |
| `reconcile_careers.R` | sorts each gap between these totals and the league's by cause |
| `slim_dashboard.R` | post-render removal of Quarto's duplicate stylesheets |
| `justfile` | test, data, render, publish |
| `METHODOLOGY.md` | what counts as a game, and how totals compare with the league's |
| `FILESIZE.md` | how the rendered page was halved |
