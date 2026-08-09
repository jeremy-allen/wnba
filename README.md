# Meet the WNBA

A cross-filtering dashboard of WNBA player and franchise history, built with
[Quarto](https://quarto.org), [reactable](https://glin.github.io/reactable/),
[plotly](https://plotly-r.com) and
[crosstalk](https://rstudio.github.io/crosstalk/), from data supplied by the
[wehoop](https://wehoop.sportsdataverse.org/) R package.

**[View the dashboard →](https://connect.posit.cloud/jeremyallen/content/019fce5e-f2d4-6c4b-7bde-bcc8564b62e3)**

![The dashboard's current-season page: a column of team standings cards down the left, a sortable player table in the middle, a comparison table on the right showing the top ten scorers side by side, and a points leaderboard and champions list below.](images/dashboard.jpg)

It is one HTML file. 7.6 MB, no server, no Shiny, no R process anywhere behind
it. Everything below happens in the reader's browser after a single download:

- Click a player, in any table, and every other table and chart on the page
  answers — leaderboards narrow to her, charts dim everyone else, her team's
  standings card lights up, and the comparison table swaps its ten default
  columns for the players actually selected.
- Click a team card and the whole dashboard filters to that roster.
- Expand any row for a portrait, splits, and a game-by-game bar chart.
- Sort, filter, and page through 20 tables; hover 6 charts.

That is the point of this repository. **The interactivity is almost entirely
stock**, and the [second half of this README](#interactivity-without-a-server)
accounts for exactly how much the standard packages do on their own, where they
stop, and what the ~190 lines of custom JavaScript at the seams are for.

---

## The dashboard

Four pages:

| Page | What's on it |
|---|---|
| **2026** | Standings cards, a player table with expandable dossiers, a comparison table, scoring-race / efficiency / points charts, eight stat leaderboards, and the champions list |
| **All Time** | Career leaders with expandable career dossiers; a tabset of time on the court, The Chase (every career worth 1,500+ points), career efficiency, three-point and attendance trends; and eight career leaderboards |
| **Franchises** | Franchise lineage — every rename and relocation the league has had, as a timeline |
| **Methods** | The rules below, stated on the page itself |

### Where the data comes from

`wehoop::load_wnba_player_box()` and `load_wnba_schedule()` read pre-built files
from the **sportsdataverse-data** GitHub releases. That is a mirror of ESPN,
rebuilt on its own schedule, and it usually runs a day or two behind. The
release asset is re-uploaded whether or not new games were ingested, so a fresh
file timestamp is not evidence of fresh data.

`update_data.R` closes that gap: after the bulk pull it asks ESPN directly for
any completed game the mirror is missing and merges it in, then asserts full
season coverage and non-NA keys before writing `dat.rds` and `sched.rds`. A
broken fetch fails loudly instead of quietly caching a hollow file.

The navbar carries two dates, answering different questions — **Data through**
(the most recent game in the data) and **Rendered** (when the page was last
built). They drift apart whenever the page is rebuilt without refreshing the
data. Rendering fetches nothing.

> The `arrow` package must stay installed. Season files before 2024 store their
> columns as arrow ALTREP vectors; without it `readRDS` silently returns
> zero-length vectors that get padded to NA — a dataset with the right row count,
> the right date range, and no data in it.

### How games and stats are computed

**Regular season only** (`type_id == 1`). All-Star exhibitions, the
Commissioner's Cup, and every playoff round are excluded, career totals
included. All-Star rosters are dropped by game type rather than by team id — the
old `team_id < 90` test worked until ESPN began issuing six-digit ids to new
franchises, at which point it silently deleted Golden State, Toronto and
Portland.

**A row in the box score is not a game played.** ESPN carries one row per
rostered player per game, whether or not she took the floor. A row counts only
when `did_not_play` is not `TRUE` *and* the row has a stat line (`points` is not
`NA`). The second condition exists because `did_not_play` only became reliable
in 2013; before that, 10–22% of rows per season are inactive players it never
flagged.

This is the rule that matters most and the easiest to get wrong. Counting roster
appearances rather than games played pushes every per-game average below ESPN's
— Caitlin Clark, who had missed three games, read 19.3 points per game instead
of 21.5, enough to drop her out of the comparison table altogether.

**Totals** are the sum of `points` across counted games; **per game** divides by
games played on that same definition, so games missed never enter the
denominator.

📄 **[METHODOLOGY.md](METHODOLOGY.md)** — the full rules, including the known
differences from ESPN's own leaderboards (no minimum-games qualifier; a
50-attempt floor on the efficiency scatter; playoffs excluded).

---

## Interactivity without a server

Every interaction on this page is client-side. There is no Shiny app, no
`runtime: shiny`, no websocket, and nothing to deploy but a file. The published
version sits on Posit Connect Cloud as a static asset; opening the local
`dashboard.html` in a browser gives you the identical thing.

### The whole mechanism, in one line

```r
SharedData$new(df, key = ~athlete, group = "experience")
```

`crosstalk::SharedData` wraps a data frame and declares two things: a **key**
column, and a **group** name. Any widget built from a `SharedData` sharing that
group joins one broadcast channel, keyed on athlete name. Selecting a row in one
widget publishes a set of keys; every other widget in the group receives it and
answers in its own idiom — reactable filters its rows, plotly dims its traces.

Nothing else is wired up. There is no event bus, no observer, no callback
registry, and no code anywhere in this repository that reads a click in one
widget and updates another.

**24 of this dashboard's 26 widgets are joined that way**: 18 of the 20
reactables and all 6 plotly charts. The two exceptions are the ones the custom
JavaScript exists to handle, described further down.

### What the standard packages do on their own

| Behavior | What provides it | Written here |
|---|---|---|
| Row selection broadcasts to the whole page | `reactable(selection = "multiple", onClick = "select")` on a `SharedData` | 2 arguments |
| Every other table narrows to the selection | crosstalk ↔ reactable, built in | nothing |
| Charts highlight the selection and dim the rest | `ggplotly() \|> highlight(color =, opacityDim =, selected = attrs_selected())` | 1 pipe per chart |
| Selection **and** filter applied together | reactable intersects the two crosstalk channels per table | nothing |
| Sort, per-column filter, pagination, striping, row highlight | `reactable()` arguments | 1 argument each |
| Expandable row → portrait, splits, game-by-game bars | `details = colDef(html = TRUE, details = function(i) ...)` | an htmltools builder |
| Bar-in-cell leaderboards | `colDef(cell = ...)` returning markup | a 15-line helper |
| Chart tooltips | `hovertemplate` | a 10-line helper |
| Tabsets, cards, scrolling, layout, navbar | Quarto `format: dashboard` | markdown headings |

The most valuable thing on that list is the fourth row, and it is the one people
usually assume needs Shiny. Selection and filtering are **separate crosstalk
channels**, and reactable intersects them per widget — so a team filter can
narrow every table on the page while a row selection still highlights *within*
that narrowed set, with no coordination code of any kind.

Three charts are deliberately *not* interactive — the franchise lineage,
three-point and attendance trends are device-drawn SVG. Nothing on them answers
a selection (no athlete is a franchise), and `ggplotly()` charges for the trip:
it drops `plot.subtitle` silently and ignores a mapped `hjust`, which centred
every franchise label and ran the long ones off the panel.

### Where the standard packages stop

![The same dashboard with two players selected: their two team cards are outlined and lit, the comparison table has narrowed from ten columns to two, the points leaderboard below shows only those two rows, and a Clear button has appeared in the player table's header.](images/dashboard-selection.jpg)

Four things on this page are outside what crosstalk can express, and they are
the entire reason there is any JavaScript at all. Each one sits at a genuine
edge of the model, not at a place where JavaScript was simply easier.

**1. The team cards are a filter source, but not a widget.** (`standings_js`,
111 lines.) The standings column is hand-built `htmltools` divs, so it cannot
*be* a crosstalk endpoint — crosstalk speaks to htmlwidgets. The script
constructs a `crosstalk.FilterHandle` by hand and sets it from an
R-generated team→roster map, so clicking a card filters the group to that
roster. The stock alternative is `crosstalk::filter_select()`, a dropdown, which
would have meant giving up the standings display entirely. The rest of this
script is affordance: the lit state, the bounce when a second player from an
already-lit team is added, and scrolling a lit card into view.

**2. The comparison table is pivoted, so players are columns.** (`compare_js`,
77 lines.) Crosstalk filters *rows*; it has no vocabulary for a selection that
should change which **columns** are visible. The script listens on a
`SelectionHandle` and calls `Reactable.setHiddenColumns()`. It falls through
three rungs, narrowest first: the players selected, else the team being filtered
to, else the top ten this table opens on.

**3. Clearing a filter that belongs to another script.** A crosstalk handle can
only clear its own contribution to the group, and the team filter belongs to the
standings script. So the Clear button — which has to live outside the tables,
since a selection can leave the table you're looking at with no row to untick —
asks for the drop over a custom DOM event, `wnba:clear-filter`.

**4. A table keyed on the wrong thing.** (4 lines.) The champions table is keyed
by season, not by athlete, so it cannot join the group at all. It takes the team
by hand through `Reactable.setMeta()`, and a `rowClass = JS(...)` callback reads
that meta on every render — so the highlight survives a sort or a re-render the
way a stamped class would not.

That is **~190 lines of JavaScript in a 2,350-line document, about 8%.** All of
it is glue. It never touches data, never builds a DOM table, never rolls its own
event system. Every line either instantiates a documented crosstalk handle,
calls a documented `Reactable.*` method, or toggles a CSS class — the visual
states themselves (`.is-selected`, `.is-filtering`, `.is-bounce` with its
`@keyframes` and a `prefers-reduced-motion` guard, `.champ-lit`) all live in
`styles.scss`.

Two costs worth knowing before copying the pattern:

- **The scripts are coupled to rendered element ids** — `main-table`,
  `all-time-table`, `compare-table`, `champions`, `#standings`. Rename an
  `elementId` and the JavaScript fails silently.
- **One line is a workaround.** Deselecting the last row broadcasts `[]`, which
  plotly ignores entirely: `TraceManager.prototype.updateSelection` restores
  `origOpacity` only on `keys === null`, and takes the highlight path only on
  `keys.length >= 1`. An empty array falls between the two branches, so every
  chart keeps the dim from the selection just cleared with nothing lit against
  it. Re-broadcasting a strict `null` is what restores them.

### What this approach cannot do

Everything the page can show has to be **in** the page. There is no query on
demand: the 933 career panels and 226 season panels behind the expandable rows
are all pre-rendered, whether or not a reader ever opens one. That is the
trade — a file you can email against a page that computes. Anything requiring a
model fit at request time, a database, user accounts, or writes still needs
Shiny.

### Keeping the file down

A dashboard that ships everything up front has to be measured. This one went
from **14.7 MB to 7.6 MB (−48%; 2.65 MB → 1.43 MB gzipped)** on the same data,
in five moves:

| Saved | Measure | Where |
|---:|---|---|
| 2.58 MB | plotly's **basic** bundle instead of the full one | `partial_bundle(type = "basic")` |
| 2.48 MB | reactable given **markup**, not htmltools tags | `colDef(html = TRUE)` |
| 1.38 MB | duplicate Bootstrap deleted after render | `slim_dashboard.R` |
| 0.39 MB | one `hovertemplate` per trace instead of per-point tooltip text | `hovertemplate` + `customdata` |
| 0.35 MB | svglite figures instead of 2× PNG | `fig-format: svg`, `dev: svglite` |

Three of those generalise to any Quarto page carrying these widgets:

- **reactable serialises htmltools tags as a React tag tree** — every element
  written out as `{"name":...,"attribs":...,"children":[...]}`, roughly twice
  the length of the markup it stands for. Hand it a markup *string* with
  `html = TRUE` on the column and it stores the markup. 5.81 MB → 3.20 MB here,
  and it is invisible from the R side.
- **`partial_bundle()` must be applied to every chart.** It renames the html
  dependency from `plotly-main` to `plotly-basic`; one chart left on the full
  bundle and the page embeds both, ending up larger than it started. Name the
  bundle rather than using `type = "auto"`, which silently falls back to the
  full one.
- **Quarto 1.10 embeds every stylesheet three times.** This is a defect, not a
  configuration mistake — a dashboard whose entire body is the word "Hello."
  renders at 2.72 MB. `slim_dashboard.R` undoes it after the render by grouping
  the `<link>`s by `href` and keeping the first of each; it prints a message and
  exits cleanly once Quarto fixes it, at which point the script can go.

📄 **[FILESIZE.md](FILESIZE.md)** — every measurement, the gotchas behind each
one, and an accounting of the 7.6 MB that remains.

---

## Building it

[`just`](https://github.com/casey/just) drives the three steps:

```sh
just data      # refresh dat.rds / sched.rds from wehoop, topped up from ESPN
just render    # quarto render + slim_dashboard.R
just publish   # upload dashboard.html to Posit Connect Cloud, --no-render
just update    # all three, in order
```

The render matters to `update`: `quarto publish` runs with `--no-render` and
uploads `dashboard.html` exactly as it sits on disk, so going straight from new
data to publishing would push the previous build and report success.

R packages are pinned with [renv](https://rstudio.github.io/renv/) —
`renv::restore()`. Note that `library(svglite)` in the setup chunk is load-
bearing for the *snapshot*, not the render: implicit snapshots read `library()`
calls, not the YAML that names the device.

## Repository layout

| | |
|---|---|
| `dashboard.qmd` | the whole dashboard — data prep, helpers, widgets, JavaScript |
| `styles.scss` | theme tokens and the CSS behind every interactive state |
| `update_data.R` | wehoop pull, ESPN top-up, assertions, cache write |
| `slim_dashboard.R` | post-render removal of Quarto's duplicate stylesheets |
| `justfile` | data → render → publish |
| `METHODOLOGY.md` | what counts as a game, and the differences from ESPN |
| `FILESIZE.md` | how the rendered page was halved |
