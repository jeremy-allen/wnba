# Where this dashboard is published, and what moving it would buy

Recorded 2026-08-12. **Nothing here is implemented.** The dashboard still
publishes to Posit Connect Cloud by hand through `just update`, and the
repository contains no workflow file. This is the case for changing that, the
measurements behind it, and the configuration to lift out if the answer is yes.

Two separate questions run through it, and they are worth keeping apart:

1. **Where the page is hosted** — a question about what the page can be.
2. **Who runs the daily update** — a question about whether a person is in the
   loop each day.

They are separable in principle. They turn out to be joined by one detail of how
`quarto publish` authenticates, recorded under [Publishing from a runner](#3-publishing-from-a-runner).

---

## The current workflow

`just update` runs three recipes in sequence:

```
just data     Rscript update_data.R          regenerate dat.rds and sched.rds
just render   quarto render dashboard.qmd    rebuild dashboard.html
              Rscript slim_dashboard.R       drop Quarto's duplicate stylesheets
just publish  quarto publish posit-connect-cloud --no-render
```

The `--no-render` is deliberate and the justfile explains it: rendering on
Connect Cloud would take `config.yml`'s `rsconnect` profile, which pulls from
wehoop rather than reading the validated caches. The intent is to **upload the
artifact exactly as it sits on disk**. Hold on to that phrase — it decides the
hosting question below.

The page is served at
`connect.posit.cloud/jeremyallen/content/019fce5e-f2d4-6c4b-7bde-bcc8564b62e3`.

What it costs today: one person running one command every day the season is on,
and a URL that cannot be spoken aloud.

---

## Part one: what Cloudflare would change

### The 3.80 MB that cannot come off a static page

`FILESIZE.md` records taking the page from 14.7 MB to 7.63 MB, and accounts for
what is left:

| | |
|---|---:|
| reactable payloads | 3.80 MB |
| React + ReactDOM + core-js + jQuery + Quarto/bslib JS | 1.81 MB |
| Bootstrap CSS (percent-encoded, after the duplicate goes) | 0.71 MB |
| plotly widget JSON | 0.46 MB |
| bootstrap-icons woff | 0.24 MB |

The largest line is 933 career panels and 226 player panels, pre-rendered
whether or not a reader ever expands the row. Half the page is detail that most
readers never open.

`FILESIZE.md` considers one escape and rejects it for good reasons: rendering
the panels client-side from hidden data columns with `JS()` would save about
1.3 MB, at the cost of moving roughly 120 lines of htmltools into JS strings
where the markup can come out unbalanced — which is what building it with
htmltools was for.

That was the only escape available, because a static page can choose only
between **shipping** the panels and **reconstructing** them in the browser.

A Worker adds a third: **fetching** them. A reader expands a row, the page
requests `/api/panel/1234`, and the panel arrives. This removes the panel markup
*and* the data the client-side alternative would have rebuilt it from, so it
takes the full 3.80 MB rather than 1.3 MB, and `dossier()` stays exactly as
written.

```
update_data.R  →  build 1,159 panels with dossier()  →  write to D1
                  (same htmltools, same output)          key → markup

Worker:  SELECT markup FROM panels WHERE key = ?      about 20 lines
```

R keeps doing the work it already does. Workers run JS, TS and WASM — never R —
so the panels are built in R at data-refresh time and stored as markup strings.
The Worker performs a key lookup.

The crosstalk design is untouched. All 24 widgets stay on the shared group;
only the `details` builders change from embedding markup to requesting it.

`README.md` currently says a database "still requires Shiny." That is right for
a **write**. For a **read of pre-built markup**, an endpoint is enough, and an
endpoint is a much smaller object than a Shiny app.

### What this costs

The self-contained file. `README.md` names the property deliberately: the page
"can be copied or emailed." Once panels live behind an endpoint, the page is a
hosted application and a saved copy is a broken one.

**This is the only genuine trade in the whole document, and it is not a
technical question.** It depends on whether anyone has ever actually saved or
mailed the file.

### What does not change

R still fetches, validates and renders — on this machine or on a runner.
`update_data.R` and `quarto render` keep their current jobs. Cloudflare replaces
the destination of the upload, not the pipeline that produces it.

### The smaller gain

A domain that can be read out loud, in place of a UUID path under someone
else's hostname.

---

## Part two: automating the daily update

### 1. Automation is not a Cloudflare feature

Running `just data && just render` on a schedule is a GitHub Actions job. It
could be set up today against the current Connect Cloud target. Nothing about
scheduling requires changing hosts, and any argument that says otherwise is
wrong.

### 2. Cloudflare does fit the publish model better

The justfile's intent is to upload the artifact exactly as rendered. That is
what `wrangler deploy` does, and it has no other mode. Connect Cloud reaches the
same behaviour through `--no-render`, an escape hatch from a workflow whose
default is to rebuild on their side from `config.yml`'s `rsconnect` profile.

This is a preference, not a blocker.

### 3. Publishing from a runner

This one is a blocker, and it is what joins the two questions.

Checked on 2026-08-12 against Quarto 1.10.18:

- `quarto publish --help` states plainly: "Accounts are configured interactively
  during publishing."
- Connect Cloud credentials are stored at
  `~/Library/Application Support/quarto/publish/accounts/posit-connect-cloud/accounts.json`,
  written by that interactive flow.
- A generic `--token` flag exists. **Whether it authenticates the
  `posit-connect-cloud` provider in a headless run is untested.** The documented
  CI credentials are for Quarto Pub, Netlify and enterprise Posit Connect
  (`CONNECT_SERVER` + `CONNECT_API_KEY`) — not Connect Cloud.

`wrangler deploy` authenticates with an API token by design.

**So the practical order is: wanting the daily run automated is what pushes the
publish to Cloudflare — not the file-size argument in part one.** If `--token`
does turn out to work against Connect Cloud, this reason disappears and the two
questions separate again. Worth ten minutes of testing before accepting the
conclusion.

---

## Findings from checking the project

Four things surfaced while working out what a runner would need. The first two
are worth fixing whether or not any of this is adopted.

### Four orphan packages in `renv.lock`

`sf`, `V8`, `magick` and `gifski` are recorded in the lockfile. No other locked
package lists them under `Requirements`, and neither `dashboard.qmd` nor any `.R`
script calls them:

```sh
for p in sf V8 magick gifski; do grep -lE "library\($p\)|$p::" dashboard.qmd *.R; done
# no output
```

They are also the four heaviest system dependencies in the set — libgdal,
libnode and libmagick++ — so they set the floor on what any fresh
`renv::restore()` costs, here or on a runner.

```r
renv::snapshot()   # confirm the removals are exactly those four
```

### Figures will not match a local render exactly

Nothing sets a font family on the R side: no `base_family`, no `theme_set`, no
`register_font`. The three svglite figures therefore draw in the graphics
device's default, which resolves to Helvetica on macOS and DejaVu Sans on
Ubuntu. Because ggrepel computes label placement from text metrics, labels will
land in slightly different positions on a runner-built page.

Nothing breaks, and svglite still writes text as text rather than outlines. But
the CI build will not be identical to the local one. Setting an explicit family
and installing that font in the workflow removes the difference.

### The crest fetch is already CI-safe

`dashboard.qmd` creates `logos/` when it is missing, downloads each crest once,
and draws a hollow dot for any that fails to arrive. A runner with an empty
`logos/` renders correctly on the first try. Caching the directory is still
worth doing — otherwise an ESPN outage quietly changes the franchise chart from
one day to the next.

### `update_data.R` is the test suite

Its `stopifnot` gates against ALTREP corruption and missing seasons are the only
automated check in the project. On a runner they are what stands between a
lagging mirror and a published dashboard built from nothing. Any workflow must
run `just data` as a step that can fail the job, never inside a step that
swallows its status.

---

## The configuration, if this is adopted

Written and tested for YAML validity on 2026-08-12, then removed from the
repository. It calls the justfile rather than restating the commands, so the
sequence CI runs is the sequence the justfile documents and the two cannot
drift.

### `.github/workflows/update.yml`

```yaml
name: Daily update

on:
  schedule:
    # 10:00 UTC = 06:00 ET while the season is on. GitHub reads cron in UTC and
    # does not observe daylight saving, so this drifts to 05:00 ET in November.
    - cron: "0 10 * * *"
  workflow_dispatch:

# A run that overlaps its predecessor would publish two builds over each other.
# Queue instead of cancelling: a half-finished render is not worth preserving,
# but neither is dropping the day's data refresh.
concurrency:
  group: publish
  cancel-in-progress: false

jobs:
  update:
    runs-on: ubuntu-latest
    timeout-minutes: 60

    steps:
      - uses: actions/checkout@v5

      # P3M ships precompiled packages for Ubuntu, so nothing below compiles.
      # These are the shared libraries those binaries load at runtime — the
      # text-shaping stack that systemfonts, textshaping, ragg and svglite need,
      # and which the runner image does not fully carry.
      - name: System libraries
        run: |
          sudo apt-get update -qq
          sudo apt-get install -y --no-install-recommends \
            libcurl4-openssl-dev libssl-dev libxml2-dev \
            libfontconfig1-dev libfreetype6-dev \
            libharfbuzz-dev libfribidi-dev \
            libpng-dev libtiff-dev libjpeg-dev

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: "4.6.1" # the version renv.lock records
          use-public-rspm: true

      # Restores renv.lock and caches the library against its hash: the first
      # run installs the whole set, later runs restore quickly and only pay
      # again when the lockfile changes.
      - uses: r-lib/actions/setup-renv@v2

      - uses: quarto-dev/quarto-actions/setup@v2

      - uses: extractions/setup-just@v3

      # dashboard.qmd fetches the fifteen ESPN crests on a render that finds
      # logos/ empty, and draws a hollow dot for any that fails to arrive. The
      # directory is gitignored, so without this every run re-fetches them and
      # an ESPN hiccup silently changes the franchise chart.
      - name: Cache the ESPN crests
        uses: actions/cache@v4
        with:
          path: logos
          key: logos-v1

      # Regenerate dat.rds and sched.rds. The stopifnot gates in update_data.R
      # are the test suite for this workflow: a mirror returning ALTREP columns
      # or a season short of data fails the job here rather than publishing a
      # dashboard built from nothing.
      - name: Refresh the data
        run: just data

      - name: Render
        run: just render

      # wrangler deploys a directory, and the site is one file.
      - name: Stage the site
        run: |
          mkdir -p _site
          cp dashboard.html _site/index.html

      - name: Publish to Cloudflare
        uses: cloudflare/wrangler-action@v3
        with:
          apiToken: ${{ secrets.CLOUDFLARE_API_TOKEN }}
          accountId: ${{ secrets.CLOUDFLARE_ACCOUNT_ID }}
          command: deploy
```

### `wrangler.toml`

```toml
name = "wnba"
compatibility_date = "2026-08-12"

[assets]
directory = "./_site"
```

There is no Worker script in this configuration — it serves `_site/` as static
assets. Nothing on Cloudflare's side can rebuild the page, so the justfile's
`--no-render` reasoning carries over unchanged: the render must have happened
first.

### `.gitignore`

```
# staged for the Cloudflare deploy; the html inside is already ignored above
_site/
```

### Steps to adopt

1. `renv::snapshot()`, and commit the trimmed lockfile.
2. Create a Cloudflare API token with the Workers Scripts: Edit permission. Add
   `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID` as repository secrets.
3. Restore the three files above, push, and trigger the workflow by hand from
   the Actions tab. `workflow_dispatch` exists so the first run happens under
   supervision rather than at 10:00 UTC.
4. Point a domain at the Worker.

---

## Caveats on a scheduled workflow

- **Cron is UTC and ignores daylight saving.** `0 10 * * *` is 06:00 ET during
  the season and 05:00 ET after the November change.
- **Scheduled runs are delayed under load.** They are not on the hour, and
  during busy periods can be late by a noticeable margin.
- **GitHub disables a scheduled workflow after 60 days with no repository
  commits.** This is the failure mode most likely to go unnoticed, because it
  arrives in the off-season and looks like nothing at all.
- **Failures email the repository owner by default.** For a job nobody watches,
  that is the entire notification channel. Confirm it arrives before relying on
  it.
- **The first run is the expensive one.** It installs the full renv library;
  later runs restore from cache and are dominated by the wehoop pull and the
  render. Neither has been measured on a runner.

---

## What is undecided

1. **Whether the self-contained file matters.** Everything in part one depends
   on it. Nothing else in this document is close.
2. **Whether `--token` authenticates Connect Cloud headlessly.** If it does,
   hosting and automation separate again and each can be decided alone.
3. **Whether to take the two steps together or apart.** Moving the publish to
   Cloudflare with the page unchanged is reversible — `just publish` keeps
   working throughout. Moving the panels behind an endpoint is not, in the sense
   that the self-contained artifact stops existing.

The order that keeps the most options open: automate the publish first with the
page byte-identical, live with it for a season, and treat the panels as a
separate decision made with a season of evidence about who reads the page and
how.
