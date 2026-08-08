# Shrinking a Quarto dashboard: 14.7 MB → 7.6 MB

A single-file `format: dashboard` with `embed-resources: true`, Quarto 1.10.18,
R 4.6.1. Six ggplotly charts, eighteen reactables, three device-drawn figures.
Measured on the same data throughout; every number below is bytes in the
rendered `dashboard.html`.

**14,710,261 → 7,628,541 bytes (−48%). Gzipped: 2.65 MB → 1.43 MB.**

| # | Measure | Saved | Where it lives |
|---|---|---:|---|
| 1 | plotly basic bundle instead of the full one | 2,582,008 | `partial_bundle(type = "basic")` |
| 2 | reactable given markup, not htmltools tags | 2,477,000 | `colDef(html = TRUE)` |
| 3 | duplicate Bootstrap deleted after render | 1,378,734 | post-render script |
| 4 | hovertemplate instead of per-point tooltip text | 392,000 | `hovertemplate` + `customdata` |
| 5 | svglite figures instead of 2× PNG | 349,000 | `fig-format: svg`, `dev: svglite` |

---

## 1. The full plotly.js bundle — 3.58 MB → 0.99 MB

`plotly-latest.min.js` covers every trace type plotly can draw. Every chart here
is `type: "scatter"`, which the **basic** bundle covers:

```r
basic_bundle <- function(p) partial_bundle(p, type = "basic")
ggplotly(p) |> highlight(...) |> basic_bundle()
```

Two things worth knowing:

- **It must be applied to every chart.** `partial_bundle()` renames the html
  dependency from `plotly-main` to `plotly-basic`. One chart left on the full
  bundle and the page embeds both, ending up larger than it started.
- **Name the bundle, don't use `type = "auto"`.** Auto silently falls back to the
  full bundle when a chart needs more than basic can draw; naming it stops the
  render and says which chart.

`local = TRUE` (the default) fetches from cdn.plot.ly at render time and caches
in `tempdir()`, so the render needs a network connection. Confirmed present in
the basic bundle: shapes, annotations, hoverlabel, images, legend, restyle,
hovertemplate.

## 2. reactable tag trees — 5.81 MB → 3.20 MB

This is the one that would generalise furthest, and it is invisible from the R
side. Handed an htmltools tag, reactable serialises it as a **React tag tree** —
every element written out as `{"name":...,"attribs":...,"children":[...]}`,
roughly twice the length of the markup it stands for. Handed a string of markup
with `html = TRUE` on the column, it stores the markup:

```r
colDef(html = TRUE, cell = function(value) markup(bar_cell(value, top)))
details = colDef(html = TRUE, details = function(i) markup(dossier(df$athlete[i])))
```

Two details that matter:

- **`details` must be passed a `colDef`, not a bare function**, or there is
  nowhere to put `html = TRUE`. `reactable()` merges it into the `.details`
  column.
- **`as.character()` is not enough.** htmltools renders a tag indented across
  many lines, and every one of those breaks is a byte in the file *and* a
  whitespace text node the tag tree never produced. Rendering without them is
  both smaller and closer to the original DOM:

```r
markup <- function(x) {
  gsub(">\n<", "><", doRenderTags(x, indent = FALSE), fixed = TRUE)
}
```

The `>\n<` substitution is exact rather than a blanket newline strip: a line
break inside a text node is never both preceded by `>` and followed by `<`.

Verified in a browser against the tag-tree version — node counts, classes,
`title` attributes and text all identical; the only difference is React
normalising `style="height:50%"` to `height: 50%;`.

## 3. Quarto embeds every stylesheet three times — 1.38 MB

**This one is a Quarto defect, not a configuration mistake.**

Quarto 1.10 writes each stylesheet three times: as `quarto-color-scheme`, as
`quarto-color-scheme quarto-color-alternate`, and as
`quarto-color-scheme-extra`. Where a document has no separate light and dark
theme, all three carry a **byte-identical `href`**, and nothing in the rendered
page ever reads those classes. For Bootstrap that is 460 KB of CSS embedded
three times — 1,378,734 bytes of pure repetition — and three elements sharing
`id="quarto-bootstrap"`, which is invalid HTML besides.

Reproduced across five theme configurations, all identical in this respect:

| `theme:` | bootstrap `<link>`s | `data-mode` |
|---|---:|---|
| `styles.scss` at document root | 3 | dark, dark, dark |
| `styles.scss` under the format | 3 | dark, dark, dark |
| `{dark: styles.scss}` | 3 | light, dark, light |
| `{light: styles.scss}` | 3 | dark, dark, dark |
| none at all (Quarto default) | 3 | light, light, light |

**A dashboard whose entire body is the word "Hello." renders at 2,724,041
bytes.** That is the floor for any Quarto 1.10 dashboard, and about 0.9 MB of it
is the same Bootstrap bundle twice over.

In `quarto.js` the three targets are built unconditionally once `hasDark` is
true, which it now always is:

```js
const hasDark = bundles.some((b) => b.dark !== undefined);
...
targets = [targets[0], darkTarget, lightTargetExtra];
```

Undone here in a post-render step, which groups the links by `href` and keeps the
first of each group. Keying on `href` rather than on the whole line is what makes
it safe: the three Bootstrap links differ only in their `class`, while the three
syntax-highlighting links include one genuine dark-mode variant with different
CSS that must survive.

A further ~227 KB sits in the surviving copy: `embed-resources` percent-encodes
CSS into `data:text/css,` URIs, which inflates 460,299 bytes to 686,800. A
`<style>` block would carry it as-is.

## 4. ggplotly's per-point tooltip text — 0.58 MB → 0.19 MB

`ggplotly(p, tooltip = ...)` builds one tooltip string per point, each repeating
the column names:

```
game_date: 2026-05-09<br />cum_points:  19<br />athlete: A'ja Wilson
```

4,651 of those on one chart. A `hovertemplate` is one string for the whole trace,
filled in by plotly from the numbers it already holds. The athlete's name is the
only thing a template cannot read off x and y — and the **crosstalk key already
carries it per point**, in plotly's order and with plotly's own gaps between
groups, so it is reused as `customdata` rather than sent a second time:

```r
hover <- function(p, template, custom = function(tr) tr$key) {
  p <- plotly_build(p)
  p$x$data <- lapply(p$x$data, function(tr) {
    tr$customdata <- custom(tr)
    tr$text <- NULL
    tr$hovertemplate <- paste0(template, "<extra></extra>")
    tr
  })
  p
}
```

Notes for anyone doing the same:

- **Every axis arrives from ggplotly with `hoverformat: ".2f"`,** so `%{y}` on an
  integer renders `19.00`. Each field has to name its own format: `%{y:,}`.
- **ggplotly renders a date scale as a plain numeric axis** carrying day numbers
  with explicit `tickvals`/`ticktext`, so `%{x}` reads `20582`. The date has to
  be derived back off the trace's own `x`.
- **crosstalk handles `customdata` correctly.** `subsetArrayAttrs` in plotly's
  htmlwidget JS subsets any array-valued key by index, so a selection trace gets
  its own aligned slice. Verified: a selected trace's `customdata` had 186
  entries against 186 x values.
- **A number cannot pluralise the noun after it,** and 322 of 933 points on one
  chart have a count of 1. Where that happens the counted phrase is built in R
  and travels as customdata; where it would be expensive (12 points in 4,651)
  an invariant abbreviation is cheaper.

## 5. Figures: 2× PNG → svglite SVG — 619 KB → 270 KB

`fig-format: svg` alone is close to a wash — it saved 12 KB. The reason is that
Quarto's `svg` is the cairo device, which **writes every glyph as a `<path>`**:
76 KB and 69 KB of glyph outlines on two charts of ~22 points each, and not one
`<text>` element in either file. Then `embed-resources` base64s the result,
adding 33%.

`svglite` writes text as text:

```yaml
format:
  dashboard:
    fig-format: svg
knitr:
  opts_chunk:
    dev: svglite
```

| chart | 2× PNG (b64) | cairo svg (b64) | svglite (b64) |
|---|---:|---:|---:|
| 22-point line chart | 116.3 KB | 119.9 KB | 19.6 KB |
| 22-point line chart | 101.3 KB | 109.2 KB | 10.3 KB |
| 13×8in chart, 15 embedded crests | 401.3 KB | 377.7 KB | 240.3 KB |

The third chart carries rasters via `annotation_raster`, which get base64'd
inside the SVG and then base64'd again whole — 164 KB of its 240 KB. It still
beats the 2× PNG, and the crests were being upscaled from their 96px source at 2×
anyway.

Two gotchas:

- `fig-format: svg` **turns off `fig-retina`**, so a per-chunk `#| dev: png`
  under it comes out at 1× and looks soft.
- `renv::snapshot()` will not record svglite. Implicit snapshots read
  `library()` calls, not the yaml, so the dependency has to be declared with a
  `library(svglite)` the render never otherwise needs.

---

## What is left, and why

7.63 MB, of which:

| | |
|---|---:|
| reactable payloads | 3.80 MB |
| React + ReactDOM + core-js + jQuery + Quarto/bslib JS | 1.81 MB |
| Bootstrap CSS (one copy, percent-encoded) | 0.71 MB |
| plotly widget JSON | 0.46 MB |
| bootstrap-icons woff | 0.24 MB |

The reactable payloads are 933 career panels and 226 player panels, pre-rendered
whether or not a reader ever expands the row. Rendering them client-side from
hidden data columns with `JS()` would take another ~1.3 MB off, at the cost of
moving ~120 lines of htmltools into JS strings — the markup could then come out
unbalanced, which is exactly what building it with htmltools was for.

## A note on measuring this

Three "hangs" that looked like a Quarto SVG/tabset layout bug were the Chrome
renderer running out of resources after a dozen-plus reloads of a 7.6 MB page in
the same tab. Each one reproduced reliably until it was retried in a fresh tab,
where it never reproduced at all. A minimal repro — a dashboard with an SVG
figure in a tabset next to a plotly widget — was clean from the start, which was
the signal that the diagnosis was wrong rather than that the repro was missing an
ingredient.
