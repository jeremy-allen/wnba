# What Quarto is actually doing in this project

Notes from a discussion on 2026-08-04, branch `wow`. No code was changed to
produce them. One thing they proposed was then acted on, on 2026-08-05: the
static prose moved into markdown. What follows describes the file as it was on
2026-08-04; where the move changed something, the section says so.

## Where things stand

Branch `wow` rebuilt the dashboard as a single scrolling page. Six commits,
none merged to `main`:

```
1b56f38  Remove the rule under each section heading
c3b567a  Remove the rule above each heading
b87b701  Drop the chapter numbering and the semicolons
18d1237  Say what the numbers mean instead of gesturing at it
c31dcae  Take the edge off the contrast
b028ef5  Rebuild the dashboard as a scrolling, six-chapter page
```

Four files carried the work: `dashboard.qmd` (767 lines), `courtside.scss`
(1,163), `courtside.js` (574), `METHODOLOGY.md` (114). The markdown move added
a fifth, `record-footnote.qmd` (5). The old `styles.scss` was deleted and is
recoverable with `git checkout main -- styles.scss`.

## The question

A data scientist opening `dashboard.qmd` would say there is no Quarto syntax
in it, only HTML. Are they wrong?

## Measurements

```
dashboard.qmd            767 lines
  YAML front matter       17
  inside R chunks        723   (94%)
      emitting HTML      182
      data logic         519
  markdown body           27   (4%, of which 8 non-blank)
```

The eight non-blank lines of markdown body were two fenced divs
(`::: {#fineprint .band}` and `::: {.method-body}`), one shortcode
(`{{< include METHODOLOGY.md >}}`), and a raw `<details>` block. That was the
whole authoring layer.

The same count after the markdown move:

```
dashboard.qmd            891 lines
  YAML front matter       17
  inside R chunks        652   (73%)
      emitting HTML      122
      data logic         486
  markdown body          223   (25%, of which 143 non-blank)
```

To re-derive after further edits:

```python
import re
lines = open('dashboard.qmd').read().split('\n')
inchunk = inyaml = False
yaml = chunk = md = html_emit = r_logic = 0
for i, l in enumerate(lines):
    if i == 0 and l.strip() == '---':
        inyaml = True; yaml += 1; continue
    if inyaml:
        yaml += 1
        if l.strip() == '---': inyaml = False
        continue
    if l.startswith('```{r'): inchunk = True; chunk += 1; continue
    if inchunk and l.startswith('```'): inchunk = False; chunk += 1; continue
    if inchunk:
        chunk += 1
        if re.search(r"['\"]<|</|h\(|cat\(|&mdash;|&rsquo;|paste0\(\s*$", l):
            html_emit += 1
        else:
            r_logic += 1
    else:
        md += 1
print(len(lines), yaml, chunk, html_emit, r_logic, md)
```

## Verdict

They would be roughly 70% right. The 30% they would be wrong about is real,
but it is build tooling rather than authoring.

## What Quarto genuinely provides here

Ranked by how much would have to be rebuilt without it.

**The SCSS theme layer.** `theme: courtside.scss` does more than compile
Sass. Quarto injects `/*-- scss:defaults --*/` *before* Bootstrap's `!default`
variable declarations and `/*-- scss:rules --*/` *after* the compiled
framework. That ordering is the only reason `$body-bg: #101318 !default`
takes effect. Replacing it means owning the concatenation order by hand.

**`{{< include >}}` and markdown rendering.** `METHODOLOGY.md` is 114 lines
of real markdown with headings, a table, a blockquote, lists and code spans.
It renders on GitHub and inside the page from one source file.

**`quarto publish posit-connect-cloud`.** `_publish.yml` still works
untouched, and the deployment target is unchanged from `main`.

**`embed-resources`** belongs to pandoc, invoked by Quarto. It inlines the
compiled CSS and the 30 team crests into one 1.7 MB file.

**Chunk execution** belongs to knitr and predates Quarto by a decade.

## What it costs

Bootstrap was net-negative for this page. Effort went into escaping it rather
than using it:

- `page-layout: custom` to get out of its container
- resetting `grid-column` on `.wall-shell > *`, because Bootstrap places every
  `<aside>` in a margin track and pulled the dossier out of its own grid
- `border-bottom: 0` on `h2.band-title` to remove an underline it adds
- overriding `<code>` chip colours, which assume a light page

A framework was taken on and then had its effects removed one at a time.

## The real criticism

The ratio is not the problem. The problem is where the prose lives.

Every sentence a reader sees sits inside an R string literal inside a code
chunk. That means hand-escaping `&rsquo;`, `&mdash;` and `&dagger;`, no
spell-check, and diffs that show string concatenation rather than sentences.

The cost showed up twice in one session. Improving the copy meant grepping R
string fragments to find prose. Removing semicolons meant the same. Both
should have been edits to text and were instead edits to code.

The tell: `METHODOLOGY.md` is the one place prose was allowed to be markdown,
and it is the one place editing was pleasant. That is the format doing its
job.

This is the criticism the 2026-08-05 move answers. The next section says how.

## What an idiomatic version looks like

Done on 2026-08-05. Every sentence a reader sees is now markdown, and the
authoring layer went from 8 non-blank lines to 143.

Each band is a fenced div, and the heading and intro inside it are prose, with
inline R supplying the computed numbers:

```markdown
::: {.band-head .reveal}
## The pecking order {.band-title}

::: {.band-sub}
The top ten in each of seven categories. Every bar starts at zero and the
longest one is the leader, so a bar half as long is half the total. Click a
row to open that player's panel.
:::
:::
```

What moved, and what stayed:

| stays generated | is now markdown |
|---|---|
| player cards, ticker, team cards, chips, tabs | hero lede |
| decoder card *examples* (interpolate data) | six section intros |
| the payload `<script>` block | decoder card *bodies* |
| | the record-book dagger footnote |

The single `page` chunk became twelve, each named for the part of the page it
builds and each emitting balanced markup, so no HTML element is left open
across a markdown boundary. The decoder examples are built as a list, `eg`, and
dropped into their cards with `` `r eg$double_double` ``.

Three things the move needed:

- **A child document.** The dagger footnote appears only when a leaderboard on
  show contains a career the data truncates, and markdown has no conditional.
  `#| child: !expr if (career_has_trunc) "record-footnote.qmd"` is knitr's
  answer: the file is knitted, inline R and all, or not read at all.
- **An explicit heading id.** `## Decoder` would generate `#decoder` on its own
  and collide with the band that carries it.
- **Four stylesheet rules, and two selectors widened.** A paragraph in a fenced
  div arrives as a `<p>` inside the div rather than as the div itself, so
  `.band-sub`, `.hero-lede` and `.term .eg` each had to account for the
  paragraph inside them. Markdown emphasis arrives as `<strong>`, so the two
  rules that coloured `<b>` now name both.

Pandoc's smart typography replaced the hand-escaped entities: `'` becomes a
curly apostrophe, `--` an en dash, and `†` is simply typed. The rendered page
is unchanged apart from element names the stylesheet does not select on: a
`<section class="band">` is now a `<div class="band">`, and each `.band-head`
is the `<section>` Quarto builds around the heading.

## Open decisions for next session

1. ~~**Refactor the static prose into markdown, or leave it?**~~ Settled on
   2026-08-05: refactored, all seven bands at once rather than one as a
   prototype. The mixed authoring model it was expected to cost turns out to
   fall on a clean line — prose in markdown, generated markup in chunks — and
   the file now reads as a page with data in it rather than as a script that
   prints a page.

2. **Keep Bootstrap or drop it?** Quarto's `html` format bundles it. Check
   whether `minimal: true` or a bare `theme` gets a cleaner base without
   losing the `scss:defaults` layering, which is the part actually worth
   having.

3. **Is `format: html` still right, or should this be `format: dashboard`
   again?** It was moved off `dashboard` deliberately, because that format's
   card grid was the thing being replaced. No reason to revisit unless the
   publishing target changes.

4. **Merge `wow` to `main`?** If squashing, note that commit `b028ef5` says
   "six-chapter" in its subject, which is the only surviving use of the word
   "chapter" anywhere in the project.

## Things deliberately not done

- `styles.scss` was deleted rather than left as dead weight.
- The hairline under headings was removed on `.band-title` only. Headings
  inside the fine print keep theirs, because that block is continuous prose
  with no section borders to chunk it.
- The `README.md` screenshot still shows the old dashboard layout.
