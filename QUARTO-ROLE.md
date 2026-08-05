# What Quarto is actually doing in this project

Notes from a discussion on 2026-08-04, branch `wow`. Nothing here has been
acted on. No code was changed to produce it.

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

Four files carry the work: `dashboard.qmd` (767 lines), `courtside.scss`
(1,163), `courtside.js` (574), `METHODOLOGY.md` (114). The old
`styles.scss` was deleted and is recoverable with
`git checkout main -- styles.scss`.

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

The eight non-blank lines of markdown body are two fenced divs
(`::: {#fineprint .band}` and `::: {.method-body}`), one shortcode
(`{{< include METHODOLOGY.md >}}`), and a raw `<details>` block. That is the
whole authoring layer.

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

## What an idiomatic version would look like

Quarto supports inline R inside markdown, so the six section intros could be
authored as prose:

```markdown
::: {.band-head .reveal}
## The pecking order

The top ten in each of seven categories. Every bar starts at zero and the
longest one is the leader, so a bar half as long is half the total. Click a
row to open that player's panel.
:::
```

with `` `r nrow(players)` `` supplying computed numbers inline.

Only genuinely generated markup needs to stay in `cat()`: the 225 player
cards, the score ticker, the team cards, the leaderboards, and the decoder
examples that interpolate player names and figures.

Rough split of the 182 HTML-emitting lines, by eye:

| stays generated | could become markdown |
|---|---|
| player cards, ticker, team cards, chips, tabs | hero lede |
| decoder card *examples* (interpolate data) | six section intros |
| the payload `<script>` block | decoder card *bodies* (static definitions) |
| | the record-book dagger footnote |

The pattern is already proven in the file. `#fineprint` uses a fenced div and
was not reused anywhere else.

## Open decisions for next session

1. **Refactor the static prose into markdown, or leave it?** The upside is
   that copy edits become text edits. The cost is a mixed authoring model
   where some sections are markdown and some are `cat()`, which could read as
   inconsistent. Worth prototyping on one section first, probably *The
   pecking order*, and judging from that.

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
