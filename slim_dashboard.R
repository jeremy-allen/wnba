# Drop the stylesheets Quarto embeds in dashboard.html more than once.
#
# Run with:  Rscript slim_dashboard.R
# The justfile runs it straight after `quarto render`, and `quarto publish
# --no-render` then uploads the slimmed file.
#
# Quarto 1.10 writes every stylesheet three times: once as `quarto-color-scheme`,
# once as `quarto-color-scheme quarto-color-alternate`, and once as
# `quarto-color-scheme-extra`. A document with no separate light and dark themes
# -- this one is dark throughout -- gets the same `href` in all three, and
# nothing in the rendered page ever reads those classes. For Bootstrap that is
# 460 KB of CSS embedded three times, 1.38 MB of pure repetition, and three
# elements sharing `id="quarto-bootstrap"`, which is invalid HTML besides.
#
# No setting avoids it. A dashboard whose entire body is the word "Hello."
# renders at 2.72 MB for this reason, with a theme or without one, with the theme
# named at the document root or under the format. So it is undone here instead,
# after the render.
#
# The links are grouped by `href` and the first of each group is kept. Keying on
# `href` rather than on the whole line is what makes that safe: the three
# Bootstrap links differ only in their `class`, while the three
# syntax-highlighting links include one genuine dark-mode variant, with different
# CSS, that has to survive.
#
# When this stops removing anything, Quarto has fixed it, and the script and its
# justfile step can go.

path <- "dashboard.html"

if (!file.exists(path)) {
  stop("no ", path, " to slim -- run `quarto render dashboard.qmd` first")
}

before <- file.size(path)

# read and write as bytes, so the file comes back out exactly as it went in
# apart from the lines removed -- including its missing final newline
html <- readChar(path, before, useBytes = TRUE)
lines <- strsplit(html, "\n", fixed = TRUE, useBytes = TRUE)[[1]]

# each <link> is on a line of its own, however long its data URI
is_scheme_link <- grepl(
  '^<link href="data:text/css,[^"]*"[^>]*class="quarto-color-scheme',
  lines,
  perl = TRUE,
  useBytes = TRUE
)

href <- rep(NA_character_, length(lines))
href[is_scheme_link] <- sub(
  '^<link href="(data:text/css,[^"]*)".*$', "\\1",
  lines[is_scheme_link],
  perl = TRUE,
  useBytes = TRUE
)

# duplicated() keeps the first of each group and marks the rest
duplicate <- is_scheme_link & duplicated(href)

if (!any(duplicate)) {
  message(
    "no duplicate stylesheets in ", path, " (", format(before, big.mark = ","),
    " bytes). Quarto may have fixed this -- see the note at the top of this file."
  )
  quit(save = "no")
}

con <- file(path, open = "wb")
writeBin(charToRaw(paste(lines[!duplicate], collapse = "\n")), con)
close(con)

after <- file.size(path)

message(
  "dropped ", sum(duplicate), " duplicate stylesheet",
  if (sum(duplicate) == 1) "" else "s", " from ", path, ": ",
  format(before, big.mark = ","), " bytes -> ",
  format(after, big.mark = ","), " bytes (-",
  format(round(100 * (before - after) / before, 1), nsmall = 1), "%)"
)
