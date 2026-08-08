# Show the available commands
default:
    @just --list

# Refresh the data, rebuild the dashboard from it, and publish
update: data render publish

# Regenerate dat.rds and sched.rds from the wehoop mirror, topped up from ESPN
data:
    Rscript update_data.R

# The render is what makes `update` mean what it says. `quarto publish` runs
# with --no-render, uploading dashboard.html exactly as it sits on disk, so
# going straight from the new .rds files to publishing would push the last
# build -- made from the previous data -- and report success.

# Rebuild dashboard.html from the caches on disk
render:
    quarto render dashboard.qmd

# --no-render because rendering on Connect Cloud would take config.yml's
# `rsconnect` profile, which pulls from wehoop rather than reading the caches.
#
# --no-prompt skips the confirm-the-destination step. The destination itself
# stays in _publish.yml, which already records the id and url; passing --id
# here as well would put the same identifier in two files to drift apart.

# Upload dashboard.html to Posit Connect Cloud as it stands
publish:
    quarto publish posit-connect-cloud dashboard.qmd --no-render --no-prompt
