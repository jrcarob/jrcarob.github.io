# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A [Quarto](https://quarto.org/) website (`project: type: website`) for José Caro's academic personal site (assistant professor, Córdoba University): about page, teaching, research/publications, conference talks, a blog, CV, and a "Demography" section with R-based data dashboards. There is no application server or build pipeline beyond Quarto's own render — this is a static site.

## Commands

- **Preview locally (live reload):** `quarto preview` — configured in `_quarto.yml` to serve on port 5555 and open a browser.
- **Render the full site:** `quarto render` — outputs to `_site/` (gitignored).
- **Render a single page:** `quarto render path/to/file.qmd` — much faster than a full rebuild when iterating on one post/page.
- **Deploy:** `./deploy.sh` — rsyncs `_site/` to the remote host (`REMOTE_HOST=cloud`), deleting remote files not present locally. Run `quarto render` first; this script does not render for you.
- **R environment:** the project uses `renv` (see `renv.lock`, `.Rprofile` auto-activates it). Run R/Quarto commands from an R session that has sourced `renv/activate.R` (RStudio does this automatically via the `.Rproj`), or run `Rscript -e 'renv::restore()'` once to sync packages before rendering for the first time.

There is no separate lint/test suite — correctness is verified by rendering and visually checking output in `_site/`.

**After any content or config change, open the affected page in the browser to confirm it visually before considering the task done.** Start `quarto preview` (or reuse a running one — check `lsof -i :5555` first, since a stale preview process racing a manual `quarto render` will corrupt `_site` output), then navigate to the changed page with the Chrome tools and screenshot it. Kill the preview server when finished.

## Structure and rendering model

- `_quarto.yml` is the single source of truth for site-wide config: navbar, footer, theme (`html/ath.scss`), bibliography (`files/bib/references.bib` + CSL), and which files get rendered (`*.qmd`/`*.md`, excluding `etc/`).
- Each top-level content section (`teaching/`, `research/`, `talks/`, `blog/`, `demography/`, `apps/`, `cv/`, `now/`) has its own `index.qmd` and, for listing-style sections, a `_metadata.yml` that sets section-wide defaults (banner color, TOC behavior, default author/execute options). When adding a page to one of these sections, check its `_metadata.yml` first rather than repeating options per-page.
- `blog/` and `research/articles/` are organized as dated/slugged subdirectories (e.g. `blog/2026/09/`, `research/articles/2026_emh/`), each with its own `index.qmd` — this is a Quarto listing pattern, not a blog engine. New posts/articles follow the existing folder-per-entry convention.
- `html/<section>/listing.ejs` templates control how each section's listing page renders its item cards; `html/research/title-block.html` is a custom partial used via `template-partials` in `research/_metadata.yml`.
- `demography/` and `apps/` contain writeups of R/Shiny dashboards (e.g. `demography/2026_shinyapp/`), each is a Quarto post describing and linking to an externally-hosted Shiny app (shinyapps.io) rather than an embedded live app — the `.R` files here are documentation artifacts, not part of the Quarto build.
- `_extensions/` holds installed Quarto extensions (`quarto-ext`, `schochastics`) — don't hand-edit; manage via `quarto add`.
- `_freeze/` caches computed output for `freeze: true` documents so unchanged code chunks aren't re-executed on render; delete a specific subfolder there if a page's cached output needs to be forced to regenerate.
- `cv/` is a mixed target: `cv/cv.Rmd` compiles to `cv/cv.pdf` via LaTeX (`twentysecondcv.cls`) outside the main Quarto render — treat it as a separate document pipeline, not a `.qmd`.
- Root-level files that must live at the site root on deploy (`.htaccess`, `keybase.txt`, `pgp_ath.asc.txt`, verification HTML files) are declared under `resources:` in `_quarto.yml`; add new root-pinned files there rather than assuming file location is enough.
