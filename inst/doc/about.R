## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  R.options = list(
    cli.num_colors = 1,
    cli.hyperlink = FALSE,
    crayon.enabled = FALSE,
    width = 80
  )
)
# Console colour carries no meaning on a rendered page. pkgdown turns it on for
# its own build, and the escape sequences then reach the reader as literal text,
# so colour is switched off here for a plain vignette render and a site build
# alike. The fixed width keeps printed output inside the documentation column.

## ----bibtex, echo = FALSE, results = "asis"-----------------------------------
# Build the BibTeX entry from the installed version so it never drifts, then
# render it with a copy button and a download link. The button uses the
# browser clipboard API; the link is a self-contained data URI, so neither
# depends on a static file being shipped alongside the site.
ver <- as.character(utils::packageVersion("lexsync"))
bib <- paste(
  "@Manual{lexsync,",
  "  title  = {{lexsync}: Lexical optimisation and hardware-timed experiment generation},",
  "  author = {Pablo Bernabeu},",
  "  year   = {2026},",
  sprintf("  note   = {R package version %s},", ver),
  "  url    = {https://github.com/pablobernabeu/lexsync},",
  "}",
  sep = "\n"
)
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
uri <- paste0(
  "data:application/x-bibtex;charset=utf-8,",
  utils::URLencode(bib, reserved = TRUE)
)
cat(sprintf(
'<div class="citation-bibtex">
<pre id="lexsync-bibtex"><code>%s</code></pre>
<p class="citation-bibtex-actions">
<button type="button" class="btn btn-primary btn-sm" onclick="lexsyncCopyBibtex(this)">Copy BibTeX</button>
<a class="btn btn-outline-primary btn-sm" download="lexsync.bib" href="%s">Download .bib</a>
</p>
</div>
<script>
function lexsyncCopyBibtex(btn) {
  var code = document.getElementById("lexsync-bibtex");
  navigator.clipboard.writeText(code.innerText).then(function () {
    var label = btn.textContent;
    btn.textContent = "Copied";
    setTimeout(function () { btn.textContent = label; }, 1500);
  });
}
</script>
', esc(bib), uri))

