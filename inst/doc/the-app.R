## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  eval = FALSE,
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

## -----------------------------------------------------------------------------
# install.packages(c("shiny", "bslib", "DT", "zip"))
# shiny::runApp("apps/r_shiny", port = 8502)

