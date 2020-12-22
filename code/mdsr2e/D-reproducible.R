## ----rmarkdowndialog, echo=FALSE, fig.cap="Generating a new R Markdown file in RStudio."----
knitr::include_graphics("gfx/rmarkdowndialog.png")


## ----mark1, echo=FALSE, fig.cap="(ref:markdown-cap)", out.width='0%'----------
cat(readChar("code/rmarkdown_default.Rmd", 1e5))
knitr::include_graphics("gfx/1x1.png")


## ----eval=FALSE---------------------------------------------------------------
## library(rmarkdown)
## render("filename.Rmd")   # creates filename.html
## browseURL("filename.html")


## ----eval=FALSE, include=FALSE------------------------------------------------
## rmarkdown::render("code/rmarkdown_default.Rmd")
## webshot::webshot(url = "code/rmarkdown_default.html", file = "gfx/rmarkdown_default.png")


## ----rmark, echo=FALSE, fig.cap="Formatted output from R Markdown example."----
knitr::include_graphics("gfx/rmarkdown_default.png")

