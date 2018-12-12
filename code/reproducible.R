## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/reproducible-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----echo=FALSE, size='footnotesize'-------------------------------------
cat(readChar("code/rmarkdown_default.Rmd", 1e5))

## ----eval=FALSE----------------------------------------------------------
## library(rmarkdown)
## render("filename.Rmd")   # creates filename.html
## browseURL("filename.html")

## ----eval=FALSE, include=FALSE-------------------------------------------
## rmarkdown::render("code/rmarkdown_default.Rmd")
## webshot::webshot(url = "code/rmarkdown_default.html", file = "gfx/rmarkdown_default.pdf")

