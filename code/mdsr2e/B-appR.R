## ----eval=FALSE---------------------------------------------------------------
## ?function
## help(function)


## ----eval=FALSE---------------------------------------------------------------
## example(mean)


## -----------------------------------------------------------------------------
x <- c(5, 7, 9, 13, -4, 8) # preferred
x =  c(5, 7, 9, 13, -4, 8)  # equivalent


## -----------------------------------------------------------------------------
x[2]
x[c(2, 4)]
x[c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)]
x[1:5]
x[-6]


## -----------------------------------------------------------------------------
x > 8


## -----------------------------------------------------------------------------
sum(x > 8)


## -----------------------------------------------------------------------------
newlist <- list(first = "hello", second = 42, Bob = TRUE)
is.list(newlist)
newlist
newlist[[2]]
newlist$Bob


## -----------------------------------------------------------------------------
unlisted <- unlist(newlist)
unlisted


## -----------------------------------------------------------------------------
A <- matrix(x, 2, 3)
A
is.matrix(A)    # is A a matrix?
is.vector(A)
is.matrix(x)


## -----------------------------------------------------------------------------
A[2, 3]
A[, 1]
A[1, ]


## -----------------------------------------------------------------------------
y <- rep(11, length(x))
y
ds <- data.frame(x, y)
ds
ds$x[3]
is.data.frame(ds)


## -----------------------------------------------------------------------------
tbl <- as_tibble(ds)
is.data.frame(tbl)
is_tibble(ds)
is_tibble(tbl)


## -----------------------------------------------------------------------------
newmat <- cbind(x, y)
newmat
is.data.frame(newmat)
is.matrix(newmat)


## -----------------------------------------------------------------------------
attributes(A)


## -----------------------------------------------------------------------------
head(methods(summary))


## -----------------------------------------------------------------------------
tbl <- as_tibble(ds)
class(tbl)
print(tbl)
print.data.frame(tbl)
print.default(tbl)


## -----------------------------------------------------------------------------
class(mtcars)
typeof(mtcars)


## -----------------------------------------------------------------------------
vals <- rnorm(1000) # generate 1000 standard normal random variables
quantile(vals)
quantile(vals, c(.025, .975))

# Return values can be saved for later use.
res <- quantile(vals, c(.025, .975))
res[1]


## -----------------------------------------------------------------------------
res <- quantile(vals, probs = c(.025, .975), type = 3)
res


## ----eval=FALSE---------------------------------------------------------------
## paste(..., sep = " ", collapse = NULL)


## ----echo=TRUE,eval=FALSE-----------------------------------------------------
## install.packages("mdsr")    # CRAN version
## remotes::install_github("mdsr-book/mdsr")    # development version


## ----eval=FALSE---------------------------------------------------------------
## install.packages("Hmisc")
## library(Hmisc)


## ----eval=FALSE, prompt=FALSE-------------------------------------------------
## > library(xaringanthemer)
## Error in library(xaringanthemer) : there is no package called 'xaringanthemer'


## ----eval=FALSE, prompt=FALSE-------------------------------------------------
## > install.packages("xaringanthemer")
## trying URL 'https://cloud.r-project.org/src/contrib/xaringanthemer_0.3.0.tar.gz'
## Content type 'application/x-gzip' length 1362643 bytes (1.3 MB)
## ==================================================
## downloaded 1.3 Mb


## ----eval=TRUE, message=FALSE-------------------------------------------------
library(xaringanthemer)


## -----------------------------------------------------------------------------
find("mean")


## ----message=FALSE------------------------------------------------------------
library(Hmisc)
find("units")


## ----echo=FALSE---------------------------------------------------------------
detach(package:Hmisc)


## ----eval=TRUE----------------------------------------------------------------
sessioninfo::session_info()


## ----cran, echo=FALSE, message=FALSE------------------------------------------
library(mdsr)
library(rvest)
library(methods)
ctv <- read_html("https://cran.r-project.org/web/views/") %>%
  html_table()
ctv_tbl <- ctv[[1]] %>%
  mutate(
    url = paste0("https://cran.r-project.org/web/views/", X1, ".html"),
    link_html = paste0("[", X1, "](", url, ")"),
    link_latex = paste0("\\href{", url, "}{", X1, "}")
  ) %>%
  arrange(link_html) %>%
  rename(Subject = X2) %>%
  mutate(
    Subject = gsub("&", "and", Subject), 
    index = tolower(Subject), 
    index = gsub("bayes", "Bayes", index), 
    index = gsub("web ", "Web ", index), 
    index = gsub(" in r", " in R", index),
    index = gsub(" with r", " with R", index),
    index_entry = paste0("task view!", index), 
    index_entry_alt = paste(index, "task view")
  )

if (knitr::is_latex_output()) {
  ctv_tbl %>%
    select(`Task View` = link_latex, Subject) %>%
    mdsr_table(
      caption = "A complete list of CRAN task views.", 
#      format = "latex",
      escape = FALSE
    ) %>%
    kableExtra::column_spec(2, width = "24em")
} else {
  ctv_tbl %>%
    select(`Task View` = link_html, Subject) %>%
    knitr::kable(
      caption = "A complete list of CRAN task views."
    )
}



## ----ctv, results='asis', echo=FALSE------------------------------------------
ctv_tbl %>%
  pull(index_entry) %>%
  map_chr(index_entry, index_label = "subject") %>%
  cat(sep = "\n")

ctv_tbl %>%
  pull(index_entry_alt) %>%
  map_chr(index_entry, index_label = "subject") %>%
  cat(sep = "\n")

