## ----install, eval=FALSE------------------------------------------------------
## # this command only needs to be run once
## install.packages("mdsr")
## # if you want the development version
## remotes::install_github("mdsr-book/mdsr")


## ----eval=FALSE---------------------------------------------------------------
## library(mdsr)
## data(package = "mdsr")


## ----warning=FALSE, echo=FALSE------------------------------------------------
library(stringr)
files <- list.files(pattern = "\\.Rmd$")

get_pkg <- function(x) {
  lines <- readLines(x)
  # match lines with "library(x)"
  lib_regex <- "^(library|require)\\(.+\\)"
  libs <- lines[grep(pattern = lib_regex, x = lines)] %>%
    str_extract(pattern = lib_regex) %>%
    gsub(pattern = "library\\(", replacement = "", x = .) %>%
    gsub(pattern = "\\)", replacement = "", x = .)
  # match lines with "`r pkg("x")`"
  lib_regex <- 'pkg\\(\\"[a-z,A-Z,0-9]+\\"\\)'
  pkgs <- lines[grep(pattern = lib_regex, x = lines)] %>%
    str_extract_all(pattern = lib_regex) %>%
    unlist() %>%
    gsub(pattern = 'pkg\\(\\"', replacement = "", x = .) %>%
    gsub(pattern = '\\"\\)', replacement = "", x = .)
  # match lines with "`r pkg("x")`"
  lib_regex <- "[a-z,A-Z,0-9]+::"
  ddots <- lines[grep(pattern = lib_regex, x = lines)] %>%
    str_extract_all(pattern = lib_regex) %>%
    unlist() %>%
    gsub(pattern = "::", replacement = "", x = .)
  union_all(libs, pkgs, ddots) %>% 
    # manual culling of false positives
    setdiff("x") 
}
pkgs <- files %>%
  map(get_pkg) %>%
  unlist() %>%
  unique() %>%
  sort()


## ---- include=FALSE-----------------------------------------------------------
# automatically create a bib database for R packages

# remotes::install_github("wch/extrafont")

knitr::write_bib(
  c(
    # all loaded packages
    .packages(), 
    # all packages in the text
    pkgs, 
    # packages required for rendering
    'bookdown', 'knitr', 'rmarkdown',
    # other packages mentioned in the text
    'viridis', 'viridisLite', "multidplyr"
    # manually remove false positives
  ) %>% 
    setdiff("location")
  , 
  'packages.bib'
)


## -----------------------------------------------------------------------------
# https://github.com/yihui/knitr/pull/1936
system("sed -i 's/and R core and/and {R Core Team} and/g' packages.bib")


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
pkg_table <- installed.packages(fields = "Title") %>% 
  as_tibble() %>%
  filter(Package %in% pkgs) %>%
  select(Package, Title) %>%
  distinct(Package, .keep_all = TRUE) %>%
  mutate(
    on_cran = map_lgl(Package, ~!as.logical(available::available_on_cran(.x))), 
    citation = ifelse(
      Package %in% c("base", "methods", "parallel"), 
      "", 
      paste0("@R-", Package)
    ),
    citation_html = paste0("[@R-", Package, "]"),
    citation_latex = paste0("\\cite{R-", Package, "}\\index{R}{library(", Package, ")}"),
    Title = gsub("\n", " ", Title)
  ) %>%
  arrange(Package)


## ----cran-pkgs, echo=FALSE----------------------------------------------------
if (knitr::is_latex_output()) {
  pkg_table %>%
    filter(on_cran) %>%
    select(Package, Citation = citation_latex, Title) %>%
    mdsr_table(caption = "List of CRAN packages used in this book.", escape = FALSE, longtable = TRUE) %>%
    kableExtra::column_spec(2, width = "10em") %>%
    kableExtra::column_spec(3, width = "20em")
} else {
  pkg_table %>%
    filter(on_cran) %>%
    select(Package, Citation = citation_html, Title) %>%
    mdsr_table(caption = "List of CRAN packages used in this book.")
}


## ----github-pkgs, echo=FALSE, warning=FALSE-----------------------------------
gh_locs <- tribble(
  ~Package, ~`GitHub User`,
  "etude", "dtkaplan",
  "fec12", "baumer-lab",
  "openrouteservice", "GIScience",
  "streamgraph", "hrbrmstr"
)

gh_pkgs <- pkg_table %>%
  filter(!on_cran) %>%
  right_join(gh_locs, by = "Package")

# manual fix for streamgraph
gh_pkgs <- gh_pkgs %>%
  mutate(Title = ifelse(Package == "streamgraph", "Build Streamgraph Visualizations", Title))

if (knitr::is_latex_output()) {
  gh_pkgs %>%
    select(Package, `GitHub User`, Citation = citation_latex, Title) %>%
    mdsr_table(caption = "List of GitHub packages used in this book.", escape = FALSE, longtable = TRUE) %>%
    kableExtra::column_spec(3, width = "7em") %>%
    kableExtra::column_spec(4, width = "15em")
} else {
  gh_pkgs %>%
    select(Package, `GitHub User`, Citation = citation_html, Title) %>%
    mdsr_table(caption = "List of GitHub packages used in this book.")
}


## ----index-entries, echo=FALSE, results='asis', eval=TRUE---------------------
pkgs %>%
  paste(., " package|see{library(", ., ")}", sep = "") %>%
  map_chr(index_entry, index_label = "R") %>%
  cat(sep = "\n")


## ---- include=FALSE, echo=FALSE-----------------------------------------------
saveRDS(pkgs, file = "2020-09-06packages.Rds")

