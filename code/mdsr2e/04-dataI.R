## \begin{tikzpicture}[

##   xscale=3,yscale=2,

##   coverage/.style={circle,draw=blue!50,fill=blue!20,thick,opacity=0.5},

##   lifetime/.style={->,dashed,thick,gray},

##   sensor/.style={circle,draw=black,fill=red!50,thick,opacity=0.75,inner sep=0pt,minimum size=2mm},

##   nrow/.style={decoration={brace,amplitude=10pt},xshift=-2pt,yshift=0pt},

##   ncol/.style={decoration={brace,amplitude=10pt},xshift=0pt,yshift=-2pt}

## ]

## % left image

## \draw[fill=gray] (0.7,0.9) rectangle (0.8,1);

## \draw[fill=gray] (0.7,0.4) rectangle (0.8,0.5);

## \draw[fill=gray] (0.7,0.1) rectangle (0.8,0.2);

## \draw [] (0,0) rectangle (1, 1);

## \draw [decorate,nrow] (0,0) -- (0,1) node [midway,xshift=-20pt] {$n$};

## \draw [decorate,ncol] (1,0) -- (0,0) node [midway,yshift=-20pt] {$p$};

## 
## % right image				

## \draw[fill=gray] (2.7,0.4) rectangle (2.8,1);

## \draw [] (2,0.4) rectangle (3, 1);

## \draw [decorate,nrow,xshift=+4pt] (3,1) -- (3,0.4) node [midway,xshift=30pt] {$m \leq n$};

## \draw [decorate,ncol] (3,0.4) -- (2,0.4) node [midway,yshift=-20pt] {$p$};

## \draw[->] (1, 0.95) to [out = 0, in = 180, looseness = 2] (2,0.9);

## \draw[->] (1, 0.45) to [out = 0, in = 180, looseness = 2] (2,0.7);

## \draw[->] (1, 0.15) to [out = 0, in = 180, looseness = 2] (2,0.5);

## \end{tikzpicture}


## \begin{tikzpicture}[

##   xscale=3,yscale=2,

##   coverage/.style={circle,draw=blue!50,fill=blue!20,thick,opacity=0.5},

##   lifetime/.style={->,dashed,thick,gray},

##   sensor/.style={circle,draw=black,fill=red!50,thick,opacity=0.75,inner sep=0pt,minimum size=2mm},

##   nrow/.style={decoration={brace,amplitude=10pt},xshift=-2pt,yshift=0pt},

##   ncol/.style={decoration={brace,amplitude=10pt},xshift=0pt,yshift=-2pt}

## ]

## % left image

## \draw[fill=gray] (0.1,0) rectangle (0.2,1);

## \draw[fill=gray] (0.4,0) rectangle (0.5,1);

## \draw[fill=gray] (0.8,0) rectangle (0.9,1);

## \draw[fill=gray] (0.9,0) rectangle (1,1);

## \draw [] (0,0) rectangle (1, 1);

## \draw [decorate,nrow] (0,0) -- (0,1) node [midway,xshift=-20pt] {$n$};

## \draw [decorate,ncol] (1,0) -- (0,0) node [midway,yshift=-20pt] {$p$};

## 
## % right image				

## \draw[fill=gray] (2,0) rectangle (2.1,1);

## \draw[fill=gray] (2.1,0) rectangle (2.2,1);

## \draw[fill=gray] (2.2,0) rectangle (2.3,1);

## \draw[fill=gray] (2.3,0) rectangle (2.4,1);

## \draw [] (2,0) rectangle (2.4, 1);

## \draw [decorate,nrow,xshift=+4pt] (2.4,1) -- (2.4,0) node [midway,xshift=30pt] {$n$};

## \draw [decorate,ncol] (2.4,0) -- (2,0) node [midway,yshift=-20pt] {$\ell \leq p$};

## \draw[->] (0.15, 1) to [out = 90, in = 90, looseness = 0.5] (2.05,1);

## \draw[->] (0.45, 1) to [out = 90, in = 90, looseness = 0.5] (2.15,1);

## \draw[->] (0.85, 1) to [out = 90, in = 90, looseness = 0.5] (2.25,1);

## \draw[->] (0.95, 1) to [out = 90, in = 90, looseness = 0.5] (2.35,1);

## \end{tikzpicture}


## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(mdsr)
presidential


## -----------------------------------------------------------------------------
select(presidential, name, party)


## -----------------------------------------------------------------------------
filter(presidential, party == "Republican")


## -----------------------------------------------------------------------------
select(
  filter(presidential, lubridate::year(start) > 1973 & party == "Democratic"), 
  name
)


## -----------------------------------------------------------------------------
presidential %>%
  filter(lubridate::year(start) > 1973 & party == "Democratic") %>% 
  select(name)


## ----eval=FALSE---------------------------------------------------------------
## dataframe %>% filter(condition)


## \begin{tikzpicture}[

##   xscale=3,yscale=2,

##   coverage/.style={circle,draw=blue!50,fill=blue!20,thick,opacity=0.5},

##   lifetime/.style={->,dashed,thick,gray},

##   sensor/.style={circle,draw=black,fill=red!50,thick,opacity=0.75,inner sep=0pt,minimum size=2mm},

##   nrow/.style={decoration={brace,amplitude=10pt},xshift=-2pt,yshift=0pt},

##   ncol/.style={decoration={brace,amplitude=10pt},xshift=0pt,yshift=-2pt}

## ]

## % left image

## %				\draw[fill=gray] (0.9,0) rectangle (1,1);

## \draw [] (0,0) rectangle (1, 1);

## \draw [decorate,nrow] (0,0) -- (0,1) node [midway,xshift=-20pt] {$n$};

## \draw [decorate,ncol] (1,0) -- (0,0) node [midway,yshift=-20pt] {$p$};

## 
## % right image				

## \draw[fill=gray] (3,0) rectangle (3.1,1);

## \draw [] (2,0) rectangle (3, 1);

## \draw [decorate,nrow,xshift=+4pt] (3.1,1) -- (3.1,0) node [midway,xshift=30pt] {$n$};

## \draw [decorate,ncol] (3.1,0) -- (2,0) node [midway,yshift=-20pt] {$p + 1$};

## %				\draw[->] (0.15, 1) to [out = 90, in = 90, looseness = 0.5] (2.05,1);

## %				\draw[->] (0.45, 1) to [out = 90, in = 90, looseness = 0.5] (2.15,1);

## %				\draw[->] (0.95, 1) to [out = 90, in = 90, looseness = 0.5] (2.25,1);

## \end{tikzpicture}


## ----message=FALSE, warning=FALSE---------------------------------------------
library(lubridate)
my_presidents <- presidential %>%
  mutate(term.length = interval(start, end) / dyears(1))
my_presidents


## -----------------------------------------------------------------------------
my_presidents <- my_presidents %>% 
  mutate(elected = year(start) - 1)
my_presidents


## -----------------------------------------------------------------------------
my_presidents <- my_presidents %>%
  mutate(elected = ifelse(elected %in% c(1962, 1973), NA, elected))
my_presidents


## -----------------------------------------------------------------------------
my_presidents <- my_presidents %>% 
  rename(term_length = term.length)
my_presidents


## \begin{tikzpicture}[

##   xscale=3,yscale=2,

##   coverage/.style={circle,draw=blue!50,fill=blue!20,thick,opacity=0.5},

##   lifetime/.style={->,dashed,thick,gray},

##   sensor/.style={circle,draw=black,fill=red!50,thick,opacity=0.75,inner sep=0pt,minimum size=2mm},

##   nrow/.style={decoration={brace,amplitude=10pt},xshift=-2pt,yshift=0pt},

##   ncol/.style={decoration={brace,amplitude=10pt},xshift=0pt,yshift=-2pt}

## ]

## % left image

## \draw [decorate,nrow] (0,0) -- (0,1) node [midway,xshift=-20pt] {$n$};

## \draw [decorate,ncol] (1,0) -- (0,0) node [midway,yshift=-20pt] {$p$};

## \shade[shading=axis,bottom color=black!60,top color=black!70] (0.7,0.9) rectangle (0.8,1);

## \shade[shading=axis,bottom color=black!10,top color=black!20] (0.7,0.8) rectangle (0.8,0.9);

## \shade[shading=axis,bottom color=black!40,top color=black!50] (0.7,0.7) rectangle (0.8,0.8);

## \shade[shading=axis,bottom color=black!20,top color=black!30] (0.7,0.6) rectangle (0.8,0.7);

## \shade[shading=axis,bottom color=black!50,top color=black!60] (0.7,0.5) rectangle (0.8,0.6);

## \shade[shading=axis,bottom color=black!30,top color=black!40] (0.7,0.4) rectangle (0.8,0.5);

## \shade[shading=axis,bottom color=black!80,top color=black!90] (0.7,0.3) rectangle (0.8,0.4);

## \shade[shading=axis,bottom color=black!90,top color=black] (0.7,0.2) rectangle (0.8,0.3);

## \shade[shading=axis,bottom color=black!70,top color=black!80] (0.7,0.1) rectangle (0.8,0.2);

## \shade[shading=axis,bottom color=black!0,top color=black!10] (0.7,0) rectangle (0.8,0.1);

## \draw [] (0,0) rectangle (1, 1);

## 
## % right image

## \draw [decorate,nrow,xshift=+4pt] (3,1) -- (3,0) node [midway,xshift=30pt] {$n$};

## \draw [decorate,ncol] (3,0) -- (2,0) node [midway,yshift=-20pt] {$p$};

## \draw[->] (1, 0.95) to [out = 0, in = 180, looseness = 2] (2,0.65);

## \draw[->] (1, 0.85) to [out = 0, in = 180, looseness = 1] (2,0.15);

## \draw[->] (1, 0.75) to [out = 0, in = 180, looseness = 2] (2,0.45);

## \draw[->] (1, 0.65) to [out = 0, in = 180, looseness = 1] (2,0.25);

## \draw[->] (1, 0.55) to [out = 0, in = 180, looseness = 2] (2,0.55);

## \draw[->] (1, 0.45) to [out = 0, in = 180, looseness = 1] (2,0.35);

## \draw[->] (1, 0.35) to [out = 0, in = 180, looseness = 2] (2,0.85);

## \draw[->] (1, 0.25) to [out = 0, in = 180, looseness = 1] (2,0.95);

## \draw[->] (1, 0.15) to [out = 0, in = 180, looseness = 2] (2,0.75);

## \draw[->] (1, 0.05) to [out = 0, in = 180, looseness = 2] (2,0.05);

## \shade[shading=axis,bottom color=white,top color=black,shading angle=0]

## (2.7,0) rectangle (2.8,1);

## \draw [] (2,0) rectangle (3, 1);

## \end{tikzpicture}


## -----------------------------------------------------------------------------
my_presidents %>% 
  arrange(desc(term_length))


## -----------------------------------------------------------------------------
my_presidents %>% 
  arrange(desc(term_length), party, elected)


## \begin{tikzpicture}[

##   xscale=3,yscale=2,

##   coverage/.style={circle,draw=blue!50,fill=blue!20,thick,opacity=0.5},

##   lifetime/.style={->,dashed,thick,gray},

##   sensor/.style={circle,draw=black,fill=red!50,thick,opacity=0.75,inner sep=0pt,minimum size=2mm},

##   nrow/.style={decoration={brace,amplitude=10pt},xshift=-2pt,yshift=0pt},

##   ncol/.style={decoration={brace,amplitude=10pt},xshift=0pt,yshift=-2pt}

## ]

## % left image

## \draw[fill=gray] (0.1,0) rectangle (0.2,1);

## \draw[fill=gray] (0.4,0) rectangle (0.5,1);

## \draw[fill=gray] (0.8,0) rectangle (0.9,1);

## \draw[fill=gray] (0.9,0) rectangle (1,1);

## \draw [] (0,0) rectangle (1, 1);

## \draw [decorate,nrow] (0,0) -- (0,1) node [midway,xshift=-20pt] {$n$};

## \draw [decorate,ncol] (1,0) -- (0,0) node [midway,yshift=-20pt] {$p$};

## 
## % right image				

## \draw[fill=gray] (2,0.9) rectangle (2.1,1);

## \draw[fill=gray] (2.1,0.9) rectangle (2.2,1);

## \draw[fill=gray] (2.2,0.9) rectangle (2.3,1);

## \draw[fill=gray] (2.3,0.9) rectangle (2.4,1);

## \draw [] (2,0.9) rectangle (2.4, 1);

## \draw [] (2.4,1) -- (2.4,0.9) node [midway,xshift=10pt] {$1$};

## \draw [decorate,ncol] (2.4,0.9) -- (2,0.9) node [midway,yshift=-20pt] {$\ell \leq p$};

## \draw[->] (0.15, 1) to [out = 90, in = 90, looseness = 0.5] (2.05,1);

## \draw[->] (0.45, 1) to [out = 90, in = 90, looseness = 0.5] (2.15,1);

## \draw[->] (0.95, 1) to [out = 90, in = 90, looseness = 0.5] (2.25,1);

## \draw[->] (0.85, 1) to [out = 90, in = 90, looseness = 0.5] (2.25,1);

## \draw[->] (0.95, 1) to [out = 90, in = 90, looseness = 0.5] (2.35,1);

## \end{tikzpicture}


## -----------------------------------------------------------------------------
my_presidents %>%
  summarize(
    N = n(), 
    first_year = min(year(start)), 
    last_year = max(year(end)), 
    num_dems = sum(party == "Democratic"), 
    years = sum(term_length), 
    avg_term_length = mean(term_length)
  )


## -----------------------------------------------------------------------------
my_presidents %>% 
  group_by(party) %>% 
  summarize(
    N = n(), 
    first_year = min(year(start)), 
    last_year = max(year(end)), 
    num_dems = sum(party == "Democratic"), 
    years = sum(term_length), 
    avg_term_length = mean(term_length)
  )


## ----message=FALSE------------------------------------------------------------
library(Lahman)
dim(Teams)


## -----------------------------------------------------------------------------
mets <- Teams %>% 
  filter(teamID == "NYN")
my_mets <- mets %>% 
  filter(yearID %in% 2004:2012)
my_mets %>% 
  select(yearID, teamID, W, L)


## -----------------------------------------------------------------------------
nrow(mets)


## -----------------------------------------------------------------------------
select(filter(Teams, teamID == "NYN" & yearID %in% 2004:2012),
  yearID, teamID, W, L)


## ----eval=FALSE---------------------------------------------------------------
## Teams %>%
##   filter(teamID == "NYN" & yearID %in% 2004:2012) %>%
##   select(yearID, teamID, W, L)


## -----------------------------------------------------------------------------
mets_ben <- Teams %>% 
  select(yearID, teamID, W, L, R, RA) %>%
  filter(teamID == "NYN" & yearID %in% 2004:2012)
mets_ben


## -----------------------------------------------------------------------------
mets_ben <- mets_ben %>% 
  rename(RS = R)    # new name = old name
mets_ben


## -----------------------------------------------------------------------------
mets_ben <- mets_ben %>% 
  mutate(WPct = W / (W + L))
mets_ben


## -----------------------------------------------------------------------------
mets_ben <- mets_ben %>% 
  mutate(WPct_hat = 1 / (1 + (RA/RS)^2))
mets_ben


## -----------------------------------------------------------------------------
mets_ben <- mets_ben %>% 
  mutate(W_hat = WPct_hat * (W + L))
mets_ben


## -----------------------------------------------------------------------------
filter(mets_ben, W >= W_hat)
filter(mets_ben, W < W_hat)


## -----------------------------------------------------------------------------
arrange(mets_ben, desc(WPct))


## -----------------------------------------------------------------------------
mets_ben %>% 
  mutate(Diff = W - W_hat) %>% 
  arrange(desc(Diff))


## ----render=knitr::normal_print-----------------------------------------------
mets_ben %>%
  skim(W)


## -----------------------------------------------------------------------------
mets_ben %>% 
  summarize(
    num_years = n(), 
    total_W = sum(W), 
    total_L = sum(L), 
    total_WPct = sum(W) / sum(W + L), 
    sum_resid = sum(W - W_hat)
  )


## ----nested-ifelse------------------------------------------------------------
mets_ben <- mets_ben %>% 
  mutate(
    gm = ifelse(
      yearID == 2004, 
      "Duquette", 
      ifelse(
        yearID >= 2011, 
        "Alderson", 
        "Minaya")
    )
  )


## ----case-when----------------------------------------------------------------
mets_ben <- mets_ben %>% 
  mutate(
    gm = case_when(
      yearID == 2004 ~ "Duquette", 
      yearID >= 2011 ~ "Alderson", 
      TRUE ~ "Minaya"
    )
  )


## -----------------------------------------------------------------------------
mets_ben %>% 
  group_by(gm) %>% 
  summarize(
    num_years = n(), 
    total_W = sum(W), 
    total_L = sum(L), 
    total_WPct = sum(W) / sum(W + L), 
    sum_resid = sum(W - W_hat)
  ) %>%
  arrange(desc(sum_resid))


## -----------------------------------------------------------------------------
Teams %>%
  select(yearID, teamID, W, L, R, RA) %>%
  filter(teamID == "NYN" & yearID %in% 2004:2012) %>%
  rename(RS = R) %>% 
  mutate(
    WPct = W / (W + L), 
    WPct_hat = 1 / (1 + (RA/RS)^2), 
    W_hat = WPct_hat * (W + L), 
    gm = case_when(
      yearID == 2004 ~ "Duquette", 
      yearID >= 2011 ~ "Alderson", 
      TRUE ~ "Minaya"
    )
  ) %>%
  group_by(gm) %>%
  summarize(
    num_years = n(), 
    total_W = sum(W), 
    total_L = sum(L),
    total_WPct = sum(W) / sum(W + L), 
    sum_resid = sum(W - W_hat)
  ) %>%
  arrange(desc(sum_resid))


## -----------------------------------------------------------------------------
Teams %>%
  select(yearID, teamID, franchID, W, L, R, RA) %>%
  filter(yearID %in% 2004:2012) %>%
  rename(RS = R) %>% 
  mutate(
    WPct = W / (W + L), 
    WPct_hat = 1 / (1 + (RA/RS)^2), 
    W_hat = WPct_hat * (W + L)
  ) %>%
  group_by(franchID) %>%
  summarize(
    num_years = n(), 
    total_W = sum(W), 
    total_L = sum(L),
    total_WPct = sum(W) / sum(W + L), 
    sum_resid = sum(W - W_hat)
  ) %>%
  arrange(sum_resid) %>%
  head(6)

