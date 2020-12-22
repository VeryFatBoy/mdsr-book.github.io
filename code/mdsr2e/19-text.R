## ----message=FALSE, echo=TRUE, eval=FALSE-------------------------------------
## library(tidyverse)
## library(mdsr)
## macbeth_url <- "http://www.gutenberg.org/cache/epub/1129/pg1129.txt"
## Macbeth_raw <- RCurl::getURL(macbeth_url)


## -----------------------------------------------------------------------------
data(Macbeth_raw)


## -----------------------------------------------------------------------------
# str_split returns a list: we only want the first element
macbeth <- Macbeth_raw %>%
  str_split("\r\n") %>%
  pluck(1)
length(macbeth)


## -----------------------------------------------------------------------------
macbeth[300:310]


## -----------------------------------------------------------------------------
macbeth_lines <- macbeth %>%
  str_subset("  MACBETH")
length(macbeth_lines)
head(macbeth_lines)


## -----------------------------------------------------------------------------
macbeth %>%
  str_subset("  MACDUFF") %>%
  length()


## -----------------------------------------------------------------------------
macbeth %>%
  str_subset("  MACBETH") %>%
  length()
macbeth %>%
  str_detect("  MACBETH") %>%
  length()


## -----------------------------------------------------------------------------
pattern <- "  MACBETH"
macbeth %>%
  str_subset(pattern) %>%
  str_extract(pattern) %>% 
  head()


## -----------------------------------------------------------------------------
macbeth %>%
  str_subset("MAC.") %>%
  head()
macbeth %>%
  str_subset("MACBETH\\.") %>%
  head()


## -----------------------------------------------------------------------------
macbeth %>%
  str_subset("MAC[B-Z]") %>%
  head()


## -----------------------------------------------------------------------------
macbeth %>%
  str_subset("MAC(B|D)") %>%
  head()


## -----------------------------------------------------------------------------
macbeth %>%
  str_subset("^  MAC[B-Z]") %>%
  head()


## -----------------------------------------------------------------------------
macbeth %>%
  str_subset("^ ?MAC[B-Z]") %>%
  head()
macbeth %>%
  str_subset("^ *MAC[B-Z]") %>%
  head()
macbeth %>%
  str_subset("^ +MAC[B-Z]") %>%
  head()


## -----------------------------------------------------------------------------
macbeth_chars <- tribble(
  ~name, ~regexp,
  "Macbeth", "  MACBETH\\.",
  "Lady Macbeth", "  LADY MACBETH\\.",
  "Banquo", "  BANQUO\\.",
  "Duncan", "  DUNCAN\\.",
) %>%
  mutate(speaks = map(regexp, str_detect, string = macbeth))


## ----message=FALSE------------------------------------------------------------
speaker_freq <- macbeth_chars %>%
  unnest(cols = speaks) %>%
  mutate(
    line = rep(1:length(macbeth), 4),
    speaks = as.numeric(speaks)
  ) %>%
  filter(line > 218 & line < 3172)
glimpse(speaker_freq)


## -----------------------------------------------------------------------------
acts <- tibble(
  line = str_which(macbeth, "^ACT [I|V]+"), 
  line_text = str_subset(macbeth, "^ACT [I|V]+"),
  labels = str_extract(line_text, "^ACT [I|V]+")
)


## ----macbeth, message = FALSE, warning=FALSE, fig.cap="Speaking parts for four major characters. Duncan is killed early in the play and never speaks again. "----
ggplot(data = speaker_freq, aes(x = line, y = speaks)) + 
  geom_smooth(
    aes(color = name), method = "loess", 
    se = FALSE, span = 0.4
  ) + 
  geom_vline(
    data = acts, 
    aes(xintercept = line), 
    color = "darkgray", lty = 3
  ) + 
  geom_text(
    data = acts, 
    aes(y = 0.085, label = labels), 
    hjust = "left", color = "darkgray"
  ) + 
  ylim(c(0, NA)) + 
  xlab("Line Number") +
  ylab("Proportion of Speeches") + 
  scale_color_brewer(palette = "Set2")


## ----eval=FALSE, message=FALSE------------------------------------------------
## library(aRxiv)
## DataSciencePapers <- arxiv_search(
##   query = '"Data Science"',
##   limit = 20000,
##   batchsize = 100
## )


## ----eval=TRUE, message=FALSE-------------------------------------------------
data(DataSciencePapers)


## -----------------------------------------------------------------------------
glimpse(DataSciencePapers)


## ----eval=TRUE, message=FALSE-------------------------------------------------
library(lubridate)
DataSciencePapers <- DataSciencePapers %>%
  mutate(
    submitted = lubridate::ymd_hms(submitted), 
    updated = lubridate::ymd_hms(updated)
  )
glimpse(DataSciencePapers)


## ----warning=FALSE,message=FALSE----------------------------------------------
mosaic::tally(~ year(submitted), data = DataSciencePapers)


## -----------------------------------------------------------------------------
DataSciencePapers %>% 
  filter(id == "1809.02408v2") %>%
  glimpse()


## -----------------------------------------------------------------------------
DataSciencePapers %>%
  group_by(primary_category) %>%
  count() %>%
  head()


## -----------------------------------------------------------------------------
DataSciencePapers <- DataSciencePapers %>%
  mutate(
    field = str_extract(primary_category, "^[a-z,-]+"),
  )
mosaic::tally(x = ~field, margins = TRUE, data = DataSciencePapers) %>%
  sort()


## -----------------------------------------------------------------------------
library(tidytext)
DataSciencePapers %>%
  unnest_tokens(word, abstract) %>%
  count(id, word, sort = TRUE)


## -----------------------------------------------------------------------------
arxiv_words <- DataSciencePapers %>%
  unnest_tokens(word, abstract) %>%
  anti_join(get_stopwords(), by = "word")

arxiv_words %>%
  count(id, word, sort = TRUE)


## -----------------------------------------------------------------------------
arxiv_abstracts <- arxiv_words %>%
  group_by(id) %>%
  summarize(abstract_clean = paste(word, collapse = " "))

arxiv_papers <- DataSciencePapers %>%
  left_join(arxiv_abstracts, by = "id")


## -----------------------------------------------------------------------------
single_paper <- arxiv_papers %>%
  filter(id == "1809.02408v2")
single_paper %>%
  pull(abstract) %>%
  strwrap() %>%
  head()
single_paper %>%
  pull(abstract_clean) %>%
  strwrap() %>%
  head(4)


## ----wordcloud1, message=FALSE, warning=FALSE, fig.cap="A word cloud of terms that appear in the abstracts of arXiv papers on data science."----
library(wordcloud)
set.seed(1962)
arxiv_papers %>%
  pull(abstract_clean) %>%
  wordcloud(
    max.words = 40, 
    scale = c(8, 1), 
    colors = topo.colors(n = 30), 
    random.color = TRUE
  )


## -----------------------------------------------------------------------------
afinn <- get_sentiments("afinn")
afinn %>%
  slice_sample(n = 15) %>%
  arrange(desc(value))


## -----------------------------------------------------------------------------
arxiv_words %>% 
  inner_join(afinn, by = "word") %>%
  select(word, id, value)


## -----------------------------------------------------------------------------
arxiv_sentiments <- arxiv_words %>% 
  left_join(afinn, by = "word") %>%
  group_by(id) %>%
  summarize(
    num_words = n(),
    sentiment = sum(value, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(sentiment_per_word = sentiment / num_words) %>%
  arrange(desc(sentiment))


## ---- render=knitr::normal_print----------------------------------------------
arxiv_papers <- arxiv_papers %>%
  left_join(arxiv_sentiments, by = "id")
arxiv_papers %>%
  skim(sentiment, sentiment_per_word)


## -----------------------------------------------------------------------------
most_positive <- arxiv_papers %>%
  filter(sentiment_per_word == max(sentiment_per_word)) %>%
  pull(abstract)
strwrap(most_positive)


## ----arxiv-papers, fig.cap = "Average sum sentiment scores over time by field.", message = FALSE----
ggplot(
  arxiv_papers, 
  aes(
    x = submitted, y = sentiment_per_word, 
    color = field == "cs"
  )
) + 
  geom_smooth(se = TRUE) + 
  scale_color_brewer("Computer Science?", palette = "Set2") +
  labs(x = "Date submitted", y = "Sentiment score per word")


## -----------------------------------------------------------------------------
arxiv_bigrams <- arxiv_papers %>%
  unnest_tokens(
    arxiv_bigram, 
    abstract_clean, 
    token = "ngrams", 
    n = 2
  ) %>%
  select(arxiv_bigram, id)
arxiv_bigrams


## -----------------------------------------------------------------------------
arxiv_bigrams %>%
  count(arxiv_bigram, sort = TRUE)


## -----------------------------------------------------------------------------
arxiv_words %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head()


## -----------------------------------------------------------------------------
tidy_DTM <- arxiv_words %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n)
tidy_DTM %>%
  arrange(desc(tf)) %>%
  head()


## -----------------------------------------------------------------------------
tidy_DTM %>%
  arrange(desc(idf), desc(n)) %>%
  head()


## -----------------------------------------------------------------------------
arxiv_papers %>%
  pull(abstract) %>%
  str_subset("wildfire") %>%
  strwrap() %>%
  head()


## -----------------------------------------------------------------------------
tidy_DTM %>%
  filter(word == "implications")


## -----------------------------------------------------------------------------
tidy_DTM %>%
  filter(id == "1809.02408v2") %>%
  arrange(desc(tf_idf)) %>%
  head()


## -----------------------------------------------------------------------------
tidy_DTM %>%
  filter(word == "covid") %>%
  arrange(desc(tf_idf)) %>%
  head() %>%
  left_join(select(arxiv_papers, id, abstract), by = "id")


## -----------------------------------------------------------------------------
tidy_DTM %>%
  arrange(desc(tf_idf)) %>%
  head() %>%
  left_join(select(arxiv_papers, id, abstract), by = "id")


## -----------------------------------------------------------------------------
tm_DTM <- arxiv_words %>%
  count(id, word) %>%
  cast_dtm(id, word, n, weighting = tm::weightTfIdf)
tm_DTM


## -----------------------------------------------------------------------------
tm::findFreqTerms(tm_DTM, lowfreq = 7)


## ----wordcount-head, eval = TRUE----------------------------------------------
tm_DTM %>% 
  as.matrix() %>% 
  as_tibble() %>%
  map_dbl(sum) %>%
  sort(decreasing = TRUE) %>%
  head()


## -----------------------------------------------------------------------------
tm::findAssocs(tm_DTM, terms = "causal", corlimit = 0.35)


## ----message=FALSE------------------------------------------------------------
library(rvest)
url <- "http://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Beatles"
tables <- url %>% 
  read_html() %>% 
  html_nodes("table")
Beatles_songs <- tables %>%
  purrr::pluck(3) %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  select(song, lead_vocal_s_d)
glimpse(Beatles_songs)


## ----warning=FALSE------------------------------------------------------------
Beatles_songs <- Beatles_songs %>%
  mutate(song = str_remove_all(song, pattern = '\\"')) %>%
  rename(vocals = lead_vocal_s_d)


## -----------------------------------------------------------------------------
Beatles_songs %>%
  group_by(vocals) %>%
  count() %>%
  arrange(desc(n))


## -----------------------------------------------------------------------------
Beatles_songs %>%
  pull(vocals) %>%
  str_subset("McCartney") %>%
  length()
Beatles_songs %>%
  pull(vocals) %>%
  str_subset("Lennon") %>%
  length()


## -----------------------------------------------------------------------------
Beatles_songs %>%
  pull(vocals) %>%
  str_subset("(McCartney|Lennon)") %>%
  length()


## -----------------------------------------------------------------------------
pj_regexp <- "(McCartney|Lennon).*(McCartney|Lennon)"
Beatles_songs %>%
  pull(vocals) %>%
  str_subset(pj_regexp) %>%
  length()


## -----------------------------------------------------------------------------
Beatles_songs %>%
  filter(str_detect(vocals, pj_regexp)) %>%
  select(song, vocals) %>%
  head()


## -----------------------------------------------------------------------------
Beatles_songs %>%
  unnest_tokens(word, song) %>%
  anti_join(get_stopwords(), by = "word") %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  head()

