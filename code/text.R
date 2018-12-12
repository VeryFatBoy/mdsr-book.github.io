## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/text-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----message=FALSE, echo=TRUE, eval=FALSE--------------------------------
## library(mdsr)
## macbeth_url <- "http://www.gutenberg.org/cache/epub/1129/pg1129.txt"
## Macbeth_raw <- RCurl::getURL(macbeth_url)

## ------------------------------------------------------------------------
data(Macbeth_raw)

## ------------------------------------------------------------------------
# strsplit returns a list: we only want the first element
macbeth <- strsplit(Macbeth_raw, "\r\n")[[1]]
length(macbeth)

## ------------------------------------------------------------------------
macbeth[300:310]

## ------------------------------------------------------------------------
macbeth_lines <- grep("  MACBETH", macbeth, value = TRUE)
length(macbeth_lines)
head(macbeth_lines)

## ------------------------------------------------------------------------
length(grep("  MACDUFF", macbeth))

## ------------------------------------------------------------------------
length(grep("  MACBETH", macbeth))
length(grepl("  MACBETH", macbeth))

## ------------------------------------------------------------------------
identical(macbeth[grep("  MACBETH", macbeth)],
          macbeth[grepl("  MACBETH", macbeth)])

## ------------------------------------------------------------------------
library(stringr)
pattern <- "  MACBETH"
grep(pattern, macbeth, value = TRUE) %>% 
  str_extract(pattern) %>% 
  head()

## ------------------------------------------------------------------------
head(grep("MAC.", macbeth, value = TRUE))
head(grep("MACBETH\\.", macbeth, value = TRUE))

## ------------------------------------------------------------------------
head(grep("MAC[B-Z]", macbeth, value = TRUE))

## ------------------------------------------------------------------------
head(grep("MAC(B|D)", macbeth, value = TRUE))

## ------------------------------------------------------------------------
head(grep("^  MAC[B-Z]", macbeth, value = TRUE))

## ------------------------------------------------------------------------
head(grep("^ ?MAC[B-Z]", macbeth, value = TRUE))
head(grep("^ *MAC[B-Z]", macbeth, value = TRUE))
head(grep("^ +MAC[B-Z]", macbeth, value = TRUE))

## ------------------------------------------------------------------------
Macbeth <- grepl("  MACBETH\\.", macbeth)  
LadyMacbeth <- grepl("  LADY MACBETH\\.", macbeth)
Banquo <- grepl("  BANQUO\\.", macbeth)
Duncan <- grepl("  DUNCAN\\.", macbeth)

## ----message=FALSE-------------------------------------------------------
library(tidyr)
speaker_freq <- data.frame(Macbeth, LadyMacbeth, Banquo, Duncan) %>%
  mutate(line = 1:length(macbeth)) %>%
  gather(key = "character", value = "speak", -line) %>%
  mutate(speak = as.numeric(speak)) %>% 
  filter(line > 218 & line < 3172)
glimpse(speaker_freq)

## ------------------------------------------------------------------------
acts_idx <- grep("^ACT [I|V]+", macbeth)
acts_labels <- str_extract(macbeth[acts_idx], "^ACT [I|V]+")
acts <- data.frame(line = acts_idx, labels = acts_labels)

## ----macbeth, warning=FALSE, fig.cap="Speaking parts in \\textit{Macbeth} for four major characters. Duncan is killed early in the play and never speaks again. "----
ggplot(data = speaker_freq, aes(x = line, y = speak)) + 
  geom_smooth(aes(color = character), method = "loess", se = 0, span = 0.4) + 
  geom_vline(xintercept = acts_idx, color = "darkgray", lty = 3) + 
  geom_text(data = acts, aes(y = 0.085, label = labels), 
            hjust = "left", color = "darkgray") + 
  ylim(c(0, NA)) + xlab("Line Number") + ylab("Proportion of Speeches")

## ----eval=FALSE, message=FALSE-------------------------------------------
## library(aRxiv)
## DataSciencePapers <- arxiv_search(query = '"Data Science"', limit = 200)

## ----eval=TRUE, message=FALSE--------------------------------------------
data(DataSciencePapers)

## ------------------------------------------------------------------------
head(DataSciencePapers)

## ----eval=TRUE, message=FALSE--------------------------------------------
library(lubridate)
DataSciencePapers <- DataSciencePapers %>%
  mutate(submitted = ymd_hms(submitted), updated = ymd_hms(updated))
glimpse(DataSciencePapers)

## ----warning=FALSE,message=FALSE-----------------------------------------
tally(~ year(submitted), data = DataSciencePapers)

## ------------------------------------------------------------------------
DataSciencePapers %>% 
  filter(year(submitted) == 2007) %>% 
  glimpse()

## ------------------------------------------------------------------------
tally(~ primary_category, data = DataSciencePapers)

## ------------------------------------------------------------------------
DataSciencePapers %>%
  mutate(field = str_extract(primary_category, "^[a-z,-]+")) %>%
  tally(x = ~field) %>%
  sort()

## ----message=FALSE-------------------------------------------------------
library(tm)
Corpus <- with(DataSciencePapers, VCorpus(VectorSource(abstract)))
Corpus[[1]] %>% 
  as.character() %>% 
  strwrap()

## ------------------------------------------------------------------------
Corpus <- Corpus %>%
  tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english"))
strwrap(as.character(Corpus[[1]]))

## ----wordcloud1,message=FALSE, warning=FALSE, fig.cap="A word cloud of terms that appear in the abstracts of arXiv papers on data science."----
library(wordcloud)
wordcloud(Corpus, max.words = 30, scale = c(8, 1), 
          colors = topo.colors(n = 30), random.color = TRUE)

## ------------------------------------------------------------------------
DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))
DTM

## ------------------------------------------------------------------------
findFreqTerms(DTM, lowfreq = 0.8)

## ----wordcount-head------------------------------------------------------
DTM %>% as.matrix() %>% 
  apply(MARGIN = 2, sum) %>%
  sort(decreasing = TRUE) %>%
  head(9)

## ------------------------------------------------------------------------
findAssocs(DTM, terms = "statistics", corlimit = 0.5)
findAssocs(DTM, terms = "mathematics", corlimit = 0.5)

## ----message=FALSE-------------------------------------------------------
library(rvest)
library(tidyr)
library(methods)
url <- "http://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Beatles"
tables <- url %>% 
  read_html() %>% 
  html_nodes(css = "table")
songs <- html_table(tables[[5]])
glimpse(songs)

## ----warning=FALSE-------------------------------------------------------
songs <- songs %>%
  mutate(Title = gsub('\\"', "", Title), Year = as.numeric(Year)) %>%
  rename(songwriters = `Songwriter(s)`)

## ------------------------------------------------------------------------
tally(~songwriters, data = songs) %>%
  sort(decreasing = TRUE) %>%
  head()

## ------------------------------------------------------------------------
length(grep("McCartney", songs$songwriters))
length(grep("Lennon", songs$songwriters))

## ------------------------------------------------------------------------
length(grep("(McCartney|Lennon)", songs$songwriters))

## ------------------------------------------------------------------------
length(grep("(McCartney|Lennon).*(McCartney|Lennon)", songs$songwriters))

## ------------------------------------------------------------------------
songs %>%
  filter(grepl("(McCartney|Lennon).*(McCartney|Lennon)", songwriters)) %>%
  select(Title) %>%
  head()

## ------------------------------------------------------------------------
song_titles <- VCorpus(VectorSource(songs$Title)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  DocumentTermMatrix(control = list(weighting = weightTfIdf))
findFreqTerms(song_titles, 25)

## ----message=FALSE-------------------------------------------------------
library(twitteR)
setup_twitter_oauth(consumer_key = "u2UthjbK6YHyQSp4sPk6yjsuV", 
  consumer_secret = "sC4mjd2WME5nH1FoWeSTuSy7JCP5DHjNtTYU1X6BwQ1vPZ0j3v", 
  access_token = "1365606414-7vPfPxStYNq6kWEATQlT8HZBd4G83BBcX4VoS9T", 
  access_secret = "0hJq9KYC3eBRuZzJqSacmtJ4PNJ7tNLkGrQrVl00JHirs")

## ----eval=FALSE----------------------------------------------------------
## tweets <- searchTwitter("#datascience", lang = "en", n = 1000,
##   retryOnRateLimit = 100)
## class(tweets)
## class(tweets[[1]])

## ----echo=FALSE,eval=FALSE-----------------------------------------------
## save(tweets, file="tweets.Rda")

## ----echo=FALSE----------------------------------------------------------
load("tweets.Rda")

## ------------------------------------------------------------------------
tweet_df <- twListToDF(tweets) %>% as.tbl()
tweet_df %>% 
  select(text) %>% 
  head()

## ----nchar-tweet, message=FALSE, fig.keep="last", fig.cap="Distribution of the number of characters in a sample of tweets."----
ggplot(data = tweet_df, aes(x = nchar(text))) + 
  geom_density(size = 2) + 
  geom_vline(xintercept = 140) + 
  scale_x_continuous("Number of Characters") 

## ------------------------------------------------------------------------
tweet_df %>% 
  filter(nchar(text) > 140) %>% 
  select(text)

## ----retweetCount, message=FALSE, fig.keep="last", fig.cap="Distribution of the number of retweets in a sample of tweets."----
ggplot(data = tweet_df, aes(x = retweetCount)) + 
  geom_density(size = 2)

## ------------------------------------------------------------------------
tweet_df %>% filter(!is.na(longitude))

## ----message=FALSE-------------------------------------------------------
tweet_db <- tempfile()
register_sqlite_backend(tweet_db)
store_tweets_db(tweets)

## ------------------------------------------------------------------------
tweets_src <- src_sqlite(tweet_db)
old_tweets <- tweets_src %>% tbl("tweets")
glimpse(old_tweets)

## ----warning=FALSE-------------------------------------------------------
big_data_tweets <- old_tweets %>% 
  collect() %>% 
  filter(grepl("#bigdata", text))
nrow(big_data_tweets) / nrow(collect(old_tweets))

## ----message=FALSE-------------------------------------------------------
library(ggmap)
smith <- geocode("44 College Lane, 01063")
smith

## ----new-haven-----------------------------------------------------------
with(smith, closestTrendLocations(lat = lat, long = lon))

## ----nh-trends-----------------------------------------------------------
head(getTrends(2458410))

