
require(tm)
require(plyr)
require(tidyverse)
require(tidytext)
require(stringr)
require(lubridate)
require(ggplot2)
require(stopwords)
require(SnowballC)
require(wordcloud)
require(wordcloud2)
require(RColorBrewer)
require(wesanderson)
require(syuzhet)
require(topicmodels)
require(ggjoy)

setwd('...')

load('articles.RData')

## Czyszczenie danych ----------------------------------------------------------

articles <- articles %>%
  filter(date >= as.Date("2020-04-01")  & date <= as.Date("2021-12-31"))
  
articles <- articles %>%
  mutate(lead = tolower(lead)) %>%
  mutate(lead = removeNumbers(lead)) %>%
  mutate(lead = removePunctuation(lead)) %>%
  mutate(lead = lapply(lead, function(x) {
    gsub("’s|”|“|‘|’", "", x)
  })) %>% 
  mutate(lead = lapply(lead, function(x) {
    gsub("—", " ", x)
  })) %>% 
  mutate(lead = lapply(lead, function(x) {
    gsub("s’", "s", x)
  })) %>%
  mutate(lead = unlist(lead))

articles <- articles %>%
  mutate(body = tolower(body)) %>%
  mutate(body = removeNumbers(body)) %>%
  mutate(body = removePunctuation(body)) %>%
  mutate(body = lapply(body, function(x) {
    gsub("’s|”|“|‘|’", "", x)
    })) %>% 
  mutate(body = lapply(body, function(x) {
    gsub("—", " ", x)
    })) %>% 
  mutate(body = lapply(body, function(x) {
    gsub("s’", "s", x)
    })) %>%
  mutate(body = unlist(body))

articles <- articles %>%
  mutate(day = day(date),
         month = month(date),
         monthf = factor(month(date),
                         levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                         labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", 
                                    "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")),
         week = week(date),
         year = year(date),
         weekday = factor(wday(date),
                          levels = c(2,3,4,5,6,7,1),
                          labels = c("pn", "wt", "śr", "cz", "pt", "sb", "nd")))

articles <- ddply(articles, .(month), transform, monthweek = 1 + week - min(week))

## 1. Timeseries dla ogółu danych ----------------------------------------------

articles %>%
  count(year, month) %>%
  ggplot() +
  geom_col(aes(x = make_date(year, month, 1), y = n), 
           fill = '#FD6467', color = "gray50") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, hjust=1, vjust=1),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(make_date(year, month, 1), n, label = n), vjust = -0.5) +
  labs(subtitle = 'W okresie od kwietnia 2020 do grudnia 2021',
       title = 'Wielkość publikacji na stronie www.science.org',
       x = '',
       y = '',
       caption = 'W oparciu o dane pobrane z: www.science.org')


articles %>%
  count(year, monthf, monthweek, weekday) %>%
  filter(!is.na(year)) %>%
  ggplot(aes(monthweek, weekday, fill = n)) + 
  geom_tile(color = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="#66FF33", high="#FD6467") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_discrete(limits=rev) +
  labs(x=NULL,
       y="",
       title = "Częstotliwość publikacji na stronie www.science.org", 
       subtitle="W latach 2020 i 2021", 
       fill="Ilość artykułów",
       caption = 'W oparciu o dane pobrane z: www.science.org')

##   2. Top 10 słów dla lead ---------------------------------------------------

articles %>%
  unnest_tokens(word, lead, token = "words") %>%
  anti_join(get_stopwords(source = 'snowball')) %>%
  filter(nchar(word)>= 4) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  arrange(n) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(15, n) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = '#FD6467', color = "gray50") +
  geom_text(aes(label = n), nudge_y = -8, color = 'gray20') +
  labs(title = 'Top 15 słów w nagłówkach',
       x = 'Termin',
       y = NULL,
       caption = 'W oparciu o dane pobrane z: www.science.org') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()

##   3. Wordcloud dla słów -----------------------------------------------------

lead_words <- articles %>%
  unnest_tokens(word, lead, token = "words") %>%
  anti_join(get_stopwords(source = 'snowball')) %>%
  filter(nchar(word)>= 4) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort = TRUE)

wordcloud(lead_words$word, lead_words$n, min.freq = 4,
          max.words = 50, random.order = FALSE, scale=c(2.0,0.65), rot.per = 0.25, 
          colors = wes_palette("GrandBudapest1", 3))

wordcloud2(lead_words, size = 0.5, minRotation = -pi/6, maxRotation = -pi/6, 
           rotateRatio=1, color = 'random-light')

##   4. Top 10 słów dla body ---------------------------------------------------

articles %>%
  unnest_tokens(word, body, token = "words") %>%
  anti_join(get_stopwords(source = 'snowball')) %>%
  filter(nchar(word)>= 4) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  arrange(n) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(15, n) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = '#FD6467', color = "gray50") +
  geom_text(aes(label = n), nudge_y = -500, color = 'gray20') +
  labs(title = 'Top 15 słów w artykułach',
       x = 'Termin',
       y = NULL,
       caption = 'W oparciu o dane pobrane z: www.science.org') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()

# bez słów "say' oraz "research"

articles %>%
  unnest_tokens(word, body, token = "words") %>%
  anti_join(get_stopwords(source = 'snowball')) %>%
  filter(nchar(word)>= 4) %>%
  mutate(word = wordStem(word)) %>%
  filter(!word %in% c("sai", "research", "also", "like", "mani")) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  arrange(n) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(15, n) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = '#FD6467', color = "gray50") +
  geom_text(aes(label = n), nudge_y = -200, color = 'gray20') +
  labs(title = 'Top 15 słów w artykułach',
       x = 'Termin',
       y = NULL,
       caption = 'W oparciu o dane pobrane z: www.science.org') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()


body_words <- articles %>%
  unnest_tokens(word, body, token = "words") %>%
  anti_join(get_stopwords(source = 'snowball')) %>%
  filter(nchar(word)>= 4) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort = TRUE)


wordcloud(body_words$word, body_words$n, min.freq = 4,
          max.words = 50, random.order = FALSE, scale=c(2.0,0.65), rot.per = 0.25, 
          colors = wes_palette("GrandBudapest1", 3))

wordcloud2(body_words, size = 0.5, minRotation = -pi/6, maxRotation = -pi/6, 
           rotateRatio=1, color = 'random-light')


body_words2 <- body_words %>%
  filter(!word %in% c("sai", "research", "also", "like", "mani"))

wordcloud(body_words2$word, body_words2$n, min.freq = 4,
          max.words = 50, random.order = FALSE, scale=c(2.0,0.65), rot.per = 0.25, 
          colors = wes_palette("GrandBudapest1", 3))

wordcloud2(body_words2, size = 0.5, minRotation = -pi/6, maxRotation = -pi/6, 
           rotateRatio=1, color = 'random-light')

##   5. LDA analiza tematu (dla chętnych) --------------------------------------

body_dtm <- data_frame(url = articles$url, body = articles$body) 
  
body_dtm <- body_dtm %>%
  unnest_tokens(word, body, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(nchar(word)>= 4) %>%
  mutate(word = wordStem(word)) %>%
  filter(!word %in% c('sai', 'research', 'univers', 'scienc', 
                      'year', 'studi', 'also', 'time', 'like'))

body_dtm <- body_dtm %>%
  count(url, word) %>%
  ungroup() %>%
  cast_dtm(url, word, n)

body_dtm <- tm::removeSparseTerms(body_dtm, 0.95)
body_dtm

body_lda <- LDA(body_dtm, k = 2, control = list(seed = 1234))

body_topics <- tidy(body_lda, matrix = "beta")

body_top_terms <- body_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

body_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = 'Najpopularniejsze słowa pośród dwóch tematów',
       caption = 'W oparciu o dane pobrane z: www.science.org',
       y = "Terminy") +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


beta_wide <- body_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>%
  group_by(log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, log_ratio), log_ratio, fill = log_ratio > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Terminy",
       y = "log2 ratio of beta in topic 2 /topic 1",
       title = "Najbardziej zróżnicowane słowa w temacie 1. i 2.",
       subtitle = "",
       caption = 'W oparciu o dane pobrane z: www.science.org') +
  scale_fill_brewer(palette = "Set1")

##   6.  Analiza sentymentu (emocji) dla body ----------------------------------

tokens <- data_frame(url = articles$url, 
                     year = articles$year, 
                     month = articles$month, 
                     day = articles$day, 
                     body = articles$body)

tokens <- tokens %>%
  unnest_tokens(word, body)

tokens_bing <- tokens %>%
  inner_join(get_sentiments("bing")) %>% 
  count(year, month, day, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  left_join(tokens %>% select(url, year, month, day)) %>%
  distinct() 

tokens_bing %>%
  ggplot(aes(x = make_date(year, month, day), y = sentiment)) + 
  geom_point(aes(color = sentiment)) +
  geom_smooth(aes(make_date(year, month, 1), sentiment), method = "auto",
              show.legend = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Data",
       y = "negatywny / pozytywny",
       title = "Analiza sentymentu w czasie",
       subtitle = "",
       caption = 'W oparciu o dane pobrane z: www.science.org') +
  scale_color_gradient2(low = "Blue", mid = "gray70", high = "Red") 
  

tokens_nrc <- tokens %>%
  left_join(get_sentiments("nrc"))

tokens_nrc %>%
  count(year, month, sentiment) %>%
  ungroup() %>%
  group_by(year, month) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(!sentiment %in% c("anticipation", "positive", 
                           "negative", "surprise", "trust")) %>%
  filter(!is.na(sentiment)) %>%
  ggplot() +
  geom_line(aes(make_date(year, month), p, color = sentiment), size = 1.6, show.legend = TRUE) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Rozkład emocji na przestrzeni lat",
       x = "",
       y = "Emocje",
       caption = 'W oparciu o dane pobrane z: www.science.org')

tokens_nrc %>%
  filter(sentiment %in% c("anger", "fear", "disgust", "joy", "sadness")) %>%
  ggplot() +
  geom_joy(aes(x = make_date(year, month, day), y = sentiment, fill = sentiment),
           rel_min_height = 0.01, alpha = 0.7, scale = 3) +
  theme_joy() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Rozkład emocji na przestrzeni lat",
       x = "",
       y = "Emocje",
       caption = 'W oparciu o dane pobrane z: www.science.org') + 
  scale_fill_discrete(guide="none")
