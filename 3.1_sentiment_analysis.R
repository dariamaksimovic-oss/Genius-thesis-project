### Exploratory Sentiment Analysis ---------------
library(sentimentr)
library(tidytext)
library(tidyverse)
library(dplyr)

# Word-Emotion Association Lexicon (NRC)
# cit. Mohammad, Saif M, and Peter D Turney. 2013. Crowdsourcing a Word-Emotion Association Lexicon. Computational Intelligence 29 (3): 436–65. https://doi.org/https://doi.org/10.1111/j.1467-8640.2012.00460.x.
nrc <- get_sentiments(lexicon = "nrc")
emotions <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
pn <- c("positive", "negative")

# dataset con id canzone, eventuali info e parole nel testo
full_tidy <- tidy_lyrics %>%
  filter(song_id %in% en_id$song_id) %>% # filtro le canzoni in inglese
  inner_join(data %>%
               select(c(song_id, year, decade, artist_name, genres)),
             by = "song_id")
# sentiment associato a ogni parola
lyrics_nrc <- full_tidy %>%
  left_join(nrc, by = "word", relationship = "many-to-many")


#### Profilo emotivo degli artisti ----
artist_freq <- lyrics_nrc %>%
  filter(sentiment %in% emotions) %>%
  group_by(artist_name, sentiment) %>% 
  summarise(n = n(), # frequenza assoluta per artista
            .groups = "drop_last") %>% # drop dell'ultimo raggruppamento (per sentiment)
  # calculate percentage of emotive words  
  mutate(perc = round(n / sum(n) * 100, 2)) %>%
  ungroup()

# artisti con almeno 4 canzoni in classifica
top_artists <- data %>%
  count(artist_name, sort = TRUE) %>%
  filter(n >= 4)

# heatmap
artist_freq %>%
  filter(artist_name %in% top_artists$artist_name) %>%
  ggplot(aes(x = sentiment, y = artist_name, fill = perc)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "D",
                       direction = 1,
                       labels = scales::percent_format(accuracy = 1, scale = 1)) +
  labs(x = "Emozione",
       y = "Artista",
       fill = "Frequenza relativa (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8))

#### Profilo emotivo dei generi ----
top_genres <- data %>% unnest(genres) %>%
  count(genres, sort = TRUE) %>%
  filter(n >= 15)

genres_freq <- lyrics_nrc %>%
  select(c(genres, sentiment)) %>%
  unnest(genres) %>%
  filter(sentiment %in% emotions) %>%
  group_by(genres, sentiment) %>%
  summarise(n = n(),
            .groups = "drop_last") %>%
  mutate(perc = round(n / sum(n) * 100, 2)) %>%
  ungroup()

genres_freq %>%
  filter(genres %in% top_genres$genres) %>%
  ggplot(aes(x = sentiment, y = genres, fill = perc)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "D",
                       direction = 1,
                       labels = scales::percent_format(accuracy = 1, scale = 1)) +
  labs(x = "Emozione",
       y = "Genere",
       fill = "Frequenza relativa (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8))

#### Termini negativi e positivi più frequenti ----
pn_count <- lyrics_nrc %>%
  filter(sentiment %in% pn) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

pn_count %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  facet_wrap(~ sentiment, scales = "free_y") +
  geom_col(show.legend = F, alpha = 0.8, color = "white") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL,
       y = "Frequenza assoluta") +
  geom_text(aes(label = n), color = "darkgrey", size = 3, fontface = "bold", hjust = -0.2) +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9)

#### Emozioni più frequenti per decennio ----
decade_freq <- lyrics_nrc %>%
  filter(sentiment %in% emotions) %>%
  group_by(decade, sentiment) %>% 
  summarise(n = n(), # frequenza assoluta per decade
            .groups = "drop_last") %>% # drop dell'ultimo raggruppamento (per sentiment)
  # calculate percentage of emotive words  
  mutate(perc = round(n / sum(n) * 100, 2)) %>%
  ungroup()

top_decade_sent <- decade_freq %>%
  group_by(decade) %>%
  arrange(decade, desc(perc))

top_decade_sent %>%
  ggplot(aes(x = reorder_within(sentiment, perc, decade), y = perc, fill = sentiment)) +
  geom_col(show.legend = F, color = "white") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 33), expand = c(0, 0)) +
  facet_wrap(~ decade, scales = "free_y") +
  scale_x_reordered() +
  labs(x = "Emozione",
       y = "Frequenza relativa (%)") +
  geom_text(aes(label = perc), color = "darkgrey", size = 3, fontface = "bold", hjust = -0.2) +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9)

#### Sviluppo del sentiment nel corso degli anni ----
year_freq <- lyrics_nrc %>%
  filter(sentiment %in% emotions) %>%
  group_by(year, sentiment) %>% 
  summarise(n = n(), # frequenza assoluta per decade
            .groups = "drop_last") %>% # drop dell'ultimo raggruppamento (per sentiment)
  # calculate percentage of emotive words  
  mutate(perc = round(n / sum(n) * 100, 2)) %>%
  ungroup()

year_freq %>%
  ggplot(aes(x = year, y = perc, color = sentiment)) +
  geom_line(linewidth = 0.75, show.legend = F,
            color = viridis(1, option = "D", begin = 0.3)) +
  facet_wrap( ~ sentiment, ncol = 4) +
  geom_smooth(se = T, color = "firebrick", linetype = "dotted", linewidth = 0.9,
              fill = "lightblue") +
  labs(x = "Anno",
       y = "Frequenza relativa (%)",
       fill = "Emozione") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#### Polarity Ratio (rapporto Positivi / Negativi)----
decade_polarity <- lyrics_nrc %>%
  # keep only positive/negative tags  
  filter(sentiment %in% pn) %>%
  group_by(decade, sentiment) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(polarity = positive / negative) # semplice ratio tra positivi e negativi

year_polarity <- lyrics_nrc %>%
  # keep only positive/negative tags  
  filter(sentiment %in% pn) %>%
  group_by(year, sentiment) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(polarity = positive / negative) # semplice ratio tra positivi e negativi

year_polarity %>%
  ggplot(aes(x = year, y = polarity)) +
  geom_line(linewidth = 0.75, color = viridis(1, option = "D", begin = 0.3)) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "firebrick") +
  labs(x = "Anno",
       y = "Ratio (Positivi / Negativi)")

# Polarity = 1: Equal positive and negative words (neutral)
# Polarity > 1: More positive than negative (optimistic tone)
# Polarity < 1: More negative than positive (pessimistic tone)


#### Polarità media (andamento lisciato) -----
trajectory <- lyrics_nrc %>%
  # keep only polarity tags  
  filter(is.na(sentiment) | sentiment %in% pn) %>%
  # convert to numeric: positive = +1, negative = −1, neutral = 0  
  mutate(sentiment = as.character(sentiment),
         sentiment = case_when(sentiment == "positive" ~ 1,
                               sentiment == "negative" ~ -1,
                               TRUE ~ 0)) %>%
  group_by(year) %>%  
  # calculate mean polarity per bin  
  summarize(polarity = mean(sentiment),
            .groups = "drop")

trajectory %>%
  ggplot(aes(x = year, y = polarity)) +
  geom_line(alpha = 0.3, color = "gray50") +
  geom_smooth(se = T, color = viridis(1, option = "D", begin = 0.3),
              fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "firebrick") +
  labs(x = "Anno",
       y = "Polarità media")




# Context-sensitive Sentiment Analysis https://ladal.edu.au/tutorials/sentiment/sentiment.html#apply_context
# takes into account negations, doubt prepositions, intensity of words (very, extremely ecc)
# For general reference:
# https://saifmohammad.com/WebDocs/NRCemotionlexicon.pdf
# https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
# https://ladal.edu.au/tutorials/sentiment/sentiment.html



