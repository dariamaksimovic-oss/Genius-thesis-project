# DATA PREPROCESSING ===========================================================
library(tidyverse)
library(tm)
library(stopwords)
library(tidytext)

text_preprocessing<- function(x) {
  gsub('http\\S+\\s*','',x) # rimozione di URLs
  gsub('#\\S+','',x) # rimozione di hashtags
  gsub('[[:cntrl:]]','',x) # rimozione di caratteri di controllo ASCII e di caratteri speciali
  gsub("^[[:space:]]*","",x) # rimozione di whitespaces a inizio stringa
  gsub("[[:space:]]*$","",x) # rimozione di whitespaces a fine stringa
  gsub(' +', ' ', x) # rimozione di whitespaces in eccesso
}

data <- read.csv("complete_metadata.csv", sep = ";", row.names = 1)
lyrics <- read.csv("clean_lyrics.csv", sep = ";", row.names = 1)

## META DATASET ================================================================

### Colonna genres -------------------------------
str(data$genres) # dato di tipo stringa
# rimozione degli spazi in eccesso nella colonna genre e trasformazione in lista
data <- data %>%
  mutate(genres = str_split(str_trim(genres, side = "right"), ",\\s*"))
class(data$genres)
head(data$genres)
sum(is.na(data$genres))
# sembrerebbe tutto ok

### DTM titoli -----------------------------------
data$song_name <- data$song_name %>%
  tolower() %>% # trasformazione in lowercase
  removeWords(words = stopwords(language="en", source="smart")) %>%  # rimozione di stopwords
  text_preprocessing() %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  stripWhitespace()

tidy_song_name <- data[, 4:5] %>% unnest_tokens(word, song_name)

write.csv2(tidy_song_name, "tidy_song_name.csv")

## LYRICS DATASET ==============================================================
### Document Term Matrix -------------------------
lyrics$lyrics_clean <- lyrics$lyrics_clean %>%
  tolower() %>% # trasformazione in lowercase
  removeWords(words = stopwords(language="en", source="smart")) %>%  # rimozione di stopwords
  text_preprocessing() %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  stripWhitespace()

tidy_lyrics <- lyrics[, c(1, 4)] %>% unnest_tokens(word, lyrics_clean)

# lemmatization
library(textstem)
tidy_lyrics <- tidy_lyrics %>%
  mutate(word = lemmatize_words(as.vector(tidy_lyrics$word)))

write.csv2(tidy_lyrics, "tidy_lyrics.csv")





