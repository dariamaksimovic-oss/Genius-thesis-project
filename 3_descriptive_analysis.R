# DESCRIPTIVE ANALYSES =========================================================
# selezione delle informazioni d'interesse
library(gt)
library(viridis)

## META DATASET ================================================================
# Struttura del primo dataset
str(data)

### Analisi Univariate ---------------------------

# informazioni assenti
c("year" = sum(is.na(data$year)),
  "genres" = sum(is.na(data$genres)),
  "song_id" = sum(is.na(data$song_id)),
  "song_name" = sum(is.na(data$song_name)),
  "song_lyrics_url" = sum(is.na(data$song_lyrics_url)),
  "song_art_image_url" = sum(is.na(data$song_art_image_url)),
  "song_pageviews" = sum(is.na(data$song_pageviews)),
  "song_annotation_count" = sum(is.na(data$song_annotation_count)),
  "artist_id" = sum(is.na(data$artist_id)),
  "artist_name" = sum(is.na(data$artist_name)),
  "artist_url" = sum(is.na(data$artist_url)),
  "album_id" = sum(is.na(data$album_id)),
  "album_name" = sum(is.na(data$album_name)),
  "album_url" = sum(is.na(data$album_url)))

# year
data <- data %>%
  mutate(decade = rep(c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s"), each = 100), .after = year)

data %>%
  ggplot(mapping = aes(year)) +
  geom_bar(fill = viridis(1, option = "D", begin = 0.4), color = "white") +
  facet_wrap(vars(decade), scales = "free_x") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 3)) +
  labs(x = "Anno",
       y = "# di canzoni",
       title = "Numero di canzoni per anno",
       subtitle = "per decennio di uscita della canzone") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11))

# genres
data %>%
  # selezione delle informazioni d'interesse
  unnest(genres) %>% 
  count(genres, sort = TRUE) %>% 
  filter(n > 9) %>% 
  mutate(genres = str_to_title(genres),
         p = round(n*100/350, 3)) %>%
  # costruzione della tabella di frequenza
  gt() %>%
  tab_header(title = md("Popolarità dei Generi Musicali"), 
             subtitle = "Generi con almeno 10 occorrenze") %>%
  cols_label(genres = md("Genere musicale"),
             n = md("Frequenza assoluta"),
             p = md("Frequenza percentuale (%)")) %>%
  opt_stylize(style = 6, color = "gray") %>%
  tab_style(style = cell_text(color = "white",
                              size = px(14)),
            locations = cells_column_labels()) %>%
  tab_options(table.width = pct(90),
              data_row.padding = px(5))

data %>%
  unnest(genres) %>%
  count(genres, sort = TRUE) %>%
  slice_max(n, n = 30) %>%
  ggplot(mapping = aes(x = n, y = fct_reorder(str_to_title(genres), n))) +
  geom_col(fill = viridis(1, option = "D", begin = 0.4), color = "white") +
  theme(axis.text.y = element_text(size = 8)) +
  geom_text(aes(label = n), color = "darkgrey", size = 3, fontface = "bold", hjust = -0.2) +
  labs(y = "Genere")

# album
data %>%
  filter(is.na(album_id) == FALSE) %>%
  count(album_name, artist_name, sort = TRUE) %>%
  filter(n > 1) %>% 
  mutate(p = round(n*100/350, 3)) %>%
  # costruzione della tabella di frequenza
  gt() %>%
  tab_header(title = md("Popolarità degli Album"),
             subtitle = "Album con più di una occorrenza") %>%
  cols_label(album_name = md("Album"),
             artist_name = md("Artista"),
             n = md("Frequenza assoluta"),
             p = md("Frequenza percentuale (%)")) %>%
  opt_stylize(style = 6,
              color = "gray") %>% 
  tab_style(style = cell_text(color = "white", 
                              size = px(14)), 
            locations = cells_column_labels()) %>% 
  tab_options(table.width = pct(90), 
              data_row.padding = px(5))

data %>%
  filter(is.na(album_name) == FALSE) %>%
  count(album_name, sort = TRUE) %>%
  slice_max(n, n = 25, with_ties = FALSE) %>%
  ggplot(mapping = aes(x = n, y = reorder(album_name, n))) +
  geom_col(fill = viridis(1, option = "D", begin = 0.4), color = "white") +
  theme(axis.text.y = element_text(size = 8)) +
  labs(y = "Album")

# artist
data %>% 
  # selezione delle informazioni d'interesse
  count(artist_name, sort = TRUE) %>% 
  filter(n > 1) %>% 
  mutate(p = round(n*100/350, 3)) %>% 
  # costruzione della tabella di frequenza
  gt() %>% 
  tab_header(title = md("Popolarità degli Artisti"), 
             subtitle = "Artisti con più di una occorrenza") %>% 
  cols_label(artist_name = md("Artista"), 
             n = md("Frequenza assoluta"), 
             p = md("Frequenza percentuale (%)")) %>%
  opt_stylize(style = 6,
              color = "gray") %>%
  tab_style(style = cell_text(color = "white",
                              size = px(14)),
            locations = cells_column_labels()) %>%
  tab_options(table.width = pct(90),
              data_row.padding = px(5))

data %>%
  count(artist_name, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(mapping = aes(x = n, y = reorder(artist_name, n))) +
  geom_col(fill = viridis(1, option = "D", begin = 0.4), color = "white") +
  theme(axis.text.y = element_text(size = 8)) +
  labs(y = "Artista") +
  geom_text(aes(label = n), color = "darkgrey", size = 3, fontface = "bold", hjust = -0.2)



### Analisi Multivariate -------------------------

# Relazione tra numero di visualizzazioni e numero di annotazioni di una pagina Genius
data %>%
  filter(song_pageviews > 0) %>%
  ggplot(mapping = aes(x = song_pageviews, y = song_annotation_count, colour = decade)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(x = "# di visualizzazioni",
       y = "# di annotazioni",
       color = "Decennio") +
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9)

# Relazione tra anno di pubblicazione e numero di visualizzazioni
data %>%
  ggplot(mapping = aes(x = year, y = song_pageviews, colour = decade)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(x = "Anno",
       y = "# di visualizzazioni",
       color = "Decennio") +
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9)

data %>%
  ggplot(mapping = aes(x = decade, y = song_pageviews)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, outlier.size = 1.5,
               fill = viridis(1, option = "D", begin = 0.1), color = "black") +
  labs(x = "Decennio",
       y = "# di visualizzazioni")

# Relazione tra anno di pubblicazione e numero di annotazioni
data %>%
  ggplot(mapping = aes(x = year, y = song_annotation_count, colour = decade)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(x = "Anno",
       y = "# di annotazioni",
       color = "Decennio") +
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9)

data %>%
  ggplot(mapping = aes(x = decade, y = song_annotation_count)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, outlier.size = 1.5,
               fill = viridis(1, option = "D", begin = 0.1),
               color = "black") +
  labs(x = "Decennio",
       y = "# di annotazioni")



## LYRICS DATASET ==============================================================
# tokenizzazione dei testi
word_count_songname <- tidy_song_name %>% count(word, sort = TRUE)
word_count_lyrics <- tidy_lyrics %>% count(word, sort = TRUE)

# Canzoni strumentali
lyrics %>%
  count(is_instrumental, sort = TRUE)

# Lingua del brano
lyrics$lang[lyrics$is_instrumental == TRUE] <- "instr"
lyrics %>%
  count(lang, sort = TRUE)
# Canzoni esclusivamente in inglese
en_id <- lyrics %>%
  filter(lang == "en") %>%
  select(song_id)

# Tabelle di frequenza delle parole nei titoli e nei testi delle canzoni
word_count_songname %>%
  filter(n > 3) %>%
  gt() %>%
  tab_header(title = md("**Frequenza dei termini nei titoli delle canzoni**"),
             subtitle = "con più di 3 occorrenze") %>%
  cols_label(word = md("**Termine**"),
             n = md("**Frequenza assoluta**")) %>%
  opt_stylize(style = 6, color = "gray") %>%
  tab_style(style = cell_text(color = "white",
                              size = px(14),
  ),
  locations = cells_column_labels()) %>%
  tab_options(table.width = pct(90),
              data_row.padding = px(5))

word_count_lyrics %>%
  filter(n > 150) %>%
  gt() %>%
  tab_header(title = md("**Frequenza dei termini nei testi delle canzoni**"),
             subtitle = "con più di 150 occorrenze") %>%
  cols_label(word = md("**Termine**"),
             n = md("**Frequenza assoluta**")) %>%
  opt_stylize(style = 6, color = "gray") %>%
  tab_style(style = cell_text(color = "white",
                              size = px(14),
  ),
  locations = cells_column_labels()) %>%
  tab_options(table.width = pct(90),
              data_row.padding = px(5))

# Barplot del numero di parole nei titoli e nei testi delle canzoni
word_count_songname %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = viridis(1, option = "D", begin = 0.4), color = "white") +
  geom_text(aes(label=n), color="darkgrey", size=3, fontface="bold", hjust=-0.2) +
  coord_flip() +
  labs(x = "Parola",
       y = "n")

word_count_lyrics %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = viridis(1, option = "D", begin = 0.4), color = "white") +
  geom_text(aes(label=n), color="darkgrey", size=3, fontface="bold", hjust=-0.2) +
  coord_flip() +
  labs(x = "Parola",
       y = "n")

# Wordclouds
palette_title <- viridis(nrow(word_count_songname), option = "D", begin = 0.1, end = 0.9)
palette_lyrics <- viridis(nrow(word_count_lyrics), option = "D", begin = 0.1, end = 0.9)

word_count_songname %>%
  filter(word != "ft") %>%
  arrange(desc(n)) %>%
  rename(word=word, freq=n) %>%
  wordcloud2::wordcloud2(backgroundColor = "white", color = palette_title)

word_count_lyrics %>%
  arrange(desc(n)) %>%
  rename(word=word, freq=n) %>%
  wordcloud2::wordcloud2(backgroundColor = "white", color = palette_lyrics)
