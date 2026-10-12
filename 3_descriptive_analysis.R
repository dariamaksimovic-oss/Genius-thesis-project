# DESCRIPTIVE ANALYSES =========================================================
# selezione delle informazioni d'interesse
library(gt)
library(MetBrewer)

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
  geom_bar(fill = met.brewer("Hokusai3", 1), color = "white") +
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
  geom_col(fill = met.brewer("Hokusai3", 1), color = "white") +
  theme(axis.text.y = element_text(size = 8)) +
  geom_text(aes(label = n), color = "darkgrey", size = 3, fontface = "bold", hjust = -0.2) +
  labs(y = "Genere",
       title = "Numero di canzoni per genere") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)))

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
  slice_max(n, n = 20) %>%
  ggplot(mapping = aes(x = n, y = reorder(album_name, n))) +
  geom_col(fill = met.brewer("Hokusai3", 1), color = "white") +
  theme(axis.text.y = element_text(size = 8)) +
  labs(y = "Album",
       title = "Numero di canzoni per album") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)))

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
  geom_col(fill = met.brewer("Hokusai3", 1), color = "white") +
  theme(axis.text.y = element_text(size = 8)) +
  labs(y = "Artista",
       title = "Numero di canzoni per artista") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2))) +
  geom_text(aes(label = n), color = "darkgrey", size = 3, fontface = "bold", hjust = -0.2)



### Analisi Multivariate -------------------------

# Relazione tra numero di visualizzazioni e numero di annotazioni di una pagina Genius
data %>%
  filter(song_pageviews > 0) %>%
  ggplot(mapping = aes(x = song_pageviews, y = song_annotation_count, colour = decade)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(x = "# di visualizzazioni",
       y = "# di annotazioni",
       color = "Decennio",
       title = "Relazione tra visualizzazioni e annotazioni su Genius",
       subtitle = "per decennio di uscita della canzone") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11)) +
  scale_color_manual(values = met.brewer("Hokusai3", 6))

# Relazione tra anno di pubblicazione e numero di visualizzazioni
data %>%
  ggplot(mapping = aes(x = year, y = song_pageviews, colour = decade)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(x = "Anno",
       y = "# di visualizzazioni",
       color = "Decennio",
       title = "Relazione tra anno di uscita e visualizzazioni",
       subtitle = "per decennio") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11)) +
  scale_color_manual(values = met.brewer("Hokusai3", 6))

data %>%
  ggplot(mapping = aes(x = decade, y = song_pageviews)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, outlier.size = 1.5,
               fill = met.brewer("Hokusai3", 1), color = "black") +
  labs(x = "Decennio",
       y = "# di visualizzazioni",
       title = "Distribuzione delle visualizzazioni per decennio") +
  theme(plot.title = element_text(face = "bold", size = 12))

# Relazione tra anno di pubblicazione e numero di annotazioni
data %>%
  ggplot(mapping = aes(x = year, y = song_annotation_count, colour = decade)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(x = "Anno",
       y = "# di annotazioni",
       color = "Decennio",
       title = "Relazione tra anno di uscita e annotazioni",
       subtitle = "per decennio") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11)) +
  scale_color_manual(values = met.brewer("Hokusai3", 6))

data %>%
  ggplot(mapping = aes(x = decade, y = song_annotation_count)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, outlier.size = 1.5,
               fill = met.brewer("Hokusai3", 1), 
               color = "black") +
  labs(x = "Decennio",
       y = "# di annotazioni",
       title = "Distribuzione delle annotazioni per decennio") +
  theme(plot.title = element_text(face = "bold", size = 12))



## LYRICS DATASET ==============================================================
# tokenizzazione dei testi
word_count_songname <- tidy_song_name %>% count(word, sort = TRUE)
word_count_lyrics <- tidy_lyrics %>% count(word, sort = TRUE)

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
  geom_col(fill = met.brewer("Hokusai3", 1), color = "white") +
  geom_text(aes(label=n), color="darkgrey", size=3, fontface="bold", hjust=-0.2) +
  coord_flip() +
  labs(x = "n",
       y = "parola",
       title = "Frequenza assoluta delle parole nei titoli delle canzoni") +
  theme(plot.title = element_text(face = "bold", size = 12))

word_count_lyrics %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = met.brewer("Hokusai3", 1), color = "white") +
  geom_text(aes(label=n), color="darkgrey", size=3, fontface="bold", hjust=-0.2) +
  coord_flip() +
  labs(x = "n",
       y = "parola",
       title = "Frequenza assoluta delle parole nei testi delle canzoni") +
  theme(plot.title = element_text(face = "bold", size = 12))

# Wordclouds
palette_title <- colorRampPalette(met.brewer("Hokusai3"))(nrow(word_count_songname))
palette_lyrics <- colorRampPalette(met.brewer("Hokusai3"))(nrow(word_count_lyrics))

word_count_songname %>%
  arrange(desc(n)) %>%
  rename(word=word, freq=n) %>%
  wordcloud2::wordcloud2(backgroundColor = "white", color = palette_title)

word_count_lyrics %>%
  arrange(desc(n)) %>%
  rename(word=word, freq=n) %>%
  wordcloud2::wordcloud2(backgroundColor = "white", color = palette_lyrics)
