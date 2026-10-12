# TOPIC MODELING ===============================================================

library(topicmodels)

# frequenze delle parole per anno
year_word_freq <- tidy_lyrics %>%
  inner_join(data %>%
               select(c(song_id, year))) %>% # associo ad ogni canzone l'anno di produzione
  count(year, word) %>% # conteggio delle parole per anno
  filter(n >= 5) # rimozione delle parole poco frequenti


# frequenze delle parole per decennio
decade_word_freq <- tidy_lyrics %>%
  inner_join(data %>%
               select(c(song_id, decade))) %>% # associo ad ogni canzone l'anno di produzione
  count(decade, word) %>% # conteggio delle parole per anno
  filter(n >= 5) # rimozione delle parole poco frequenti

# lista termini inutili
useless_words <- c("yes", "hey", "ah", "eh", "oh", "uh", "je", "ha", "hu", "la",
                   "le", "mwahahaha", "ahah", "ahahah", "haha", "haah",
                   "shanananananananananananaknees", "duhha", "chachachacha", 
                   "chachachachacha", "hahahahaha", "hahahahahahahaha",
                   "hahahahahahahahahahahaha", "shaba", "shabadaba", "uhhuh", 
                   "uhuh", "huh", "uhhuh", "nana", "nanana", "nananana", 
                   "nanananana", "nananananana", "nanananananana", "lalalaah", 
                   "tralala", "tralalala", "shalala", "lala", "lalala", "lalalala",
                   "lalalalala", "lalalalalaah", "mm", "mmm", "mmmm", "hmmmm", 
                   "umm", "ummmm", "ummmm", "mmmmmmmmm", 
                   "hmmhmmhmmhmmhmmhmmhmmhmmhmmhmmhmmhmm ", "badada", "dada", 
                   "dabadaba", "dadada", "dadadadada", "dadadada", "woah", 
                   "woahwoah", "whop", "whoo", "whoa", "woo", "shoobydoowopbabaa", 
                   "woohoo", "woooo", "waah", "wee", "sho", "shi", "yeahyo",
                   "hah", "heh", "hih", "hoh", "yeahyeah", "nah", "geah", "uhyeah",
                   "gegegeyeah", "aah", "haa", "ahh", "geyeah", "yeyeah", "heehee",
                   "heeheehee", "oohooh", "oohoohooh", "oohoohoohyeah", 
                   "woohoohoohoohoo", "oohoohoohooh", "oohooh", "yo", "ayo",
                   "ooh", "doodoo", "ayy")

# rimozione
dim(year_word_freq) # 3035 x 3
year_word_freq <- year_word_freq %>%
  filter(!word %in% useless_words) %>%
  filter(!str_detect(word, "^(ha|ah|oh|la|na|da|ba|ye|wo){2,}$"))
dim(year_word_freq) # 2783 x 3

dim(decade_word_freq) # 2867 x 3
decade_word_freq <- decade_word_freq %>%
  filter(!word %in% useless_words) %>%
  filter(!str_detect(word, "^(ha|ah|oh|la|na|da|ba|ye|wo){2,}$"))
dim(decade_word_freq) # 2736 x 3

# canzoni che compaiono in più di 30 anni di storia musicale
year_word_freq %>%
  group_by(word) %>%
  summarise(tot = sum(n),
            tot_years = n_distinct(year)) %>%
  filter(tot_years > 30) %>%
  arrange(desc(tot))

ambiguous_words <- year_word_freq %>%
  group_by(word) %>%
  summarise(tot = sum(n),
            tot_years = n_distinct(year)) %>%
  filter(tot_years > 30) %>%
  arrange(desc(tot)) %>%
  select(word) %>%
  as.list()

ambiguous_words$word <- c(ambiguous_words$word, "wanna", "gonna", "gotta", "thing",
                          "good", "give", "back", "sha", "doo", "da", "talkin", 
                          "fahr", "bout")

year_word_freq <- year_word_freq %>%
  filter(!word %in% ambiguous_words$word)
decade_word_freq <- decade_word_freq %>%
  filter(!word %in% ambiguous_words$word)

c(dim(year_word_freq), dim(decade_word_freq)) # 2344 e 2642 righe

# Document Term Matrix (anno come documento)
year_dtm <- year_word_freq %>% 
  cast_dtm(year, word, n)

# Document Term Matrix (anno come documento)
decade_dtm <- decade_word_freq %>% 
  cast_dtm(decade, word, n)


## LATENT DIRICHLET ALLOCATION =================================================
library(topicmodels)
# per la riproducibilità
set.seed(12345)

devtools::install_github("nikita-moor/ldatuning") # non più disponibile nel CRAN

### Years as documents ---------------------------

# numero ottimale di topics
year_fit <- ldatuning::FindTopicsNumber(
  year_dtm,
  topics  = seq(from = 3, to = 10, by = 1), # topic K = 3, ..., 10
  metrics = c("CaoJuan2009", "Deveaud2014"), # metriche di valutazione
  method  = "Gibbs", # algoritmo di Gibbs
  control = list(seed = 123)) # per la riproducibilità del modello

ldatuning::FindTopicsNumber_plot(year_fit)

# CaoJuan2009: LOWER values indicate a better model (seek a minimum)
# Deveaud2014: HIGHER values indicate a better model (seek a maximum)

# 6 topic
year_lda_6 <- LDA(year_dtm, k = 6, method = "Gibbs",
                  control = list(seed = 12345, iter = 2000, burnin = 500))

# 7 topic
year_lda_7 <- LDA(year_dtm, k = 7, method = "Gibbs",
                  control = list(seed = 12345, iter = 2000, burnin = 500))

# 8 topic
year_lda_8 <- LDA(year_dtm, k = 8, method = "Gibbs",
                  control = list(seed = 12345, iter = 2000, burnin = 500))

# Valore stimato di alpha
c("6 topic" = attr(year_lda_6, "alpha"),
  "7 topic" = attr(year_lda_7, "alpha"),
  "8 topic" = attr(year_lda_8, "alpha"))

year_beta_6 <- tidy(year_lda_6, matrix = "beta")
year_beta_6

year6_top10 <- year_beta_6 %>% 
  group_by(topic) %>% # raggruppamento per topic
  slice_max(beta, n = 10, with_ties = F) %>% # 10 termini più rappresentativi
  ungroup()%>% 
  arrange(topic, -beta)
year6_top10

year6_top10 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  labs(title = "Top 10 termini per Topic",
       x = expression(beta),
       y = NULL)+
  theme(axis.text.y = element_text(size = 9, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.2, "lines")) +
  scale_fill_manual(values = met.brewer("Hokusai3", 6))

year_beta_7 <- tidy(year_lda_7, matrix = "beta")

year7_top10 <- year_beta_7 %>% 
  group_by(topic) %>% # raggruppamento per topic
  slice_max(beta, n = 10, with_ties = F) %>% # 10 termini più rappresentativi
  ungroup()%>% 
  arrange(topic, -beta)

year7_top10 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  labs(title = "Top 10 termini per Topic",
       x = expression(beta),
       y = NULL)+
  theme(axis.text.y = element_text(size = 9, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.2, "lines")) +
  scale_fill_manual(values = met.brewer("Hokusai3", 7))

year_beta_8 <- tidy(year_lda_8, matrix = "beta")

year8_top10 <- year_beta_8 %>% 
  group_by(topic) %>% # raggruppamento per topic
  slice_max(beta, n = 10, with_ties = F) %>% # 10 termini più rappresentativi
  ungroup()%>% 
  arrange(topic, -beta)

year8_top10 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  labs(title = "Top 10 termini per Topic",
       x = expression(beta),
       y = NULL)+
  theme(axis.text.y = element_text(size = 9, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.2, "lines")) +
  scale_fill_manual(values = met.brewer("Hokusai3", 8))

# modello selezionato: a 7 topic
extra_words <- c("rocrocrocrocrocrocrocrocroc", "autobahn", "der", "auf")
year_word_freq <- year_word_freq %>%
  filter(!word %in% extra_words)
decade_word_freq <- decade_word_freq %>%
  filter(!word %in% extra_words)

year_dtm <- year_word_freq %>% 
  cast_dtm(year, word, n)
decade_dtm <- decade_word_freq %>% 
  cast_dtm(decade, word, n)

# modello LDA a 7 topic
year_lda <- LDA(year_dtm, k = 7, method = "Gibbs",
                control = list(seed = 12345, iter = 2000, burnin = 500))

# analisi dei beta
year_beta <- tidy(year_lda, matrix = "beta")
year_top10 <- year_beta %>% 
  group_by(topic) %>% # raggruppamento per topic
  slice_max(beta, n = 10, with_ties = F) %>% # 10 termini più rappresentativi
  ungroup()%>% 
  arrange(topic, -beta)
# distribuzione parole per topic
year_top10 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  labs(title = "Top 10 termini per Topic",
       x = expression(beta),
       y = NULL)+
  theme(axis.text.y = element_text(size = 9, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.2, "lines")) +
  scale_fill_manual(values = met.brewer("Hokusai3", 7))

# assegnazione di etichette ai topic
year_topic <- c("American Punk Youth", "Obscure and Childhood",
                "Resistance, Freedom and Hope", "Funk and Soul",
                "Rock n Roll and Change", "Rock Energy", "Night Life and Disco")

# analisi di gamma
year_gamma <- tidy(year_lda, matrix = "gamma")
head(year_gamma)

# ciascun valore è la porzione di parole provenienti da un determinato
# documento generate da un determinato topic

year_gamma %>%
  mutate(year = as.numeric(document),
         topic = rep(year_topic, each = 60)) %>%
  ggplot(aes(x = year, y = gamma, color = factor(topic))) +
  geom_line(linewidth = 0.80, show.legend = T) +
  labs(title = "Distribuzione dei topic negli anni",
       x = "Anno",
       y = expression(gamma),
       color = "Topic") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_color_manual(values = met.brewer("Hokusai3", 7))

year_gamma %>%
  mutate(year = as.numeric(document),
         topic = rep(year_topic, each = 60)) %>%
  ggplot(aes(x = year, y = gamma)) +
  geom_line(linewidth = 0.80, show.legend = T, color = met.brewer("Hokusai3", 1)) +
  geom_smooth(se = F, color = "firebrick", linewidth = 1) +
  facet_wrap(~ factor(topic), ncol = 3) +
  labs(title = "Distribuzione dei topic negli anni",
       x = "Anno",
       y = expression(gamma)) +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

### Decades as documents -------------------------

decade_fit <- ldatuning::FindTopicsNumber(
  decade_dtm,
  topics  = seq(from = 4, to = 10, by = 1), # topic K = 4, ..., 10
  metrics = c("CaoJuan2009", "Deveaud2014"), # metriche di valutazione
  method  = "Gibbs", # algoritmo di Gibbs
  control = list(seed = 123)) # per la riproducibilità del modello

ldatuning::FindTopicsNumber_plot(decade_fit)

# 7 topic
decade_lda_7 <- LDA(decade_dtm, k = 7, method = "Gibbs",
                    control = list(seed = 12345, iter = 2000, burnin = 500))

# 8 topic
decade_lda_8 <- LDA(decade_dtm, k = 8, method = "Gibbs",
                    control = list(seed = 12345, iter = 2000, burnin = 500))

# 9 topic
decade_lda_9 <- LDA(decade_dtm, k = 9, method = "Gibbs",
                    control = list(seed = 12345, iter = 2000, burnin = 500))

# Valore stimato di alpha
c("7 topic" = attr(decade_lda_7, "alpha"),
  "8 topic" = attr(decade_lda_8, "alpha"),
  "9 topic" = attr(decade_lda_9, "alpha"))

decade_beta_7 <- tidy(decade_lda_7, matrix = "beta")

decade7_top10 <- decade_beta_7 %>% 
  group_by(topic) %>% # raggruppamento per topic
  slice_max(beta, n = 10, with_ties = F) %>% # 10 termini più rappresentativi
  ungroup()%>% 
  arrange(topic, -beta)

decade7_top10 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  labs(title = "Top 10 termini per Topic",
       x = expression(beta),
       y = NULL)+
  theme(axis.text.y = element_text(size = 9, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.2, "lines")) +
  scale_fill_manual(values = met.brewer("Hokusai3", 7))

decade_beta_8 <- tidy(decade_lda_8, matrix = "beta")

decade8_top10 <- decade_beta_8 %>% 
  group_by(topic) %>% # raggruppamento per topic
  slice_max(beta, n = 10, with_ties = F) %>% # 10 termini più rappresentativi
  ungroup()%>% 
  arrange(topic, -beta)

decade8_top10 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  labs(title = "Top 10 termini per Topic",
       x = expression(beta),
       y = NULL)+
  theme(axis.text.y = element_text(size = 9, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.2, "lines")) +
  scale_fill_manual(values = met.brewer("Hokusai3", 8))

decade_lda <- LDA(decade_dtm, k = 8, method = "Gibbs",
                  control = list(seed = 12345, iter = 2000, burnin = 500))

# beta
decade_beta <- tidy(decade_lda, matrix = "beta")

# etichette assegnate ai topic
decade_topic <- c("Funk and Soul", "Tension and Resistance", "Rock and Resistance",
                  "Street Culture", "Relationships", "Growth",
                  "Rock n Roll, Blues and Sound", "Rythm and Energy")

# Analisi gamma (distribuzione topic per documento)
# per-documento-per-topic probabilities
# ciascun valore è la porzione di parole provenienti da un determinato
# documento generate da un determinato topic
decade_gamma <- tidy(decade_lda, matrix = "gamma")
head(decade_gamma)

decade_gamma %>%
  mutate(year = rep(seq(1950, 2000, by = 10), times = 8),
         topic = rep(decade_topic, each = 6)) %>%
  ggplot(aes(x = year, y = gamma, color = factor(topic))) +
  geom_line(linewidth = 0.80, show.legend = T) +
  labs(title = "Distribuzione dei topic negli anni",
       x = "Anno",
       y = expression(gamma),
       color = "Topic") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_color_manual(values = met.brewer("Hokusai3", 8))

decade_gamma %>%
  mutate(year = factor(document),
         topic = rep(decade_topic, each = 6)) %>%
  ggplot(aes(x = year, y = gamma, fill = factor(topic))) +
  geom_bar(stat = "identity", position = "stack", show.legend = T) +
  labs(title = "Distribuzione dei topic negli anni",
       x = "Anno",
       y = expression(gamma),
       fill = "Topic") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = met.brewer("Hokusai3", 8))


## LDA PAIRED WITH TIME SERIES =================================================

# devtools::install_github("weecology/LDATS")
library(LDATS)

# andamento lisciato della gamma per ciascun topic
year_gamma %>%
  mutate(year = as.numeric(document),
         topic = rep(year_topic, each = 60)) %>%
  ggplot(aes(x = year, y = gamma, color = factor(topic))) +
  geom_smooth(se = F) +
  labs(title = "Traiettoria smussata di gamma per topic",
       x = "Anno",
       y = expression(gamma),
       color = "Topic") +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_color_manual(values = met.brewer("Hokusai3", 8))

dtm_mat <- as.matrix(year_dtm)
covariate_matrix <- data.frame(id = 1:60,
                               year = as.numeric(1950:2009))

data <- list(document_term_table = dtm_mat,
             document_covariate_table = covariate_matrix)

check_document_covariate_table(data$document_covariate_table)

set.seed(123)
ldats_year <- LDA_TS(data, topics = 5:9, nseeds = 20, nchangepoints = 1:5,
                     formulas = ~1, timename = "year")

names(ldats_year)
# [1] "LDA models"         "Selected LDA model" "TS models"          "Selected TS model"

# numero di topic
ldats_year$`Selected LDA model`$k@k
# [1] 9

# Akaike Information Criteria
ldats_year$`Selected TS model`$AIC
# [1] 265.8513

# numero di changepoint
ldats_year$`Selected TS model`$nchangepoints
# [1] 3

# posizione dei changepoint
ldats_year$`Selected TS model`$rho_summary
#                  Mean Median Mode Lower_95% Upper_95%   SD MCMCerr   AC10      ESS
# Changepoint_1 1969.27   1969 1969      1968      1975 1.84  0.0184 0.3266 497.6322
# Changepoint_2 1981.57   1982 1982      1979      1983 1.40  0.0140 0.4528 360.5460
# Changepoint_3 1997.51   1998 1998      1994      1999 1.29  0.0129 0.5239 263.3213

plot(ldats_year$`Selected LDA model`)
plot(ldats_year$`Selected TS model`)

ldats_words <- tidy(ldats_year$`Selected LDA model`$`k: 9, seed: 28`,
                    matrix = "beta")

ldats_top10 <- ldats_words %>% 
  group_by(topic) %>% # raggruppamento per topic
  slice_max(beta, n = 10, with_ties = F) %>% # 10 termini più rappresentativi
  ungroup() %>%
  arrange(topic, -beta)

ldats_top10 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y", ncol = 3) +
  labs(title = "Top 10 termini per Topic",
       x = expression(beta),
       y = NULL)+
  theme(axis.text.y = element_text(size = 9, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.2, "lines")) +
  scale_fill_manual(values = met.brewer("Hokusai3", 9))