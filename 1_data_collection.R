# DATA COLLECTION ==============================================================
# caricamento delle funzioni nella risorsa R
source("genius_functions.R")
# e della libreria tidyverse
library(tidyverse)

# copia in file csv dei dati in: https://www.acclaimedmusic.net/years.htm
# e lettura di questi file
data1950s <- read.csv("acclaimed_music_list/topsongs50s.csv", sep = ";", header = T)
data1960s <- read.csv("acclaimed_music_list/topsongs60s.csv", sep = ";", header = T)
data1970s <- read.csv("acclaimed_music_list/topsongs70s.csv", sep = ";", header = T)
data1980s <- read.csv("acclaimed_music_list/topsongs80s.csv", sep = ";", header = T)
data1990s <- read.csv("acclaimed_music_list/topsongs90s.csv", sep = ";", header = T)
data2000s <- read.csv("acclaimed_music_list/topsongs2000s.csv", sep = ";", header = T)

# contenuto dei dataset
str(data1950s)
# - rank: posizione in classifica, associata a ciascun anno di produzione
# - artist: nome dell'artista
# - song: nome del bano associato all'artista
# - genres: lista dei generi associati alla canzone
# - year: anno di uscita del brano
# per ciascun anno è stata stilata la top 10

data1950s$year <- rep(1950:1959, each = 10)
data1960s$year <- rep(1960:1969, each = 10)
data1970s$year <- rep(1970:1979, each = 10)
data1980s$year <- rep(1980:1989, each = 10)
data1990s$year <- rep(1990:1999, each = 10)
data2000s$year <- rep(2000:2009, each = 10)

# raccolta dei metadati per ciascun decennio
data1950s <- data1950s %>%
  rowwise() %>%
  mutate(song_metadata(song, artist)) %>%
  ungroup()
data1960s <- data1960s %>%
  rowwise() %>%
  mutate(song_metadata(song, artist)) %>%
  ungroup()
data1970s <- data1970s %>%
  rowwise() %>%
  mutate(song_metadata(song, artist)) %>%
  ungroup()
data1980s <- data1980s %>%
  rowwise() %>%
  mutate(song_metadata(song, artist)) %>%
  ungroup()
data1990s <- data1990s %>%
  rowwise() %>%
  mutate(song_metadata(song, artist)) %>%
  ungroup()
data2000s <- data2000s %>%
  rowwise() %>%
  mutate(song_metadata(song, artist)) %>%
  ungroup()

# raccolta dei testi delle canzoni
lyrics1950s <- data1950s %>% select("song_id") %>%
  rowwise() %>%
  mutate(lyrics = get_lyrics_id(song_id)) %>%
  ungroup()
lyrics1960s <- data1960s %>% select("song_id") %>%
  rowwise() %>%
  mutate(lyrics = get_lyrics_id(song_id)) %>%
  ungroup()
lyrics1970s <- data1970s %>% select("song_id") %>%
  rowwise() %>%
  mutate(lyrics = get_lyrics_id(song_id)) %>%
  ungroup()
lyrics1980s <- data1980s %>% select("song_id") %>%
  rowwise() %>%
  mutate(lyrics = get_lyrics_id(song_id)) %>%
  ungroup()
lyrics1990s <- data1990s %>% select("song_id") %>%
  rowwise() %>%
  mutate(lyrics = get_lyrics_id(song_id)) %>%
  ungroup()
lyrics2000s <- data2000s %>% select("song_id") %>%
  rowwise() %>%
  mutate(lyrics = get_lyrics_id(song_id)) %>%
  ungroup()

# aggregazione dei dataset in un file unico
data <- data1950s %>%
  add_row(data1960s) %>%
  add_row(data1970s) %>%
  add_row(data1980s) %>%
  add_row(data1990s) %>%
  add_row(data2000s)

lyrics <- lyrics1950s %>%
  add_row(lyrics1960s) %>%
  add_row(lyrics1970s) %>%
  add_row(lyrics1980s) %>%
  add_row(lyrics1990s) %>%
  add_row(lyrics2000s)

# compilazione di file CSV contenenti le info estratte (per preservare una copia delle ricerche)
write.csv2(data, "songs_metadata.csv")
write.csv2(lyrics, "songs_lyrics.csv")






# DATA CLEANING ================================================================
# individuazione di dati mancanti o errati (per quanto possibile)
data <- read.csv("songs_metadata.csv", sep = ";")
lyrics <- read.csv("songs_lyrics.csv", sep = ";")

## META DATASET ================================================================
str(data)
# le colonne artist, song, X e year sono ridondanti
# -> confronto con l'informazione contenuta in artist_name e song_name
#    rimozione di X e song_release_date

### Errate corrispondenze ------------------------
# indici delle canzoni per le quali artista o titolo non corrispondono
wrong_idx <- which((tolower(data$artist) != tolower(data$artist_name)) | (tolower(data$song) != tolower(data$song_name)))
view(data[wrong_idx, c(2, 3, 14, 7)]) # 112 elementi errati
# tra questi gli indici
correct_idx <- c(4, 6, 7, 9, 12, 14, 17, 20, 23, 27, 31, 33, 35, 42, 44, 46, 48,
                 49, 56, 57, 64, 70, 72, 86, 98, 109, 121, 123, 124, 137, 138, 
                 143, 145, 147, 150, 159, 178, 190, 202, 219, 228, 232, 237, 238, 
                 257, 270, 278, 286, 297, 303, 324, 327, 328, 362, 371, 372, 377, 
                 397, 401, 402, 403, 410, 437, 438, 461, 465, 468, 485, 489, 496, 
                 503, 504, 509, 516, 520, 526, 528, 534, 560, 564, 569, 581, 586)
# sono in realtà corretti (le incongruenze sono dovute a errori di battitura e simili)
data$song[correct_idx] <- data$song_name[correct_idx]
data$artist[correct_idx] <- data$artist_name[correct_idx]

wrong_idx <- which((tolower(data$artist) != tolower(data$artist_name)) | (tolower(data$song) != tolower(data$song_name)))
view(data[wrong_idx, c(2, 3, 14, 7)]) # 29 canzoni errate
# ricerca manuale delle rimanenti canzoni
source("genius_functions.R")
new_research <- matrix(c("Bernie’s Tune", "Mel Tormé",
                         "Mystery Train", "Junior Parker",
                         "Rock Around The Clock", "Bill Haley & His Comets",
                         "Move it", "The Coasters",
                         "Shout (Pt. 1 & 2)", "The Isley Brothers",
                         "The Sound of Silence", "Simon & Garfunkel",
                         "Águas de Março", "Elis Regina & Antônio Carlos Jobim",
                         "Blitzkrieg Bop", "Ramones",
                         "Rapper’s Delight", "Sugarhill Gang",
                         "Under Pressure", "Queen & David Bowie",
                         "O Superman (For Massenet)", "Laurie Anderson",
                         "The Killing Moon", "Echo & the Bunnymen",
                         "Hallelujah", "Leonard Cohen",
                         "Running Up That Hill (A Deal with God)", "Kate Bush",
                         "Fight for Your Right", "Beastie Boys",
                         "Pump Up the Volume","M/A/R/R/S",
                         "Rockin’ in the Free World (Electric)", "Neil Young",
                         "Nuthin’ But A ‘G’ Thang", "Dr. Dre",
                         "California Love", "2Pac",
                         "No Diggity", "Blackstreet",
                         "Smooth", "Santana",
                         "Stan", "Eminem",
                         "Drop It Like It’s Hot", "Snoop Dogg",
                         "Feel Good Inc.", "Gorillaz",
                         "My Love", "Justin Timberlake",
                         "Boy From School", "Hot Chip",
                         "Umbrella", "Rihanna",
                         "Single Ladies (Put a Ring on It)", "Beyoncé",
                         "Empire State of Mind", "JAŸ-Z"),
                       ncol = 2, byrow = T)
new_research <- cbind(new_research[, 2], new_research[, 1])
colnames(new_research) <- c("artist", "song")

new_research <- new_research %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(song_metadata(song, artist)) %>%
  ungroup()
data[wrong_idx, -c(1, 4:5)] <- new_research[wrong_idx, ]

lyrics[wrong_idx, ] <- new_research %>% select("song_id") %>%
  rowwise() %>%
  mutate(lyrics = get_lyrics_id(song_id)) %>%
  ungroup()

wrong_idx <- which((tolower(data$artist) != tolower(data$artist_name)) | (tolower(data$song) != tolower(data$song_name)))
view(data[wrong_idx, c(2, 3, 14, 7)])
# ora è tutto a posto

### Dati mancanti --------------------------------
c("year" = sum(is.na(data$year)),
  "genres" = sum(is.na(data$genres)),
  "song_id" = sum(is.na(data$song_id)),
  "song_name" = sum(is.na(data$song_name)),
  "song_lyrics_url" = sum(is.na(data$song_lyrics_url)),
  "song_art_image_url" = sum(is.na(data$song_art_image_url)),
  "song_pageviews" = sum(is.na(data$song_pageviews)), # !!
  "song_annotation_count" = sum(is.na(data$song_annotation_count)),
  "artist_id" = sum(is.na(data$artist_id)),
  "artist_name" = sum(is.na(data$artist_name)),
  "artist_url" = sum(is.na(data$artist_url)),
  "album_id" = sum(is.na(data$album_id)), # !!
  "album_name" = sum(is.na(data$album_name)), # essendo strettamente legato a 'album_id' ci aspettiamo che anche questa informazione sia assente
  "album_url" = sum(is.na(data$album_url)))

# colonna year
data$year <- rep(1950:2009, each = 10)

# colonna song_id (e di conseguenza le successive)
na_idx <- which(is.na(data$song_id))
view(data[na_idx, c(2, 3, 14, 7)])
# recuperiamo le info in song e artist
data2 <- read.csv("songs_metadata.csv", sep = ";")
data[na_idx, 2:3] <- data2[na_idx, 2:3]
# ricerca dei titoli mancanti
new_research <- data[na_idx, 1:5]
new_research$song_id <- c(NA, 2063833, 51780, 816054, 74453, 332798, 255691, 79538,
                          835664, 547977, 79652, 110134, 524741, 207947, 1043770, 57824,
                          76012, 129202, 453, 82653, 73849, 1813, 75951, 152350,
                          134267, 1644, 529, 67272, 129806, 142106, 1237, 344944,
                          91174, 4279, 1659890, 1846, 119163, 100856, 246, 105031, 
                          53156, 244, 6320, 122549, 349, 114915, 772, 99654, 32167, 
                          5139, 1113, 1267, 21569, 2586, 96861, 1629, 50873, 108833, 75)


new_research <- new_research %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(get_song_df(song_id)) %>%
  ungroup()
data[na_idx, ] <- new_research

c("year" = sum(is.na(data$year)),
  "genres" = sum(is.na(data$genres)),
  "song_id" = sum(is.na(data$song_id)), # solo un elemento (assente su Genius)
  "song_name" = sum(is.na(data$song_name)),
  "song_lyrics_url" = sum(is.na(data$song_lyrics_url)),
  "song_art_image_url" = sum(is.na(data$song_art_image_url)),
  "song_pageviews" = sum(is.na(data$song_pageviews)), # !!
  "song_annotation_count" = sum(is.na(data$song_annotation_count)),
  "artist_id" = sum(is.na(data$artist_id)),
  "artist_name" = sum(is.na(data$artist_name)),
  "artist_url" = sum(is.na(data$artist_url)),
  "album_id" = sum(is.na(data$album_id)), # !!
  "album_name" = sum(is.na(data$album_name)), # essendo strettamente legato a 'album_id' ci aspettiamo che anche questa informazione sia assente
  "album_url" = sum(is.na(data$album_url)))
# ora tutto ok

### Colonne ridondanti ---------------------------
# rimozione delle colonne in eccesso
data <- data[, -c(2, 3, 10)]

### Salvataggio file -----------------------------
write.csv2(data, "complete_metadata.csv")
# sovrascriviamo prima della conversione di genres in lista, essendo le colonne-lista
# non convertibili in .csv


## LYRICS DATASET ==============================================================

lyrics <- read.csv("clean_lyrics.csv", sep = ";")
### Errate corrispondenze ------------------------
# confronto tra song_id in data e song_id in lyrics
which(data$song_id != lyrics$song_id)
# canzone 88
lyrics$song_id[88] <- data$song_id[88]
lyrics$lyrics[88] <- get_lyrics_id(lyrics$song_id[88])

### Dati mancanti --------------------------------
# numero di campi vuoti per colonna
c("id" = sum(is.na(lyrics$song_id)),
  "lyrics" = sum(is.na(lyrics$lyrics)))
# ricerca degli id e testi mancanti
for (i in which(is.na(lyrics$song_id))) {
  lyrics$song_id[i] <- data$song_id[i]
  lyrics$lyrics[i] <- get_lyrics_id(lyrics$song_id[i])
}
# check
sum(is.na(lyrics$lyrics))
# tutto ok (manca solo la canzone assente su Genius)

### Tag/Descrizioni iniziali ---------------------
cat(lyrics$lyrics[11])
# certe canzoni contengono una descrizione e/o un tag iniziale che inica l'inizio
# del primo verso
cat(lyrics$lyrics[27])
# altre il tag "This song is an instrumental" che inidica l'assenza di un testo vero e proprio

# filtraggio canzoni strumentali
lyrics <- lyrics %>%
  mutate(is_instrumental = stringr::str_detect(lyrics, fixed("This song is an instrumental")))
table(lyrics$is_instrumental)
# 14 canzoni strumentali, le restanti (585) contengono un testo

# rimozione tag e descrizione brano
pattern <- "^.*?\\[[^\\]]+\\]\\s*"
lyrics[88, ] <- lyrics[88, ] %>%
  mutate(lyrics_clean = stringr::str_remove(lyrics, regex("^.*?Read More\\s*", dotall = TRUE)))
lyrics <- lyrics %>%
  mutate(lyrics_clean = stringr::str_remove(lyrics_clean, regex(pattern, dotall = TRUE)))

### Salvataggio file -----------------------------
write.csv2(lyrics, "clean_lyrics.csv")





