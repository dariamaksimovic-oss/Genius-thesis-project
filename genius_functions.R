# FILE PER LA DEFINIZIONE DI FUNZIONI UTILI

library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(dplyr)
library(purrr)

# FUNZIONI DI UTILITA' -----

# verifica la connessione internet dell'utente
check_internet <- function() {
  if (!curl::has_internet()) stop("Please check your internet connection")
}

# wrapper per GET
genius_get <- function(url, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  GET(url = url,
      add_headers(Authorization = paste0("Bearer ", access_token))
  )
}

# ripete l'elemento di x con l'ultimo elemento TRUE di y
repeat_before = function(x, y) {
  ind = which(y)
  if(!y[1])
    ind = c(1,ind)
  rep(x[ind], times = diff(
    c(ind, length(x) + 1) ))
}

# FUNZIONI DI RICERCA ------

## Ricerca generale --------
search_genius <- function(search_term, n_results = 10,
                          access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  path <- sprintf("api.genius.com/search?q=%s", search_term)
  
  results <- list()
  
  # limita le pagine restituite
  if (is.numeric(n_results)) {
    pages_limit <- ceiling(n_results / 10)
  }
  
  # contatore per le pagine
  i <- 1
  
  # ciclo while per il recupero dei risultati
  while (i > 0 && i <= pages_limit) {
    # cerca il termine
    req <- GET(
      url = "https://api.genius.com/search",
      query = list(
        q = search_term, page = i,
        access_token = access_token
      )
    )
    
    # interruzione se lo stato di richiesta è inatteso
    stop_for_status(req)
    # estrazione del contenuto richiesto
    res <- httr::content(req)
    results[[i]] <- res$response$hits
    
    # verifica dei risultati
    if (length(res$response$hits) > 0) {
      i <- i + 1
    } else {break}
  }
  
  # unione dei risultati
  results <- do.call(c, results)
  results <- sapply(results, "[", "result")
  names(results) <- NULL
  
  results
}

## Cerca artisti ----------
search_artist <- function(search_term, n_results = 10,
                          access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  results <- search_genius(search_term, n_results, access_token)
  
  # estrazione delle informazioni sull'artista
  artist_info <- lapply(seq_along(results),
                        function(x) {
                          artist <- results[[x]]$primary_artist
                          tibble(artist_id = artist$id,
                                 artist_name = artist$name,
                                 artist_url = artist$url)
                          }
                        )
  
  artist_info <- do.call(rbind, artist_info)
  
  # in output gli artisti unici
  as_tibble(unique(artist_info))
}

## Search songs -----------
search_song <- function(search_term, n_results = 10,
                        access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  results <- search_genius(search_term, n_results, access_token)
  
  # estrazione delle informazioni su cantante e canzone
  song_info <- lapply(seq_along(results),
                      function(x) {
                        track <- results[[x]]
                        artist <- results[[x]]$primary_artist
                        tibble(song_id = track$id,
                               song_name = track$title_with_featured,
                               song_lyrics_url = track$url,
                               artist_id = artist$id,
                               artist_name = artist$name)
                        }
                      )
  
  song_info <- do.call(rbind, song_info)
  
  # in output le canzoni uniche
  as_tibble(unique(song_info))
}

## Esplora -----------------
# per aprire sul proprio browser il link associato a album/canzone/artista
# x corrisponde all'oggetto ritornato da get_song(), get_artist() o get_album()
browse_genius <- function(x) {
  check_internet()
  
  if (interactive()) {
    utils::browseURL(
      url = x$url,
      browser = getOption("browser")
    )
  }
  invisible(x$content$url)
}


# FUNZIONI PER CANZONI -----

get_song <- function(song_id, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()

  # check id
  if (is.na(song_id) || song_id == "") return(NA)
  
  path <- sprintf("https://api.genius.com/songs/%s", song_id)
  # request track
  req <- genius_get(url = path, access_token)
  
  if (status_code(req) == 200) {
    return(fromJSON(httr::content(req, as = "text"))$response$song)
  } else {
    return(NA)
  }
}

# conversione della canzone in dataframe (tibble)
# utile per ordinare le informazioni presenti nella variabile
song_to_df <- function(song) {
  if (!is.list(song)) return(tibble(song_id = NA,
                                    song_name = NA,
                                    song_lyrics_url = NA,
                                    song_art_image_url = NA,
                                    song_release_date = NA,
                                    song_pageviews = NA,
                                    song_annotation_count = NA,
                                    artist_id = NA, 
                                    artist_name = NA, 
                                    artist_url = NA, 
                                    album_id = NA,
                                    album_name = NA, album_url = NA))
  
  # recupera i dati di album e di artista, e le statistiche
  album <- song$album
  artist <- song$primary_artist
  stats <- song$stats
  
  # lista per song_info (default NA)
  song_info <- list(
    song_id = song$id %||% NA,
    song_name = song$title_with_featured %||% NA,
    song_lyrics_url = song$url %||% NA,
    song_art_image_url = song$song_art_image_url %||% NA,
    song_release_date = song$release_date %||% NA,
    song_pageviews = stats$pageviews %||% NA,
    song_annotation_count = song$annotation_count %||% NA,
    artist_id = artist$id %||% NA,
    artist_name = artist$name %||% NA,
    artist_url = artist$url %||% NA,
    album_id = album$id %||% NA,
    album_name = album$name %||% NA,
    album_url = album$url %||% NA
  )
  
  as_tibble(song_info)
}

get_song_df <- function(song_id, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  # metadati della canzone
  song <- get_song(song_id, access_token)
  song_to_df(song)
}

## Esempio -----------------
# mac_miller_song <- get_song(50220)
# names(mac_miller_song)
# sono numerosi gli elementi coinvolti
# a quali siamo realmente interessati?
# get_song_df(50220)
# browse_genius(mac_miller_song)

# other_song <- get_song(235765)
# other_song$writer_artists$name


# FUNZIONI PER ARTISTI -----

get_artist <- function(artist_id, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  path <- sprintf("https://api.genius.com/artists/%s", artist_id)
  # request track
  req <- genius_get(url = path, access_token)
  
  if (status_code(req) == 200) {
    return(fromJSON(httr::content(req, as = "text"))$response$artist)
  } else {
    stop("Connection Failed with status: ", status_code(req))
  }
}

artist_to_df <- function(artist) {
  # lista per artist_info
  artist_info <- list(
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url,
    artist_image_url = artist$image_url,
    artist_followers_count = artist$followers_count,
    artist_twitter_name = artist$twitter_name
  )
  
  # modifica in NA dei valori NULL
  ndxNULL <- which(unlist(lapply(artist_info, is.null)))
  for(i in ndxNULL) {artist_info[[i]] <- NA}
  
  as_tibble(artist_info)
}

get_artist_df <- function(artist_id, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  # metadati per la canzone
  artist <- get_artist(artist_id, access_token)
  
  artist_to_df(artist)
}

## Esempio -----------------
# search_artist("Mac Miller")
# mac_miller <- get_artist(820)
# names(mac_miller)
# get_artist_df(820)


# FUNZIONI PER ALBUM -------

get_album <- function(album_id, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  path <- sprintf("https://api.genius.com/albums/%s", album_id)
  # request track
  req <- genius_get(url = path, access_token)
  
  if (status_code(req) == 200) {
    return(fromJSON(httr::content(req, as = "text"))$response$album)
  } else {
    stop("Connection Failed with status: ", status_code(req))
  }
}

album_to_df <- function(album) {
  artist <- album$artist
  
  # lista per album_info
  album_info <- list(
    album_id = album$id,
    album_name = album$name,
    album_url = album$url,
    album_cover_art_url = album$cover_art_url,
    album_release_date = album$release_date,
    album_comment_count = album$comment_count,
    song_pageviews = album$song_pageviews,
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url
  )
  
  # modifica in NA dei valori NULL
  ndxNULL <- which(unlist(lapply(album_info, is.null)))
  for(i in ndxNULL) {album_info[[i]] <- NA}
  
  as_tibble(album_info)
}

get_album_df <- function(album_id, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  # metadati per la canzone
  album <- get_album(album_id, access_token)
  
  album_to_df(album)
}

## Esempio -----------------
# alb_id <- get_song(50220)$album$id
# alb_id
# alb_df <- get_album_df(alb_id)

# potrebbe essere interessante analizzare anche le copertine degli album e la
# loro scala cromatica in base al tema/sentiment associato

# METADATI DA TITOLO E ARTISTA ----
# Unisco le funzioni search_genius, search_song, get_song_df
song_metadata <- function(title, artist, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  search_term <- paste(title, artist, sep = " ")
  
  req <- GET(
    url = "https://api.genius.com/search",
    query = list(
      q = search_term,
      access_token = access_token
    )
  )
  
  # interruzione se lo stato di richiesta è inatteso
  stop_for_status(req)
  # estrazione del contenuto richiesto
  res <- httr::content(req)
  hits <- res$response$hits
  # if (length(res$response$hits) <= 0) {break}
  
  # ID della prima canzone nei risultati - la più pertinente
  first_song_id <- hits[[1]]$result$id
  
  # dati della canzone
  song_url <- sprintf("https://api.genius.com/songs/%s", first_song_id)
  song_req <- genius_get(song_url)
  
  stop_for_status(song_req)
  song_res <- httr::content(song_req)
  song <- song_res$response$song
  
  album <- song$album
  artist <- song$primary_artist
  stats <- song$stats
  
  # gestione/trasformazione dei NULL in NA
  song_df <- tibble(song_id = song$id %||% NA,
                    song_name = song$title_with_featured %||% NA,
                    song_lyrics_url = song$url %||% NA,
                    song_art_image_url = song$song_art_image_url %||% NA,
                    song_release_date = song$release_date %||% NA,
                    song_pageviews = stats$pageviews %||% NA,
                    song_annotation_count = song$annotation_count %||% NA,
                    artist_id = artist$id %||% NA,
                    artist_name = artist$name %||% NA,
                    artist_url = artist$url %||% NA,
                    album_id = album$id %||% NA,
                    album_name = album$name %||% NA,
                    album_url = album$url %||% NA
  )
  
  return(song_df)
}

song_metadata_url <- function(url, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  # dati della canzone
  song_req <- genius_get(url)
  
  stop_for_status(song_req)
  song_res <- httr::content(song_req)
  song <- song_res$response$song
  
  album <- song$album
  artist <- song$primary_artist
  stats <- song$stats
  
  # gestione/trasformazione dei NULL in NA
  song_df <- tibble(song_id = song$id %||% NA,
                    song_name = song$title_with_featured %||% NA,
                    song_lyrics_url = song$url %||% NA,
                    song_art_image_url = song$song_art_image_url %||% NA,
                    song_release_date = song$release_date %||% NA,
                    song_pageviews = stats$pageviews %||% NA,
                    song_annotation_count = song$annotation_count %||% NA,
                    artist_id = artist$id %||% NA,
                    artist_name = artist$name %||% NA,
                    artist_url = artist$url %||% NA,
                    album_id = album$id %||% NA,
                    album_name = album$name %||% NA,
                    album_url = album$url %||% NA
  )
  
  return(song_df)
}

# FUNZIONE PER LYRICS -------------

# vettore che associa ad ogni espressione la sostituzione da effettuare
bad_lyric_strings <- c("\\s*\\(Ft.[^\\)]+\\)" = "", # rimozione del featuring
                       "&" = "and",
                       "\\$" = " ",
                       "'" = "",
                       "\u00e9" = "e", # rimozione degli accenti
                       "\u00f6" = "o",
                       "[[:punct:]]" = "", # rimozione della punteggiatura
                       "[[:blank:]]+" = " ") # sostituzione di spazi bianchi e tabs consecutivi

# estrazione delle lyrics dall'HTML della pagina web
get_lyrics <- function(session) {
  # ricerca del testo nelle sezioni "Lyrics Container"
  lyrics_node <-  session %>%
    html_nodes(xpath = '//div[contains(@class, "Lyrics__Container")]')
  
  # preservazione degli "a capo" (per mantenere la struttura delle lyrics)
  xml2::xml_find_all(lyrics_node, ".//br") %>% xml2::xml_add_sibling("p", "\n")
  xml2::xml_find_all(lyrics_node, ".//br") %>% xml2::xml_remove()
  
  # estrazione del testo
  lyrics <- html_text(lyrics_node, trim = TRUE)
  
  # vettore di stringhe - ogni elemento è delimitato da "\n"
  lines <- unlist(strsplit(lyrics, split = "\n"))
  
  # se la canzone non ha testo o strumentale, viene posto un valore NA
  if (length(lines) == 0) return (NA)
  
  # rimozione metadati in prima riga (Contributors, ecc) fino al termine "Lyrics"
  # (che indica effettivo l'inizio della canzone)
  lines[1] <- sub("^.*?Lyrics", "", lines[1], ignore.case = TRUE)
  
  # rimozione di righe senza contenuto
  lines <- grep(pattern = "[[:alnum:]]", lines, value = TRUE)
  
  # individuazione dei tag di intestazione (es. "[Chorus]")
  section_tags <- grepl("^\\[.*\\]$", lines)
  # rimozione di tutto ciò che si trova prima della prima '['
  # text <- sub("^[^[]*", "", text)
  # viene restituito il testo della canzone, senza i tag di intestazione
  lines <- lines[!section_tags]
  
  paste(lines, collapse = " \n")
}

# recupera il testo associato a un id di Genius
get_lyrics_id <- function(song_id, access_token = Sys.getenv("GENIUS_API_TOKEN")) {
  check_internet()
  
  if (is.na(song_id)) return(NA)
  
  # metadati della canzone
  song <- get_song(song_id, access_token)
  # check id
  if (is.null(song) || is.null(song$url)) return(NA)
  
  # inizio sessione di ricerca
  session <- read_html(song$url)
  
  # testo associato alla sessione
  get_lyrics(session)
} 

# recupera il testo associato all'URL di una pagina Genius
get_lyrics_url <- function(song_lyrics_url) {
  check_internet()
  # inizio sessione di ricerca
  session <- read_html(song_lyrics_url)
  # testo associato alla sessione
  get_lyrics(session)
}

# recupera il testo associato a una canzone Genius
# -> tentativo tramite titolo e artista associati
get_lyrics_search <- function(artist_name, song_title) {
  # rimozione di stringhe problematiche
  artist_name <- str_replace_all(artist_name, bad_lyric_strings)
  song_title <- str_replace_all(song_title, bad_lyric_strings)
  # sono ammessi perciò piccoli errori di battitura (che non coivolgano la natura
  # stessa del nome) in quanto verranno corretti
  
  # percorso di ricerca
  path <- sprintf("https://genius.com/%s-%s-lyrics",
                  tolower(artist_name), tolower(song_title))
  
  check_internet()
  # inizio sessione di ricerca
  session <- read_html(path)
  # testo associato alla sessione
  get_lyrics(session)
}

## Annotazioni associate alle lyrics ----
# con annotazioni intendiamo le spiegazioni associate a righe complesse di testo
# le individuiamo nella pagina web con una sottolineatura gialla
# file "primitivo" annotations.R -> da integrare alla risorsa