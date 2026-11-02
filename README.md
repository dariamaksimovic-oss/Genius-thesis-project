# Genius-thesis-project
L'obiettivo generale delle successive analisi è quello di esaminare l'evoluzione della musica in relazione al suo contesto storico e culturale.

In particolare, l'attenzione si focalizza su canzoni prodotte tra il 1950 e il 2009.

Per la composizione del dataset, sono state selezionate le 10 canzoni più popolari per ciascun anno secondo la piattaforma Acclaimed Music (www.acclaimedmusic.net), che assegna un punteggio ai brani, combinando matematicamente le classifiche elaborate da riviste e critici di rilievo. Sono stati raccolti i testi e i relativi metadati tramite l'uso dello script genius_functions.R. Le funzioni in esso contenute derivano da un aggiornamento e da un'ottimizzazione di quelle fornite dalle librerie rgenius (sviluppata Alberto Almuinha) e geniusr (dell'utente GitHub ewenme).

La sovrascrizione delle funzioni è avvenuta modificando i codici originali, mantenendo inalterato il principio di estrazione: viene creato un Genius API Client e poi generato un Client Access Token (https://docs.genius.com/). Il token consiste in una chiave d'accesso strettamente personale, valida a tempo indeterminato fino alla sua diffusione; nel caso in cui Genius rilevi una fuga di dati, il sito revoca automaticamente l'autorizzazione.

In primo luogo, ci concentreremo in fase esplorativa sull'individuazione del sentiment presente nei testi.

In un secondo momento, verrà costruito prima un modello LDA e poi applicato un algoritmo LDATS, che combina l'analisi delle Serie Storiche (Time Series) e la Latent Dirichlet Allocation (LDA) per la rilevazione dell'evoluzione temporale dei topics all'interno del corpus.

I file R della repository sono i seguenti:
| File | Contenuto |
| ------------- | ------------- |
| genius_functions.R | libreria per l'estrazione da Genius (API e **web scraping**) |
| 1_data_collection.R | estrazione dei dati |
| 2_data_preprocessing.R | pulizia e preprocessing dei dati |
| 3_descriptive_analysis.R | analisi descrittive |
| 3.1_sentiment_analysis.R | **Sentiment Analysis** |
| 4_LDA_LDATS.R | modelli **LDA** e **LDATS** |

La cartella **acclaimed_music_list** contiene al suo interno le 10 canzoni più popolari per ogni anno, dal 1950 al 2009, suddivise per decennio. I rimanenti file *.csv* sono il risultato della rielaborazione (tramite i codici forniti) delle classifiche Acclaimed Music.
