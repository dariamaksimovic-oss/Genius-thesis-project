# Genius-thesis-project
L'obiettivo generale delle successive analisi è quello di esaminare l'evoluzione della musica in relazione al suo contesto storico e culturale.

In particolare, l'attenzione si focalizza su canzoni prodotte tra il 1950 e il 2009. Questo cinquantennio include periodi fondamentali per la storia della musica, caratterizzata dalla nascita di generi nuovi (quali il Rock n Roll, il Punk e l'Hip Hop) e segnato dall'avvento di nuove tecnologie e di Internet.

Per la composizione del dataset, sono state selezionate le 10 canzoni più popolari per ciascun anno secondo la piattaforma Acclaimed Music, che assegna un punteggio ai brani, combinando matematicamente le classifiche elaborate da riviste e critici di rilievo. Successivamente, sono stati raccolti i testi e i relativi metadati tramite l'uso dello script genius_functions.R. Le funzioni in esso contenute derivano da un aggiornamento e da un'ottimizzazione di quelle fornite dalle librerie rgenius (sviluppata Alberto Almuinha) e geniusr (dell'utente GitHub ewenme).

La revisione di tali pacchetti era necessaria a causa degli errori riscontrati durante l'estrazione dei dati: gli ultimi aggiornamenti risalivano a circa 6 anni prima, a fronte delle modifiche strutturali dell sito Genius e del continuo aggiornamento delle funzioni di R.

La sovrascrizione delle funzioni è avvenuta modificando i codici originali, mantenendo inalterato il principio di estrazione: viene creato un Genius API Client e poi generato un Client Access Token. Il token consiste in una chiave d'accesso strettamente personale valida a tempo indeterminato, fino alla sua diffusione; nel caso in cui Genius rilevi una fuga di dati, revoca automaticamente l'autorizzazione.

Lo script genius_functions.R consente la ricerca e l'estrazione di informazioni dati un URL, un ID Genius o una combinazione "titolo-artista". Grazie al comando source(), lo script viene importato cosicché le sue funzioni siano riuttilizzabili in ogni momento. Tramite una procedura iterativa sulla classifica di Acclaimed Music, è stato costruito il dataset oggetto di analisi.

In primo luogo, ci concentreremo in fase esplorativa sull'individuazione del sentiment presente nei testi. E' opportuno premettere che non esiste una verità assoluta: la percezione delle emozioni è di natura soggettiva. Di conseguenza un modello computazionale, basato su misure oggettive quali dizionari preimpostati, non può cogliere con certezza le sfumature racchiuse in un brano. La valutazione umana (in quanto soggettiva) può fungere solo in parte da metodo di validazione del modello.

In un secondo momento, verrà applicato un modello LDATS che combina l'analisi delle Serie Storiche (Time Series) e la Latent Dirichlet Allocation (LDA). Vogliamo studiare l'evoluzione temporale dei topics all'interno di un corpus di testi, supportando questi mutamenti con eventi storici e, in risposta, la nascita di nuovi generi musicali.

I file principali della repository sono i seguenti:
| File | Contenuto |
| ------------- | ------------- |
| genius_functions.R | libreria per l'estrazione da Genius (API e **web scraping**) |
| 1_data_collection.R | estrazione dei dati |
| 2_data_preprocessing.R | pulizia e preprocessing dei dati |
| 3_descriptive_analysis.R | analisi descrittive |
| 3.1_sentiment_analysis.R | **Sentiment Analysis** |
| 4_LDA_LDATS.R | modelli LDA e LDATS |
