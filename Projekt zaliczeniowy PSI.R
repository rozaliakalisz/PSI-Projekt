#' ---
#' title: "Text Mining - analiza kultowego serialu Friends"
#' author: "Maria Jastrzębska and Rozalia Kalisz "
#' date:   "08.06.2025"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      
#'     highlight: espresso      
#'     toc: true            
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false 
#' ---


#' # Potrzebne pakiety
# Pakiety ----
library(tm)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(tidytext)
library(topicmodels)


#' # Dane
# Wczytujemy dane ----
load("C:/Users/Marysia/Downloads/friends.rda")
corpus <- VCorpus(VectorSource(friends$text))

#Sprawdzenie
corpus[[1]][[1]]
corpus[[73]][[1]]


#' # Przetwarzanie i oczyszczanie tekstu
# Przetwarzanie i oczyszczanie tekstu ----
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

#Usunięcie interpunkcji
corpus <- tm_map(corpus, toSpace, "'")
corpus <- tm_map(corpus, removePunctuation)

#Usuwamy podwójne spacje
corpus <- tm_map(corpus, stripWhitespace)

# Zamiana na małe litery
corpus <- tm_map(corpus, content_transformer(tolower))

#Usunięcie zbędnych słów
stopwords <- c("you", "the", "and", "that", "what", "this", "just", "yeah", "okay", "know", "are", "have", "with", "don", "not", "well", "for", "can", "scene", "confidentscene", "wildscene", "satisfiedscene","leftscene", "achhh", "ummooh", "emnow", "128", "wooooooo", "mah", "bleblahblar", "notnot", "notwe", "welwelwell", "isy", "ayyyayyyy", "nopeop", "mimimii", "poppop", "aboutooh", "diddiddid", "lalalalalalalalalala", "whatwhwhat", "mmmhm")
corpus <- tm_map(corpus, removeWords, stopwords)

#Sprawdzenie
corpus[[1]][[1]]
corpus[[73]][[1]]

#Jeszcze raz usuwamy podwójne spacje
corpus <- tm_map(corpus, stripWhitespace)

#Sprawdzenie
corpus[[1]][[1]]
corpus[[73]][[1]]

#' # Tokenizacja
# Tokenizacja ----

# Macierz częstości TDM ----
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)


#' # Zliczanie częstości słów
# Zliczanie częstości słów ----

v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 15)

#' # Wizualizacja - chmura słów
# Chmura słów ----

wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 1000, 
          colors = brewer.pal(8, "Dark2"))

#Widzimy, że po oczyszczeniu imiona głównych bohaterów znajdują się w grupie najczęsciej występujących słów
#Zakładając, że raczej postacie nie mówią o sobie w trzeciej osobie
#Sprawdzimy, jakie słowa najczęściej padały w serialu, gdy ktoś mówił o każdym z głównych bohaterów
#Dzięki temu będziemy mogli dowiedzieć się czegoś o ich charakterze i relacjach z innymi postaciami
#Posłużymy się do tego asocjacjami


#' # Asocjacje
# Asocjacje i ich wizualizacja ----

findAssocs(tdm,"joey",0.3)

#Wektor z imionami
names_vector <- c("ross", "joey", "chandler", "monica", "rachel", "phoebe")

#Ze względu na to, że bohaterów jest całkiem sporo, to posłużyłam się pętlą
#Do wizualizacji wyników asocjacji dla każdego z nich osobno
for (name in names_vector) {
  #Obliczanie asocjacji
  associations <- findAssocs(tdm, name, corlimit = 0.3)
  assoc_vector <- associations[[name]]
  #Jeśli nie ma asocjacji z tym bohaterem
  if (length(assoc_vector) == 0) {
    message(paste(name," nie ma asocjacji na poziomie 0.3"))
    next
  }
  assoc_sorted <- sort(assoc_vector, decreasing = TRUE)
  # Ramka danych
  assoc_df <- data.frame(
    word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
    score = assoc_sorted
  )
  # Lollipop chart - wizualizacja wyników
  p <- ggplot(assoc_df, aes(x = score, y = reorder(word, score))) +
    geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#a6bddb", linewidth = 1.2) +
    geom_point(color = "#0570b0", size = 4) +
    geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
    scale_x_continuous(limits = c(0, max(assoc_df$score) + 0.1), expand = expansion(mult = c(0, 0.2))) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste0("Słowa związane z bohaterem/bohaterką: '", name, "'"),
      subtitle = paste0("Próg r ≥ ", 0.3),
      x = "Współczynnik korelacji Pearsona",
      y = "Słowo"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
    print(p)
}

#Widzimy, że Monica i Chandler mają wiele słów wspólnych ze sobą
#Możemy dzięki temu wnioskować, że te postacie mają ze sobą bardzo bliską relację
#Chandler i Joey są również ze sobą mocno skorelowani w porównaniu do ich poziomu korelacji z innymi słowami
#Dzięki temu również możemy przypuszczać, że ci dwaj bohaterowie mocno się ze sobą przyjaźnią
#(W serialu rzeczywiście tak było)

#Ross, Rachel i Phoebe w ogóle nie mają korelacji z żadnym słowem większej lub równej 0.3
#Stąd też możemy obniżyć ten próg na 0.1
findAssocs(tdm,"rachel",0.1)
findAssocs(tdm,"ross",0.1)
findAssocs(tdm,"phoebe",0.1)

#Widzimy, że Rachel ma bliską relację z Monicą, a Phoebe z Mikem, Rachel i Monicą
#U Phoebe widzimy też korelację ze słowami "central" i "perk"


#' # Funkcja top_terms_by_topic_LDA
# Funkcja top_terms_by_topic_LDA ----

top_terms_by_topic_LDA <- function(input_text, 
                                   plot = TRUE, 
                                   k = number_of_topics) # wyznaczona liczba k tematów
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  # usuń wszystkie puste wiersze w macierzy częstości
  # ponieważ spowodują błąd dla LDA
  unique_indexes <- unique(DTM$i) # pobierz indeks każdej unikalnej wartości
  DTM <- DTM[unique_indexes,]    # pobierz z DTM podzbiór tylko tych unikalnych indeksów
  
  # wykonaj LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") # pobierz słowa/tematy w uporządkowanym formacie tidy
  
  # pobierz dziesięć najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # uporządkuj słowa w malejącej kolejności informatywności
  
  
  
  # rysuj wykres (domyślnie plot = TRUE)
  if(plot == T){
    # dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% # posortuj słowa według wartości beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # rysuj beta według tematu
      geom_col(show.legend = FALSE) + # wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # każdy temat na osobnym wykresie
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  }else{ 
    # jeśli użytkownik nie chce wykresu
    # wtedy zwróć listę posortowanych słów
    return(top_terms)
  }
  
  
}

 
#' # Inżynieria cech w modelu Bag of Words:
#' # Reprezentacja słów i dokumentów w przestrzeni wektorowej
# Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)

# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)


#' # UCZENIE MASZYNOWE NIENADZOROWANE
# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)


#' # Modelowanie tematów: ukryta alokacja Dirichleta
# Modelowanie tematów: ukryta alokacja Dirichleta (LDA) ---


# Rysuj dziesięć słów 
# o największej informatywności według tematu
# dla wyznaczonej liczby tematów 

# Dobór liczby tematów
number_of_topics = 2
top_terms_by_topic_LDA(tdm_df$word)

# Zmień wyznaczoną liczbę tematów
number_of_topics = 3
top_terms_by_topic_LDA(tdm_df$word)

# Zmień wyznaczoną liczbę tematów
number_of_topics = 4
top_terms_by_topic_LDA(tdm_df$word)

# Zmień wyznaczoną liczbę tematów
number_of_topics = 6
top_terms_by_topic_LDA(tdm_df$word)