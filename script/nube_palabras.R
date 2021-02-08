library(NLP)
library(tm)
library(ggplot2)
library(ggwordcloud)

texto <- readLines(file.choose(),encoding = "UTF-8")
texto <- iconv(texto,from="UTF-8",to="latin1")

corpus <- Corpus(VectorSource(texto))

mi_corpus <- tm_map(corpus,tolower) # Convertimos mayúsculas en minúsculas
mi_corpus <- tm_map(mi_corpus,stripWhitespace) #Elimino espacios
mi_corpus <- tm_map(mi_corpus,removePunctuation) #Quito signos de puntuación
mi_corpus <- tm_map(mi_corpus,removeNumbers) #Quito números 

stopwords("spanish")

mi_corpus <- tm_map(mi_corpus,removeWords,stopwords("spanish"))

#mi_corpus <- tm_map(mi_corpus,removeWords,c("Hola","Adiós"))

################

palabras <- TermDocumentMatrix(mi_corpus)
palabras <- as.matrix(palabras)

palabras <- sort(rowSums(palabras),decreasing=TRUE)

df <- data.frame(palabra = names(palabras),freq=palabras)

barplot(df[1:10,]$freq,
        names.arg=df[1:10,]$palabra,
        col="lightblue",
        main="Palabras más frecuentes",
        ylab="Frecuencia")

sub_df <- df[df$freq>=3,]

View(sub_df)

sub_df$colores <- cut(sub_df$freq,
                      breaks=c(0,sort(unique(sub_df$freq))),
                      labels=unique(sub_df$freq))

set.seed(2021)
sub_df$angulo <- sample(c(0,60,90),nrow(sub_df),replace=TRUE)
sub_df$angulo[1] <- 0

View(sub_df)


###########################

names(sub_df)

set.seed(2021)
ggplot(sub_df,aes(label=palabra,
                  size=freq,
                  color=colores,
                  angle=angulo)) +
  geom_text_wordcloud() +
  scale_size_area(max_size=15) +
  theme_minimal()

  
  
  









