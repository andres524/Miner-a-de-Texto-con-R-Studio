#MINER�A DE TEXTO APLICADO AL AN�LISIS DE CANCIONES (COLECCI�N_GRUPO: ENANITOS VERDES)
#PRESENTADO POR: ANDR�S GUILLERMO ANGARITA MONROY - LUZ MARINA DELGADO MONROY


#�QU� ES LA MINER�A DE TEXTO ?

#La miner�a de textos es el proceso de analizar colecciones de materiales de texto con el objeto de capturar los temas y conceptos 
#clave y descubrir las relaciones ocultas y las tendencias existentes sin necesidad de conocer las palabras o los t�rminos exactos 
#que los autores han utilizado para expresar dichos conceptos. La miner�a de textos y la acci�n de recuperar informaci�n son conceptos 
#que a veces se confunden, aunque son bastante diferentes. Una recuperaci�n precisa de la informaci�n y su almacenamiento supone un 
#reto importante, pero la extracci�n y administraci�n de contenido de calidad, de terminolog�a y de las relaciones contenidas en la 
#informaci�n son procesos cruciales y determinantes.


#MOTIVACI�N
#Hoy en d�a, se maneja cada vez m�s informaci�n en formatos no estructurados o semiestructurados, como mensajes de correo electr�nico, 
#notas de los centros de servicio al cliente, respuestas de encuestas con final abierto, fuentes de noticias, formularios web, etc. 
#Esta abundancia de informaci�n se presenta como un problema para muchas empresas a la hora de preguntarse c�mo recopilar, explorar y 
#aprovechar toda esta informaci�n.



#Paso 1. Instalar las librer�as requeridas

install.packages("tidyverse")

library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(readr)
library(cluster)
library(readxl)
library(plyr)
library(dplyr)



#Paso 2. Cargar la data (Canciones)
#Nota: A criterio del desarrollador, de eligi� el formato .txt para trabajar las canciones, pero puede usarse cualquier otro formato
#por ejemplo excel, pdf, etc..

C1<-read.csv("C:\\Users\\ANDRES ANGARITA\\Desktop\\TRABAJO FINAL ANALISIS MULTIVARIANTE\\MINERIA DE TEXTO\\C1.txt", sep=";")
View(C1)

C2 <- read.csv("C:\\Users\\ANDRES ANGARITA\\Desktop\\TRABAJO FINAL ANALISIS MULTIVARIANTE\\MINERIA DE TEXTO\\C2.txt", sep=";")
View(C2)

C3<- read.csv("C:\\Users\\ANDRES ANGARITA\\Desktop\\TRABAJO FINAL ANALISIS MULTIVARIANTE\\MINERIA DE TEXTO\\C3.txt", sep=";")
View(C3)

C4<-read.csv("C:\\Users\\ANDRES ANGARITA\\Desktop\\TRABAJO FINAL ANALISIS MULTIVARIANTE\\MINERIA DE TEXTO\\C4.txt", sep=";")
View(C4)
  
C5<-read.csv("C:\\Users\\ANDRES ANGARITA\\Desktop\\TRABAJO FINAL ANALISIS MULTIVARIANTE\\MINERIA DE TEXTO\\C5.txt", sep=";")
View(C5)

#Paso 3. Concatenar la data en una sola estructura
consol <- bind_rows(C1,C2,C3,C4,C5)


#Librer�as alternas para fechas de manera consistente (lubridate) y funciones para realizar tareas comunes de an�lisis y 
#presentaci�n de datos (zoo y scales). 

library(lubridate)
library(zoo)
library(scales)


#Paso 4. Creaci�n del Corpus(Cuerpo) y Limpieza de la data

# se empieza la limpieza de los datos, espacios, pasar a minuscula, palabras que no dicen nada 
corpus=VCorpus(VectorSource(consol))

ejemplo <- tm_map(corpus, stripWhitespace)
ejemplo <- tm_map(ejemplo, content_transformer(tolower))
ejemplo <- tm_map(ejemplo, removeWords, stopwords("spanish"))
ejemplo <- tm_map(ejemplo, removePunctuation)
ejemplo <- tm_map(ejemplo, stemDocument, language = "spanish")

#Adcional a lo anterior, es posible crear una bolsa de palabras que desea remover de la data consolidada
mystopwords <- c("hacen","tan","y","para","entonces","asi","no","de","tanto","sin","embargo","muy","tambien","van")
ejemplo <- tm_map(ejemplo, removeWords, mystopwords)


#Paso 5. Convertir el Cuerpo en una tabla con datos estructurados

dtm <- DocumentTermMatrix(ejemplo)
inspect(dtm)
dim(dtm)


#Paso 6.  An�lisis del Corpus
#Mapearemos nuestro Corpus como un documento de texto plano

a=as.matrix(dtm)

#Librer�a alterna necesaria para el an�lisis
library(wordcloud2)

#Nube de palabras
rownames(a)<-c("Canci�n 1","Canci�n 2","Canci�n 3","Canci�n 4","Canci�n 5")
term.freq <- colSums(as.matrix(dtm))
term.freq <- subset(term.freq, term.freq >= 1)
df <- data.frame(term = names(term.freq), freq = term.freq)
#Opcion 1 (Nube de palabras)
wordcloud2(df, size = 0.3,  shape="star", backgroundColor = 'black', fontFamily="Enanitos Verdes")
#Opcion 2 (Nube de palabras)
wordcloud2(df, size = 1,shape = 'star')

#Tabla de frecuencias 
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terminos") + ylab("Cuenta") + coord_flip()

#Tabla de frecuencias que muestra las palabras con mayor incidencia o repetici�n
barplot(df[1:35,]$freq, las = 2, names.arg = df[1:35,]$term,
        col ="blue", main ="Palabras m�s frecuentes",
        ylab = "frecuencia de palabras")


#Opcion 3 (Nube de palabras)
# se calcula la frecuencia de palabras y se ordenan por frecuencia
word.freq <- sort(colSums(a), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

# plot word cloud
library(wordcloud)
wordcloud(word = names(word.freq), frec = word.freq, min.freq = 1,
          random.order = F, colors = pal)

#Opcion 4 (Nube de palabras)
# o tambien puede hacer la siguiente nube de palabras
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1,
          random.order = F, colors=brewer.pal(8, "Dark2"))


#Paso 7. Agrupamiento jer�rquico (Hierarchical clustering)

#clustering
install.packages("FactoMineR") # includes a module for conducting CA
install.packages("factoextra") # library to prettify our CA graphs

# import the libraries:
library(FactoMineR)
library(factoextra)

#An�lisis de clusters dtm

tdm2 <- removeSparseTerms(dtm, sparse = 0.95)
# la transpuesta de una matriz: m1=t(tdm2)
m2 <- as.matrix(tdm2)
rownames(m2)<-c("Canci�n 1","Canci�n 2","Canci�n 3","Canci�n 4","Canci�n 5")

# cluster terms
matrizDist <- dist(scale(m2))
fit <- hclust(matrizDist, method = "ward.D2")
plot(fit,hang = -1)
rect.hclust(fit, k = 2) # el �rbol se recorta en 2 clusters
k2 <- kmeans(m2, centers = 2)
str(k2)
fviz_cluster(k2, data = m2)

