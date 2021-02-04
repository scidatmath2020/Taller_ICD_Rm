setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\taller_icd_rm")
getwd()

install.packages("DMwR")
library(DMwR)
mfv(c(1,1,1,3,3,1,2,4,4,4,4))

#Con esto hacemos comentarios 
dir()  #dir() sirve para enlistar los archivos que tengo en la ubicación

maraton <- read.csv("Maraton_NY.csv", header = TRUE, row.names = 1)

head(maraton,3)
dim(maraton)
str(maraton)
names(maraton)

#######################

maraton[ ,2]  #esto me va a dar la columna 2 de maraton
maraton[2, ]  #esto me va a dar la fila 2 de maraton

subtabla1 <- maraton[,c(4,5,2)]

subtabla2 <- maraton[   ,c(15:30)]

subtable3 <- maraton[c(10:20,30:45),   ]

subtable4 <- maraton[c(10:20,30:45),c(3,5)]

dim(subtable4)

subtable5 <- t(subtable4)

names(maraton)
subtable6 <- maraton[,c("gender","home")]

head(subtable6)

subtable7 <- maraton$gender #maraton[,2]


class(maraton$gender)
class(maraton[,2])

#########################

names(maraton)

maraton$age > 30


maraton_edad_1 <- maraton[maraton$age > 30,  ]

maraton_edad_2 <- maraton[!(maraton$age > 30),  ]

maraton[maraton$time<=200 & !(maraton$gender == "Male"),]

maraton[maraton$time<=200 | maraton$age == 30,]

maraton[maraton$time<=200 | maraton$time == 250,]

maraton[maraton$home %in% c("MEX","ARG","BRA","PER","COL","VEN","PAR") & maraton$age <30,][,5]


latam <- maraton[maraton$home %in% c("MEX","ARG","BRA","PER","COL","VEN","PAR"),]

write.csv(latam,"tabla_latam.csv",row.names=FALSE)
 
latam[latam$age>20,]

########

View(maraton)

maraton$categoria <- cut(maraton$time,breaks=3,labels=c("alto","medio","bajo"))

max(maraton$time)

intervalos <- c(-Inf,100,200,300,Inf)
maraton$nva_categoria <- cut(maraton$time,breaks=intervalos,labels=c("c1","c2"))

summary(maraton$nva_categoria)


dim(latam)

min(latam$time)

which.min(latam$time)
latam[37,]


latam[which.min(latam$time),]$age

ejemplo <- data.frame(V1=c("a","b","c","d","e"),V2=c(1,1,6,2,6))

which.max(ejemplo$V2)
ejemplo

ejemplo[ejemplo$V2 == min(ejemplo$V2),]

contaminacion <- read.csv("contaminacion.csv", header = TRUE)

install.packages("mice")
library(mice)

md.pattern(contaminacion)

conta0 <- contaminacion[-33,]

md.pattern(conta0)

View(conta0)


is.na(conta0$CN)
conta0$Numero_de_empresas[is.na(conta0$CN)]

conta0[is.na(conta0$CN),]

conta0
complete.cases(conta0)

conta0[complete.cases(conta0),]  #na.omit(conta0)

md.pattern(na.omit(conta0))

conta0

sum(conta0$CN)

### NA+numero = NA; NA*numero=NA; NA/número = NA; número/NA

sum(conta0$CN,na.rm=TRUE)
mean(conta0$CN,na.rm=TRUE)


conta0$CN_medias <- ifelse(is.na(conta0$CN),mean(conta0$CN,na.rm=TRUE),conta0$CN)

B <- data.frame(V1 = c("a","b","c","d","e"),V2 = c("practica","hola","hola",NA,"adios"))
B

install.packages("modeest")
library(modeest)

B$V2_modas <- ifelse(is.na(B$V2),mfv(B$V2,na_rm=TRUE),B$V2)
B

summary(factor(B$V2))

summary(factor(c("a","b","c")))


####################

install.packages("DMwR")
library(DMwR)

conta0
conta1 <- knnImputation(conta0)

View(conta1)
######################

duplicated(conta0)
View(conta0)
which(duplicated(conta0))


conta_sin_dupl <- unique(conta0)

dim(conta_sin_dupl)

duplicated(conta_sin_dupl)

conta0[26,]

#############

help(rivers)

boxplot(rivers,horizontal=TRUE)

boxplot.stats(rivers)$out

View(rivers)

rivers[rivers %in% as.vector(boxplot.stats(rivers)$out)]

rivers_data <- data.frame(rivers)

rivers_data[rivers_data[,1] %in% as.vector(boxplot.stats(rivers_data[,1])$out),]
  
  
as.vector(boxplot.stats(rivers_data[,1])$out)

boxplot.stats(rivers_data[,1])$out

boxplot(rivers,horizontal=TRUE)

minimo1 <- min(boxplot.stats(rivers)$out)
rivers1 <- rivers[rivers<minimo1]
boxplot(rivers1,horizontal=TRUE)


minimo2 <- min(boxplot.stats(rivers1)$out)
rivers2 <- rivers1[rivers1<minimo2]
boxplot(rivers2,horizontal=TRUE)

minimo3 <- min(boxplot.stats(rivers2)$out)
rivers3 <- rivers2[rivers2<minimo3]
boxplot(rivers3,horizontal=TRUE)


rivers_1 <- rivers[rivers<max(boxplot.stats(rivers)$out)]
boxplot(rivers_1,horizontal=TRUE)

rivers_2 <- rivers_1[rivers_1<max(boxplot.stats(rivers_1)$out)]
boxplot(rivers_2,horizontal=TRUE)


boxplot(maraton[,c(3,5)])

install.packages("scales")
library(scales)

View(rescale(maraton$age))

### El reescalamiento de valores consiste en tomar cada valor x y aplicar la 
### fórmula (x-x_min)/(x_max-x_min)


min(maraton$age)
max(maraton$age)

(maraton$age-min(maraton$age))/(max(maraton$age)-min(maraton$age))

### La normalización de valores consiste en tomar cada valor x y aplicar la
### fórmula (x-media)/desv

names(maraton)

graficador <- function(x){
  boxplot(rescale(x))
}

reescalado <- sapply(maraton[,c(3,5)],rescale)

View(reescalado)

boxplot(reescalado)

lista_graficas <- lapply(maraton[,c(3,5)],graficador)

lista_graficas[[2]]

####################

sort(latam$place)
order(latam$place)

View(latam[order(latam$place),])

summary(latam$home)

#### Quiénes son los tres peruanos más rápidos

per <- latam[latam$home == "PER",]

per[order(per$place),][1:3,]

A <- data.frame(V1=c("a","b","a","c"),V2=c(5,3,2,1))

A[order(A$V1,A$V2),]

A[order(A$V1,A$V2,decreasing = c("TRUE","FALSE")),]

tapply(A$V2,A$V1,mean)

B <- data.frame(V1=c("a","b","a","c","b","b","c"),
                V2=c(5,3,2,1,3,2,4),
                V3=c("h","m","m","h","h","m","m"))

tapply(B$V2,list(B$V1=="a",B$V3),sum)

rbind(A,B[,c(1,2)])


L=list(A,B[,-3])

L[[2]]

do.call(rbind,L)