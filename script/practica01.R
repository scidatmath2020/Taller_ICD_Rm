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


