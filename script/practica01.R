setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\taller_icd_rm")
getwd()

#Con esto hacemos comentarios 
dir()  #dir() sirve para enlistar los archivos que tengo en la ubicación

maraton <- read.csv("Maraton_NY.csv", header = TRUE, row.names = 1)

View(maraton)

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




