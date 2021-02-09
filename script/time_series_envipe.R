setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\taller_icd_rm\\envipe_clase")
dir()

library(foreign)
library(tidyverse)

# necesito que lea del 13 al 17
indices <- c(13:17)

####### Lectura con for
#tablas <- list()
#for(i in indices){
#  tablas[[i]] <- read.dbf(dir()[i])
#}

lectura <- function(x){
  read.dbf(dir()[x])
}

tablas <- lapply(indices,lectura)

#tablas[[1]]$FAC_DEL <- as.numeric(as.character(tablas[[1]]$FAC_DEL))
#tablas[[1]] <- tablas[[1]][!(tablas[[1]]$BP1_1 %in% c("88","99")),]
#tablas[[1]]$BP1_1 <- as.factor(paste(tablas[[1]]$BP1_1,2010,sep="/"))

#mapply

preprocesado <- function(x,y){
  x$FAC_DEL <- as.numeric(as.character(x[,"FAC_DEL"]))
  x <- x[!(x$BP1_1 %in% c("88","99")),]
  x$BP1_1 <- as.factor(paste(x$BP1_1,y,sep="/"))
  x
}

years <- c(2010:2014)

tmod <- mapply(preprocesado,tablas,years)

###########################

descriptor_1 <- read_tsv("descriptor_11_12.csv",col_names=TRUE)
descriptor_2 <- read_tsv("descriptor_13_20.csv",col_names=TRUE)

dim(tmod[[1]])

tmod[[1]] <- left_join(tmod[[1]],
                       descriptor_1,
                       by=c("BPCOD"="CODIGO"))

tmod[[2]] <- left_join(tmod[[2]],
                       descriptor_1,
                       by=c("BPCOD"="CODIGO"))

tmod[[3]] <- left_join(tmod[[3]],
                       descriptor_2,
                       by=c("BPCOD"="CODIGO"))

tmod[[4]] <- left_join(tmod[[4]],
                       descriptor_2,
                       by=c("BPCOD"="CODIGO"))

tmod[[5]] <- left_join(tmod[[5]],
                       descriptor_2,
                       by=c("BPCOD"="CODIGO"))


#####################

del_2010 <- tapply(tmod[[1]]$FAC_DEL,
       list(tmod[[1]]$BP1_1,tmod[[1]]$DESCRIPCION),
       function(x){sum(x,na.rm=TRUE)})

#### Hacer la serie de tiempo

del_2010 <- data.frame(rownames(del_2010),
                       del_2010,
                       total=rowSums(del_2010,na.rm=TRUE))

#install.packages("xts")
library(xts)

del_2010 <- xts(del_2010[,-1],
                order.by=as.yearmon(del_2010[,1],format="%m/%Y"))

color <- rainbow(ncol(del_2010))

plot.xts(del_2010[,-15],col=color,
         legend.loc = "topright",
         cex=0.5,
         cex.axis = 0.7,
         main="Total de delitos por tipo")

pdf("ts_total_2011.pdf")
plot.xts(del_2010[,15],col=1,
         legend.loc = "topright",
         cex=0.5,
         cex.axis = 0.7,
         main="Total de delitos")
dev.off()



