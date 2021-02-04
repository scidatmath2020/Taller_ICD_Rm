setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\taller_icd_rm\\covid19_04_feb")

dir()
install.packages("tidyverse")
library(tidyverse)
covid <- read.csv("210203COVID19MEXICO.csv")

dim(covid)

4762394/40

m <- 119060

partido <- function(x,y){
  covid[c(x:y),]
}

# 1, 119060
# 119061, 238120
# 238121, 357180
# ....
# 4524281 4643340
# 4643341 4762394
dim(covid)

inicio <- seq(1,4762394,119060)
final <- seq(119060,4762394,119060)

final <- c(final,4762394)


inicio
final

cortes <- data.frame(inicio,final)

cortes

partido(cortes[3,1],cortes[3,2])

partido(cortes[23,1],cortes[23,2])

for(i in 1:40)
{
  partido(cortes[i,1],cortes[i,2])
}

setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\taller_icd_rm\\covid19_04_feb\\tablas")
# write.csv(partido(cortes[3,1],cortes[3,2]),"tabla3.csv")


#paste0

#paste0("HOla","Mundo")
for(i in 1:40)
{
  write.csv(partido(cortes[i,1],cortes[i,2]), paste0("Tabla",i,".csv"))
}


escritura <- function(i){
  write.csv(partido(cortes[i,1],cortes[i,2]), paste0("Tabla",i,".csv"))
}

escritura(3)

for(i in 1:40)
{
  escritura(i)
}

indices <- c(1:40)
lapply(indices,escritura)
