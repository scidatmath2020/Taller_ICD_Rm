setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\taller_icd_rm\\envipe_2020")
library(foreign)
library(tidyverse)

tper_vic <- read.dbf("TPer_Vic1.dbf")
tmod_vic <- read.dbf("TMod_Vic.dbf")
tvivienda <- read.dbf("TVivienda.dbf")

descriptor_del <- read_tsv("descriptor_2020.csv")

catalogo_entidades <- data.frame("CVE_ENT"=unique(tvivienda$CVE_ENT),
                                 "NOM_ENT"=unique(tvivienda$NOM_ENT))


tmod_vic <- left_join(tmod_vic,descriptor_del,
                      by=c("BPCOD"="CODIGO"))


save(list=c("tper_vic","tmod_vic",
            "descriptor_del",
            "catalogo_entidades"),
     file="envipe_2020.RData")



tapply(tper_vic$FAC_ELE,list(tper_vic$CONDICION_VICTIMA,tper_vic$SEXO),
       sum)


data.frame(tapply(tmod_vic$FAC_DEL,tmod_vic$DESCRIPCION,sum))
names(tper_vic)