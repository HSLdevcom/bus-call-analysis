#rm(list = ls())
library(proto)
library("ggplot2")
library(plyr)
library(RODBC) 
#library(sm)
library(lattice)


channel <- odbcConnect("palvelin", uid="tunnus", pwd="salasana", believeNRows=FALSE)


alkuvk <- 201439
loppuvk <- 201446

linjaluettelokysely <- paste("SELECT 
                DISTINCT reitti
                
                FROM ajoaika_gps
                WHERE 
                ajoaika_gps.vuosiviikko BETWEEN ",alkuvk," AND ",loppuvk,"
                
                AND ajoaika_gps.ptyyppi IN(1,2,3,4,5)
                 
              ",sep = "")

linjaluettelo <- sqlQuery(channel, linjaluettelokysely, as.is = TRUE)

#-----------------------------------------------------------------------------

for(i in 1:length(linjaluettelo$reitti)){
  print(i)
  reitti <- as.character(linjaluettelo$reitti[i])
  print(reitti)

kysely <- paste("SELECT 
                ajoaika_gps.liikpaiva, ajoaika_gps.suunta, ajoaika_gps.lahtopysakki, ajoaika_gps.tulopysakki, ajoaika_gps.pysakkijarj, 
                ajoaika_gps.pysakkityyppi, ajoaika_gps.tuloaika_time, ajoaika_gps.ohitusaika_ero, ajoaika_gps.ajoaika, 
                ajoaika_gps.pysakkiaika,  ajoaika_gps.pysakkialueella_oloaika , ajoaika_gps.pysahdyskpl, 
                ajoaika_gps.kumul_pysakkiaika, ajoaika_gps.kumul_pysakkialueella_oloaika, ajoaika_gps.kumul_matkaaika
                
                FROM ajoaika_gps
                WHERE ajoaika_gps.reitti = '",reitti,"'
                AND ajoaika_gps.vuosiviikko BETWEEN ",alkuvk," AND ",loppuvk,"
                
                AND ajoaika_gps.ptyyppi IN(1,2,3,4,5)
                AND ajoaika_gps.virhe_pysakki = 0
                AND ajoaika_gps.virhe_gps = 0
                AND ajoaika_gps.virhe_askellus = 0
                AND ajoaika_gps.virhe_ohitusaika = 0
                AND ajoaika_gps.virhe_lahto = 0 
                ",sep = "")

raakadata <- sqlQuery(channel, kysely, as.is = TRUE)

#--------------------------------------------------------------------------------------


raakadata$bonusaika <- as.POSIXlt(raakadata$tuloaika_time, format = "%H:%M:%S", "GMT") # the current time in GMT
#raakadata$bonusaika2 <- strptime(raakadata$tuloaika_time, format = "%H:%M:%S", "GMT") # the current time in GMT
#raakadata$bonuspaiva <- as.POSIXlt(raakadata$liikpaiva, format = "%Y-%m:%d", "GMT") # the current time in GMT
aamuruuhka <- subset(raakadata, raakadata$bonusaika > as.POSIXlt("07:00:00", format = "%H:%M:%S", "GMT") & raakadata$bonusaika < as.POSIXlt("09:00:00", format = "%H:%M:%S", "GMT")) 
paiva <- subset(raakadata, raakadata$bonusaika > as.POSIXlt("9:00:01", format = "%H:%M:%S", "GMT") & raakadata$bonusaika < as.POSIXlt("15:00:00", format = "%H:%M:%S", "GMT")) 
iltaruuhka <- subset(raakadata, raakadata$bonusaika > as.POSIXlt("15:00:01", format = "%H:%M:%S", "GMT") & raakadata$bonusaika < as.POSIXlt("17:00:00", format = "%H:%M:%S", "GMT")) 
ilta <- subset(raakadata, raakadata$bonusaika > as.POSIXlt("17:00:01", format = "%H:%M:%S", "GMT") & raakadata$bonusaika < as.POSIXlt("20:00:00", format = "%H:%M:%S", "GMT"))
yo <- subset(raakadata, raakadata$bonusaika > as.POSIXlt("20:00:01", format = "%H:%M:%S", "GMT") & raakadata$bonusaika < as.POSIXlt("23:59:59", format = "%H:%M:%S", "GMT")) #toimii

ifelse(length(aamuruuhka$ajoaika) < 1, aamuruuhka <- feikkivektori, aamuruuhka <- aamuruuhka)
ifelse(length(paiva$ajoaika) < 1, paiva <- feikkivektori, paiva <- paiva)
ifelse(length(iltaruuhka$ajoaika) < 1, iltaruuhka <- feikkivektori, iltaruuhka <- iltaruuhka)
ifelse(length(ilta$ajoaika) < 1, ilta <- feikkivektori, ilta <- ilta)
ifelse(length(yo$ajoaika) < 1, yo <- feikkivektori, yo <- yo)

aamuruuhka$ajankohta <- "aamuruuhka"
paiva$ajankohta <- "paiva"
iltaruuhka$ajankohta <- "iltaruuhka"
ilta$ajankohta <- "ilta"
yo$ajankohta <- "yo"

kimppa <- rbind.fill(aamuruuhka, paiva, iltaruuhka, ilta, yo) #yhdistää päällekkäin ja tekee NA:t tyhjiin
kimppa$pysakkityyppi <- as.integer(kimppa$pysakkityyppi)

kimppa_1 <- subset(kimppa, kimppa$suunta == 1)
kimppa_2 <- subset(kimppa, kimppa$suunta == 2)
ifelse(length(kimppa_1$ajoaika) < 1, kimppa_1 <- feikkivektori, kimppa_1 <- kimppa_1)
ifelse(length(kimppa_2$ajoaika) < 1, kimppa_2 <- feikkivektori, kimppa_2 <- kimppa_2)
ifelse(length(kimppa_1$ajoaika) < 3, kimppa_1$ajankohta <- "apuva", kimppa_1 <- kimppa_1)
ifelse(length(kimppa_2$ajoaika) < 3, kimppa_2$ajankohta <- "apuva", kimppa_2 <- kimppa_2)
ifelse(grepl(paste("1751"), reitti), reitti <- "6666", reitti <- reitti)

####HUOM! Feikkivektorit ja muut säätämiset liittyvät siihen, että plottaus menee sekaisin tyhjistä muuttujista. Sen takia sinne keksitään pseudoarvoja. Tietokannasta palautuu linjaluetteloon myös välillä kirjoitusvirheitä tms., joita vastaan täytyy jollain tavalla suojautua

source("Erotus_ajoaika_ja_pysaika_boxplot.R")
#)))))
rm(raakadata)
rm(aamuruuhka)
rm(paiva)
rm(iltaruuhka)
rm(ilta)
rm(yo)
rm(kimppa)
rm(kimppa_1)
rm(kimppa_2)
print("tyhjennetty työtila")

}
odbcClose(channel)  #Mviimeinen sulkee oven
print("KOKO HOMMA HOIDETTU!!!")
