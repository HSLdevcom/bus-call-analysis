print("Nyt tultiin printtaamaan!")
ajankohtien_nimet <- c("7.00 - 9.00","17.00 - 20.00","15.00 - 17.00", "9.00 - 15.00", "20.00 - 0.00") #ei käytetä toistaiseksi
varisuora <- c("orange","blue", "red","yellow", "grey")

png(paste("",reitti,"_s_1_Erotus_ajo_ja_pysakkiaika_arkisin_",alkuvk,"_",loppuvk,".png", sep = ""),
    width= 1600, height= 1200, pointsize = 5, res=60)
#png("Ero_aikatauluun_kokeilu.png",width= 1500, height= 800, pointsize = 8, res=30)

par(mfrow=c(3,1))
boxplot(kimppa_1$ohitusaika_ero~kimppa_1$ajankohta*kimppa_1$pysakkijarj, 
        col=varisuora,
        ylim= c(-100,390),
        main= paste("Ero aikatauluun pysäkeittäin arkipäivinä reitillä: ",reitti,", suunta: 1, viikot: ",alkuvk," - ",loppuvk,"", sep = ""), 
        #label = kimppa$tulopysakki,
        
        las = 2, 
        ylab ="Erotus aikatauluun sekunneissa",
        #xlab= "Direction 1",
        par(mar = c(1, 3.5, 1, 1)+ 0.1), #pohja, vasen, yläosa, oikea, joku skaalauskerroin
        par(cex.lab=3.5), #labin tekstikoko
        par(cex.axis=0.1), #nimien tekstikoko
        par(cex.main=7.1) #otsikon tekstikoko
        
        
)
abline(h=0, col="green",lty=2)
abline(h=-59, col="red",lty=2)
abline(h=119, col="red",lty=2)
text(0,0,"Aikataulu", cex = 5.9)
text(0,135,"+120 sec", cex = 5.9)
text(0,-44,"-59 sec", cex = 5.9)
text(18,290, paste("Ero aikatauluun arkisin: ",reitti,", s: 1, vk: ",alkuvk," - ",loppuvk,"", sep = ""),cex = 5.9)

legend("topleft",
       legend=ajankohtien_nimet,
       fill=varisuora,
       cex = 3.9)

#legend(x=4.5,y=22,legend=c("Aamuruuhka","Ilta","Iltaruuhka", "Päivä", "Yö"),fill=varisuora)

boxplot(kimppa_1$ajoaika~kimppa_1$ajankohta*kimppa_1$pysakkijarj, 
        col=varisuora,
        ylim= c(30,180),
        main= paste("Ajoajat pysäkeittäin arkipäivinä reitillä: ",reitti,", suunta: 1, viikot: ",alkuvk," - ",loppuvk,"", sep = ""), 
        #label = kimppa$tulopysakki,
        
        las = 2, 
        ylab ="Ajoaika sekunneissa",
        #xlab= "Direction 1",
        par(mar = c(0.1, 3.5, 1.8, 1)+ 0.1), #pohja, vasen, yläosa, oikea, joku skaalauskerroin
        par(cex.lab=0.5), #labin tekstikoko
        par(cex.axis=0.1), #nimien tekstikoko
        par(cex.main=7.1) #otsikon tekstikoko
        
)
abline(h=30, col="green",lty=2)
abline(h=60, col="green",lty=2)
abline(h=90, col="green",lty=2)
abline(h=120, col="green",lty=2)
text(0,35,"30 sec", cex = 5.9)
text(0,65,"60 sec", cex = 5.9)
text(0,95,"90 sec", cex = 5.9)
text(0,125,"120 sec", cex = 5.9)
text(18,150, paste("Ajoajat arkisin: ",reitti,", s: 1, vk: ",alkuvk," - ",loppuvk,"", sep = ""),cex = 5.9)
legend("topleft",
       legend=ajankohtien_nimet,
       fill=varisuora,
       cex = 3.9
)

boxplot(kimppa_1$pysakkiaika~kimppa_1$ajankohta*kimppa_1$pysakkijarj, 
        col=varisuora,
        ylim= c(0,40),
        main= paste("Pysäkkiajat pysäkeittäin arkipäivinä reitillä: ",reitti,", suunta: 1, viikot: ",alkuvk," - ",loppuvk,"", sep = ""), 
        #label = kimppa$tulopysakki,
        
        las = 2, 
        ylab ="Pysäkkiajat sekunneissa",
        #xlab= "Direction 1",
        par(mar = c(1, 3.5, 1.8, 1)+ 0.1), #pohja, vasen, yläosa, oikea, joku skaalauskerroin
        par(cex.lab=0.5), #labin tekstikoko
        par(cex.axis=0.1), #nimien tekstikoko
        par(cex.main=7.1) #otsikon tekstikoko
        
)
abline(h=5, col="green",lty=2)
abline(h=10, col="green",lty=2)
abline(h=15, col="green",lty=2)
abline(h=20, col="green",lty=2)
text(0,6,"5 sec", cex = 5.9)
text(0,11,"10 sec", cex = 5.9)
text(0,16,"15 sec", cex = 5.9)
text(0,21,"20 sec", cex = 5.9)
text(18,30, paste("Pysäkkiajat arkisin: ",reitti,", s: 1, vk: ",alkuvk," - ",loppuvk,"", sep = ""),cex = 5.9)

legend("topleft",
       legend=ajankohtien_nimet,
       fill=varisuora,
       cex = 3.9
)


dev.off()
print("eka graafi ok ja laite sammutettu")

#------------------------------------------------------------------------------------------------------Toinen suunta

png(paste("",reitti,"_s_2_Erotus_ajo_ja_pysakkiaika_arkisin_",alkuvk,"_",loppuvk,".png", sep = ""),
    width= 1600, height= 1200, pointsize = 5, res=60)
#png("Ero_aikatauluun_kokeilu.png",width= 1500, height= 800, pointsize = 8, res=30)

par(mfrow=c(3,1))
boxplot(kimppa_2$ohitusaika_ero~kimppa_2$ajankohta*kimppa_2$pysakkijarj, 
        col=varisuora,
        ylim= c(-100,390),
        main= paste("Ero aikatauluun pysäkeittäin arkipäivinä reitillä: ",reitti,", suunta: 2, viikot: ",alkuvk," - ",loppuvk,"", sep = ""), 
        #label = kimppa$tulopysakki,
        
        las = 2, 
        ylab ="Erotus aikatauluun sekunneissa",
        #xlab= "Direction 1",
        par(mar = c(1, 3.5, 1, 1)+ 0.1), #pohja, vasen, yläosa, oikea, joku skaalauskerroin
        par(cex.lab=3.5), #labin tekstikoko
        par(cex.axis=0.1), #nimien tekstikoko
        par(cex.main=7.1) #otsikon tekstikoko
        
        
)
abline(h=0, col="green",lty=2)
abline(h=-59, col="red",lty=2)
abline(h=119, col="red",lty=2)
text(0,0,"Aikataulu", cex = 5.9)
text(0,135,"+120 sec", cex = 5.9)
text(0,-44,"-59 sec", cex = 5.9)
text(18,290, paste("Ero aikatauluun arkisin: ",reitti,", s: 2, vk: ",alkuvk," - ",loppuvk,"", sep = ""),cex = 5.9)

legend("topleft",
       legend=ajankohtien_nimet,
       fill=varisuora,
       cex = 3.9)

#legend(x=4.5,y=22,legend=c("Aamuruuhka","Ilta","Iltaruuhka", "Päivä", "Yö"),fill=varisuora)

boxplot(kimppa_2$ajoaika~kimppa_2$ajankohta*kimppa_2$pysakkijarj, 
        col=varisuora,
        ylim= c(30,180),
        main= paste("Ajoajat pysäkeittäin arkipäivinä reitillä: ",reitti,", suunta: 2, viikot: ",alkuvk," - ",loppuvk,"", sep = ""), 
        #label = kimppa$tulopysakki,
        
        las = 2, 
        ylab ="Ajoaika sekunneissa",
        #xlab= "Direction 1",
        par(mar = c(0.1, 3.5, 1.8, 1)+ 0.1), #pohja, vasen, yläosa, oikea, joku skaalauskerroin
        par(cex.lab=0.5), #labin tekstikoko
        par(cex.axis=0.1), #nimien tekstikoko
        par(cex.main=7.1) #otsikon tekstikoko
        
)
abline(h=30, col="green",lty=2)
abline(h=60, col="green",lty=2)
abline(h=90, col="green",lty=2)
abline(h=120, col="green",lty=2)
text(0,35,"30 sec", cex = 5.9)
text(0,65,"60 sec", cex = 5.9)
text(0,95,"90 sec", cex = 5.9)
text(0,125,"120 sec", cex = 5.9)
text(18,150, paste("Ajoajat arkisin: ",reitti,", s: 2, vk: ",alkuvk," - ",loppuvk,"", sep = ""),cex = 5.9)

legend("topleft",
       legend=ajankohtien_nimet,
       fill=varisuora,
       cex = 3.9
)

boxplot(kimppa_2$pysakkiaika~kimppa_2$ajankohta*kimppa_2$pysakkijarj, 
        col=varisuora,
        ylim= c(0,40),
        main= paste("Pysäkkiajat pysäkeittäin arkipäivinä reitillä: ",reitti,", suunta: 2, viikot: ",alkuvk," - ",loppuvk,"", sep = ""), 
        #label = kimppa$tulopysakki,
        
        las = 2, 
        ylab ="Pysäkkiajat sekunneissa",
        #xlab= "Direction 1",
        par(mar = c(1, 3.5, 1.8, 1)+ 0.1), #pohja, vasen, yläosa, oikea, joku skaalauskerroin
        par(cex.lab=0.5), #labin tekstikoko
        par(cex.axis=0.1), #nimien tekstikoko
        par(cex.main=7.1) #otsikon tekstikoko
        
)
abline(h=5, col="green",lty=2)
abline(h=10, col="green",lty=2)
abline(h=15, col="green",lty=2)
abline(h=20, col="green",lty=2)
text(0,6,"5 sec", cex = 5.9)
text(0,11,"10 sec", cex = 5.9)
text(0,16,"15 sec", cex = 5.9)
text(0,21,"20 sec", cex = 5.9)
text(18,30, paste("Pysäkkiajat arkisin: ",reitti,", s: 2, vk: ",alkuvk," - ",loppuvk,"", sep = ""),cex = 5.9)

legend("topleft",
       legend=ajankohtien_nimet,
       fill=varisuora,
       cex = 3.9
)


dev.off()
print("toka graafi ok ja laite sammutettu")

rm(ajankohtien_nimet)
rm(varisuora)