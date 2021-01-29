library(data.table)
## sciezki plikow
setwd("C:/Users/Ania/Desktop/Studia-SMAD/Analiza danych w R/PD4/dane_benchmarkowe")
pliki<-list.files(path="C:/Users/Ania/Desktop/Studia-SMAD/Analiza danych w R/PD4/dane_benchmarkowe", pattern=".data.gz", full.names = F)



###################### HCLUST ###########################################

metody <- c("complete", "average","single", "median")

for (j in 1:length(metody)){
  
  wyniki <- data.frame(DANE = rep(0,length(pliki)),FM = rep(0,length(pliki)), AR = rep(0,length(pliki) ), FM_stand = rep(0,length(pliki)), AR_stand = rep(0,length(pliki)))
  for (i in 1:length(pliki))
    {

# wczytanie danych   
  dataset <- sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(pliki[i]))
  dane_temp   <- read.table(paste0(dataset, ".data.gz"))
  label_temp <- as.integer(read.table(paste0(dataset, ".labels0.gz"))[,1])
  
# ile etykiet 
ile_etykiet <- length(unique(label_temp))


#  hclust_complete
clusters <- hclust(dist(dane_temp), method = metody[j])
clusterCut <- cutree(clusters, ile_etykiet)
# table(clusterCut, as.vector(label_temp))

# liczenie indeksow 
fm <- dendextend::FM_index(label_temp,clusterCut) # FM
ar <- mclust::adjustedRandIndex(clusterCut,label_temp) #AR
wyniki[i,1] <- dataset
wyniki[i,2] <- fm
wyniki[i,3] <- ar


# standaryzacja 
dane_temp_stand <- apply(dane_temp, 2, function(x) {(x - mean(x))/ sd(x)})
clusters <- hclust(dist(dane_temp_stand), method = metody[j])
clusterCut <- cutree(clusters, ile_etykiet)

fm_stand <- dendextend::FM_index(label_temp,clusterCut) # FM
ar_stand <- mclust::adjustedRandIndex(clusterCut,label_temp) #AR
wyniki[i,4] <- fm_stand
wyniki[i,5] <- ar_stand

}

write.csv2(file = paste0("C:/Users/Ania/Desktop/Studia-SMAD/Analiza danych w R/PD4/wyniki/","hclust_", metody[j], ".csv"), wyniki, row.names = F )
}



################################################ GENIE ##########################################################################################

library(genie)
metody2 <- c(0.1, 0.9, 0.01)

for (j in 1:length(metody2)){
  
  wyniki <- data.frame(DANE = rep(0,length(pliki)),FM = rep(0,length(pliki)), AR = rep(0,length(pliki) ), FM_stand = rep(0,length(pliki)), AR_stand = rep(0,length(pliki)))
  for (i in 1:length(pliki))
  {
    
    # wczytanie danych   
    dataset <- sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(pliki[i]))
    dane_temp   <- read.table(paste0(dataset, ".data.gz"))
    label_temp <- as.integer(read.table(paste0(dataset, ".labels0.gz"))[,1])
    
    # ile etykiet 
    ile_etykiet <- length(unique(label_temp))
    
    
    #  hclust2
    gini<- hclust2( dist(dane_temp),  thresholdGini = metody2[j])
    gini_cut<- cutree(gini, ile_etykiet)
    
    
    # liczenie indeksow 
    fm <- dendextend::FM_index(label_temp,gini_cut) # FM
    ar <- mclust::adjustedRandIndex(gini_cut,label_temp) #AR
    wyniki[i,1]<- dataset
    wyniki[i,2] <- fm
    wyniki[i,3] <- ar
    
    
    # standaryzacja 
    dane_temp_stand <- apply(dane_temp, 2, function(x) {(x - mean(x))/ sd(x)})
    genie <- hclust2(dist(dane_temp_stand), method = metody2[j])
   genieCut <- cutree(genie, ile_etykiet)
    
    fm_stand <- dendextend::FM_index(label_temp,genieCut) # FM
    ar_stand <- mclust::adjustedRandIndex(genieCut,label_temp) #AR
    wyniki[i,4] <- fm_stand
    wyniki[i,5] <- ar_stand
    
  }
  
  write.csv2(file = paste0("C:/Users/Ania/Desktop/Studia-SMAD/Analiza danych w R/PD4/wyniki/","genie_", metody2[j], ".csv"), wyniki, row.names = F )
}

################# hybridHclust ######################################################

library(hybridHclust)

  wyniki <- data.frame(FM = rep(0,length(pliki)), AR = rep(0,length(pliki) ), FM_stand = rep(0,length(pliki)), AR_stand = rep(0,length(pliki)))
  for (i in 1:length(pliki))
  {
    
    # wczytanie danych   
    dataset <- sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(pliki[i]))
    dane_temp   <- read.table(paste0(dataset, ".data.gz"))
    label_temp <- as.integer(read.table(paste0(dataset, ".labels0.gz"))[,1])
    
    # ile etykiet 
    ile_etykiet <- length(unique(label_temp))
    
    
    # hybridHclust
    hybrid <- hybridHclust(t(dist(dane_temp)), themc = NULL, trace = FALSE)
    
    
    
    # liczenie indeksow 
    fm <- dendextend::FM_index(label_temp,gini_cut) # FM
    ar <- mclust::adjustedRandIndex(gini_cut,label_temp) #AR
    wyniki[i,1] <- fm
    wyniki[i,2] <- ar
    
    
    # standaryzacja 
    dane_temp_stand <- apply(dane_temp, 2, function(x) {(x - mean(x))/ sd(x)})
    genie <- hclust2(dist(dane_temp_stand), method = metody2[j])
    genieCut <- cutree(genie, ile_etykiet)
    
    fm_stand <- dendextend::FM_index(label_temp,genieCut) # FM
    ar_stand <- mclust::adjustedRandIndex(genieCut,label_temp) #AR
    wyniki[i,3] <- fm_stand
    wyniki[i,4] <- ar_stand
    
  }
  
  write.csv2(file = paste0("C:/Users/Ania/Desktop/Studia-SMAD/Analiza danych w R/PD4/wyniki/","hybridHclust_", metody2[j], ".csv"), wyniki, row.names = F )










