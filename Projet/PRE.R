#NA traitement
cleaning=function(fusion) {
  row = nrow(fusion)
  row
  col = ncol(fusion)
  col
  n = sum(is.na(fusion))
  n
  
  library("stringr")
  mean_fusion=vector(,length(fusion))
  mean_fusion
  for (i in 1:length(fusion)) {
    mean_fusion[i]=summary(fusion[i])[4]
  }
  mean_fusion
  for (i in 1:length(mean_fusion)) {
    mean_fusion[i]=str_squish(mean_fusion[i])
  }
  mean_fusion
  for (i in 1:length(mean_fusion)) {
    mean_fusion[i]=substr(mean_fusion[i],7,length(mean_fusion))
  }
  mean_fusion
  mean_fusion=na.omit(mean_fusion)
  mean_fusion
  #Remplacement des NA par la moyenne ou 0
  missing<-function(row, col, n,fusion) {
    if ((n/(row*col)*100) < 10) {
      fusion[is.na(fusion)]=0
    }
    else {
      for (i in 1:length(mean_fusion)) {
        fusion[is.na(fusion)]=mean_fusion[i];
      }
    }
    return(fusion)
  }
  
  fusion_clean =missing(row, col, n, fusion)
  summary(fusion_clean)
  return(fusion_clean)
}


import = function(annee, separ) {
  chemin=readline(prompt="Veuillez saisir le chemin des fichiers en rajoutant un '\\' à la fin : " )
  setwd(chemin)
  fileC = paste("caracteristiques_",annee,".csv",sep="")
  caract = read.csv(file = fileC, header=TRUE, sep = separ,dec=",",row.names = NULL)
  #View(caract)
  fileL = paste("lieux_",annee,".csv",sep="")
  lieux = read.csv(file = fileL, header=TRUE, sep = separ,dec=",",row.names = NULL)
  #View(lieux)
  fileU = paste("usagers_",annee,".csv",sep="")
  usg = read.csv(file = fileU, header=TRUE, sep = separ,dec=",",row.names = NULL)
  #View(usg)
  fileV = paste("vehicules_",annee,".csv",sep="")
  veh = read.csv(file = fileV, header=TRUE, sep = separ,dec=",",row.names = NULL)
  #View(veh)
  
  #Fusionner
  all_fich = merge(caract, lieux,all=T)
  head(all_fich) 
  
  all_fich1 = merge(usg, veh, all=T)
  
  fusion = merge(all_fich,all_fich1, all=T)
  summary(fusion)
  View(fusion)
  
  fusion_clean=cleaning(fusion)
  return(fusion_clean)
}

import_2009 = function(annee, separ1, separ2) {
  chemin=readline(prompt="Veuillez saisir le chemin des fichiers en rajoutant un '\\' à la fin : " )
  setwd(chemin)
  fileC = paste("caracteristiques_",annee,".csv",sep="")
  caract = read.csv(file = fileC, header=TRUE, sep = separ1,dec=",",row.names = NULL)
  #View(caract)
  fileL = paste("lieux_",annee,".csv",sep="")
  lieux = read.csv(file = fileL, header=TRUE, sep = separ2,dec=",",row.names = NULL)
  #View(lieux)
  fileU = paste("usagers_",annee,".csv",sep="")
  usg = read.csv(file = fileU, header=TRUE, sep = separ2,dec=",",row.names = NULL)
  #View(usg)
  fileV = paste("vehicules_",annee,".csv",sep="")
  veh = read.csv(file = fileV, header=TRUE, sep = separ2,dec=",",row.names = NULL)
  #View(veh)
  
  #Fusionner
  all_fich = merge(caract, lieux,all=T)
  head(all_fich) 
  
  all_fich1 = merge(usg, veh, all=T)
  
  fusion = merge(all_fich,all_fich1, all=T)
  summary(fusion)
  View(fusion)
  
  
  fusion_clean=cleaning(fusion)
  return(fusion_clean)
}


fusion_clean_2019=import(2019, ";")
View(fusion_clean_2019)
summary(fusion_clean_2019)

fusion_clean_2018=import(2018, ",")
View(fusion_clean_2018)
summary(fusion_clean_2018)

fusion_clean_2017=import(2017, ",")
View(fusion_clean_2017)
summary(fusion_clean_2017)

fusion_clean_2016=import(2016, ",")
View(fusion_clean_2016)
summary(fusion_clean_2016)

fusion_clean_2015=import(2015, ",")
View(fusion_clean_2015)
summary(fusion_clean_2015)

fusion_clean_2014=import(2014, ",")
View(fusion_clean_2014)
summary(fusion_clean_2014)

fusion_clean_2013=import(2013, ",")
View(fusion_clean_2013)
summary(fusion_clean_2013)

fusion_clean_2012=import(2012, ",")
View(fusion_clean_2012)
summary(fusion_clean_2012)

fusion_clean_2011=import(2011, ",")
View(fusion_clean_2011)
summary(fusion_clean_2011)

fusion_clean_2010=import(2010, ",")
View(fusion_clean_2010)
summary(fusion_clean_2010)

fusion_clean_2009=import_2009(2009, "\t", ",")
View(fusion_clean_2009)
summary(fusion_clean_2009)

fusion_clean_2008=import(2008, ",")
View(fusion_clean_2008)
summary(fusion_clean_2008)

fusion_clean_2007=import(2007, ",")
View(fusion_clean_2007)
summary(fusion_clean_2007)

fusion_clean_2006=import(2006, ",")
View(fusion_clean_2006)
summary(fusion_clean_2006)

fusion_clean_2005=import(2005, ",")
View(fusion_clean_2005)
summary(fusion_clean_2005)


fusion_final_grv=rbind(fusion_clean_2005,fusion_clean_2006,fusion_clean_2007,fusion_clean_2008,
                   fusion_clean_2009,fusion_clean_2010,fusion_clean_2011,fusion_clean_2012,
                   fusion_clean_2013,fusion_clean_2014,fusion_clean_2015,fusion_clean_2016,
                   fusion_clean_2017,fusion_clean_2018)
View(fusion_final_grv)

write.table(fusion_final_grv,"E:/3eme/S6/R/Projet/Fichier/Fusion_2005_2018.csv",sep=",", row.names = FALSE)

fusion_final_hsp=rbind(fusion_clean_2005,fusion_clean_2006,fusion_clean_2007,fusion_clean_2008,
                   fusion_clean_2009,fusion_clean_2010,fusion_clean_2011,fusion_clean_2012,
                   fusion_clean_2013,fusion_clean_2014,fusion_clean_2015,fusion_clean_2016,
                   fusion_clean_2017)
View(fusion_final_hsp)

write.table(fusion_final_hsp,"E:/3eme/S6/R/Projet/Fichier/Fusion_2005_2017.csv",sep=",", row.names = FALSE)
