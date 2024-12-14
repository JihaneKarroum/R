#Importaion du tableau fusionner
fichier=file.choose()
fusion_final_grv=read.csv(file =fichier, sep=",")
View(fusion_final_grv)

fichier=file.choose()
fusion_final_hsp=read.csv(file =fichier, sep=",")
View(fusion_final_hsp)

#Q1
#extraction de la colonne grav
proportion=function(n,fusion_final) {
  grv = fusion_final$grav
  grv
  count=0
  for (i in 1:length(grv)) {
    if(grv[i]==n){
      count = count + 1
    }
  }
  prop_grv = count / nrow(fusion_final)
  return(prop_grv)
}

prop_grav=proportion(2,fusion_final_grv)
prop_grav
prop_grav_2019=proportion(2,fusion_clean_2019)
prop_grav_2019
Q1=matrix(c(prop_grav,prop_grav_2019))
Q1

write.table(Q1,"E:/3eme/S6/R/Projet/Fichier/Q1.csv",sep=",", row.names = FALSE)

#Q2
prop_grav=proportion(3,fusion_final_hsp)
prop_grav
prop_grav_2019=proportion(3,fusion_clean_2019)
prop_grav_2019

Q2=matrix(c(prop_grav,prop_grav_2019))
Q2

write.table(Q2,"E:/3eme/S6/R/Projet/Fichier/Q2.csv",sep=",", row.names = FALSE)


#Q3
#2 Roues: [1,2,4,5,30,31,32,33,34]
#Voiture: [3,6,7,8,9,10,11,12,35,36]
#Poids lourd: [13,14,15,17,18,19,20,21,37,38,39,40]

categorie_veh=function(table) {
  table_catv=matrix(nrow = 1, ncol=3,byrow=T)
  colnames(table_catv)=c("2 Roues","Voiture","Poids lourd")
  cp_r=0
  cp_v=0
  cp_p=0
  
  for (vc in table$catv) {
    if(vc == 1 || vc == 2 || vc == 4 || vc == 5 || vc == 30 ||vc == 31 || vc == 32 || vc == 33 || vc == 34) {
      cp_r=cp_r+1
    }
    else if(vc == 3 || vc == 6 || vc == 7 || vc == 8 || vc == 9 ||vc == 10 || vc == 11 || vc == 12 || vc == 35 || vc == 36) {
      cp_v=cp_v+1
    }
    else if(vc == 13 || vc == 14 || vc == 15 || vc == 17 || vc == 18 || vc == 19 ||vc == 20 || vc == 21 || vc == 37 || vc == 38 || vc == 39 || vc == 40) {
      cp_p=cp_p+1
    }
  }
  
  table_catv[1,1]=cp_r
  table_catv[1,2]=cp_v
  table_catv[1,3]=cp_p
  return(table_catv)
}

catgr_veh_golbal=categorie_veh(fusion_final_grv)
catgr_veh_golbal

write.table(catgr_veh_golbal,"E:/3eme/S6/R/Projet/Fichier/Q3.csv",sep=",", row.names = FALSE)

#Q4
categorie_route=function(table) {
  table_catv=matrix(nrow = 1, ncol=3,byrow=T)
  colnames(table_catv)=c("Autoroute","Route Nationale ","Route départementale")
  cp_a=0
  cp_n=0
  cp_d=0
  
  for (vc in table$catv) {
    if(vc == 1) {
      cp_a=cp_a+1
    }
    else if(vc == 2) {
      cp_n=cp_n+1
    }
    else if(vc == 3) {
      cp_d=cp_d+1
    }
  }
  
  table_catv[1,1]=cp_a
  table_catv[1,2]=cp_n
  table_catv[1,3]=cp_d
  return(table_catv)
}

catgr_route=categorie_route(fusion_final_grv)
catgr_route

write.table(catgr_route,"E:/3eme/S6/R/Projet/Fichier/Q4.csv",sep=",", row.names = FALSE)

#Q5:
#choc = [1, 2, 3]
#sexe = [1,2]

femme_homme=function(fusion_final_grv){
  danger=matrix(nrow = 1, ncol=2,byrow=T)
  colnames(danger)=c("Homme","Femme")
  h=0
  f=0
  for (i in 1:length(fusion_final_grv$sexe)) {
    if(fusion_final_grv$sexe[i]==1 && (fusion_final_grv$choc[i]==1 || fusion_final_grv$choc[i]==2 ||fusion_final_grv$choc[i]==3)){
      h=h+1
    }
    else if(fusion_final_grv$sexe[i]==2 && (fusion_final_grv$choc[i]==1 || fusion_final_grv$choc[i]==2 ||fusion_final_grv$choc[i]==3)) {
      f=f+1
    }
  }
  danger[1,1]=h
  danger[1,2]=f
  return(danger)
}

danger_f_h=femme_homme(fusion_final_grv)
danger_f_h

write.table(danger_f_h,"E:/3eme/S6/R/Projet/Fichier/Q5.csv",sep=",", row.names = FALSE)
