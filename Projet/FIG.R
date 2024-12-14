#Q1:
fichier=file.choose()
Q1=read.csv(file =fichier,sep=",")
value=c(Q1[1,1],Q1[2,1])
barplot(value, names.arg = c("2005-2018","2019"), col = "#1B98E0", main = "Proportion des accidents mortels")

#Q2:
fichier=file.choose()
Q2=read.csv(file =fichier,sep=",")
val=c(Q2[1,1],Q2[2,1])
barplot(val, names.arg = c("2005-2018","2019"), col = "purple", main = "Proportion des blessés hospitalisés",color =I("black"))

#Q3:
fichier=file.choose()
catgr_veh_golbal=read.csv(file =fichier,sep=",")
barplot(c(catgr_veh_golbal$X2.Roues,catgr_veh_golbal$Voiture,catgr_veh_golbal$Poids.lourd),
        names.arg = c("2 Roues","Voiture","Poids Lourd"), col = "black", main = "Catégorie véhicule")

#Q4:
fichier=file.choose()
catgr_route=read.csv(file =fichier,sep=",")

barplot(c(catgr_route$Autoroute,catgr_route$Route.Nationale,catgr_route$Route.départementale),
        names.arg = c("Autoroute","Route Nationale","Route Départementale"), col = "brown", border = "white", main = "Catégorie route")

#Q5:
fichier=file.choose()
danger_f_h=read.csv(file =fichier,sep=",")

library(plotrix)
noms = c("Hommes","Femmes")
val = c(danger_f_h[1,1],danger_f_h[1,2])
pct = round(val/sum(val)*100)
new_labels=paste(noms, "-", pct, "%", sep="")
pie(val, labels = new_labels, main="Dangerosité des conducteurs", col=c("#fc5c65","#fd9644"))
