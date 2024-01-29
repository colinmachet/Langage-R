summary(resultats_pres2022$Part_Macron)


moyenneMacron <- mean(resultats_pres2022$Part_Macron)
moyenneMacron


paste("La moyenne des % de vote Macron est de ", moyenneMacron)


paste("La moyenne des parts de vote Macron est de ", round(moyenneMacron, digits=2), "%")


Requete1 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 10 , ]
View (Requete1)

Requete2 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 30 & resultats_pres2022$Part_LePen < 60 , ]
View (Requete2)

VoteJadot2040 <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_Jadot < 40 , ]

voteRochefort <- resultats_pres2022 [resultats_pres2022$Libelle =='Rochefort' & resultats_pres2022$Code_dep == '17' ,]
View (voteRochefort)

voteSaintes <- resultats_pres2022 [resultats_pres2022$Libelle =='Saintes' & resultats_pres2022$Code_dep == '17' ,]
> View (voteSaintes)

write.csv(voteSaintes,"c:/SIG/LUP_SIG/Semestre_1/TD_semestre_1/TD_R/vote_Saintes.csv",row.names = FALSE)

voteCandidats1_4 <- resultats_pres2022 [ , c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
View (voteCandidats1_4)

voteSaintes_Candidats1_3 <- voteSaintes [, c("Part_Macron", "Part_LePen", "Part_Melenchon")]
View (voteSaintes_Candidats1_3)

voteCandidats1_4$colonneTest <- NA

voteCandidats1_4$colonneTest <- voteCandidats1_4$Part_Macron +
+     voteCandidats1_4$Part_Pecresse
> View (voteCandidats1_4)

voteCandidats1_4$colonneTest <- ifelse(voteCandidats1_4$Part_Macron > 25,
voteCandidats1_4$Part_Macron + voteCandidats1_4$Part_Pecresse,
NA)

Transpo <- voteCandidats1_4 %>% pivot_longer(

cols = c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse),
names_to="Candidat", values_to="Parts_vote")

View(Transpo)

voteSaintes_Candidats1_3$MacronSup30<- NA

voteSaintes_Candidats1_3$colonne <- ifelse(voteSaintes_Candidats1_3$Part_Macron > 30,
"MacronSup30", NA)

sapply(voteCandidats1_4, mean)

tapply(resultats_pres2022$Part_abs,resultats_pres2022$Libelle_dep, mean )

dataList <- c("Part_Macron", "Part_LePen", "Part_Melenchon")
for(i in dataList){
print(i)
}

dataList <- c("Part_Macron", "Part_LePen", "Part_Melenchon")
for(i in dataList){
print(summary(resultats_pres2022[, i]))
}

dataList <- c("Part_Macron", "Part_LePen", "Part_Melenchon")
for(i in dataList){

moy <- round(mean(as.numeric(unlist(resultats_pres2022[, i]))), digits=2)
print (paste("Moyenne ", i, ": ", moy, " %"))

}

tapply(resultats_pres2022$Part_abs,resultats_pres2022$Libelle_dep, max )

# On définit les noms des candidats dans une variable nommée « col1 »
col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse")

# On reprend les % de vote issus de l’extraction précédente, dans une variable nommée « col2 »
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon),
mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse))

# On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »
data <- data.frame(group=col1, value=col2)

# On crée le graphique :
ggplot(data, aes(x="", y=value , fill=group)) +
geom_bar(stat="identity", width=1) +
geom_col() +
coord_polar("y", start=0) +

# On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
scale_fill_manual(values = c("#0a3895", "#b831f3",
"#f33157", "#6091f6"))

# On définit les noms des candidats dans une variable nommée « col1 »
col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse")

# On reprend les % de vote issus de l’extraction précédente, dans une variable nommée « col2 »
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon),
mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse))

# On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »
data <- data.frame(group=col1, value=col2)

# On crée le graphique :
ggplot(data= data, aes(x=reorder(group, -value), y=value, fill=group)) +
geom_bar(stat="identity")+

# On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
geom_text(aes(label=round(value, digits=2)), vjust=1.6, color="white", size=3.5)+
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))