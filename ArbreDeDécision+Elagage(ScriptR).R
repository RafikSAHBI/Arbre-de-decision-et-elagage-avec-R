meteo<-c('soleil','soleil','soleil','soleil','nuages','nuages','nuages','nuages','soleil');
amis<-c('presnts','absents','presents','absents','absents','presents','absents','presents','absents');
vent<-c('faible','fort','fort','faible','faible','fort','fort','faible','faible')
jour<-c('weekend','semaine','semaine','semaine','weekend','weeekend','semaine','weeekend','weeekend');
decision<-c('oui','non','non','oui','non','non','non','oui','non');
df<-data.frame(meteo,amis,vent,jour,decision)
print(df)

#install.packages("rattle")
library("rattle")

#install.packages("RColorBrewer")
library("RColorBrewer")

library("rpart.plot")

library("rpart")

summary(df)

#plot(dfTree, uniform=TRUE, branch=0.5, margin=0.1)
#text(dfTree, all=FALSE, use.n=TRUE)

dfTree <- rpart(decision~meteo+amis+vent+jour,data=df,method="class",
                control=rpart.control(minsplit=0))

rpart.plot(dfTree, main="Barbecure Decision")
rpart.rules(dfTree)

#Affichage du cp optimal
print(dfTree$cptable[which.min(dfTree$cptable[,4]),1])

#Elagage de l’arbre avec le cp optimal
dfTreeOpt <- prune(dfTree,cp=dfTree$cptable[which.min(dfTree$cptable[,4]),1])

#Représentation graphique de l’arbre optimal
prp(dfTreeOpt,extra=1, main="Elagage")

#Prédiction du modèle sur les données de test
#dfTreeTestPredict<-predict(dfTreeOpt,newdata=dfTreeTest,type="class")

#Matrice de confusion
#MC<-table(dfTree$oui,dfTreeTestPredict)
#print(MC)

#Erreur de classement
#ErreurClassement<-1.0-(MC[1,1]+MC[2,2])/sum(MC)
#print(ErreurClassement)

#Taux de prédiction
#Prediction=MC[2,2]/sum(MC[2,])
#print(Prediction)

#Affichage des règles de construction de l’arbre
#print(dfTreeOpt)