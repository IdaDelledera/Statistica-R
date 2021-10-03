#-------------------------------Esercizio1-------------------------------------#

set.seed(1)
x<-Boston

#a partire dal dataset di Boston man mano si vanno a non considerare le variabili dove il valore Pr(>|t|)
#è più alto fra tutti i valori
out.lm<-lm(formula = medv~crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo age poichè ha il valore maggiore pari esattamente a:0.958229
out.lm<-lm(formula = medv~crim + zn + indus + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo indus poichè ha il valore maggiore pari esattamente a:0.737989
out.lm<-lm(formula = medv~crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo chas poichè ha il valore maggiore pari esattamente a:0.001551
out.lm<-lm(formula = medv~crim + zn  + nox + rm + dis + rad + tax + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo zn poichè ha il valore maggiore pari esattamente a:0.000864
out.lm<-lm(formula = medv~crim + nox + rm + dis + rad + tax + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo tax poichè ha il valore maggiore pari esattamente a:0.001982 
out.lm<-lm(formula = medv~crim + nox + rm + dis + rad + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo crim poichè ha il valore maggiore pari esattamente a:0.002252
out.lm<-lm(formula = medv~ + nox + rm + dis + rad + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo rad poichè ha il valore maggiore pari esattamente a:0.0152
out.lm<-lm(formula = medv~ + nox + rm + dis + ptratio + black + lstat, data = x)

summary(out.lm)

#escludo black poichè ha il valore maggiore pari esattamente a:0.000381

out.lm<-lm(formula = medv~ + nox + rm + dis + ptratio + lstat, data = x)

summary(out.lm)

#le variabili predittrici sono: nox, rm, dis, ptratio, lstat 
#Alla fine avremo che Adjusted R-squared:  0.7052 

#--------------------------------Esercizio2------------------------------------#

set.seed(1)
#PuntoA
x<-Admission

n<-dim(Admission)[1]
train<-sort(sample(1:n,300,replace = FALSE))
datatrain<-Admission[train,]
datatest<-Admission[-train,]

#considero solo le variabili gre e gpa
out.glm<-glm(formula = admit~gre+gpa, family=binomial, data=datatrain)
summary(out.glm)

#vado ad effettuare la previsione di glm, come tipo="response"
previsione_glm<-predict.glm(out.glm, newdata = datatest, type="response")
summary(previsione_glm)
#calcolo il range della previsione di glm calcolata precedentemente
range(previsione_glm)
#inizializzo questo array
Arrayprevisione<-numeric(100)

#laddove la previsione di glm è >0.36, assegno 1 come valore nella posizione j-esima dell'array
j<-which(previsione_glm>0.36)
Arrayprevisione[j]<-1
#vengono memorizzati tutti i valori della variabile binaria admit
previsione_glm.true<-datatest$admit
#posso a questo punto ccostruire la tabella glm
table_glm<-table(previsione_glm.true,Arrayprevisione)
table_glm
#quota percentuale di glm(che effettuo dopo aver costruito la tabella)
percent_glm<-100*sum(diag(table_glm))/sum(table_glm)
percent_glm


 
library(FNN)
#in admit inseriamo vector, che è un vettore nel quale andiamo a memorizzare i dati 
#admit di datatrain
admit<-as.vector(datatrain$admit)
out.knn<-knn(datatrain[,2:3], datatest[,2:3], admit, k=9)
#costruisco la tabella di knn 
tabella_knn<-table(previsione_glm.true, out.knn)
tabella_knn
#calcolo la percentuale di knn come richiesto
percent_knn<-100*sum(diag(tabella_knn))/sum(tabella_knn)
percent_knn

#per quanto riguarda il classificatore logistico, vediamo che ha una percentuale 
#pari a 64, mentre KNN ha una percentuale di 63, quindi il logistico è meglio di knn


#PuntoB#
set.seed(1)
#aggiungo anche la variabile rank
out.glm<-glm(formula = admit~gre+gpa+rank, family=binomial, data=datatrain)
summary(out.glm)

previsioneConRank<-predict.glm(out.glm, newdata = datatest, type="response")
#calcolo la previsione del range con Rank
range(previsioneConRank)
#inizializzo l'array di previsione con rank
ArrayPrevisioneConRank<-numeric(100)
#laddove la previsione con rank è >0.34, assegno 1 come valore nella posizione j-esima dell'array
j<-which(previsioneConRank>0.34)
ArrayPrevisioneConRank[j]<-1

#costruisco la tabella con rank
tabella_con_rank<-table(ArrayPrevisioneConRank, datatest$admit)
tabella_con_rank

#calcolo la percentuale con rank
percent_con_rank<-100*sum(diag(tabella_con_rank))/sum(tabella_con_rank)
percent_con_rank

#percent_con_rank = 64

#AIC è minore ripetto alla prima chiamata di 
#'out.glm<-glm(formula = admit~gre+gpa, family=binomial, data=datatrain)
#per questo possiamo dire che migliora poichè nella prima chiamata 
#AIC valeva 354.49 mentre nella seconda chiamata 
#'glm(formula = admit ~ gre + gpa + rank, family = binomial, data = datatrain)'
#AIC vale 343.86

#---------------------------------Esercizio3-----------------------------------#
set.seed(1)
#seleziono del dataset tutte le colonne eccetto la colonna Type, e la memorizzo
#in x
x<-Wine[,2:14]

#applicazione metodo k-means
out.km<-kmeans(x, centers = 3)
plot(x, col=out.km$cluster, pch=20)
cluster.km<-as.vector(out.km$cluster)
plot(cluster.km, col=cluster.km)

#applicazione metodo Hierarchical Cluster
Distance<-dist(scale(x))
out.hc<-hclust(Distance)
plot(out.hc)

#attraverso questa istruzione posso tagliare l'albero a k=3
cluster.hc<-as.vector(cutree(out.hc, k=3))
plot(cluster.hc, col=cluster.hc)
#nella variabile TypeWine memroizzo la colonna relativa a Type che ho 
#precedentemente escluso
TypeWine<-Wine$Type

#a questo punto ho tutto per poter costruire la tabella di classificazione per k-means
tabella_km<-table(TypeWine,cluster.km)
tabella_km

#dopo aver terminato la costruzione della tabella k-means, costruisco la tabella 
#di classifiocazione di hc
tabella_hc<-table(TypeWine, cluster.hc)
tabella_hc


#posso caclolcare a questo punto la percentuale sia di tabella_km(k-means) e sia 
#la tabella_hc(hierarchical cluster)
percentuale_km<-100*sum(diag(tabella_km))/sum(tabella_km)
percentuale_km

percentuale_hc<-100*sum(diag(tabella_hc))/sum(tabella_hc)
percentuale_hc

#La percentuale più alta è di hc, che vale 83,70787, mentre quella di km vale
#70,22472








