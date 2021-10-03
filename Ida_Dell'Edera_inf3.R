#--------------------------------Esercizio2---------------------------------#

set.seed(1)

a<-0
b<-10
n<-1000
k<-100000
stimaIntegrale<-numeric(0)


#PuntoA#

#applichiamo il metodo Monte Carlo per il calcolo dell'integrale di f facendo 100000 simulazioni 

for (i in 1:k) {
#genera n numeri casuali uniformi tra a e b
x <- runif(n, a, b)
f <- (exp(-x) / (1 + log(1+x)))
stimaIntegrale<-c(stimaIntegrale, (b-a) * mean(f))
}

#memorizzo nella variabile rinominata Integrale la media delle stime calcolate con metodo Monte Carlo
Integrale<-mean(stimaIntegrale)
print(Integrale)

#PuntoB#

p<-0.95
alpha<-1-p
alphaMezzi<-alpha/2

#data una distribuzione gaussiana standard qnorm mi consente di calcolare la probabilità in un certo valore della variabile aleatoria
z=qnorm(p+alphaMezzi)

#applico la formula per trovare l'intervallo di confidenza richiesto
c1<-Integrale-z*sd(stimaIntegrale)
c2<-Integrale+z*sd(stimaIntegrale)

#stampo i valori che ho calcolato precedentemente 
print(c1)
print(c2)

#stampo i tre valori
#c1=0,5684323, c2=0,7682387 mean(stimaIntegrale)=0,668355
c(c1, mean(stimaIntegrale), c2)


#--------------------------------Esercizio3---------------------------------#
set.seed(1)

A<-1
B<-2
N<-1000
K<-10000

StimaIntegrale<-numeric(0)
StimaIntegrale2<-numeric(0)

#con il metodo Monte Carlo calcolo il valore atteso della funzione F

for (i in 1:K) {
#genera n numeri casuali uniformi tra A e B
x<-runif(N, A, B)
funzione<- 8/(3*x^2)
StimaIntegrale<-c(StimaIntegrale, (B-A) * mean(funzione))
}

#in questo modo vado a calcolare il valore atteso della funzione F
#E=1,333415
E<-mean(StimaIntegrale)
print(E)

set.seed(1)

#calcolo E[x^2] utilizzando sempre il metodo Monte Carlo 
for (i in 1:K) {
  x<-runif(N, A, B)
  #poichè faccio x*f(x)
  funzione<- (8*(x^2))/(3*(x^3))
  StimaIntegrale2<-c(StimaIntegrale2, (B-A) * mean(funzione))
  E2<-mean(StimaIntegrale2)
}

print(E2)
hist(StimaIntegrale2)

#dopo aver calcolato E[x] ed E[x^2], posso calcolare la varianza
#V=0,07044611
V<-E2-((E)^2)
print(V)

#--------------------------Esericizio1--------------------------------------#
set.seed(1)

ALPHA<-1
LAMBDA<-0.1

numeroIterazioni<-100
arrayStimaIntegrale<-numeric(0)

#PuntoA#

#utilizzando il metodo Monte Carlo andiamo ad applicare la Legge dei Grandi numeri 
#per mostrare la convergenza della media delle simulazioni al valore atteso come richiesto
for (i in 0:numeroIterazioni) {
#dato che i và da 0 a 100, definiamo l'iesima iterazione in questo modo
j<-i*100+1
#generiamo i numeri pseudocasuali con densità gamma di parametri
u<-rgamma(j, shape=ALPHA, rate=LAMBDA)
#calcolo dell'integrale
arrayStimaIntegrale<-c(arrayStimaIntegrale, mean(u))
}

#calcolo del valore atteso della v.a. Gamma che è uguale a E[x]=alpha/lambda
#valoreAtteso=10
valoreAtteso<-ALPHA/LAMBDA
print(valoreAtteso)

plot.ts(arrayStimaIntegrale, ylim=c(9,11))
abline(h=valoreAtteso, col="red", lwd=1)
#la media tende al valore atteso (Legge dei Grandi Numeri)
#mean(arrayStimaIntegrale)=9,923019
mean(arrayStimaIntegrale)

#PuntoB#

dimSucc<-500
k<-500000

arrayStime<-numeric(0)

#con il metodo Monte Carlo applichiamo il Teorema Centrale del Limite 

for(i in 1:k){
#generiamo i numeri pseudocasuali con la funzione rgamma (vista nel punto precedente)
  app<-rgamma(dimSucc, shape = ALPHA, rate=LAMBDA)
  arrayStime<-c(arrayStime, mean(app)) 
}

#rappresentiamo con un istogramma il risultato
hist(arrayStime,100)

#la distribuzione delle medie in arrayStime ha una distribuzione gaussiana 



