n=150
sigma=5.47
mu=10
# P(X_n<12)
P1=pnorm(12,mu,sigma/sqrt(n))
P1
#plus que 99% des individus ont un score moyen<12
Xbar=10.2
alpha=0.01
Z_alpha_2=qnorm(1-alpha/2,0,1)
Binf=Xbar-Z_alpha_2*sigma/sqrt(n)
Bsup=Xbar+Z_alpha_2*sigma/sqrt(n)
print(c(Binf,Bsup))
# 5.1) test d'hypothese paramétrique
#H_0: mu=8
#H_1: mu>8
#5.2)
Z_alpha=qnorm(1-alpha,0,1)
Zobs=(Xbar-8)/(sigma/sqrt(n))
if (Zobs<Z_alpha) {
    print("on accepte h0")
}else{ print("ON ACCEPTE H1")}

#Partie 2 : Tests d’hypothèse
#importation des donnees
data <- read.table('poids.txt')
data
poids=read.table(file = file.choose(),header = TRUE)
#extraction des informations:
str(poids)
#type de donnees:data.frame
#nombre d'observation: 200 obs pour
#nom de variables : caramels et chocolat

#un resumé plus detailé sur les variables:
summary(poids)
caram=poids$caramel
choco=poids$chocolat
caram
length(choco)
#H0: mu=100
#H1: mu >100
#sd(x) : ecart type de x
#calcul de l'exart type
sigma_caramel=sd(caram)
sigma_caramel
sigma_ch=sd(choco)
sigma_ch
#calcul de la moyenne emperique
XbarC=mean(caram)
XbarC
XbarCh=mean(choco)
XbarCh
#test d'hypothese parametrique pour les caramels
#H0 : mu=100
#H1 : mu>100
alp=0.05
Z_alp=qnorm(1-alp,0,1)
ZobsC=(XbarC-100)/(sigma_caramel/sqrt(200))
if(ZobsC<Z_alp){print("on accepte h0")} else{print("on accepte h1")}
#4)
#un echantillon de 20 parmi 200
Data=sample(caram,20)
Data
#install.packages("OneTwoSamples")
#on a besoin de package onetwosamles
#library(OneTwoSamples)
#sigma inconnue
t.test(Data,mu=100,alternative = "greater")$p_value


