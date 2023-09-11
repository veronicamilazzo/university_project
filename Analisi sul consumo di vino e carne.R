library(tidyverse)
library(lubridate)
library(VIM)
library(mice)
library(ggcorrplot)
library(gridExtra)
library(rpart)
library(partykit)
library(gbm)

clienti<- read.table("clienti.csv", header=T, sep="\t")
clienti1<- clienti %>% mutate(Age=year(today())-Year_Birth) %>% 
           select("ID", "Year_Birth", "Age", "Education":"Dt_Customer", "Complain", "MntWines":"MntGoldProds")
                         
str(clienti1)
clienti1$Education <- factor(clienti1$Education, levels=c("Basic","2n Cycle","Graduation","Master","PhD"))
clienti1$Marital_Status <- as.factor(clienti1$Marital_Status)
clienti1$Complain <- as.factor(clienti1$Complain)
clienti1$Dt_Customer <- dmy(clienti1$Dt_Customer)
str(clienti1)

### ANALISI DESCRITTIVA ###
### Duplicati ###

clienti1 %>% count(ID) %>% filter(n>1)

d<-which(duplicated(clienti1[,-1]))
length(d)
#201 duplicati, cioè clienti presenti più di una volta con diverso ID
clienti2<-clienti1[-d,]

### Riduzione livelli Marital_Status ###

levels(clienti2$Marital_Status)
clienti2[which(clienti2$Marital_Status %in% c("Absurd", "YOLO")),5] <- NA
clienti2$Marital_Status <- fct_collapse(clienti2$Marital_Status, Couple=c("Married","Together"),
                                                                 Alone=c("Single","Divorced","Widow","Alone"))
clienti2$Marital_Status <- droplevels(clienti2$Marital_Status)
levels(clienti2$Marital_Status)
                          
### Valori anomali e mancanti ###

summary(clienti2) 
# Income max 666666 
# Age max 129 

options(scipen=999)
boxplot(clienti2$Income, main="Distribuzione del reddito familiare annuo del cliente") #grafico 1
clienti2[which(clienti2$Income==666666),6] <- NA

clienti2 %>% filter(Age>100)
# probabilmente gli anni di nascita corretti sono: 2000, 1993 e 1999

clienti2[which(clienti2$Age>100),2:3] <- NA
summary(clienti2)

aggr(clienti2, cex.axis=0.7) #grafico 2
md.pattern(clienti2, rotate.names=T) #grafico 3
100-sum(complete.cases(clienti2))/nrow(clienti2)*100
# 1.5% di dati mancanti.

corr_mat<-cor(as.matrix(keep(clienti2,is.numeric)), use="complete.obs")
#grafico 4
ggcorrplot(corr_mat, hc.order=T, type = "lower", lab=TRUE, title="Correlazioni delle variabili numeriche", tl.cex=10, lab_size=3)

g1<-ggplot(data=clienti2, aes(Education, MntWines))+
    geom_boxplot()+theme_bw()

g2<-ggplot(data=clienti2[complete.cases(clienti2$Marital_Status),], aes(Marital_Status, MntWines))+
    geom_boxplot()+theme_bw()

g3<-ggplot(data=clienti2, aes(Complain, MntWines))+
  geom_boxplot()+theme_bw()

g4<-ggplot(data=clienti2, aes(Dt_Customer, MntWines))+
  geom_point(size=0.5)+
  geom_smooth(se=F)+theme_bw()

g5<-ggplot(data=clienti2, aes(Education, MntMeatProducts))+
  geom_boxplot()+theme_bw()

g6<-ggplot(data=clienti2[complete.cases(clienti2$Marital_Status),], aes(Marital_Status, MntMeatProducts))+
  geom_boxplot()+theme_bw()

g7<-ggplot(data=clienti2, aes(Complain, MntMeatProducts))+
  geom_boxplot()+theme_bw()

g8<-ggplot(data=clienti2, aes(Dt_Customer, MntMeatProducts))+
  geom_point(size=0.5)+
  geom_smooth(se=F)+theme_bw()

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8, nrow=2) #grafico 5

### BOOSTING E ALBERO DI REGRESSIONE ###

clienti2$Dt_Customer<-as.numeric(clienti2$Dt_Customer)

set.seed(123) 
train = sample(1:nrow(clienti2), nrow(clienti2)*0.75)
clienti.train = clienti2[train,]
clienti.test = clienti2[-train,]

### Vino ###

#ottimizzazione:
#per ntree: 5000; 
# per lamda: 0.001, 0.005, 0.01
# per interaction.depth: 1, 3, 6

set.seed(123)

primo<-gbm(MntWines~., data=clienti.train, interaction.depth = 1, n.trees=5000, shrinkage=0.001, train.fraction = 0.75)
secondo<-gbm(MntWines~., data=clienti.train, interaction.depth = 3, n.trees=5000, shrinkage=0.001, train.fraction = 0.75)
terzo<-gbm(MntWines~., data=clienti.train, interaction.depth = 6, n.trees=5000, shrinkage=0.001, train.fraction = 0.75)
quarto<-gbm(MntWines~., data=clienti.train, interaction.depth = 1, n.trees=5000, shrinkage=0.005, train.fraction = 0.75)
quinto<-gbm(MntWines~., data=clienti.train, interaction.depth = 3, n.trees=5000, shrinkage=0.005, train.fraction = 0.75)
sesto<-gbm(MntWines~., data=clienti.train, interaction.depth = 6, n.trees=5000, shrinkage=0.005, train.fraction = 0.75)
settimo<-gbm(MntWines~., data=clienti.train, interaction.depth = 1, n.trees=5000, shrinkage=0.01, train.fraction = 0.75)
ottavo<-gbm(MntWines~., data=clienti.train, interaction.depth = 3, n.trees=5000, shrinkage=0.01, train.fraction = 0.75)
nono<-gbm(MntWines~., data=clienti.train, interaction.depth = 6, n.trees=5000, shrinkage=0.01, train.fraction = 0.75)

matrice.wine<-data.frame(
  iteration=rep(seq(1,5000),9),
  test_Error=c(primo$valid.error,secondo$valid.error,terzo$valid.error,quarto$valid.error,quinto$valid.error,sesto$valid.error,settimo$valid.error,ottavo$valid.error,nono$valid.error),
  Procedura=c(rep("lamda=0.001, interaction.depth = 1",5000),rep("lamda=0.001, interaction.depth = 3",5000),rep("lamda=0.001, interaction.depth = 6",5000),
              rep("lamda=0.005, interaction.depth = 1",5000),rep("lamda=0.005, interaction.depth = 3",5000),rep("lamda=0.005, interaction.depth = 6",5000),
              rep("lamda=0.01, interaction.depth = 1",5000),rep("lamda=0.01, interaction.depth = 3",5000),rep("lamda=0.01, interaction.depth = 6",5000))
)

#grafico 6
ggplot(data = matrice.wine,mapping = aes(x=iteration,y=test_Error,color=Procedura))+
  geom_line()+
  theme_classic()+
  labs(title="Andamento del test error")

#confronto ultima iterazione
matrice.wine%>%filter(iteration==5000)
gbm.perf(terzo) 

#scelgo il boosting con lamda=0.001, depth=6, n.tree=4264

boost.wine<-gbm(MntWines~., data=clienti.train, interaction.depth = 6, n.trees=5000, shrinkage=0.001, train.fraction = 1)
summary(boost.wine, main="Importanza delle variabili") #grafico 7
table(clienti2$Complain)
y.boost.wine<-predict(boost.wine, clienti.test,n.trees=4264)

##############################

reg.tree.wine <- rpart(MntWines ~., data = clienti.train, cp=0.001)

plotcp(reg.tree.wine) #Potare
min(reg.tree.wine$cptable[,4])+reg.tree.wine$cptable[which.min(reg.tree.wine$cptable[,4]),5]
#scegliere cp corrispondente al primo valore xerror inferiore a 0.47602

printcp(reg.tree.wine)
pruned.tree.wine<-prune(reg.tree.wine, cp=0.0124750)
plot(as.party(pruned.tree.wine)) #grafico 8

y.reg.tree.wine<-predict(pruned.tree.wine, clienti.test, type="vector")

#grafico 9
x11()
plot(clienti.test[, 11],y.boost.wine, col=2, xlab = "Valori osservati", ylab = "Valori previsti", main="Confronto albero di regressione - boosting per l'importo speso per il vino" )
points(clienti.test[, 11], y.reg.tree.wine)
abline(0,1)
legend("bottomright", c("regression tree","boosting"), col=c(1,2), lty=1.5)

corr.wine<- c(cor(clienti.test[, 11],y.reg.tree.wine), cor(clienti.test[, 11], y.boost.wine)) #0.7621169 0.827037
legend("right", as.character(round(corr.wine,4)), text.col=1:2, title="Correlazioni")

### Carne ###

set.seed(123)

#ottimizzazione:
#per ntree: 5000; 
# per lamda: 0.001, 0.005, 0.01
# per interaction.depth: 1, 3, 6

primo<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 1, n.trees=5000, shrinkage=0.001, train.fraction = 0.75)
secondo<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 3, n.trees=5000, shrinkage=0.001, train.fraction = 0.75)
terzo<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 6, n.trees=5000, shrinkage=0.001, train.fraction = 0.75)
quarto<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 1, n.trees=5000, shrinkage=0.005, train.fraction = 0.75)
quinto<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 3, n.trees=5000, shrinkage=0.005, train.fraction = 0.75)
sesto<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 6, n.trees=5000, shrinkage=0.005, train.fraction = 0.75)
settimo<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 1, n.trees=5000, shrinkage=0.01, train.fraction = 0.75)
ottavo<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 3, n.trees=5000, shrinkage=0.01, train.fraction = 0.75)
nono<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 6, n.trees=5000, shrinkage=0.01, train.fraction = 0.75)

matrice.meat<-data.frame(
  iteration=rep(seq(1,5000),9),
  test_Error=c(primo$valid.error,secondo$valid.error,terzo$valid.error,quarto$valid.error,quinto$valid.error,sesto$valid.error,settimo$valid.error,ottavo$valid.error,nono$valid.error),
  Procedura=c(rep("lamda=0.001, interaction.depth = 1",5000),rep("lamda=0.001, interaction.depth = 3",5000),rep("lamda=0.001, interaction.depth = 6",5000),
              rep("lamda=0.005, interaction.depth = 1",5000),rep("lamda=0.005, interaction.depth = 3",5000),rep("lamda=0.005, interaction.depth = 6",5000),
              rep("lamda=0.01, interaction.depth = 1",5000),rep("lamda=0.01, interaction.depth = 3",5000),rep("lamda=0.01, interaction.depth = 6",5000))
)

#grafico 10
ggplot(data = matrice.meat,mapping = aes(x=iteration,y=test_Error,color=Procedura))+
  geom_line()+
  theme_classic()+
  labs(title="Andamento del test error")

#confronto ultima iterazione
matrice.meat%>%filter(iteration==5000)
gbm.perf(secondo)

#scelgo il boosting con lamda=0.001, depth=3, n.tree=2888

boost.meat<-gbm(MntMeatProducts~., data=clienti.train, interaction.depth = 3, n.trees=5000, shrinkage=0.001, train.fraction = 1)
summary(boost.meat, main="Importanza delle variabili") #grafico 11
y.boost.meat<-predict(boost.meat, clienti.test, n.trees=2888)

##############################

reg.tree.meat <- rpart(MntMeatProducts ~., data = clienti.train, cp=0.001)

plotcp(reg.tree.meat) #Potare
min(reg.tree.meat$cptable[,4])+reg.tree.meat$cptable[which.min(reg.tree.meat$cptable[,4]),5]
#scegliere cp corrispondente al primo valore xerror inferiore a 0.4634676

printcp(reg.tree.meat)
pruned.tree.meat<-prune(reg.tree.meat, cp=0.0182744)
plot(as.party(pruned.tree.meat)) #grafico 12

y.reg.tree.meat<-predict(pruned.tree.meat, clienti.test, type="vector")

#grafico 13
x11()
plot(clienti.test[, 13],y.boost.meat, col=2, xlab = "Valori osservati", ylab = "Valori previsti", main="Confronto albero di regressione - boosting per l'importo speso per la carne" )
points(clienti.test[, 13], y.reg.tree.meat)
abline(0,1)
legend("bottom", c("regression tree","boosting"), col=c(1,2), lty=1.5)

corr.meat<- c(cor(clienti.test[, 13],y.reg.tree.meat), cor(clienti.test[, 13], y.boost.meat)) #0.7102511 0.7387471
legend("topright", as.character(round(corr.meat,4)), text.col=1:2, title="Correlazioni")

### CONFRONTO DELL'EFFETTO DEL REDDITO CON L'ANALISI ESPLORATIVA ###

a<-plot(boost.wine,i="Income",ylim=c(0,1600), ylab="MntWines") 
b<-ggplot(data=clienti2, aes(Income, MntWines))+
   geom_point(cex=0.5)+
   geom_smooth(se=F)+ theme_bw()
grid.arrange(a,b,nrow=1) #grafico 14

c<-plot(boost.meat,i="Income",ylim=c(0,2100), ylab="MntMeatProducts") 
d<-ggplot(data=clienti2, aes(Income, MntMeatProducts))+
   geom_point(cex=0.5)+
   geom_smooth(se=F)+ theme_bw()
grid.arrange(c,d,nrow=1) #grafico 15
