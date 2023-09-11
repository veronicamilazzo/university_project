library(qcc)
library(tidyverse)
library(gridExtra)
load("~/Università/LM 2° anno/valutazione statistica ed economica in sanità/metodi statistici per la valutazione in sanità/Esame/struttureosp.Rdata")

ospedale2bis <- as.data.frame(ospedale2)

ospedale4bis <- as.data.frame(cbind(as.vector(ospedale4), c(rep(1,31),rep(2,31),rep(3,31),rep(4,31))))
colnames(ospedale4bis) <- c("tempo", "rilevazione")
ospedale4bis$rilevazione <- as.factor(ospedale4bis$rilevazione)

ospedale5 <- as.data.frame(ospedale5)
ospedale5$siringa <- as.factor(ospedale5$siringa)
ospedale5$macchina <- as.factor(ospedale5$macchina)

#ANALISI DESCRITTIVA

summary(ospedale3$data)

g1 <- ggplot(data = ospedale2bis ) +
      geom_boxplot(aes(y=ospedale2), fill="lightblue") + 
      labs(x = "a", y = "peso alla nascita")+
      ylim(2000,4000)+
      theme_bw()

summary(ospedale2)

g2 <- ggplot(data = ospedale3 ) +
      geom_boxplot(aes(y=peso), fill="lightblue", outlier.colour="red") + 
      labs(x = "b", y = "peso alla nascita")+
      ylim(2000,4000)+
      theme_bw()

summary(ospedale3$peso)

grid.arrange(g1,g2,nrow=1) #grafico 1

#grafico 2
ggplot(data = ospedale4bis ) +
  geom_boxplot(aes(rilevazione, tempo), fill="lightblue", outlier.colour="red") + 
  labs(y = "tempo richiesto")+
  theme_bw()

apply(ospedale4, 2, mean)

#grafico 3
ggplot(data = ospedale5 ) +
  geom_boxplot(aes(macchina, tempoerog, fill=siringa), outlier.colour="red") + 
  scale_fill_brewer(palette = "Blues") +
  labs(y = "tempo erogazione")+
  theme_bw()

#OSPEDALE 2

R <- abs(diff(ospedale2))
R.bar <- mean(R)
LCL <- 0
UCL <- 3.627*R.bar

plot(R, type="o", ylim=c(-0.1,1300), main="Carta di controllo MR")
abline(h=LCL,lty=2)
abline(h=UCL,lty=2)
abline(h=R.bar)

#X.bar <- mean(ospedale2)
#s <- R.bar/1.128
#LCL <- X.bar - 3*s
#UCL <- X.bar + 3*s
#plot(ospedale2, type="o", ylim=c(2250,4200), main="Carta di controllo X", ylab="peso alla nascita")
#abline(h=LCL,lty=2)
#abline(h=UCL,lty=2)
#abline(h=X.bar)

q11 = qcc(ospedale2, type="xbar.one", title="Carta di controllo X")

#OSPEDALE 3

peso = with(ospedale3, qcc.groups(peso, data))

qcc(peso, type = "S", title="Carta di controllo S")
q <- qcc(peso, type = "xbar", title="Carta di controllo X-bar")
process.capability(q, c(2500,4500), 3300)

#OSPEDALE 4

qcc(ospedale4, type = "xbar")
shapiro.test(ospedale4[,1])
shapiro.test(ospedale4[,2])
shapiro.test(ospedale4[,3])
shapiro.test(ospedale4[,4])

q1 <- cusum(ospedale4, title="Carta di controllo CUSUM")
summary(q1)

q2 <- ewma(ospedale4, title="Carta di controllo EWMA")
summary(q2)

#OSPEDALE 5

a <- anova(lm(tempoerog ~ siringa+macchina+siringa*macchina,
         data = ospedale5))

s2e <- a$`Mean Sq`[4]
s2alpha <- (a$`Mean Sq`[2] - a$`Mean Sq`[3])/(3*6) # negativo quindi posto uguale a 0
s2beta <- (a$`Mean Sq`[1] - a$`Mean Sq`[3])/(3*6)
s2gamma <- (a$`Mean Sq`[3] - a$`Mean Sq`[4])/6

s2t <- s2e+s2beta+s2gamma
s2m <- s2t

sqrt(s2m)/sqrt(s2t)*100

#tutta la variabilità dipende dalla variabilità della misurazione 
#quindi le diverse tipologiè di siringhe non sono equivalenti

library(SixSigma)
ss.rr(tempoerog, macchina, siringa, data=ospedale5)
