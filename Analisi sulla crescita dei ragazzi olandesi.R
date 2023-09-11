library(gamlss.data)
library(moments)
library(segmented)
library(splines)
library(quantregGrowth)

data("dbbmi")
dbbmi <- dbbmi[order(dbbmi$age), ]

set.seed(1)
id <- sample(nrow(dbbmi), size = round(.5*nrow(dbbmi)))
dbbmi$id <- ifelse(1:nrow(dbbmi) %in% id, 1, 0)

######
#ANALISI ESPLORATIVA

summary(dbbmi[,1:2])
skewness(dbbmi[,1:2])
kurtosis(dbbmi[,1:2])

#grafico 1
hist(dbbmi$bmi, freq = F, main = "Distribuzione del BMI", 
     xlab = "BMI", ylab = "densità")

#grafico 2
hist(dbbmi$age, freq = F,  main = "Distribuzione dell'età",
     xlab = "età", ylab = "densità")

#grafico 3
plot(dbbmi$age, dbbmi$bmi, cex = 0.3, pch = 19, main = "Andamento del BMI rispetto all'età",
     xlab = "età", ylab = "BMI")

######

train <- dbbmi[which(dbbmi$id==0),]
test <- dbbmi[which(dbbmi$id==1),]

#REGRESSIONE POLINOMIALE

m.poli <- lm(bmi ~ poly(age, 10L), data = train)
summary(m.poli)

#REGRESSIONE SEGMENTATA

m.glm.inv <- glm(bmi ~ age, data = train, family = Gamma())
m.glm.ide <- glm(bmi ~ age, data = train, family = Gamma(link="identity"))
m.glm.log <- glm(bmi ~ age, data = train, family = Gamma(link="log"))
AIC(m.glm.inv, m.glm.ide, m.glm.log)
m.glm <- m.glm.inv

m.seg <- selgmented(m.glm, type = "bic", Kmax = 5)
summary(m.seg)
slope(m.seg)

#REGRESSIONE SPLINE

k <- with(train, quantile(age, prob = c(.20,.40,.60,.80)))

m.bs <- glm(bmi ~ bs(age, knots = k), data = train, family = Gamma())
summary(m.bs)

m.ns <- glm(bmi ~ ns(age, knots = k), data = train, family = Gamma())
summary(m.ns)

# confronto fra i modelli

y <- test$bmi
n <- length(y)
y.glm <- predict(m.glm, type = "response", newdata = test)
RMSE.glm <- sqrt(sum((y-y.glm)^2)/n)
y.poli <- predict(m.poli, newdata = test)
RMSE.poli <- sqrt(sum((y-y.poli)^2)/n)
y.seg <- predict(m.seg, type = "response", newdata = test)
RMSE.seg <- sqrt(sum((y-y.seg)^2)/n)
y.bs <- predict(m.bs, type = "response", newdata = test)
RMSE.bs <- sqrt(sum((y-y.bs)^2)/n)
y.ns <- predict(m.ns, type = "response", newdata = test)
RMSE.ns <- sqrt(sum((y-y.ns)^2)/n)

out <- rbind(c(AIC(m.glm), AIC(m.poli), AIC(m.seg), AIC(m.bs), AIC(m.ns)),
             c(RMSE.glm, RMSE.poli, RMSE.seg, RMSE.bs, RMSE.ns))
rownames(out) <- c("AIC", "RMSE")
colnames(out) <- c("GLM", "Polinomiale", "Segmentato", "S-spline", "N-spline")
out

#grafico 4
par(mfrow = c(2,2))

pred <- predict(m.poli, newdata = data.frame(age = seq(0, 21.7, by = 0.1)))
plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Regressione polinomiale",
     xlab = "età", ylab = "BMI", col = "gray")
lines(pred ~ seq(0, 21.7, by = 0.1), col = "red", lwd = 2)

plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Regressione segmentata",
     xlab = "età", ylab = "BMI", col = "gray")
plot(m.seg, add = T, link = F, col = 2, lwd = 2)

y.hat <- 1/m.bs$linear.predictors
plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Regressione B-spline",
     xlab = "età", ylab = "BMI", col = "gray")
lines(train$age, y.hat, col = 2, lwd = 2)

y.hat <- 1/m.ns$linear.predictors
plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Regressione N-spline",
     xlab = "età", ylab = "BMI", col = "gray")
lines(train$age, y.hat, col = 2, lwd = 2)

age <- seq(21.7, 30, .1)

p.poli <- predict(m.poli, newdata = as.data.frame(age))
p.seg <- predict(m.seg, type = "response", newdata = as.data.frame(age))
p.bs <- predict(m.bs, type = "response", newdata = as.data.frame(age))
p.ns <- predict(m.ns, type = "response", newdata = as.data.frame(age))

#grafico 5
plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Previsioni regressione polinomiale",
     xlab = "età", ylab = "BMI", xlim = c(0,30), col = "gray")
lines(pred ~ seq(0, 21.7, by = 0.1), col = "red", lwd = 2)
points(age, p.poli, col = 3, pch = 19, cex = 0.5)

plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Previsioni regressione segmentata",
     xlab = "età", ylab = "BMI", xlim = c(0,30), col = "gray")
plot(m.seg, add = T, link = F, col = 2, lwd = 2)
points(age, p.seg, col = 3, pch = 19, cex = 0.5)

plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Previsioni regressione B-spline",
     xlab = "età", ylab = "BMI", xlim = c(0,30), col = "gray")
y.hat <- 1/m.bs$linear.predictors
lines(train$age, y.hat, col = 2, lwd = 2)
points(age, p.bs, col = 3, pch = 19, cex = 0.5)

plot(train$age, train$bmi, cex = 0.3, pch = 19, main = "Previsioni regressione N-spline",
     xlab = "età", ylab = "BMI", xlim = c(0,30), col = "gray")
y.hat <- 1/m.ns$linear.predictors
lines(train$age, y.hat, col = 2, lwd = 2)
points(age, p.ns, col = 3, pch = 19, cex = 0.5)

#REGRESSIONE QUANTILE  con termini non lineari

taus <- c(.05,.85,.95)
m.qs <- gcrq(bmi ~ ps(age), data = dbbmi, tau = taus, n.boot=50)

#grafico 6
par(mfrow = c(1,1))

plot(m.qs, res = TRUE, cex.p = 0.5, col = 1:3, conf.level = .9, 
     shade = TRUE, xlab = 'età', ylab = 'BMI', main = 'Regressione quantile')
