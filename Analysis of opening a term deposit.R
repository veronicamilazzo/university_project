########################
## Stochastic Network ##
########################

rm(list = ls())

# packages 

library(tidyverse)
library(gridExtra)
library(gRbase) 
library(gRim) 
library(igraph)
library(ggm)
library(gRain)
library(bnlearn)
library(vcdExtra)
library(caret)
library(ROSE)

#########
dati <- read.csv("train.csv", sep=";", header=T)
str(dati)

for (i in 1:ncol(dati)) {
  if (is.character(dati[,i])) 
    dati[,i] <- as.factor(dati[,i])
}

table(dati$poutcome, dati$y)
table(dati$y)

#dati <- dati[-c("contact", "day", "pdays", "poutcome")]
dati <- dati[,-c(9, 10, 14, 16)]

## Factorization of quantitative variables

# Age

hist(dati$age)
summary(dati$age)
quantile(dati$age)
dati$age <- cut(dati$age, breaks = c(17, 30, 59, 95), labels= c("<=30", "31-59",">=60"))
table(dati$age)

# Balance

hist(dati$balance)
summary(dati$balance)
quantile(dati$balance[dati$balance > 0])
dati$balance <- cut(dati$balance, breaks = c(-8020, 0, median(dati$balance[dati$balance > 0]), max(dati$balance)), labels= c("negative", "medium-low","medium-high"))
table(dati$balance)

# duration

hist(dati$duration)
summary(dati$duration)
dati$duration <- cut(dati$duration, breaks = c(-1, 180, 4918), labels = c("<= 3 min", "> 3 min"))
table(dati$duration)

# campaign

hist(dati$campaign)
summary(dati$campaign)
dati$campaign <- cut(dati$campaign, breaks = c(0, 3, 63), labels = c("<= 3 contacts", "> 3 contacts"))
table(dati$campaign)

# previous

hist(dati$previous)
summary(dati$previous)
boxplot(dati$previous)
dati$previous <- cut(dati$previous, breaks = c(-1, 0, 275), labels = c("No", "Yes"))
table(dati$previous)

# year 

table(dati$month)

dati$year <- as.factor(c(rep(2008, 27729), rep(2009, 14862), rep(2010, 2620)))

# delete month
dati <- dati[,-9]          

##########################
## Exploratory Analysis ##
##########################

# y

ggplot(dati,
       aes(x = y)) +
  geom_bar(col = "red", fill = "cornflowerblue") +
  labs(title="Barplot of y")

# y vs espicative

g1 <- ggplot(dati,
       aes(x = age,fill = y)) +
  geom_bar( position = "fill",  show.legend = F)

g2 <- ggplot(dati,
             aes(x = job,fill = y)) +
  geom_bar( position = "fill",  show.legend = F) +
  theme(axis.text.x = element_text(size=7, angle=20))

g3 <- ggplot(dati,
             aes(x = marital,fill = y)) +
  geom_bar( position = "fill")

g4 <- ggplot(dati,
             aes(x = education,fill = y)) +
  geom_bar( position = "fill",  show.legend = F)

g5 <- ggplot(dati,
             aes(x = default,fill = y)) +
  geom_bar( position = "fill",  show.legend = F)

g6 <- ggplot(dati,
             aes(x = balance,fill = y)) +
  geom_bar( position = "fill")

g7 <- ggplot(dati,
             aes(x = housing,fill = y)) +
  geom_bar( position = "fill",  show.legend = F)

g8 <- ggplot(dati,
             aes(x = loan,fill = y)) +
  geom_bar( position = "fill",  show.legend = F)

g9 <- ggplot(dati,
             aes(x = duration,fill = y)) +
  geom_bar( position = "fill")

g10 <- ggplot(dati,
             aes(x = campaign,fill = y)) +
  geom_bar( position = "fill",  show.legend = F)

g11 <- ggplot(dati,
             aes(x = previous,fill = y)) +
  geom_bar( position = "fill",  show.legend = F)

g12 <- ggplot(dati,
             aes(x = year,fill = y)) +
  geom_bar( position = "fill") 
  
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12, nrow = 4, ncol = 3)


# job - balance

plotdata <- dati %>%
  group_by(job, balance) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))

plotdata

ggplot(plotdata,
       aes(x = job,y = pct,
           fill = balance)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = lbl), position = position_dodge(width = .9),       
          vjust = -0.5,          
          size = 3) +
  labs(title="Balance by job",
       y= "%") +
  theme(axis.text.x = element_text(size=10, angle=20))
  
# age - housing

plotdata1 <- dati %>%
  group_by(age, housing) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))

ggplot(plotdata1,
       aes(x = age,y = pct,
           fill = housing)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = lbl), position = position_dodge(width = .9),       
            vjust = -0.5,          
            size = 3) +
  labs(title="Housing by age",
       y= "%")

# age - duration

plotdata2 <- dati %>%
  group_by(age, duration) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))


ggplot(plotdata2,
       aes(x = age,y = pct,
           fill = duration)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = lbl), position = position_dodge(width = .9),       
            vjust = -0.5,          
            size = 3) +
  labs(title="Duration by age",
       y= "%")

# year - balance

plotdata3 <- dati %>%
  group_by(year, balance) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))


ggplot(plotdata3,
       aes(x = year,y = pct,
           fill = balance)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = lbl), position = position_dodge(width = .9),       
            vjust = -0.5,          
            size = 3) +
  labs(title="Balance by year",
       y= "%")

# year - campaign

plotdata4 <- dati %>%
  group_by(year, campaign) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))


ggplot(plotdata4,
       aes(x = year,y = pct,
           fill = campaign)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = lbl), position = position_dodge(width = .9),       
            vjust = -0.5,          
            size = 3) +
  labs(title="Campaign by year",
       y= "%")

# job - balance

plotdata5 <- dati %>%
  group_by(job, housing) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))

plotdata5

ggplot(plotdata5,
       aes(x = job,y = pct,
           fill = housing)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = lbl), position = position_dodge(width = .9),       
            vjust = -0.5,          
            size = 3) +
  labs(title="Housing by job",
       y= "%") +
  theme(axis.text.x = element_text(size=10, angle=20))

# association among variables

corrplot::corrplot(DescTools::PairApply(dati, DescTools::CramerV))

#########################

###  Training set and test set 

set.seed(123)
train = sample(1:nrow(dati), nrow(dati)*0.75)
df_train <- dati[train,]
df_test <-  dati[-train,]

#####################
## Graphical Model ##
#####################

# undirected graph

mod <- dmod(~.^., data = df_train)

## select the best decomposable model 
n <- nrow(dati)
mod_step_d <- stepwise(mod, k = log(n), 
                       type = "decomposable",
                       direction = "backward", 
                       details = 0, search = "all") ## BIC

## transform in a graph
g_sel <- ug(formula(mod_step_d), 
            result = "igraph")
tkplot(g_sel, vertex.color = "lightblue")

# directed graph

bn.learn <- hc(df_train,
               perturb = 1, restart = 1, 
               score = "bic")
tkplot(as(amat(bn.learn), "igraph"), vertex.color = "lightblue")


# Black list

names(df_train)
blk <- matrix(0, nrow = length(names(df_train)), 
              ncol =  length(names(df_train)))
rownames(blk) <- colnames(blk) <- names(df_train)
blk[,1]<- 1 # age
blk[,13]<- 1 # year
blk[2,4]<- 1 # job - education

blackList <- data.frame(get.edgelist(as(blk, "igraph")))
names(blackList) <- c("from", "to")
new.graph <- hc(df_train, 
                blacklist = blackList, score = "bic")
tkplot(as(amat(new.graph), "igraph"), vertex.color = "lightblue")

###########
## Query ##
###########

## Compile and Propagate 
dag0 <- as(amat(new.graph), "graphNEL")
grn1c <- grain(dag0, data = df_train, smooth = .1) 
grn1c <- propagate(grn1c)

##################################################################
## GOAL 1:  variables influencing to open a term deposit ##
##################################################################

## query 1: Probability of opening a term deposit given duration and year

# evidence
q1.ev <- setFinding(grn1c, nodes=c("year"), states=c("2008"))
q1.ev.1 <- setFinding(grn1c, nodes=c("year"), states=c("2009"))
q1.ev.2 <- setFinding(grn1c, nodes=c("year"), states=c("2010"))

## conditional distribution
querygrain(q1.ev, nodes = c("y", "duration"), type = "conditional")
querygrain(q1.ev.1, nodes = c("y", "duration"), type = "conditional")
querygrain(q1.ev.2, nodes = c("y", "duration"), type = "conditional")


## query 2: Probability of opening a term deposit given campaign and year

## conditional distribution
querygrain(q1.ev, nodes = c("y", "campaign"), type = "conditional")
querygrain(q1.ev.1, nodes = c("y", "campaign"), type = "conditional")
querygrain(q1.ev.2, nodes = c("y", "campaign"), type = "conditional")

## query 2.1: Probability of opening a term deposit given campaign

# evidence
q2.1.ev <- setFinding(grn1c, nodes=c("campaign"), states=c("<= 3 contacts"))
q2.1.ev.1 <- setFinding(grn1c, nodes=c("campaign"), states=c("> 3 contacts"))

## marginal distribution
querygrain(q2.1.ev, nodes = c("y"), type = "marginal")
querygrain(q2.1.ev.1, nodes = c("y"), type = "marginal")

## query 3: Probability of opening a term deposit given previous and year

## conditional distribution
querygrain(q1.ev, nodes = c("y", "previous"), type = "conditional")
querygrain(q1.ev.1, nodes = c("y", "previous"), type = "conditional")
querygrain(q1.ev.2, nodes = c("y", "previous"), type = "conditional")

## query 3.1: Probability of opening a term deposit given previous 

# evidence
q3.1.ev <- setFinding(grn1c, nodes=c("previous"), states=c("No"))
q3.1.ev.1 <- setFinding(grn1c, nodes=c("previous"), states=c("Yes"))

## marginal distribution
querygrain(q3.1.ev, nodes = c("y"), type = "marginal")
querygrain(q3.1.ev.1, nodes = c("y"), type = "marginal")


## query 4: Probability of opening a term deposit given balance and housing

# evidence
q4.ev <- setFinding(grn1c, nodes=c("housing"), states=c("no"))
q4.ev.1 <- setFinding(grn1c, nodes=c("housing"), states=c("yes"))

## conditional distribution
querygrain(q4.ev, nodes = c("y", "balance"), type = "conditional")
querygrain(q4.ev.1, nodes = c("y", "balance"), type = "conditional")

## query 4.1: Probability of opening a term deposit given balance

# evidence
q4.1.ev <- setFinding(grn1c, nodes=c("balance"), states=c("negative"))
q4.1.ev.1 <- setFinding(grn1c, nodes=c("balance"), states=c("medium-low"))
q4.1.ev.2 <- setFinding(grn1c, nodes=c("balance"), states=c("medium-high"))

## marginal distribution
querygrain(q4.1.ev, nodes = c("y"), type = "marginal")
querygrain(q4.1.ev.1, nodes = c("y"), type = "marginal")
querygrain(q4.1.ev.2, nodes = c("y"), type = "marginal")

###################################################################
## GOAL 2: kind of subject is more likely to open a term deposit ##
###################################################################

# evidence
q5.ev <- setFinding(grn1c, nodes=c("y"), states=c("yes"))

## Marginal distribution

## query 5: Job by term deposit 
querygrain(q5.ev, nodes = c("job"), type = "marginal")

## query 6: Education by term deposit 
querygrain(q5.ev, nodes = c("education"), type = "marginal")

## query 7: Marital by term deposit 
querygrain(q5.ev, nodes = c("marital"), type = "marginal")

## query 8: Age by term deposit 
querygrain(q5.ev, nodes = c("age"), type = "marginal")

# evidence
q6.ev <- setFinding(grn1c, nodes=c("y", "housing"), states=c("yes", "yes"))
q6.ev.1 <- setFinding(grn1c, nodes=c("y", "housing"), states=c("yes", "no"))

## Marginal distribution

## query 5.1: Job by term deposit and housing
querygrain(q6.ev, nodes = c("job"), type = "marginal")
querygrain(q6.ev.1, nodes = c("job"), type = "marginal")

# evidence
q7.ev <- setFinding(grn1c, nodes=c("y", "year"), states=c("yes","2008"))
q7.ev.1 <- setFinding(grn1c, nodes=c("y", "year"), states=c("yes","2009"))
q7.ev.2 <- setFinding(grn1c, nodes=c("y", "year"), states=c("yes","2010"))

## Marginal distribution

## query 7.1: Marital by term deposit and year
querygrain(q7.ev, nodes = c("marital"), type = "marginal")
querygrain(q7.ev.1, nodes = c("marital"), type = "marginal")
querygrain(q7.ev.2, nodes = c("marital"), type = "marginal")

################
## Prediction ##
################

fitted = bn.fit(new.graph, data=df_train)   
pred = predict(fitted, node = "y", data=df_test, method = "bayes-lw")

conf_matrix_model<-confusionMatrix(data = pred, reference = df_test$y, positive = "yes")
conf_matrix_model$table

conf_matrix_model$byClass[1:2]
# Sensitivity = TP/(TP+FN) = 294/(294+1014) 
# Specificity = TN/(TN+FP) = 9874/(9874+121)

conf_matrix_model$overall[1]     
# Accuracy = (TN+TP)/Tot = (9874+294)/11303

# Plot confusion matrix
x11()
mosaic(conf_matrix_model$table, labeling=labeling_values, main = "Confusion Matrix", pop=F)

seekViewport("cell:Prediction=no,Reference=no")
grid.rect(gp=gpar(col="red", lwd=4)) 
seekViewport("cell:Prediction=yes,Reference=yes")
grid.rect(gp=gpar(col="red", lwd=4)) 
#########################################

#######################
## Balancing methods ##
#######################

## oversampling
over <- ovun.sample(y~., seed = 123,  data = df_train, method = "over", N = 59854)$data
table(over$y)

# directed graphical model

# black list
names(over)
blk <- matrix(0, nrow = length(names(over)), 
              ncol =  length(names(over)))
rownames(blk) <- colnames(blk) <- names(over)
blk[,1]<- 1 # age
blk[,13]<- 1 # year
blk[2,4]<- 1 # job - education
blk[12,] <- 1 # y

blackList <- data.frame(get.edgelist(as(blk, "igraph")))
names(blackList) <- c("from", "to")

new.graph.over <- hc(over, 
                blacklist = blackList, score = "bic")
tkplot(as(amat(new.graph.over), "igraph"), vertex.color = "lightblue")

# Prediction
fitted.over = bn.fit(new.graph.over, data=over)   
pred.over = predict(fitted.over, node = "y", data=df_test, method = "bayes-lw")

conf_matrix_model.over<-confusionMatrix(data = pred.over, reference = df_test$y, positive = "yes")
conf_matrix_model.over$table
mosaic(conf_matrix_model.over$table, labeling=labeling_values, main = "Confusion Matrix")


## undersampling 

under <- ovun.sample(y~., seed = 123, data = df_train, method = "under", N = 7962)$data
table(under$y)

# directed graphical model
new.graph.under <- hc(under, 
                     blacklist = blackList, score = "bic")
tkplot(as(amat(new.graph.under), "igraph"), vertex.color = "lightblue")

# Prediction
fitted.under = bn.fit(new.graph.under, data=under)   
pred.under = predict(fitted.under, node = "y", data=df_test, method = "bayes-lw")

conf_matrix_model.under<-confusionMatrix(data = pred.under, reference = df_test$y, positive = "yes")
conf_matrix_model.under$table
mosaic(conf_matrix_model.under$table, labeling=labeling_values, main = "Confusion Matrix")


## both
both <- ovun.sample(y~., seed = 123, data = df_train, method = "both", N = 11303)$data
table(both$y)

# directed graphical model
new.graph.both <- hc(both, 
                      blacklist = blackList, score = "bic")
tkplot(as(amat(new.graph.both), "igraph"), vertex.color = "lightblue")

# Prediction
fitted.both = bn.fit(new.graph.both, data=both)   
pred.both = predict(fitted.both, node = "y", data=df_test, method = "bayes-lw")

conf_matrix_model.both<-confusionMatrix(data = pred.both, reference = df_test$y, positive = "yes")
conf_matrix_model.both$table
mosaic(conf_matrix_model.both$table, labeling=labeling_values, main = "Confusion Matrix")

######################
## Model comparison ##
######################

model <- as.table(c(conf_matrix_model$byClass[1:2],conf_matrix_model$overall[1]))
over  <- as.table(c(conf_matrix_model.over$byClass[1:2],conf_matrix_model.over$overall[1]))
under <- as.table(c(conf_matrix_model.under$byClass[1:2],conf_matrix_model.under$overall[1]))
both  <- as.table(c(conf_matrix_model.both$byClass[1:2],conf_matrix_model.both$overall[1]))

table <- cbind(model, over, under, both)

barplot <- barplot(table*100,
        main = "Model comparison",
        xlab = "Model", ylab = "%",
        col = c("green", "cornflowerblue", "coral1"),
        beside = TRUE, ylim = c(0, 108))
text(x = barplot, y = round(table*100, 2), label = round(table*100, 2), pos = 3, cex = 0.9)

legend("topright", rownames(table),
       col=c("green", "cornflowerblue", "coral1"),fill = c("green", "cornflowerblue", "coral1"), cex= 0.7)
