


#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Fall 2018/FINE/Financial Modeling/Class R files")  ## This is your working directory. This is the place from where you will be reading and writting data 


### Class 15: Logistic Regressions and Partitioning  #####
# by Santiago Truffa
# I thank Ariful Mondal for sharing his codes 

library(stats)
# Model: Stepwise Logistic Regression Model  # Model: Stepwise Logistic Regression Model  # Model: Stepwise Logistic Regression Model# Model: Stepwise Logistic Regression Model


help(glm)
m1 <- glm(good_bad_21~.,data=train,family=binomial())

m1 <- step(m1)

summary(m1)

# List of significant variables and features with p-value <0.01

significant.variables <- summary(m1)$coeff[-1,4] < 0.01

names(significant.variables)[significant.variables == TRUE]


prob <- predict(m1, type = "response")

res <- residuals(m1, type = "deviance")


## CIs using profiled log-likelihood
confint(m1)

## CIs using standard errors
confint.default(m1)

## odds ratios and 95% CI
exp(cbind(OR = coef(m1), confint(m1)))


#score test data set

test$m1_score <- predict(m1,type='response',test)

m1_pred <- prediction(test$m1_score, test$good_bad_21)

m1_perf <- performance(m1_pred,"tpr","fpr")

#ROC
par(mfrow=c(1,1))

plot(m1_perf, lwd=2, colorize=TRUE, main="ROC m1: Logistic Regression Performance")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# Plot precision/recall curve
m1_perf_precision <- performance(m1_pred, measure = "prec", x.measure = "rec")

plot(m1_perf_precision, main="m1 Logistic:Precision/recall curve")


### Partitioning ######## Partitioning ######## Partitioning ######## Partitioning ######## Partitioning #####
### Partitioning ######## Partitioning ######## Partitioning ######## Partitioning ######## Partitioning #####
### Partitioning ######## Partitioning ######## Partitioning ######## Partitioning ######## Partitioning #####
### Partitioning ######## Partitioning ######## Partitioning ######## Partitioning ######## Partitioning #####

#Basic recursive partitioning

library(rpart)

m2 <- rpart(good_bad_21~.,data=train)

# Print tree detail
printcp(m2)

# Tree plot
plot(m2, main="Tree:Recursive Partitioning");text(m2)
#correlation matrix######################88888
# Better version of plot
prp(m2,type=2,extra=1,  main="Tree:Recursive Partitioning")

# score test data
test$m2_score <- predict(m2,type='prob',test)

m2_pred <- prediction(test$m2_score[,2],test$good_bad_21)

m2_perf <- performance(m2_pred,"tpr","fpr")

# MOdel performance plot
plot(m2_perf, lwd=2, colorize=TRUE, main="ROC m2: Traditional Recursive Partitioning")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# Plot precision/recall curve
m2_perf_precision <- performance(m2_pred, measure = "prec", x.measure = "rec")
plot(m2_perf_precision, main="m2 Recursive Partitioning:Precision/recall curve")


# Plot accuracy as function of threshold
m2_perf_acc <- performance(m2_pred, measure = "acc")
plot(m2_perf_acc, main="m2 Recursive Partitioning:Accuracy as function of threshold")

# KS & AUC m1
m2_AUROC <- round(performance(m2_pred, measure = "auc")@y.values[[1]]*100, 2)
m2_KS <- round(max(attr(m2_perf,'y.values')[[1]]-attr(m2_perf,'x.values')[[1]])*100, 2)
m2_Gini <- (2*m2_AUROC - 100)
cat("AUROC: ",m2_AUROC,"\tKS: ", m2_KS, "\tGini:", m2_Gini, "\n")


## Bayesian Partitioning


# Fit Model 
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.9,.1)),cp=.0002) #- build model using 90%-10% priors
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.8,.2)),cp=.0002) #- build model using 80%-20% priors
m2_1 <- rpart(good_bad_21~.,data=train,parms=list(prior=c(.7,.3)),cp=.0002)  #- build model using 70%-30% priors    ## Provide priors to a bayesian model  ## Notice, this are the correct priors
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.75,.25)),cp=.0002) #- build model using 75%-25% priors
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.6,.4)),cp=.0002) #- build model using 60%-40% priors

# Print tree detail
printcp(m2_1)

# plot trees
plot(m2_1, main="m2_1-Recursive Partitioning - Using Bayesian N 70%-30%");text(m2_1)

prp(m2_1,type=2,extra=1, main="m2_1-Recursive Partitioning - Using Bayesian N 70%-30%")

test$m2_1_score <- predict(m2_1,type='prob',test)
m2_1_pred <- prediction(test$m2_1_score[,2],test$good_bad_21)
m2_1_perf <- performance(m2_1_pred,"tpr","fpr")

# MOdel performance plot
plot(m2_1_perf, colorize=TRUE, main="ROC m2_1: Recursive Partitioning - Using Bayesian N 70%-30%")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

#plots complexity vs. error
plotcp(m2)
plotcp(m2_1)

# ROC Comparision
plot(m2_perf,  colorize=TRUE, lwd=2,lty=1, main='Recursive partitioning:Model Performance Comparision (m2 ROC)') 
plot(m2_1_perf, col='blue',lty=2, add=TRUE); # simple tree
legend(0.4,0.4,
       c("m2: Traditional", "m2_1: Bayesian -70%-30%"),
       col=c('orange', 'blue'),
       lwd=3);
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)


