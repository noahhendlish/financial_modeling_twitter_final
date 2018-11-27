


#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Financial_modeling")  ## This is your working directory. This is the place from where you will be reading and writting data 


### Class 16: Random Forest and Bayesian Networks  #####
# by Santiago Truffa
# https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/

# Random Forest

library(randomForest)
cdata<-read.table("data.txt", h=F, sep="")

colnames(cdata) <- c("chk_ac_status_1",
                     "duration_month_2", "credit_history_3", "purpose_4",
                     "credit_amount_5","savings_ac_bond_6","p_employment_since_7", 
                     "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10", 
                     "present_residence_since_11","property_type_12","age_in_yrs_13",
                     "other_instalment_type_14", "housing_type_15", 
                     "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
                     "telephone_19", "foreign_worker_20", 
                     "good_bad_21")


cdatanum<-read.table("german.data-numeric.txt", h=F, sep="")

cdatanum <- as.data.frame(sapply(cdatanum, as.numeric ))

## Lets see what we have in this data set!

str(cdata)  ## This function is very helpful. Is like applying class, but to the complete data set

summary(cdata)


head(cdata, 5)

## Data procesecing

cdata$duration_month_2  <- as.numeric(cdata$duration_month_2)
cdata$credit_amount_5   <-  as.numeric(cdata$credit_amount_5 )
cdata$instalment_pct_8 <-  as.numeric(cdata$instalment_pct_8)
cdata$present_residence_since_11 <-  as.numeric(cdata$present_residence_since_11)
cdata$age_in_yrs_13        <-  as.numeric(cdata$age_in_yrs_13)
cdata$number_cards_this_bank_16    <-  as.numeric(cdata$number_cards_this_bank_16)
cdata$no_people_liable_for_mntnance_18 <-  as.numeric(cdata$no_people_liable_for_mntnance_18)

m3 <- randomForest(good_bad_21 ~ ., data = train)

m3_fitForest <- predict(m3, newdata = test, type="prob")[,2]

m3_pred <- prediction( m3_fitForest, test$good_bad_21)

m3_perf <- performance(m3_pred, "tpr", "fpr")

#plot variable importance
varImpPlot(m3, main="Random Forest: Variable Importance")


# Model Performance plot
plot(m3_perf,colorize=TRUE, lwd=2, main = "m3 ROC: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# Plot precision/recall curve
m3_perf_precision <- performance(m3_pred, measure = "prec", x.measure = "rec")
plot(m3_perf_precision, main="m3 Random Forests:Precision/recall curve")

# Plot accuracy as function of threshold
m3_perf_acc <- performance(m3_pred, measure = "acc")
plot(m3_perf_acc, main="m3 Random Forests:Accuracy as function of threshold")


#KS & AUC  m3
m3_AUROC <- round(performance(m3_pred, measure = "auc")@y.values[[1]]*100, 2)
m3_KS <- round(max(attr(m3_perf,'y.values')[[1]] - attr(m3_perf,'x.values')[[1]])*100, 2)
m3_Gini <- (2*m3_AUROC - 100)
cat("AUROC: ",m3_AUROC,"\tKS: ", m3_KS, "\tGini:", m3_Gini, "\n")

## Some variants

## Conditional random forest

# Random forests show a bias towards correlated predictor variables.

# We identify two mechanisms responsible for this finding: 
#(i) A preference for the selection of correlated predictors in the tree building process and 
#(ii) an additional advantage for correlated predictor variables induced by the unconditional permutation scheme that is employed in the computation of the variable importance measure.
# Based on these considerations we develop a new, conditional permutation scheme for the computation of the variable importance measure.

# The resulting conditional variable importance reflects the true impact of each predictor variable more reliably than the original marginal approach.


## https://epub.ub.uni-muenchen.de/9387/1/techreport.pdf

## Note how info, and packages are publicly available!!
# Lets use this package

library(party)
set.seed(123456742)
m3_1 <- cforest(good_bad_21~., control = cforest_unbiased(mtry = 2, ntree = 50), data = train)

summary(m3_1)

# Model Performance
m3_1_fitForest <- predict(m3, newdata = test, type = "prob")[,2]
m3_1_pred <- prediction(m3_1_fitForest, test$good_bad_21)
m3_1_perf <- performance(m3_1_pred, "tpr", "fpr")

# Model Performance Plot
plot(m3_1_perf, colorize=TRUE, lwd=2, main = " m3_1 ROC: Conditional Random Forests")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)




####### BAyesian Network

library(bnlearn)


train_2<-train

train_2$duration_month_2 <- as.factor(train_2$duration_month_2)

train_2$credit_amount_5 <- as.factor(train_2$credit_amount_5)

train_2$instalment_pct_8 <- as.factor(train_2$instalment_pct_8)

train_2$age_in_yrs_13 <- as.factor(train_2$age_in_yrs_13)

bn.gs <- gs(train_2)
bn.gs

bn2 <- iamb(train_2)
bn2

bn3 <- fast.iamb(train_2)
bn3

bn4 <- inter.iamb(train_2)
bn4

compare(bn.gs, bn2)

#On the other hand hill-climbing results in a completely directed network, which diers from
#the previous one because the arc between A and B is directed (A ! B instead of A  B).
bn.hc <- hc(train_2, score = "aic")
bn.hc

opm5<-par(mfrow = c(1,2))
plot(bn.gs, main = "Constraint-based algorithms")
plot(bn.hc, main = "Hill-Climbing")

res2 = hc(train_2)
fitted2 = bn.fit(res2, train_2)
fitted2


