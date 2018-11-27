


#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Financial_modeling")  ## This is your working directory. This is the place from where you will be reading and writting data 


### Class 18: Lasso Regression #####
# by Santiago Truffa

install.packages(c('lars','glmnet'))

library('lars')
library('glmnet')  ## always explore the package.. help(package)


mat1 <- model.matrix(V25 ~ . , data = train_num  ) # convert to numeric matrix

mat2 <- model.matrix(V25 ~ . , data = test_num  )  # convert to numeric matrix

# Model using lars package
m9_1<- lars(x= mat1, y= as.numeric(train_num$V25), trace = T)


#lm9_2<- glmnet(x= mat1, y= train_num$V25, family="binomial")

# Summary
summary(m9_1)

# Plots 
plot(m9_1, plottype="coefficients")

#In process improvement efforts, the process capability index or process capability ratio is a statistical measure of process capability: the ability of a process to produce output within specification limits.
# Process capability indices measure how much "natural variation" a process experiences relative to its specification limits and allows different processes to be compared with respect to how well an organization controls them.

plot(m9_1, plottype="Cp")

# Predict
m9_pred <- predict.lars(m9_1, mat2, type="fit")

las1.mspe <- mean( (as.numeric(test_num$V25) - m9_pred$fit)^2 )  

cat("Error:", round(las1.mspe,2)*100, "%\n")

### extract the coefficient vector with L1 norm=4.1
coef4.1 <- predict(m9_1, s=4.1, type="coef", mode="norm")

coef4.1

# By using cross-validation with the lasso, a good (hopefully near-optimal) value for
# the "fraction" can be determined.
cvlas <- cv.lars(mat1,y= as.numeric(train_num$V25), type="lasso")

# Alternative method - using glmnet package
# library(glmnet)
# For help type >?cv.glmnet
# Ref: http://www.moseslab.csb.utoronto.ca/alan/glmnet_presentation.pdf

#with cross validation
m9_2<-cv.glmnet(mat1,as.numeric(train_num$V25), alpha=1, family="binomial", type.measure = 'auc')

summary(m9_2)

m9_2_pred <- predict(m9_2, mat2)

cor(m9_2_pred, as.numeric(test_num$V25)) 

plot(m9_2_pred, test_num$V25, xlab="Predicted", ylab="Observed", main="Lasso-Compare Predicted to Observed")

mydata<-as.data.frame(cbind(m9_2_pred -.5, test_num$V25))

ggplot(mydata, aes(m9_2_pred,as.numeric(test_num$V25)))+geom_point()


# Apply model to testing dataset
test_num$lasso.prob <- predict(m9_2,type="response", newx =mat2, s = 'lambda.min')

m9_2_pred <- prediction(test_num$lasso.prob,test_num$V25)

# calculate probabilities for TPR/FPR for predictions
m9_2_perf <- performance(m9_2_pred,"tpr","fpr")

#performance(m9_2_pred,"auc") # shows calculated AUC for model
plot(m9_2_perf,colorize=T, col="blue", main="m9-ROC: Lasso Regression") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

# Plot precision/recall curve
m9_2_perf_precision <- performance(m9_2_pred, measure = "prec", x.measure = "rec")
plot(m9_2_perf_precision, main="m9_2 LASSO:Precision/recall curve")

# Plot accuracy as function of threshold
m9_2_perf_acc <- performance(m9_2_pred, measure = "acc")
plot(m9_2_perf_acc, main="m3_2 Lasso:Accuracy as function of threshold")


#KS & AUC m1
m9_KS <- round(max(attr(m9_2_perf,'y.values')[[1]]-attr(m9_2_perf,'x.values')[[1]])*100, 2)
m9_AUROC <- round(performance(m9_2_pred, measure = "auc")@y.values[[1]]*100, 2)
m9_Gini <- (2*m9_AUROC - 100)
cat("AUROC: ",m9_AUROC,"\tKS: ", m9_KS, "\tGini:", m9_Gini, "\n")

