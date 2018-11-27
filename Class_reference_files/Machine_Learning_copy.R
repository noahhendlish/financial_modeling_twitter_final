

#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Fall 2018/FINE/Financial Modeling/Class R files")  ## This is your working directory. This is the place from where you will be reading and writting data 

## For this part of the class, we will need to install several new packages:

install.packages(c('DT', 'lattice','knitr' ,'ClustOfVar','ape','Information','ROCR','caret','rpart','rpart.utils','rpart.plot','randomForest','party','bnlearn','DAAG','vcd','kernlab')); #if you have not already installed these packages



library(DT)          # For Data Tables
library(lattice)     # The lattice add-on of Trellis graphics for R
library(knitr)       # For Dynamic Report Generation in R 
library(ClustOfVar)  # Clustering of variables 
library(ape)         # Analyses of Phylogenetics and Evolution (as.phylo) 
library(Information) # Data Exploration with Information Theory (Weight-of-Evidence and Information Value)
library(ROCR)        # Model Performance and ROC curve
library(caret)       # Classification and Regression Training -  for any machine learning algorithms
library(rpart)       # Recursive partitioning for classification, regression and survival trees
library(rpart.utils) # Tools for parsing and manipulating rpart objects, including generating machine readable rules
library(rpart.plot)  # Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'
library(randomForest)# Leo Breiman and Cutler's Random Forests for Classification and Regression 
library(party)       # A computational toolbox for recursive partitioning - Conditional inference Trees
library(bnlearn)     # Bayesian Network Structure Learning, Parameter Learning and Inference
library(DAAG)        # Data Analysis and Graphics Data and Functions
library(vcd)         # Visualizing Categorical Data
library(kernlab)     # Support Vector Machine



### Class 13: Machine Learning applied to Finance / Credit Risk  #####
# by Santiago Truffa
# I thank Ariful Mondal for sharing his codes 

## data.txt is German credit data that was downloaded from the internet (https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data)

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

#In this data data good_bad_21 is our response/target variable where 1 as good/non-event and 2 as bad/event.
#Kindly note that there could be many situations where we have to create response variables based on business objectives and analysis goal as the response variable may not be readily available with the data. 

cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 1, "Good", "Bad"))  ## we create a factor, and code it as good or bad 

## As always, lets visualize the most relevant outcomes


op <- par(mfrow=c(1,2), new=TRUE)

plot(as.numeric(cdata$good_bad_21), ylab="Good-Bad", xlab="n", main="Good ~ Bad")

hist(as.numeric(cdata$good_bad_21), breaks=2, 
     xlab="Good(1) and Bad(2)", col="blue")



