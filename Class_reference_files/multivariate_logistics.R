
#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Fall 2018/FINE/Financial Modeling/Class R files")  ## This is your working directory. This is the place from where you will be reading and writting data 


### Class 14: Multivariate analysis and logistic regressions applied to Credit Risk  #####
# by Santiago Truffa
# I thank Ariful Mondal for sharing his codes 

cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 0, "Good", "Bad"))  ## Lets use the outcome variable as a factor

IV <- c(max(A1$IV),max(A2$IV),max(A3$IV),max(A4$IV),max(A5$IV),max(A6$IV),max(A7$IV),max(A8$IV),max(A9$IV),max(A10$IV),max(A11$IV),max(A12$IV),max(A13$IV),max(A14$IV),max(A15$IV),max(A16$IV),max(A17$IV),max(A18$IV),max(A19$IV),max(A20$IV),2)

names(cdata)
IV

## lets keep variables that have a predictive power higher than 1
myvars <- names(cdata) %in% c("present_residence_since_11", "number_cards_this_bank_16", "job_17","no_people_liable_for_mntnance_18", "telephone_19")
cdata_reduced_1 <- cdata[!myvars]


# Multivariate Analysis - Dimension(Variable) Reduction using Variable Clustering Approach
# Clustering of variables is as a way to arrange variables into homogeneous clusters, i.e., groups of variables which are strongly related to each other and thus bring the same information.

# When we have large number of variables, this should be done well before univariate analysis.
# This can also be done using Principal Component Analysis (PCA) and Multiple Correspondence Analysis (MCA) or Factor Analysis.

# Step 1: Subset quantitative and qualitative variables X.quanti and X.quali

factors <- sapply(cdata_reduced_1, is.factor)

cdata_reduced_1$chk_ac_status_1 <- as.factor(cdata_reduced_1$chk_ac_status_1 )
cdata_reduced_1$purpose_4 <- as.factor(cdata_reduced_1$purpose_4 )
cdata_reduced_1$savings_ac_bond_6 <- as.factor(cdata_reduced_1$savings_ac_bond_6)
cdata_reduced_1$p_employment_since_7 <- as.factor(cdata_reduced_1$p_employment_since_7 )
cdata_reduced_1$instalment_pct_8 <- as.factor(cdata_reduced_1$instalment_pct_8 )
cdata_reduced_1$personal_status_9 <- as.factor(cdata_reduced_1$personal_status_9 )
cdata_reduced_1$other_debtors_or_grantors_10 <- as.factor(cdata_reduced_1$other_debtors_or_grantors_10 )
cdata_reduced_1$property_type_12 <- as.factor(cdata_reduced_1$property_type_12 )
cdata_reduced_1$other_instalment_type_14 <- as.factor(cdata_reduced_1$other_instalment_type_14 )
cdata_reduced_1$housing_type_15 <- as.factor(cdata_reduced_1$housing_type_15 )
cdata_reduced_1$foreign_worker_20  <- as.factor(cdata_reduced_1$foreign_worker_20  )


#subset Qualitative variables 
vars_quali <- cdata_reduced_1[,factors]

#vars_quali$good_bad_21<-vars_quali$good_bad_21[drop=TRUE] # remove empty factors
str(cdata_reduced_1)

#subset Quantitative variables 
vars_quanti <- cdata_reduced_1[,!factors]
str(vars_quanti)

#Step 2: Hierarchical Clustering of Variables (by PCA)
library(ClustOfVar)
# Need help type ?hclustvar on R console

tree <- hclustvar(X.quali=vars_quali)

plot(tree, main="variable clustering")

rect.hclust(tree, k=10,  border = 1:10)

summary(tree)

# Phylogenetic trees
library("ape")

plot(as.phylo(tree), type = "fan",
     tip.color = hsv(runif(15, 0.65,  0.95), 1, 1, 0.7),
     edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
     edge.width = runif(20,  0.5, 3), use.edge.length = TRUE, col = "gray80")

summary.phylo(as.phylo(tree))

stab<-stability(tree,B=50) # Bootstrap 50 times

# plot(stab,main="Stability of the partitions")
boxplot(stab$matCR)

part<-cutreevar(tree,10)
print(part)

summary(part)

# K-means clustering of variables
# We may also cross check the outcomes of hierarchical clustering using K-means variable clustering:


kfit<-kmeansvar( X.quali = vars_quali, init=5,
                iter.max = 150, nstart = 1, matsim = TRUE)
summary(kfit)

plot(vars_quali, as.factor(kfit$cluster))

kfit$E


## from clustering analysis.. 10 variables
keep<- c(1:8,12,13,21)

cdata_reduced_2 <- cdata[,keep]

str(cdata_reduced_2)
cdata_reduced_2$chk_ac_status_1 <- as.factor(cdata_reduced_2$chk_ac_status_1 )
cdata_reduced_2$purpose_4  <- as.factor(cdata_reduced_2$purpose_4  )
cdata_reduced_2$savings_ac_bond_6  <- as.factor(cdata_reduced_2$savings_ac_bond_6  )
cdata_reduced_2$p_employment_since_7  <- as.factor(cdata_reduced_2$p_employment_since_7  )
cdata_reduced_2$property_type_12 <- as.factor(cdata_reduced_2$property_type_12  )


######### Sampling  ##################### Sampling  ##################### Sampling  ############
######### Sampling  ##################### Sampling  ##################### Sampling  ############
######### Sampling  ##################### Sampling  ##################### Sampling  ############
######### Sampling  ##################### Sampling  ##################### Sampling  ############
######### Sampling  ##################### Sampling  ##################### Sampling  ############



#Random Sampling (Train and Test)
#We may split the data (given population) into random samples with 50-50, 60-40 or 70-30 ratios
# for Training (Development Sample on which model will be developed or trained) and Test (validation/holdout sample on which model will be tested) based on population size.
#In this exercise we will split the sample into 70-30. You may perform this step even before Univariate analysis.


#Simple random sampling is the most basic sampling technique where we select a group of subjects (a sample) for study from a larger group (a population).
# Each individual is chosen entirely by chance and each member of the population has an equal chance of being included in the sample.

div_part <- sort(sample(nrow(cdata_reduced_2), nrow(cdata_reduced_2)*.7))  ## sample from total rows, a fraction

#select training sample 

train<-cdata_reduced_2[div_part,] # 70% here

pct(train$good_bad_21)


# put remaining into test sample
test<-cdata_reduced_2[-div_part,] # rest of the 30% data goes here

pct(test$good_bad_21)















