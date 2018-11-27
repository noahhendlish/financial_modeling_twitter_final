

#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Financial_modeling")  ## This is your working directory. This is the place from where you will be reading and writting data 

## For this part of the class, we will need to install several new packages:

install.packages(c('lars','glmnet','DT', 'lattice','knitr' ,'ClustOfVar','ape','Information','ROCR','caret','rpart','rpart.utils','rpart.plot','randomForest','party','bnlearn','DAAG','vcd','kernlab')); #if you have not already installed these packages
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
plot(as.numeric(cdata$malicious), ylab="Good-Bad", xlab="n", main="Good ~ Bad")

hist(as.numeric(cdata$good_bad_21), breaks=2, 
		 xlab="Good(1) and Bad(2)", col="blue")




gbpct <- function(x, y=cdata$good_bad_21){
	
	mt <- as.matrix(table(as.factor(x), as.factor(y))) # x -> independent variable(vector), y->dependent variable(vector)
	
	Total <- mt[,1] + mt[,2]                          # Total observations
	
	Total_Pct <- round(Total/sum(mt)*100, 2)          # Total PCT
	
	Bad_pct <- round((mt[,1]/sum(mt[,1]))*100, 2)     # PCT of BAd or event or response
	
	Good_pct <- round((mt[,2]/sum(mt[,2]))*100, 2)   # PCT of Good or non-event
	
	Bad_Rate <- round((mt[,1]/(mt[,1]+mt[,2]))*100, 2) # Bad rate or response rate
	
	grp_score <- round((Good_pct/(Good_pct + Bad_pct))*10, 2) # score for each group
	
	WOE <- round(log(Good_pct/Bad_pct)*10, 2)      # Weight of Evidence for each group
	
	g_b_comp <- ifelse(mt[,1] == mt[,2], 0, 1)
	
	IV <- ifelse(g_b_comp == 0, 0, (Good_pct - Bad_pct)*(WOE/10)) # Information value for each group
	
	Efficiency <- abs(Good_pct - Bad_pct)/2                       # Efficiency for each group
	
	otb<-as.data.frame(cbind(mt, Good_pct,  Bad_pct,  Total, 
													 Total_Pct,  Bad_Rate, grp_score, 
													 WOE, IV, Efficiency ))
	otb$Names <- rownames(otb)
	rownames(otb) <- NULL
	otb[,c(12,2,1,3:11)] # return IV table
}

## First Variable: Checking account status
# Attribute 1:  (qualitative)
#-----------------------------------------------------------
# Checking account status

#          Status of existing checking account
#                A11 :      ... <    0 DM
#          A12 : 0 <= ... <  200 DM
#          A13 :      ... >= 200 DM /
#            salary assignments for at least 1 year
#                A14 : no checking account

# Function: function to calculate IV, WOE and Efficiency 
# The idea is to analyse the predictive power of each variable independetly

A1 <- gbpct(cdata$chk_ac_status_1)
help('gbpct')
barplot(A1$WOE, col="brown", names.arg=c(A1$Levels), 
				main="Score:Checking Account Status",
				xlab="Category",
				ylab="WOE")

barplot(A1$IV, col="brown", names.arg=c(A1$Levels), 
				main="Score:Checking Account Status",
				xlab="Category",
				ylab="IV")


## Loan Duration

# Attribute 2:  (numerical)
#-----------------------------------------------------------
# Loan Duration (Tenure) in Month

summary(cdata$duration_month_2)

op2<-par(mfrow=c(1,2))
boxplot(cdata$duration_month_2, ylab="Loan Duration(Month)", main="Boxplot:Loan Duration")

plot(as.factor(cdata$duration_month_2), cdata$good_bad_21, 
		 ylab="Good-Bad", xlab="Loan Duration(Month)",
		 main="Loan Duration(Before Groupping)")

# Create some groups from contious variables
cdata$duration_month_2 <-as.factor(ifelse(cdata$duration_month_2<=6,'00-06',
																					ifelse(cdata$duration_month_2<=12,'06-12',
																								 ifelse(cdata$duration_month_2<=24,'12-24', 
																								 			 ifelse(cdata$duration_month_2<=30,'24-30',
																								 			 			 ifelse(cdata$duration_month_2<=36,'30-36',
																								 			 			 			 ifelse(cdata$duration_month_2<=42,'36-42','42+')))))))

plot(cdata$duration_month_2, cdata$good_bad_21,
		 main="Loan Duration(after grouping) ",
		 xlab="Loan Duration (Month)",
		 ylab="Good-Bad")


# Lets compute WOE and IV

A2<-gbpct(cdata$duration_month_2)

op2<-par(mfrow=c(1,2))
barplot(A2$WOE, col="brown", names.arg=c(A2$Levels),
				main="Loan Duration",
				xlab="Duration(Months)",
				ylab="WOE")

barplot(A2$IV, col="brown", names.arg=c(A2$Levels),
				main="Loan Duration",
				xlab="Duration(Months)",
				ylab="IV")

# Attribute 3:  (qualitative)
#-----------------------------------------------------------
# Credit History

#         A30 : no credits taken/
#           all credits paid back duly
#               A31 : all credits at this bank paid back duly
#         A32 : existing credits paid back duly till now
#               A33 : delay in paying off in the past
#         A34 : critical account/
#           other credits existing (not at this bank)

# Combine few groups together based on WOE and bad rates

cdata$credit_history_3<-as.factor(ifelse(cdata$credit_history_3 == "A30", "01.A30",
																				 ifelse(cdata$credit_history_3 == "A31","02.A31",
																				 			 ifelse(cdata$credit_history_3 == "A32","03.A32.A33",
																				 			 			 ifelse(cdata$credit_history_3 == "A33","03.A32.A33",
																				 			 			 			 "04.A34")))))


op3<-par(mfrow=c(1,2))

plot(cdata$credit_history_3, cdata$good_bad_21, 
		 main = "Credit History ~ Good-Bad",
		 xlab = "Credit History",
		 ylab = "Good-Bad")

plot(cdata$credit_history_3, cdata$good_bad_21, 
		 main = "Credit History(After Groupping) ~ Good-Bad ",
		 xlab = "Credit History",
		 ylab = "Good-Bad")



A3<-gbpct(cdata$credit_history_3)

op2<-par(mfrow=c(1,2))

barplot(A3$WOE, col="brown", names.arg=c(A3$Levels),
				main="Credit History",
				xlab="Credit History",
				ylab="WOE")

barplot(A3$IV, col="brown", names.arg=c(A3$Levels),
				main="Credit History",
				xlab="Credit History",
				ylab="IV")

# Attribute 4:  (qualitative)
#-----------------------------------------------------------
# Purpose of the loan

# 
#         A40 : car (new)
#         A41 : car (used)
#         A42 : furniture/equipment
#         A43 : radio/television
#         A44 : domestic appliances
#         A45 : repairs
#         A46 : education
#         A47 : (vacation - does not exist?)
#         A48 : retraining
#         A49 : business
#         A410 : others


A4<-gbpct(cdata$purpose_4)


op4<-par(mfrow=c(1,2))


barplot(A4$WOE, col="brown", names.arg=c(A4$Levels),
				main="Purpose of Loan",
				xlab="Category",
				ylab="WOE")
barplot(A4$IV, col="brown", names.arg=c(A4$Levels),
				main="Purpose of Loan",
				xlab="Category",
				ylab="IV")

# Attribute 5:  (numerical)
#-----------------------------------------------------------
# Credit (Loan) Amount

cdata$credit_amount_5 <- as.double(cdata$credit_amount_5)

summary(cdata$credit_amount_5)

boxplot(cdata$credit_amount_5)

# Create groups based on their distribution
cdata$credit_amount_5<-as.factor(ifelse(cdata$credit_amount_5<=1400,'0-1400',
																				ifelse(cdata$credit_amount_5<=2500,'1400-2500',
																							 ifelse(cdata$credit_amount_5<=3500,'2500-3500', 
																							 			 ifelse(cdata$credit_amount_5<=4500,'3500-4500',
																							 			 			 ifelse(cdata$credit_amount_5<=5500,'4500-5500','5500+'))))))


A5<-gbpct(cdata$credit_amount_5)



plot(cdata$credit_amount_5, cdata$good_bad_21, 
		 main="Credit Ammount (After Grouping) ~ Good-Bad",
		 xlab="Amount",
		 ylab="Good-Bad")

op5<-par(mfrow=c(1,2))

barplot(A5$WOE, col="brown", names.arg=c(A5$Levels),
				main="Credit Ammount",
				xlab="Amount",
				ylab="WOE")

barplot(A5$IV, col="brown", names.arg=c(A5$Levels),
				main="Credit Ammount",
				xlab="Amount",
				ylab="IV")

# Attibute 6:  (qualitative)
#-----------------------------------------------------------
# Savings account/bonds

#         A61 :          ... <  100 DM
#         A62 :   100 <= ... <  500 DM
#         A63 :   500 <= ... < 1000 DM
#         A64 :          .. >= 1000 DM
#               A65 :   unknown/ no savings account

A6<-gbpct(cdata$savings_ac_bond_6)

op6<-par(mfrow=c(1,2))

barplot(A6$WOE, col="brown", names.arg=c(A6$Levels),
				main="Credit Ammount",
				xlab="Amount",
				ylab="WOE")

barplot(A6$IV, col="brown", names.arg=c(A6$Levels),
				main="Credit Ammount",
				xlab="Amount",
				ylab="IV")

# Attribute 7:  (qualitative)
#-----------------------------------------------------------
# Present employment since

# A71 : unemployed
# A72 :       ... < 1 year
# A73 : 1  <= ... < 4 years
# A74 : 4  <= ... < 7 years
# A75 :       .. >= 7 years

A7<-gbpct(cdata$p_employment_since_7)

op7<-par(mfrow=c(1,2))


barplot(A7$WOE, col="brown", names.arg=c(A7$Levels),
				main="Present employment",
				xlab="Category",
				ylab="WOE")

barplot(A7$IV, col="brown", names.arg=c(A7$Levels),
				main="Present employment",
				xlab="Category",
				ylab="IV")

# Attribute 8:  (numerical)
#-----------------------------------------------------------
# instalment rate in percentage of disposable income

summary(cdata$instalment_pct_8)

op8<-par(mfrow=c(1,2))
boxplot(cdata$instalment_pct_8)
hist(cdata$instalment_pct_8,
		 main = "instalment rate in percentage of disposable income",
		 xlab = "instalment percent",
		 ylab = "Percent Population")
par(op8)

A8<-gbpct(cdata$instalment_pct_8)

op8_1<-par(mfrow=c(1,2))
plot(as.factor(cdata$instalment_pct_8), cdata$good_bad_21,
		 main="instalment rate in % of disposable income ~ G-B",
		 xlab="Percent",
		 ylab="Good-Bad")

barplot(A8$WOE, col="brown", names.arg=c(A8$Levels),
				main="instalment rate",
				xlab="Percent",
				ylab="WOE")

# Attribute 9:  (qualitative)
#-----------------------------------------------------------
# Personal status and sex - you may not use for some country due to regulations

#         A91 : male   : divorced/separated
#         A92 : female : divorced/separated/married
#               A93 : male   : single
#         A94 : male   : married/widowed
#         A95 : female : single

A9<-gbpct(cdata$personal_status_9)

op9<-par(mfrow=c(1,2))


barplot(A9$WOE, col="brown", names.arg=c(A9$Levels),
				main="Personal status",
				xlab="Category",
				ylab="WOE")

barplot(A9$IV, col="brown", names.arg=c(A9$Levels),
				main="Personal status",
				xlab="Category",
				ylab="IV")


# Attribute 10: (qualitative)      
#-----------------------------------------------------------
# Other debtors / guarantors

#         A101 : none
#         A102 : co-applicant
#         A103 : guarantor

A10<-gbpct(cdata$other_debtors_or_grantors_10)

op10<-par(mfrow=c(1,2))


barplot(A10$WOE, col="brown", names.arg=c(A10$Levels),
				main="Other debtors / guarantors",
				xlab="Category",
				ylab="WOE")

barplot(A10$IV, col="brown", names.arg=c(A10$Levels),
				main="Other debtors / guarantors",
				xlab="Category",
				ylab="IV")

# Attribute 11: (numerical)
#-----------------------------------------------------------
# Present residence since
summary(cdata$present_residence_since_11)

A11<-gbpct(cdata$present_residence_since_11)

op11<-par(mfrow=c(1,2))

hist(cdata$present_residence_since_11,
		 main="Present Residence~ Good-Bad",
		 xlab="Present residence Since", 
		 ylab="Percent Population")

barplot(A11$WOE, col="brown", names.arg=c(A11$Levels),
				main="Present Residence",
				xlab="Category",
				ylab="WOE")


# Attribute 12: (qualitative)
#-----------------------------------------------------------
# Property
#         A121 : real estate
#         A122 : if not A121 : building society savings agreement/
#                  life insurance
#               A123 : if not A121/A122 : car or other, not in attribute 6
#         A124 : unknown / no property

A12 <- gbpct(cdata$property_type_12)

op12 <- par(mfrow = c(1,2))


barplot(A12$WOE, col="brown", names.arg=c(A12$Levels),
				main="Property Type",
				xlab="Category",
				ylab="WOE")

barplot(A12$IV, col="brown", names.arg=c(A12$Levels),
				main="Property Type",
				xlab="Category",
				ylab="IV")

# Attribute 13: (numerical)
#-----------------------------------------------------------
# Age in Years

summary(cdata$age_in_yrs_13)

op13 <- par(mfrow = c(1,2))
boxplot(cdata$age_in_yrs_13)

plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21,
		 main = "Age",
		 xlab = "Age in Years",
		 ylab = "Good-Bad")


par(op13)

# Group AGE - Coarse Classing (after some iterations in fine classing stage)
cdata$age_in_yrs_13 <- as.factor(ifelse(cdata$age_in_yrs_13<=25, '0-25',
																				ifelse(cdata$age_in_yrs_13<=30, '25-30',
																							 ifelse(cdata$age_in_yrs_13<=35, '30-35', 
																							 			 ifelse(cdata$age_in_yrs_13<=40, '35-40', 
																							 			 			 ifelse(cdata$age_in_yrs_13<=45, '40-45', 
																							 			 			 			 ifelse(cdata$age_in_yrs_13<=50, '45-50',
																							 			 			 			 			 ifelse(cdata$age_in_yrs_13<=60, '50-60',
																							 			 			 			 			 			 '60+'))))))))


A13<-gbpct(cdata$age_in_yrs_13)

op13_1<-par(mfrow=c(1,2))


barplot(A13$WOE, col="brown", names.arg=c(A13$Levels),
				main="Age",
				xlab="Category",
				ylab="WOE")


barplot(A13$IV, col="brown", names.arg=c(A13$Levels),
				main="Age",
				xlab="Category",
				ylab="IV")


# Attribute 14: (qualitative)
#-----------------------------------------------------------
#         Other instalment plans 
#         A141 : bank
#         A142 : stores
#         A143 : none

A14<-gbpct(cdata$other_instalment_type_14)

op14<-par(mfrow=c(1,2))

barplot(A14$WOE, col="brown", names.arg=c(A14$Levels),
				main="Other instalment plans",
				xlab="Category",
				ylab="WOE")

barplot(A14$IV, col="brown", names.arg=c(A14$Levels),
				main="Other instalment plans",
				xlab="Category",
				ylab="IV")

# Attribute 15: (qualitative)
#-----------------------------------------------------------
#         Housing
#         A151 : rent
#         A152 : own
#         A153 : for free

A15<-gbpct(cdata$housing_type_15)

op15<-par(mfrow=c(1,2))


barplot(A15$WOE, col="brown", names.arg=c(A15$Levels),
				main="Home Ownership Type",
				xlab="Type",
				ylab="WOE")

barplot(A15$IV, col="brown", names.arg=c(A15$Levels),
				main="Home Ownership Type",
				xlab="Type",
				ylab="IV")


# Attribute 16: (numerical)
#-----------------------------------------------------------
#               Number of existing credits at this bank

summary(cdata$number_cards_this_bank_16)

A16<-gbpct(cdata$number_cards_this_bank_16)

op16<-par(mfrow=c(1,2))
plot(as.factor(cdata$number_cards_this_bank_16), cdata$good_bad_21,
		 main="Number of credits at this bank",
		 xlab="Number of Cards",
		 ylab="Good-Bad")

barplot(A16$WOE, col="brown", names.arg=c(A16$Levels),
				main="Number of credits at this bank",
				xlab="Number of Cards",
				ylab="WOE")

# Attribute 17: (qualitative)
#-----------------------------------------------------------
#         Job
#         A171 : unemployed/ unskilled  - non-resident
#         A172 : unskilled - resident
#         A173 : skilled employee / official
#         A174 : management/ self-employed/
#            highly qualified employee/ officer

A17<-gbpct(cdata$job_17)

op17<-par(mfrow=c(1,2))


barplot(A17$WOE, col="brown", names.arg=c(A17$Levels),
				main="Employment Type",
				xlab="Job",
				ylab="WOE")

barplot(A17$IV, col="brown", names.arg=c(A17$Levels),
				main="Employment Type",
				xlab="Job",
				ylab="IV")

# Attribute 18: (numerical)
#-----------------------------------------------------------
#         Number of people being liable to provide maintenance for

summary(cdata$no_people_liable_for_mntnance_18)

A18<-gbpct(cdata$no_people_liable_for_mntnance_18)

op18<-par(mfrow = c(1,2))

plot(as.factor(cdata$no_people_liable_for_mntnance_18), cdata$good_bad_21, 
		 main = "Number of people being liable",
		 xlab = "Number of People",
		 ylab = "Good-Bad")

barplot(A18$WOE, col = "brown", names.arg=c(A18$Levels),
				main = " Number of people being liable",
				xlab = "Number of People",
				ylab = "WOE")

# Attribute 19: (qualitative)
#-----------------------------------------------------------
#         Telephone
#         A191 : none
#         A192 : yes, registered under the customers name

A19<-gbpct(cdata$telephone_19)

op19<-par(mfrow=c(1,2))


barplot(A19$WOE, col="brown", names.arg=c(A19$Levels),
				main="Telephone",
				xlab="Telephone(Yeas/No)",
				ylab="WOE")

barplot(A19$IV, col="brown", names.arg=c(A19$Levels),
				main="Telephone",
				xlab="Telephone(Yeas/No)",
				ylab="IV")


# Attribute 20: (qualitative)
#-----------------------------------------------------------
#         foreign worker
#         A201 : yes
#         A202 : no


A20<-gbpct(cdata$foreign_worker_20)

op20<-par(mfrow=c(1,2))


barplot(A20$WOE, col="brown", names.arg=c(A20$Levels),
				main="Foreign Worker",
				xlab="Foreign Worker(Yes/No)",
				ylab="WOE")

barplot(A20$IV, col="brown", names.arg=c(A20$Levels),
				main="Foreign Worker",
				xlab="Foreign Worker(Yes/No)",
				ylab="IV")


