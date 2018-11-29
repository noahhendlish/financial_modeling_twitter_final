#Twitter Malicious Tweets Data Analysis
#Noah Hendlish
#FINAL PROJECT: CLASSIFICATION OF MISINFORMATION CAMPAIGN BOTS
install.packages(c('data.table','stringr','lars','glmnet','DT', 'lattice','knitr' ,'ClustOfVar','ape','Information','ROCR','caret','rpart','rpart.utils','rpart.plot','randomForest','party','bnlearn','DAAG','vcd','kernlab')); #if you have not already installed these packages
install.packages('rapportools')
install.packages("rmarkdown")
library(rmarkdown)
library(rapportools)
library(stats)
library(plyr)
library(data.table)
library(stringr)
library(lars)
library(glmnet)
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
library(stringr)

#In this all data set any variable that starts with "is" --> binary (0 or 1): 1 --> true, 0--> false
#to classify as factors: "account_language", "tweet_language", "tweet_client"
#as.factor(all_tweets$account_language)
#cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 1, "Good", "Bad"))  ## we create a factor, and code it as good or bad 
csv_headers = c("num","tweetid", "user_reported_location", "follower_count" ,"following_count", 
								"account_creation_date", "tweet_date", "account_language",              
								"tweet_language", "tweet_text", "tweet_client", "is_reply",  "is_quote",
								"is_Retweet", "retweet_count", "like_count", "hashtags", "urls", 
								"user_mentions","user_has_reported_location", "acct_tweet_lang_same",         
								"tweet_lang_english","tweet_lang_russian", "has_hashtags","num_hashtags",
								"has_urls", "num_urls", "has_user_mentions", "num_user_mentions",            
								"diff_tweet_acct_creation_time", "malicious")
nonmalicious_tweets <- read.csv('all_random_tweets_cleaned.csv')
malicious_tweets <- read.csv('all_ira_tweets_cleaned.csv')
all_tweets <- read.csv('all_tweets.csv')
all_tweets_testing <- read.csv('all_tweets.csv')
colnames(nonmalicious_tweets) <- csv_headers
colnames(malicious_tweets) <- csv_headers
colnames(all_tweets) <- csv_headers
colnames(all_tweets_testing) <- csv_headers
## Data procesecing
all_tweets$num <- as.numeric(all_tweets$num)
all_tweets$tweetid  <-  as.numeric(all_tweets$tweetid )
all_tweets$follower_count <-  as.numeric(all_tweets$follower_count)
all_tweets$following_count <-  as.numeric(all_tweets$following_count)
all_tweets$account_creation_date <-  as.Date(all_tweets$account_creation_date)
all_tweets$tweet_date <-  as.Date(all_tweets$tweet_date)
all_tweets$is_reply <-  as.numeric(all_tweets$is_reply)
all_tweets$is_quote <-  as.numeric(all_tweets$is_quote)
all_tweets$is_Retweet <-  as.numeric(all_tweets$is_Retweet)
all_tweets$retweet_count <-  as.numeric(all_tweets$retweet_count)
all_tweets$like_count <-  as.numeric(all_tweets$like_count)
all_tweets$user_has_reported_location <-  as.numeric(all_tweets$user_has_reported_location)
all_tweets$acct_tweet_lang_same <-  as.numeric(all_tweets$acct_tweet_lang_same)
all_tweets$tweet_lang_english <-  as.numeric(all_tweets$tweet_lang_english)
all_tweets$tweet_lang_russian <-  as.numeric(all_tweets$tweet_lang_russian)
all_tweets$has_hashtags <-  as.numeric(all_tweets$has_hashtags)
all_tweets$num_hashtags <-  as.numeric(all_tweets$num_hashtags)
all_tweets$num_urls <-  as.numeric(all_tweets$num_urls)
all_tweets$has_urls <-  as.numeric(all_tweets$has_urls)
all_tweets$has_user_mentions <-  as.numeric(all_tweets$has_user_mentions)
all_tweets$num_user_mentions <-  as.numeric(all_tweets$num_user_mentions)
all_tweets$diff_tweet_acct_creation_time <-  as.numeric(all_tweets$diff_tweet_acct_creation_time)
all_tweets$malicious <-  as.numeric(all_tweets$malicious)
#define as factor based on language keyword
#factor account langauages (most popular)	
all_tweets$account_language = as.factor(ifelse(all_tweets$account_language=='ar', 'arabic',
																							 ifelse(all_tweets$account_language=='de', 'german',
																							 			 ifelse(all_tweets$account_language=='en', 'english',
																							 			 			 			 ifelse(all_tweets$account_language=='en-gb', 'english',
																							 			 			 			 			 ifelse(all_tweets$account_language=='en-GB', 'english',
																							 			 			 			 			 			 ifelse(all_tweets$account_language=='es', 'spanish',
																							 			 			 			 			 			 			 ifelse(all_tweets$account_language=='fr', 'french',
																							 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='id', 'indonesian',
																							 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='ja', 'japanese',
																							 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='ko', 'korean',
																							 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='pt', 'portugese',
																							 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='ru', 'russian',
																							 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='th', 'thai',
																							 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='tr', 'turkish',
																							 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='zh-cn', 'chinese',
																							 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$account_language=='zh-CN', 'chinese',
																							 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 'Other')))))))))))))))))

#FACTOR TWEET LANGUAGES
#factor tweet langauages--most popular			 			 			 			 			 			 			 			 			 			 			 
all_tweets$tweet_language <-as.factor(ifelse(all_tweets$tweet_language=='ar', 'arabic',
																						 ifelse(all_tweets$tweet_language=='bg','bulgarian',
																						 			 ifelse(all_tweets$tweet_language=='ca','catalan',
																						 			 			 ifelse(all_tweets$tweet_language=='de', 'german',
																						 			 			 			 ifelse(all_tweets$tweet_language=='en','english',
																						 			 			 			 			 ifelse(all_tweets$tweet_language=='es', 'spanish',
																						 			 			 			 			 			 ifelse(all_tweets$tweet_language=='et','estonian',
																						 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='fi','finnish',
																						 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='fr', 'french',
																						 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='hi','hindi',
																						 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='ht','haitian-creole',
																						 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='it','italian',
																						 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='ja','japanese',
																						 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='ko', 'korean',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='nl','dutch',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='no','norwegian',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='pl', 'polish',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='pt','portuguese',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='ro','romanian',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='ru','russian',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='sv','swedish',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='th', 'thai',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='tl','tagalog',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='tr','turkish',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='uk', 'ukrainian',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='zh','chinese',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='und','undefined',
																						 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_language=='in', 'international/unkown','other')))))))))))))))))))))))))))))

#summary(all_tweets$account_language)
#define as factor based on most popular clients (rest are "other")
all_tweets$tweet_client <-as.factor(ifelse(all_tweets$tweet_client=='Twitter Web Client','Twitter Web Client',
																										 ifelse(all_tweets$tweet_client=='Twitter for iPhone','Twitter for iPhone',
																										 			 ifelse(all_tweets$tweet_client=='Twitter for Android','Twitter for Android', 
																										 			 			 ifelse(all_tweets$tweet_client=='Twitter Lite','Twitter Lite',
																										 			 			 			 ifelse(all_tweets$tweet_client=='twitterfeed','twitterfeed',
																										 			 			 			 			 ifelse(all_tweets$tweet_client=='TweetDeck','TweetDeck',
																										 			 			 			 			 			 ifelse(all_tweets$tweet_client=='Instagram','Instagram',
																										 			 			 			 			 			 			 ifelse(all_tweets$tweet_client=='twittbot.net','twittbot.net',
																										 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_client=='Twitter for iPad','Twitter for iPad',
																										 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_client=='IFTTT','IFTTT',
																										 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_client=='newtwittersky','newtwittersky',
																										 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_client=='Facebook','Facebook',
																										 			 			 			 			 			 			 			 			 			 			 			 ifelse(all_tweets$tweet_client=='Google','Google',
																										 			 			 			 			 			 																																			'Other'))))))))))))))
#all_tweets <- na.omit(all_tweets)
#train <-na.omit(train)
#test <-na.omit(test)
#split data sets into qualitative and quantitative
quanti_headers = c("follower_count" ,"following_count",
									 "is_reply",  "is_quote","is_Retweet", "retweet_count", "like_count","user_has_reported_location", 
									 "acct_tweet_lang_same", "tweet_lang_english","tweet_lang_russian","num_hashtags",
									 "num_urls", "num_user_mentions",            
									 "diff_tweet_acct_creation", "malicious")
quali_headers = c("account_language", "tweet_language", "tweet_client")
all_final_headers =c("account_language", "tweet_language", "tweet_client","follower_count" ,"following_count",
										 "is_reply",  "is_quote","is_Retweet", "retweet_count", "like_count","user_has_reported_location", 
										 "acct_tweet_lang_same", "tweet_lang_english","tweet_lang_russian","num_hashtags",
										 "num_urls", "num_user_mentions",            
										 "diff_tweet_acct_creation", "malicious")
all_tweets_quanti <- data.frame(all_tweets$follower_count, all_tweets$following_count, 
														all_tweets$is_reply,  all_tweets$is_quote, all_tweets$is_Retweet, all_tweets$retweet_count, 
														all_tweets$like_count, all_tweets$user_has_reported_location, all_tweets$acct_tweet_lang_same,         
														all_tweets$tweet_lang_english,all_tweets$tweet_lang_russian,
														all_tweets$num_hashtags, all_tweets$num_urls, 
														all_tweets$num_user_mentions,all_tweets$diff_tweet_acct_creation_time, all_tweets$malicious)
all_tweets_quali <- data.frame(all_tweets$account_language, all_tweets$tweet_language, all_tweets$tweet_client)
all_tweets_final <- data.frame(all_tweets$account_language, all_tweets$tweet_language, all_tweets$tweet_client, all_tweets$follower_count, all_tweets$following_count, 
																all_tweets$is_reply,  all_tweets$is_quote, all_tweets$is_Retweet, all_tweets$retweet_count, 
																all_tweets$like_count, all_tweets$user_has_reported_location, all_tweets$acct_tweet_lang_same,         
																all_tweets$tweet_lang_english,all_tweets$tweet_lang_russian,
																all_tweets$num_hashtags, all_tweets$num_urls, 
																all_tweets$num_user_mentions,all_tweets$diff_tweet_acct_creation_time, all_tweets$malicious) 
colnames(all_tweets_quali) <-quali_headers
colnames(all_tweets_quanti) <- quanti_headers
colnames(all_tweets_final) <- all_final_headers
str(all_tweets_final)

non_malicious_in_total_set <- subset(x=all_tweets_final, all_tweets_final$malicious ==0)
malicious_in_total_set <- subset(x=all_tweets_final, all_tweets_final$malicious == 1)
percent_of_malicious_in_total_set= nrow(malicious_in_total_set)/nrow(all_tweets_final)
#######BASIC ANALYSIS##########
sapply(all_tweets_final,function(x) sum(x == 'international/unkown'))
sapply(all_tweets_final,function(x) sum(x == 'undefined'))
sapply(all_tweets_final,function(x) sum(x == 'other'))
sapply(all_tweets_final,function(x) sum(x == 'Other'))
sapply(all_tweets_final, function(x) length(unique(x)))
sapply(all_tweets_final, sd)
install.packages("corrplot")
install.packages("heuristica")
library(corrplot)
library(caret)
library(heuristica)
#corrplot(all_tweets_final, method = 'shade')
#rquery.cormat(all_tweets_final)
#rquery.cormat(all_tweets_final, type="upper")
#rquery.cormat(all_tweets_final, type="full")
#col<- colorRampPalette(c("blue", "white", "red"))(20)
#cormat<-rquery.cormat(mydata, type="full", col=col)
#cormat<-rquery.cormat(mydata, graphType="heatmap")

names(all_tweets_final)
#ONLY TAKE SIGNIFICGANT VARIABLES (COLUMNS)
all_tweets_sig = all_tweets_final[c(1:nrow(all_tweets_final)),c(2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19)]
######### Sampling  ##################### Sampling  ##################### Sampling  ############
#Random Sampling (Train and Test)
#We may split the data (given population) into random samples with 50-50, 60-40 or 70-30 ratios
# for Training (Development Sample on which model will be developed or trained) and Test (validation/holdout sample on which model will be tested) based on population size.
#In this exercise we will split the sample into 70-30. You may perform this step even before Univariate analysis.
non_malicious_in_set <- subset(x=all_tweets_final, all_tweets_final$malicious ==0)
malicious_in_set <- subset(x=all_tweets_final, all_tweets_final$malicious == 1)
percent_of_malicious_in_set= nrow(malicious_in_set)/nrow(all_tweets_final)
#Simple random sampling is the most basic sampling technique where we select a group of subjects (a sample) for study from a larger group (a population).
# Each individual is chosen entirely by chance and each member of the population has an equal chance of being included in the sample.
#div_part <- sort(sample(nrow(all_tweets_final), nrow(all_tweets_final)))  ## sample from total rows, a fraction
div_part <- sort(sample(nrow(all_tweets_final), nrow(all_tweets_final)*.5))
#select training sample 
train_all<-all_tweets_final[div_part,] # 70% here
train <- all_tweets_final[div_part,c(2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19)]
non_malicious_in_train <- subset(x=train, train$malicious ==0)
malicious_in_train <- subset(x=train, train$malicious ==1)
percent_of_malicious_in_train = nrow(malicious_in_train)/nrow(train)
# put remaining into test sample
test_all<-all_tweets_final[-div_part,] # rest of the 30% data goes here
test <- all_tweets_final[-div_part,c(2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19)]
non_malicious_in_test<- subset(x=train, train$malicious ==0)
malicious_in_test <- subset(x=train, train$malicious ==1)
percent_of_malicious_in_test = nrow(malicious_in_test)/nrow(test)
summary(div_part)
##get samples with same % of malicious in test as in train (~16.5%)--> which is consistent with population
# Model: Stepwise Logistic Regression Model  # Model: Stepwise Logistic Regression Model  # Model: Stepwise Logistic Regression Model# Model: Stepwise Logistic Regression Model
#malicious~.
########LOGISTIC REGRESSION #########
help(glm)
regression1 <-glm(malicious~.,data=train_all,family=binomial())
regression2 <- glm(malicious~  tweet_client+follower_count+following_count+is_reply+is_quote +is_Retweet +retweet_count+like_count+user_has_reported_location+acct_tweet_lang_same+tweet_lang_english+tweet_lang_russian+num_hashtags+ num_urls+num_user_mentions+diff_tweet_acct_creation_time,data=train,family=binomial())
regression3 <- glm(malicious~  tweet_client+follower_count+following_count+is_reply+is_quote +is_Retweet +retweet_count+like_count+user_has_reported_location+acct_tweet_lang_same+tweet_language+num_hashtags+ num_urls+num_user_mentions+diff_tweet_acct_creation_time,data=train,family=binomial())
regression4 <- glm(malicious~  follower_count+following_count+is_reply+is_quote +is_Retweet +retweet_count+like_count+user_has_reported_location+acct_tweet_lang_same+tweet_lang_english+tweet_lang_russian+num_hashtags+ num_urls+num_user_mentions+diff_tweet_acct_creation_time,data=train,family=binomial())
regression_sig <-glm(malicious~.,data=train,family=binomial())
m1 <-glm(malicious~.,data=train,family=binomial())
summary(m1)
summary(regression1)
summary(regression2)
summary(regression3)
summary(regression4)
summary(regression_sig)
help(anova)
anova(regression_sig, test="Chisq")
#likelihood_ratio:
anova(regression_sig, test="rao")

# List of significant variables and features with p-value <0.01
significant.variables <- summary(regression_sig)$coeff[-1,4] < 0.01
names(significant.variables)[significant.variables == TRUE]
signif_var_names = names(significant.variables)[significant.variables == TRUE]
prob <- predict(regression_sig, type = "response")
help(predict)
res <- residuals(regression_sig, type = "deviance")

## CIs using profiled log-likelihood
CI_profile_log_likelihood = confint(regression_sig)
summary(CI_profile_log_likelihood)
## CIs using standard errors
CI_using_stderrors = confint.default(regression_sig)
summary(CI_using_stderrors)
## odds ratios and 95% CI
odd_ratios_95CI = exp(cbind(OR = coef(regression_sig), confint(regression_sig)))
summary(odd_ratios_95CI)
#score test data set
test$m1_score <- predict(regression_sig,type='response',test)
m1_pred <- prediction(test$m1_score, test$malicious)
m1_perf <- performance(m1_pred,"tpr","fpr")
#wald test
#wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
#exp(coef(mylogit))
#exp(cbind(OR = coef(mylogit), confint(mylogit)))
#newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
#newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
#https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
#ROC
par(mfrow=c(1,1))
plot(m1_perf, lwd=2, colorize=TRUE, main="ROC m1: Logistic Regression Performance")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

####CONFUSION MATRIX
# Plot precision/recall curve
m1_perf_precision <- performance(m1_pred, measure = "prec", x.measure = "rec")
plot(m1_perf_precision, main="m1 Logistic:Precision/recall curve")
#confusionMatrix(m1_pred, m1_perf)
#confusionMatrix(factor(m1_pred),factor(m1_perf_precision))

### Partitioning ######## Partitioning ######## Partitioning ######## Partitioning ######## Partitioning #####

#Basic recursive partitioning
library(rpart)
library(kernlab) #for SVM..always explore the library!
#test1 <- na.exclude(test)
#train1 <- na.exclude(train)
train_numeric = train
test_numeric = test
test_numeric <- na.omit(test_numeric)
train_numeric <- na.omit(train_numeric)
test_numeric$malicious <- as.numeric(test_numeric$malicious)
train_numeric$malicious <- as.numeric(train_numeric$malicious)

test_factor = test
train_factor = train
test_factor <- na.omit(test_factor)
train_factor <- na.omit(train_factor)
test_factor$malicious <- as.factor(test_factor$malicious)
train_factor$malicious <- as.factor(train_factor$malicious)

test_factor1 <- na.omit(test_factor)
train_factor1 <- na.omit(train_factor)
test_factor1 <- as.factor(test_factor1)
train_factor1 <- as.factor(train_factor1)

m2 <- rpart(malicious~.,data=train)
#m2_a <- rpart(malicious~.,data=train)
m2_b <- rpart(malicious~.,data=train_factor)

# Print tree detail
printcp(m2)
#printcp(m2_a)
printcp(m2_b)
# Tree plot
plot(m2, main="Tree:Recursive Partitioning");text(m2)
#plot(m2_a, main="Tree:Recursive Partitioning");text(m2_a)

# Better version of plot
prp(m2,type=2,extra=1,  main="Tree:Recursive Partitioning")
prp(m2_a,type=2,extra=1,  main="Tree:Recursive Partitioning")
# score test data
test2 <- na.omit(test)
#test$m2_score1 <- predict(m2, test_factor1, type='prob')
test$m2_score <- predict(m2, test, type='vector')
#m2_pred <- prediction(test$m2_score[,2],test$malicious)
m2_pred <- prediction(test$m2_score,test$malicious)
m2_perf <- performance(m2_pred,"tpr","fpr")

### MOdel performance plot ###
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


#### Bayesian Partitioning ####

# Fit Model 
#### m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.9,.1)),cp=.0002) #- build model using 90%-10% priors
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.8,.2)),cp=.0002) #- build model using 80%-20% priors
#m2_1 <- rpart(malicious~.,data=train,parms=list(prior=c(.7,.3)),cp=.0002)  #- build model using 70%-30% priors    ## Provide priors to a bayesian model  ## Notice, this are the correct priors
#### m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.75,.25)),cp=.0002) #- build model using 75%-25% priors
#m2_1 <- rpart(malicious~.,data=train_1,parms=list(prior=c(.6,.4)),cp=.0002) #- build model using 60%-40% priors
m2_1 <- rpart(malicious~.,data=train_1,parms=list(prior=c(.83,.17)),cp=.0002) #- build model using 60%-40% priors

# Print tree detail
printcp(m2_1)

help(prediction)
# plot trees 
plot(m2_1, main="m2_1-Recursive Partitioning - Using Bayesian N 83%-17%");text(m2_1)
prp(m2_1,type=2,extra=1, main="m2_1-Recursive Partitioning - Using Bayesian N 83%-17%")
#test$m2_1_score <- predict(m2_1,type='prob',test)
test$m2_1_score <- predict(m2_1,type='vector',test)
#m2_1_pred <- prediction(test$m2_1_score[,2],test$malicious)
m2_1_pred <- prediction(test$m2_1_score,test$malicious)
m2_1_perf <- performance(m2_1_pred,"tpr","fpr")

# MOdel performance plot
plot(m2_1_perf, colorize=TRUE, main="ROC m2_1: Recursive Partitioning - Using Bayesian N 83%-17%")
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

#################RANDOM FOREST ##################
test2 <- na.omit(test)
train2 <- na.omit(train)
#test2 <- na.exclude(test)
#train2 <- na.exclude(train)
test2$malicious <- as.factor(test2$malicious)
train2$malicious <- as.factor(train2$malicious)
test3 <- (test2)
train3 <- (train2)
#m2 <- rpart(malicious~.,data=train)
############
m3 <- randomForest(malicious ~ ., data = train2)
#m3 <- randomForest(malicious ~ ., data = train3, importance=TRUE,prOximity=TRUE,na.action=na.roughfix
#m3_fitForest <- predict(m3, newdata = test, type="prob")[,2]
m3_fitForest <- predict(m3, newdata = test, type="prob")[,2]
#m3_fitForest <- predict(m3, newdata = test, type="regression")[,2]
m3_pred <- prediction( m3_fitForest, test$malicious)
m3_perf <- performance(m3_pred, "tpr", "fpr")
m4 <- cforest(malicious~., data = train, control = cforest_unbiased(mtry = 2, ntree = 50))

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
# (i) A preference for the selection of correlated predictors in the tree building process and 
# (ii) an additional advantage for correlated predictor variables induced by the unconditional permutation scheme that is employed in the computation of the variable importance measure.
# Based on these considerations we develop a new, conditional permutation scheme for the computation of the variable importance measure.
# The resulting conditional variable importance reflects the true impact of each predictor variable more reliably than the original marginal approach.
## https://epub.ub.uni-muenchen.de/9387/1/techreport.pdf
## Note how info, and packages are publicly available!!
## Lets use this package
library(party)
set.seed(123456742)
m3_1 <- cforest(malicious~., control = cforest_unbiased(mtry = 2, ntree = 50), data = train)

summary(m3_1)

# Model Performance
m3_1_fitForest <- predict(m3, newdata = test, type = "prob")[,2]
m3_1_pred <- prediction(m3_1_fitForest, test$malicious)
m3_1_perf <- performance(m3_1_pred, "tpr", "fpr")

# Model Performance Plot
plot(m3_1_perf, colorize=TRUE, lwd=2, main = " m3_1 ROC: Conditional Random Forests")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)



####### BAyesian Network#############
library(bnlearn)
train_2<-train
names(train_2)
#names:
#tweet_language                
#tweet_client    
train_2_1 = train_2
train_2_1$follower_count <- as.factor(train_2_1$follower_count)
train_2_1$following_count <- as.factor(train_2_1$following_count)
train_2_1$retweet_count <- as.factor(train_2_1$retweet_count)
train_2_1$like_count <- as.factor(train_2_1$like_count)
train_2_1$num_hashtags <- as.factor(train_2_1$num_hashtags)
train_2_1$num_urls <- as.factor(train_2_1$num_urls)
train_2_1$num_user_mentions <- as.factor(train_2_1$num_user_mentions)
train_2_1$is_reply <- as.factor(train_2_1$is_reply)
train_2_1$is_quote <- as.factor(train_2_1$is_quote)
train_2_1$is_Retweet <- as.factor(train_2_1$is_Retweet)
train_2_1$user_has_reported_location <- as.factor(train_2_1$user_has_reported_location)
train_2_1$acct_tweet_lang_same <- as.factor(train_2_1$acct_tweet_lang_same)
train_2_1$malicious <- as.factor(train_2$malicious)
train_2_1 <- na.omit(train_2_1)
bn.gs <- gs(train_2_1)
bn.gs
bn2 <- iamb(train_2_1)
bn2
bn3 <- fast.iamb(train_2_1)
bn3
bn4 <- inter.iamb(train_2_1)

######HILL CLIMBING ######
compare(bn.gs, bn2)
#On the other hand hill-climbing results in a completely directed network, which differs from
#the previous one because the arc between A and B is directed (A ! B instead of A  B).
#train_2_2$follower_count, train_2_2$following_count, train_2_2$retweet_count,
#train_2_2$like_count, train_2_2$num_hashtags,train_2_2$num_urls,
#train_2_2$num_user_mentions,
#train_2_2$is_reply,train_2_2$is_quote,train_2_2$is_Retweet,
#ttrain_2_2$user_has_reported_location,train_2_2$acct_tweet_lang_same, 
#train_2_2$diff_tweet_acct_creation_time, train_2_2$malicious
names(train)
train_2_2 <- train_2
train_2_2 <-subset(x = train_2, select = c(3,4,5,6,7,8,9,10,11,12,13,14,15))
names(train_2_2)
train_2_2$follower_count <- as.numeric(train_2_2$follower_count)
train_2_2$following_count <- as.numeric(train_2_2$following_count)
train_2_2$retweet_count <- as.numeric(train_2_2$retweet_count)
train_2_2$like_count <- as.numeric(train_2_2$like_count)
train_2_2$num_hashtags <- as.numeric(train_2_2$num_hashtags)
train_2_2$num_urls <- as.numeric(train_2_2$num_urls)
train_2_2$num_user_mentions <- as.numeric(train_2_2$num_user_mentions)
train_2_2$is_reply <- as.numeric(train_2_2$is_reply)
train_2_2$is_quote <- as.numeric(train_2_2$is_quote)
train_2_2$is_Retweet <- as.numeric(train_2_2$is_Retweet)
train_2_2$user_has_reported_location <- as.numeric(train_2_2$user_has_reported_location)
train_2_2$acct_tweet_lang_same <- as.numeric(train_2_2$acct_tweet_lang_same)
train_2_2$diff_tweet_acct_creation_time <- as.numeric(train_2_2$diff_tweet_acct_creation_time)
train_2_2 <- na.omit(train_2_2)
bn.hc <- hc(train_2_2, score = "aic")
#only works with discrete data... doesnt work.
bn.hc
opm5<-par(mfrow = c(1,2))
plot(bn.gs, main = "Constraint-based algorithms")
plot(bn.hc, main = "Hill-Climbing")
res2 = hc(train_2)
fitted2 = bn.fit(res2, train_2)
fitted2

##########SVM#############
library(kernlab) #for SVM..always explore the library!
# Basic Model
help(sample)
train_sample_size = nrow(train)*0.3
train_sample_index = sample(x=1:nrow(train), replace = FALSE, size =train_sample_size)
train_sample<-train[train_sample_index,]
test_sample_size = nrow(test)*0.3
test_sample_index = sample(x=1:nrow(test), replace = FALSE, size =test_sample_size)
test_sample<-test[test_sample_index,]
help(ksvm)
m3 <- ksvm(malicious ~., data = train_sample, kernel = "vanilladot", na.action = na.omit)
test2 <- na.omit(test_sample)
test2$m3_score <- predict(m3, test2[,1:22], type="response")
#m7_1_pred <- predict(m7_1, test[,1:10], type="response")
m3_pred <- prediction(test2$m3_score, test2$malicious)
m3_perf <- performance(m3_pred,"tpr","fpr")

# Plot ROC curve
m3_perf <- performance(m3_pred, measure = "tpr", x.measure = "fpr")
plot(m3_perf, colorize=TRUE, lwd=2, main="SVM:Plot ROC curve - Vanilladot")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

###### Random forrest 2#####
library(party)
#set.seed(123456742)
#m4 <- cforest(malicious~., data = train_sample, control = cforest_unbiased(mtry = 2, ntree = 50))
#m4_fitForest <- predict(m4, newdata = test_sample)
#m4_pred <- prediction( m4_fitForest, test_sample$malicious)
#m4_perf <- performance(m4_pred, "tpr", "fpr")

# Model Performance plot

#plot(m4_perf,colorize=TRUE, lwd=2, main = "m3 ROC: Random Forest", col = "blue")
#lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
#lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
library(rmarkdown)
render("/final_fin_modeling_R_report.Rmd", pdf_document())
#MS Word
help(rmarkdown)

######DATA ANALYSIS AND BIVARIATE ANALYSIS#######
op <- par(mfrow=c(1,2), new=TRUE)
#plot(as.numeric(all_tweets$malicious), ylab="Malicious-NonMalicious", xlab="n", main="Malicious~NonMalicious")
hist(as.numeric(all_tweets$malicious), breaks =2, xlab="Non-Malicious(0) and Malicious(1)", col="blue", main="Non-Malicious vs. Malicious Tweets")
#help(hist)

###SKIP BIVARIATE UNTIL END
#START ON ML: Logistic Regression, Random Forrest, SVM, Lasso
eval_tweets <- function(x , y=all_tweets$malicious){
	tb <- table(as.factor(x), as.factor(y))
	mt <- as.matrix(tb) # x -> independent variable(vector), y->dependent variable(vector)
	Total <- sum((mt[,1]),(mt[,2]))                        # Total observations
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

# MULTIVARIATE ANALYSIS - Dimension(Variable) Reduction using Variable Clustering Approach
# Clustering of variables is as a way to arrange variables into homogeneous clusters, i.e., groups of variables which are strongly related to each other and thus bring the same information.
# When we have large number of variables, this should be done well before univariate analysis.
# This can also be done using Principal Component Analysis (PCA) and Multiple Correspondence Analysis (MCA) or Factor Analysis.

#CLUSTERING TREE
#Just Qualitative
tweets_quali_cluster_tree <- hclustvar(X.quali = all_tweets_quali)
plot(tweets_quali_cluster_tree, main="variable quali clustering")
rect.hclust(tweets_quali_cluster_tree, k=2,  border = 1:10)
summary(tweets_quali_cluster_tree)
#just  Quantitative
tweets_quanti_cluster_tree <- hclustvar(X.quanti = all_tweets_quanti)
plot(tweets_quanti_cluster_tree, main="variable quanti clustering")
rect.hclust(tweets_quanti_cluster_tree, k=10,  border = 1:10)
summary(tweets_quanti_cluster_tree)
# Phylogenetic trees
library("ape")
#qualitative
plot(as.phylo(tweets_quali_cluster_tree), type = "fan",
		 tip.color = hsv(runif(15, 0.65,  0.95), 1, 1, 0.7),
		 edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
		 edge.width = runif(20,  0.5, 3), use.edge.length = TRUE, col = "gray80")
summary.phylo(as.phylo(tweets_quali_cluster_tree))
stab_quali<-stability(tweets_quali_cluster_tree,B=50) # Bootstrap 50 times
# plot(stab,main="Stability of the partitions")
boxplot(stab_quali$matCR)
part_quali<-cutreevar(tweets_quali_cluster_tree,3)
print(part_quali)
summary(part_quali)
#quantitative
plot(as.phylo(tweets_quanti_cluster_tree), type = "fan",
		 tip.color = hsv(runif(15, 0.65,  0.95), 1, 1, 0.7),
		 edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
		 edge.width = runif(20,  0.5, 3), use.edge.length = TRUE, col = "gray80")
summary.phylo(as.phylo(tweets_quanti_cluster_tree))
stab_quanti<-stability(tweets_quanti_cluster_tree,B=50) # Bootstrap 50 times
# plot(stab,main="Stability of the partitions")
boxplot(stab_quanti$matCR)
part_quanti<-cutreevar(tweets_quanti_cluster_tree,10)
print(part_quanti)
summary(part_quanti)
#plot(tweets_quanti_cluster_tree, as.factor(kfit_quanti$cluster), type = "index")
#plot(tweets_quanti_cluster_tree, as.factor(kfit_quanti$cluster), type = "tree")
# K-means clustering of variables
# We may also cross check the outcomes of hierarchical clustering using K-means variable clustering:
#qualitative
###NEED TO CHANGE B/C TWEET LANG AND ACCT LANG have SAME CATEGORIES 
#help(kmeansvar)
#kfit_quali<-kmeansvar(X.quali = all_tweets_quali,iter.max = 150, matsim = TRUE)
#kfit_quali<-kmeansvar(X.quali = all_tweets_quali,iter.max = 150, init =2, nstart =1, matsim = TRUE, rename.level = TRUE)
#summary(kfit_quali)
#plot(all_tweets_quali, as.factor(kfit_quali$cluster))
#kfit_quali$E
#quantitative
#kfit_quanti<-kmeansvar( X.quanti = all_tweets_quanti, init=5, iter.max = 150, nstart = 1, matsim = TRUE)
kfit_quanti<-kmeansvar( X.quanti = all_tweets_quanti, init = 5, iter.max = 150, nstart= 1, matsim = TRUE)
summary(kfit_quanti)
plot(all_tweets_quanti, as.factor(kfit_quanti$cluster))
kfit_quanti$E

hist(all_tweets$follower_count)
hist(all_tweets$following_count)
hist(all_tweets$is_reply)
hist(all_tweets$is_quote)
hist(all_tweets$is_Retweet)
hist(all_tweets$retweet_count)
summary(all_tweets$retweet_count)
hist(all_tweets$like_count)
summary(all_tweets$like_count)
hist(all_tweets$user_has_reported_location)
hist(all_tweets_testing$tweet_lang_russian)
mean(all_tweets_testing$tweet_lang_russian)
hist(all_tweets_testing$tweet_lang_english)
mean(all_tweets_testing$tweet_lang_english)
hist(all_tweets$has_hashtags)
hist(all_tweets$num_hashtags)
hist(all_tweets$has_urls)
hist(all_tweets$num_urls)
hist(all_tweets$has_user_mentions)
hist(all_tweets$num_user_mentions)
hist(all_tweets$diff_tweet_acct_creation_time)

#hist(all_tweets_testing[all_tweets_testing$tweet_lang_english == 1,]$malicious, breaks =2)
#hist(all_tweets_testing[all_tweets_testing$tweet_lang_russian == 1,]$malicious, breaks =2)
par(mfrow=c(1,1))
#boxplot(all_tweets[all_tweets$tweet_lang_english == 1,]$malicious,all_tweets[all_tweets$tweet_lang_english == 0,]$malicious,
#all_tweets[all_tweets$tweet_lang_russian == 1,]$malicious,all_tweets[all_tweets$tweet_lang_russian == 0,]$malicious)

# Attribute 1:  follower_count (numerical)
summary(all_tweets_testing$follower_count)
op2<-par(mfrow=c(1,2))
boxplot(all_tweets_testing$follower_count, ylab="Follower Count", main="Boxplot: Follower Count")
#plot(as.factor(all_tweets_testing$follower_count), all_tweets_testing$malicious, ylab="Malicious-NonMalicious", xlab="Follower Count", main="Follower Count (Before Groupping)")
plot(as.factor(all_tweets_testing$follower_count), all_tweets_testing$malicious, ylab="Malicious-NonMalicious", xlab="Follower Count", main="Follower Count (After Groupping)")
follower_count1 = mnpct(all_tweets_testing$follower_count)
#follower_count1$Good_pct
sum(is.na(all_tweets_testing$follower_count))
barplot(follower_count1$WOE, col="brown", names.arg=c(follower_count1$Levels), 
				main="Score:Follower Count",
				xlab="Category",
				ylab="WOE")

barplot(follower_count1$IV, col="brown", names.arg=c(follower_count1$Levels), 
				main="Score:Follower Count",
				xlab="Category",
				ylab="IV")



