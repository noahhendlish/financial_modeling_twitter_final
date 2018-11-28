#Twitter Malicious Tweets Data Analysis
#Noah Hendlish
#FINAL PROJECT: CLASSIFICATION OF MISINFORMATION CAMPAIGN BOTS
install.packages(c('data.table','stringr','lars','glmnet','DT', 'lattice','knitr' ,'ClustOfVar','ape','Information','ROCR','caret','rpart','rpart.utils','rpart.plot','randomForest','party','bnlearn','DAAG','vcd','kernlab')); #if you have not already installed these packages
install.packages('rapportools')
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
all_tweets$tweet_date <-  as.Date(all_tweets$tweet_date )
all_tweets$is_reply <-  as.numeric(all_tweets$is_reply )
all_tweets$is_quote <-  as.numeric(all_tweets$is_quote)
all_tweets$is_Retweet <-  as.numeric(all_tweets$is_Retweet )
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

#split data sets into qualitative and quantitative
quanti_headers = c("follower_count" ,"following_count",
									 "is_reply",  "is_quote","is_Retweet", "retweet_count", "like_count","user_has_reported_location", 
									 "acct_tweet_lang_same", "tweet_lang_english","tweet_lang_russian", "has_hashtags","num_hashtags",
									 "has_urls", "num_urls", "has_user_mentions", "num_user_mentions",            
									 "diff_tweet_acct_creation_time", "malicious")
quali_headers = c("account_language", "tweet_language", "tweet_client")
all_final_headers =c("account_language", "tweet_language", "tweet_client","follower_count" ,"following_count",
										 "is_reply",  "is_quote","is_Retweet", "retweet_count", "like_count","user_has_reported_location", 
										 "acct_tweet_lang_same", "tweet_lang_english","tweet_lang_russian", "has_hashtags","num_hashtags",
										 "has_urls", "num_urls", "has_user_mentions", "num_user_mentions",            
										 "diff_tweet_acct_creation_time", "malicious")
all_tweets_quanti <- data.frame(all_tweets$follower_count, all_tweets$following_count, 
														all_tweets$is_reply,  all_tweets$is_quote, all_tweets$is_Retweet, all_tweets$retweet_count, 
														all_tweets$like_count, all_tweets$user_has_reported_location, all_tweets$acct_tweet_lang_same,         
														all_tweets$tweet_lang_english,all_tweets$tweet_lang_russian, all_tweets$has_hashtags,
														all_tweets$num_hashtags, all_tweets$has_urls, all_tweets$num_urls, all_tweets$has_user_mentions, 
														all_tweets$num_user_mentions,all_tweets$diff_tweet_acct_creation_time, all_tweets$malicious)
all_tweets_quali <- data.frame(all_tweets$account_language, all_tweets$tweet_language, all_tweets$tweet_client)
all_tweets_final <- data.frame(all_tweets$account_language, all_tweets$tweet_language, all_tweets$tweet_client, all_tweets$follower_count, all_tweets$following_count, 
																all_tweets$is_reply,  all_tweets$is_quote, all_tweets$is_Retweet, all_tweets$retweet_count, 
																all_tweets$like_count, all_tweets$user_has_reported_location, all_tweets$acct_tweet_lang_same,         
																all_tweets$tweet_lang_english,all_tweets$tweet_lang_russian, all_tweets$has_hashtags,
																all_tweets$num_hashtags, all_tweets$has_urls, all_tweets$num_urls, all_tweets$has_user_mentions, 
																all_tweets$num_user_mentions,all_tweets$diff_tweet_acct_creation_time, all_tweets$malicious) 
colnames(all_tweets_quali) <-quali_headers
colnames(all_tweets_quanti) <- quanti_headers
colnames(all_tweets_final) <- all_final_headers
non_malicious_in_total_set <- subset(x=all_tweets_final, all_tweets_final$malicious ==0)
malicious_in_total_set <- subset(x=all_tweets_final, all_tweets_final$malicious == 1)
percent_of_malicious_in_total_set= nrow(malicious_in_total_set)/nrow(all_tweets_final)

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
train<-all_tweets_final[div_part,] # 70% here
colnames(train)<- all_final_headers
non_malicious_in_train <- subset(x=train, train$malicious ==0)
malicious_in_train <- subset(x=train, train$malicious ==1)
percent_of_malicious_in_train = nrow(malicious_in_train)/nrow(train)
# put remaining into test sample
test<-all_tweets_final[-div_part,] # rest of the 30% data goes here
non_malicious_in_test<- subset(x=train, train$malicious ==0)
colnames(test)<- all_final_headers
malicious_in_test <- subset(x=train, train$malicious ==1)
percent_of_malicious_in_test = nrow(malicious_in_test)/nrow(test)
summary(div_part)
##get samples with same % of malicious in test as in train (~16.5%)--> which is consistent with population
# Model: Stepwise Logistic Regression Model  # Model: Stepwise Logistic Regression Model  # Model: Stepwise Logistic Regression Model# Model: Stepwise Logistic Regression Model
#malicious~.
help(glm)
regression1 <-glm(malicious~.,data=train,family=binomial())
regression2 <- glm(malicious~  tweet_client+follower_count+following_count+is_reply+is_quote +is_Retweet +retweet_count+like_count+user_has_reported_location+acct_tweet_lang_same+tweet_lang_english+tweet_lang_russian+num_hashtags+ num_urls+num_user_mentions+diff_tweet_acct_creation_time,data=train,family=binomial())
regression3 <- glm(malicious~  tweet_client+follower_count+following_count+is_reply+is_quote +is_Retweet +retweet_count+like_count+user_has_reported_location+acct_tweet_lang_same+tweet_language+num_hashtags+ num_urls+num_user_mentions+diff_tweet_acct_creation_time,data=train,family=binomial())

summary(regression1)
summary(regression2)
summary(regression3)

# List of significant variables and features with p-value <0.01
significant.variables <- summary(regression3)$coeff[-1,4] < 0.01
names(significant.variables)[significant.variables == TRUE]
prob <- predict(regression3, type = "response")
res <- residuals(regression3, type = "deviance")

## CIs using profiled log-likelihood
confint(regression3)

## CIs using standard errors
confint.default(regression3)
## odds ratios and 95% CI
exp(cbind(OR = coef(regression3), confint(regression3)))

#score test data set
#test$m1_score <- predict(regression3,type='response',test)
#m1_pred <- prediction(test$m1_score, test$good_bad_21)
#m1_perf <- performance(m1_pred,"tpr","fpr")

#ROC
#par(mfrow=c(1,1))
#plot(m1_perf, lwd=2, colorize=TRUE, main="ROC m1: Logistic Regression Performance")
#lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
#lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# Plot precision/recall curve
#m1_perf_precision <- performance(m1_pred, measure = "prec", x.measure = "rec")

plot(m1_perf_precision, main="m1 Logistic:Precision/recall curve")







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
part_quali<-cutreevar(tweets_quali_cluster_tree,10)
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



