#Twitter Malicious Tweets Data Analysis
#Noah Hendlish
#FINAL PROJECT: CLASSIFICATION OF MISINFORMATION CAMPAIGN BOTS
install.packages(c('data.table','stringr','lars','glmnet','DT', 'lattice','knitr' ,'ClustOfVar','ape','Information','ROCR','caret','rpart','rpart.utils','rpart.plot','randomForest','party','bnlearn','DAAG','vcd','kernlab')); #if you have not already installed these packages
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
library(readr)
library(stringr)

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
colnames(nonmalicious_tweets) <- csv_headers
colnames(malicious_tweets) <- csv_headers
colnames(all_tweets) <- csv_headers

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
all_tweets$has_urls <-  as.numeric(all_tweets$has_urls)
all_tweets$has_urls <-  as.numeric(all_tweets$has_urls)
all_tweets$has_user_mentions <-  as.numeric(all_tweets$has_user_mentions)
all_tweets$num_user_mentions <-  as.numeric(all_tweets$num_user_mentions)
all_tweets$diff_tweet_acct_creation_time <-  as.numeric(all_tweets$diff_tweet_acct_creation_time)
all_tweets$malicious <-  as.numeric(all_tweets$malicious)

all_tweets_numeric_only <- data.frame(all_tweets$num, all_tweets$tweetid, all_tweets$follower_count, all_tweets$following_count, 
														all_tweets$is_reply,  all_tweets$is_quote, all_tweets$is_Retweet, all_tweets$retweet_count, 
														all_tweets$like_count, all_tweets$user_has_reported_location, all_tweets$acct_tweet_lang_same,         
														all_tweets$tweet_lang_english,all_tweets$tweet_lang_russian, all_tweets$has_hashtags,
														all_tweets$num_hashtags, all_tweets$has_urls, all_tweets$num_urls, all_tweets$has_user_mentions, 
														all_tweets$num_user_mentions,all_tweets$diff_tweet_acct_creation_time, all_tweets$malicious)

all_tweets_testing <- all_tweets
all_tweets_final <- data.frame(all_tweets$num, all_tweets$tweetid, all_tweets$follower_count, all_tweets$following_count, 
															 all_tweets$account_language, all_tweets$tweet_language, all_tweets$tweet_client,
															 all_tweets$is_reply,  all_tweets$is_quote, all_tweets$is_Retweet, all_tweets$retweet_count, 
															 all_tweets$like_count, all_tweets$user_has_reported_location, all_tweets$acct_tweet_lang_same,         
															 all_tweets$tweet_lang_english,all_tweets$tweet_lang_russian, all_tweets$has_hashtags,
															 all_tweets$num_hashtags, all_tweets$has_urls, all_tweets$num_urls, all_tweets$has_user_mentions, 
															 all_tweets$num_user_mentions,all_tweets$diff_tweet_acct_creation_time, all_tweets$malicious)

#In this all data set any variable that starts with "is" --> binary (0 or 1): 1 --> true, 0--> false
#to classify as factors: "account_language", "tweet_language", "tweet_client"
#as.factor(all_tweets$account_language)
#cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 1, "Good", "Bad"))  ## we create a factor, and code it as good or bad 

op <- par(mfrow=c(1,2), new=TRUE)
#plot(as.numeric(all_tweets$malicious), ylab="Malicious-NonMalicious", xlab="n", main="Malicious~NonMalicious")
hist(as.numeric(all_tweets$malicious), breaks =2, xlab="Non-Malicious(0) and Malicious(1)", col="blue", main="Non-Malicious vs. Malicious Tweets")
#help(hist)

###SKIP BIVARIATE UNTIL END
#START ON ML: Logistic Regression, Random Forrest, SVM, Lasso


######DATA ANALYSIS AND BIVARIATE ANALYSIS#######
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
mnpct <- function(x, y=all_tweets$malicious){
	mt <- as.matrix(table(as.factor(x), as.factor(y))) # x -> independent variable(vector), y->dependent variable(vector)
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

all_tweets_testing$follower_count <-as.factor(ifelse(all_tweets$follower_count<=100,'0-100',
																										 ifelse(all_tweets$follower_count<=1000,'100-1000',
																										 			 ifelse(all_tweets$follower_count<=10000,'1000-10,000', 
																										 			 			 ifelse(all_tweets$follower_count<=100000,'10,000-100,000',
																										 			 			 			 ifelse(all_tweets$follower_count<=1000000,'100,000-1,000,000',
																										 			 			 			 			 ifelse(all_tweets$follower_count<=10000000,'1,000,000-10,000,000','10,000,000+')))))))



