#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Fall 2018/FINE/Financial Modeling/Final Project")  ## This is your working directory. This is the place from where you will be reading and writting data 

#NOAH HENDLISH
#FINAL PROJECT: ANALYSIS OF POLITICAL

#DATA ACQUIRED FROM:
#https://about.twitter.com/en_us/values/elections-integrity.html#data
#http://followthehashtag.com/datasets/free-twitter-dataset-usa-200000-free-usa-tweets/
#Live Twitter Stream using Tweepy with Twitters API

###---ABOUT DATA---###
#MALICIOUS DATA:
#https://abcnews.go.com/Politics/twitter-releases-motherlode-data-alleged-russian-iranian-influence/story?id=58565639
#https://blog.twitter.com/official/en_us/topics/company/2018/enabling-further-research-of-information-operations-on-twitter.html
#Twitter: "We will continue to strengthen Twitter against attempted manipulation, including malicious automated accounts and spam, as well as other activities that violate our Terms of Service."
#data is on: "alleged foreign influence campaigns, Twitter is making publicly available archives of Tweets and media that we believe resulted from potentially state-backed information operations on our service."
# include information from 3,841 accounts believed to be connected to the Russian Internet Research Agency
#"These datasets include all public, nondeleted Tweets and media (e.g., images and videos) from accounts we believe are connected to state-backed information operations. "
#"Tweets deleted by these users prior to their suspension (which are not included in these datasets) comprise less than 1% of their overall activity. Note that not all of the accounts we identified as connected to these campaigns actively Tweeted, so the number of accounts represented in the datasets may be less than the total number of accounts listed here."
#MORE : https://storage.googleapis.com/twitter-election-integrity/hashed/Twitter_Elections_Integrity_Datasets_hashed_README.txt


#US Tweets is just dataset of random us tweets from 48hr period? *SEE LINK TO CONFIRM
#LIVE TWITTER STREAM: used combination of search queries and streaming live tweets.

#install.packages(c('lars','glmnet','DT', 'lattice','knitr' ,'ClustOfVar','ape','Information','ROCR','caret','rpart','rpart.utils','rpart.plot','randomForest','party','bnlearn','DAAG','vcd','kernlab')); #if you have not already installed these packages
if (!requireNamespace("httpuv", quietly = TRUE)) {
	install.packages("httpuv")
}
install.packages(c('twitteR','lars','glmnet','DT', 'lattice','knitr' ,'ClustOfVar','ape','Information','ROCR','caret','rpart','rpart.utils','rpart.plot','randomForest','party','bnlearn','DAAG','vcd','kernlab')); #if you have not already installed these packages
install.packages("rtweet")
install.packages("streamR")
install.packages("RJSONIO")
install.packages("RCurl")
install.packages("ROAuth")
install.packages("sjmisc")
install.packages("stringr")
library(data.table)
library(sjmisc)
library(RCurl)
library(RJSONIO)
library(stringr)
library(ROAuth)
## load rtweet package
library(streamR)     #for twitter streaming
library(rtweet)      #for twitter searching/streaming with API
library(twitteR)	   #Twitter API for R
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

#requestURL <- "https://api.twitter.com/oauth/request_token"
#accessURL <- "https://api.twitter.com/oauth/access_token"
#authURL <- "https://api.twitter.com/oauth/authorize"
#consumerKey <- "9kH1Id2ws479lvqmM4IyHIMy7" # From dev.twitter.com
#consumerSecret <- "ZkBdWsDXE7envjJH1Fr3BcE1e88Iltx7dFed90dVhWGhVE50j8" # From dev.twitter.com
#my_oauth <- OAuthFactory$new(consumerKey = consumerKey,consumerSecret = consumerSecret,requestURL = requestURL, accessURL = accessURL, authURL = authURL)
#my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
### STOP HERE!!! ###
# PART 2: Save the my_oauth data to an .Rdata file
#save(my_oauth, file = "my_oauth.Rdata")
#LOAD IN DATA SET OF IRA MALICIOUS TWEETS
# Features from IRA CSV: (in order)
#tweetid,  userid,  user_display_name,  user_screen_name,  
#user_reported_location, user_profile_description, user_profile_url,  
#follower_count,  following_count,  account_creation_date,  account_language,  
#tweet_language,  tweet_text,  tweet_time (and date),  tweet_client_name,  
#in_reply_to_tweetid,  in_reply_to_userid,  quoted_tweet_tweetid,  
#is_Retweet,  retweet_userid,  retweet_tweetid,  latitude,  longitude,  
#quote_count,  reply_count,  like_count,  retweet_count,  hashtags,  urls,  
#user_mentions,  poll_choices
headers_ira <- c("tweetid",  "userid",  "user_display_name", "user_screen_name",  
								 "user_reported_location", "user_profile_description", "user_profile_url",  
								 "follower_count",  "following_count",  "account_creation_date",  "account_language",  
								 "tweet_language",  "tweet_text",  "tweet_date_time",  "tweet_client_name",  
								 "in_reply_to_tweetid",  "in_reply_to_userid",  "quoted_tweet_tweetid",  
								 "is_Retweet",  "retweet_userid",  "retweet_tweetid",  "latitude",  "longitude",  
								 "quote_count",  "reply_count",  "like_count",  "retweet_count",  "hashtags", "urls",  
								 "user_mentions", "poll_choices")
ira_tweets_sub1 <- read.csv('ira_tweet_split/ira_tweets_split1.csv', header = FALSE, col.names = headers_ira)
#names(ira_tweets_sub1) <- headers_ira
help("read.csv")
#ira_tweets1dt <- as.data.frame(ira_tweets1)
ira_tweets_sub2 <- read.csv('ira_tweet_split/ira_tweets_split2.csv',  header = FALSE, col.names = headers_ira)
ira_tweets_sub3 <- read.csv('ira_tweet_split/ira_tweets_split3.csv',  header = FALSE, col.names = headers_ira)
ira_tweets_sub4 <- read.csv('ira_tweet_split/ira_tweets_split4.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub5 <- read.csv('ira_tweet_split/ira_tweets_split5.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub6 <- read.csv('ira_tweet_split/ira_tweets_split6.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub7 <- read.csv('ira_tweet_split/ira_tweets_split7.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub8 <- read.csv('ira_tweet_split/ira_tweets_split8.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub9 <- read.csv('ira_tweet_split/ira_tweets_split9.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub10 <- read.csv('ira_tweet_split/ira_tweets_split10.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub11 <- read.csv('ira_tweet_split/ira_tweets_split11.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub12 <- read.csv('ira_tweet_split/ira_tweets_split12.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub13 <- read.csv('ira_tweet_split/ira_tweets_split13.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub14 <- read.csv('ira_tweet_split/ira_tweets_split14.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub15 <- read.csv('ira_tweet_split/ira_tweets_split15.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub16 <- read.csv('ira_tweet_split/ira_tweets_split16.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub17 <- read.csv('ira_tweet_split/ira_tweets_split17.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub18 <- read.csv('ira_tweet_split/ira_tweets_split18.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub19 <- read.csv('ira_tweet_split/ira_tweets_split19.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub20 <- read.csv('ira_tweet_split/ira_tweets_split20.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub21 <- read.csv('ira_tweet_split/ira_tweets_split21.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub22 <- read.csv('ira_tweet_split/ira_tweets_split22.csv', header = FALSE, col.names = headers_ira)
ira_tweets_sub23 <- read.csv('ira_tweet_split/ira_tweets_split23.csv', header = FALSE, col.names = headers_ira)
#sample randomly from subsets
ira_tweets_sub1_sample <- ira_tweets_sub1[sample(nrow(ira_tweets_sub1), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub2_sample <- ira_tweets_sub2[sample(nrow(ira_tweets_sub2), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub3_sample <- ira_tweets_sub3[sample(nrow(ira_tweets_sub3), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub4_sample <- ira_tweets_sub4[sample(nrow(ira_tweets_sub4), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub5_sample <- ira_tweets_sub5[sample(nrow(ira_tweets_sub5), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub6_sample <- ira_tweets_sub6[sample(nrow(ira_tweets_sub6), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub7_sample <- ira_tweets_sub7[sample(nrow(ira_tweets_sub7), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub8_sample <- ira_tweets_sub8[sample(nrow(ira_tweets_sub8), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub9_sample <- ira_tweets_sub9[sample(nrow(ira_tweets_sub9), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub10_sample <- ira_tweets_sub10[sample(nrow(ira_tweets_sub10), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub11_sample <- ira_tweets_sub11[sample(nrow(ira_tweets_sub11), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub12_sample <- ira_tweets_sub12[sample(nrow(ira_tweets_sub12), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub13_sample <- ira_tweets_sub13[sample(nrow(ira_tweets_sub13), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub14_sample <- ira_tweets_sub14[sample(nrow(ira_tweets_sub14), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub15_sample <- ira_tweets_sub15[sample(nrow(ira_tweets_sub15), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub16_sample <- ira_tweets_sub16[sample(nrow(ira_tweets_sub16), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub17_sample <- ira_tweets_sub17[sample(nrow(ira_tweets_sub17), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub18_sample <- ira_tweets_sub18[sample(nrow(ira_tweets_sub18), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub19_sample <- ira_tweets_sub19[sample(nrow(ira_tweets_sub19), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub20_sample <- ira_tweets_sub20[sample(nrow(ira_tweets_sub20), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub21_sample <- ira_tweets_sub21[sample(nrow(ira_tweets_sub21), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub22_sample <- ira_tweets_sub22[sample(nrow(ira_tweets_sub22), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
ira_tweets_sub23_sample <- ira_tweets_sub23[sample(nrow(ira_tweets_sub23), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]

#if were to only sample english tweets, would first do this:
#english = function(){
#	ira_tweets_sub1_eng <- ira_tweets_sub1[which(ira_tweets_sub1$tweet_language=='en'),]
#	ira_tweets_sub1_sample_eng <- ira_tweets_sub1_eng[sample(nrow(ira_tweets_sub1_eng), 200, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
#...}
all_ira_samples <- rbind(ira_tweets_sub1_sample,ira_tweets_sub2_sample,ira_tweets_sub3_sample,
														 ira_tweets_sub4_sample,ira_tweets_sub5_sample,ira_tweets_sub6_sample,
														 ira_tweets_sub7_sample,ira_tweets_sub8_sample,ira_tweets_sub9_sample,
														 ira_tweets_sub10_sample,ira_tweets_sub11_sample,ira_tweets_sub12_sample,
														 ira_tweets_sub13_sample,ira_tweets_sub14_sample,ira_tweets_sub15_sample,
														 ira_tweets_sub16_sample,ira_tweets_sub17_sample,ira_tweets_sub18_sample,
														 ira_tweets_sub19_sample,ira_tweets_sub20_sample,ira_tweets_sub21_sample,
														 ira_tweets_sub22_sample,ira_tweets_sub23_sample)
#remove poll options column
all_ira_samples_cleaned <- all_ira_samples[,c(1,5,7,8,9,13,10,11,12,14,15,16,17,25,24,26,27,28)]
#remove 22 and 23 (number_replies and quotes: cant acquire this data using twitters free API):
new_headers <- c("tweetid", "user_reported_location",  
								 "follower_count",  "following_count",  "account_creation_date", "tweet_date_time",
								 "account_language",  "tweet_language",  "tweet_text",  "tweet_client", 
								 "is_reply",  "is_quote", "is_Retweet",   "retweet_count", "like_count", "hashtags", "urls",  
								 "user_mentions")
colnames(all_ira_samples_cleaned) <- new_headers
unique(all_ira_samples_cleaned$user_reported_location)
#("diff_tweet_acct_creation_time","is_malicious")
#cdatanum <- as.data.frame(sapply(cdatanum, as.numeric ))
all_ira_samples_cleaned$tweetid  <- as.numeric(all_ira_samples_cleaned$tweetid)
all_ira_samples_cleaned$follower_count  <- as.numeric(all_ira_samples_cleaned$follower_count)
all_ira_samples_cleaned$following_count  <- as.numeric(all_ira_samples_cleaned$following_count)
all_ira_samples_cleaned$account_creation_date  <- as.Date(all_ira_samples_cleaned$account_creation_date)
all_ira_samples_cleaned$tweet_date_time  <- as.Date(all_ira_samples_cleaned$tweet_date_time)
all_ira_samples_cleaned$retweet_count  <- as.numeric(all_ira_samples_cleaned$retweet_count)
all_ira_samples_cleaned$like_count  <- as.numeric(all_ira_samples_cleaned$like_count)
###Add data attribute: is_quote? to binary (was int id of quoted tweet)
#all_ira_samples$is_quote = 0
#all_samples = cbind(all_ira_samples, all_rand_samples)
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	#num_quotes = 0
	if (is.na(all_ira_samples_cleaned$is_quote[i])){
		all_ira_samples_cleaned$is_quote[i] = 0
	}
	else{
		all_ira_samples_cleaned$is_quote[i] = 1
		#num_quotes =num_quotes+1
		#print(all_ira_samples_cleaned$is_quote[i])
		#print("is_quote")
		#print(num_quotes)
	}
}
all_ira_samples_cleaned$is_quote  <- as.numeric(all_ira_samples_cleaned$is_quote)

###Add data attribute: is_reply? to binary (was int id of quoted tweet)
#all_ira_samples$is_reply = 0
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	#num_replys = 0
	if (is.na(all_ira_samples_cleaned$is_reply[i])){
		all_ira_samples_cleaned$is_reply[i] = 0
	}
	else{
		all_ira_samples_cleaned$is_reply[i] = 1
		#print(all_ira_samples_cleaned$is_reply[i])
		#print("is_reply")
		#num_replys =num_replys+1
		#print(num_replys)
	}
}
all_ira_samples_cleaned$is_reply  <- as.numeric(all_ira_samples_cleaned$is_reply)

#Change data attribute: is_retweet? to binary (was string)
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	if (all_ira_samples_cleaned$is_Retweet[i] == "false"){
		all_ira_samples_cleaned$is_Retweet[i] = 0
	}
	else if(all_ira_samples_cleaned$is_Retweet[i] == "true"){
		all_ira_samples_cleaned$is_Retweet[i] = 1
	}
	else{
		print("error:  ")
		print(all_ira_samples_cleaned$is_Retweet[i])
	}
}
all_ira_samples_cleaned$is_Retweet  <- as.numeric(all_ira_samples_cleaned$is_Retweet)

###Add data attribute: user_has_reported_location
all_ira_samples$user_has_reported_location = 0
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	if (is.na(all_ira_samples_cleaned$user_reported_location[i]) || all_ira_samples_cleaned$user_reported_location[i] == ""){
		all_ira_samples_cleaned$user_has_reported_location[i] = 0
	}
	else{
		all_ira_samples_cleaned$user_has_reported_location[i] = 1
		#print(all_ira_samples_cleaned$user_reported_location[i])
	 }
}
all_ira_samples_cleaned$user_has_reported_location  <- as.numeric(all_ira_samples_cleaned$user_has_reported_location)

#unique(all_ira_samples_cleaned$user_reported_location)
#could also add list of all US State names & abbreviations, 
#all US cities, USA, US, U.S.A, U.S, United States, United States of America,
#etc. (all possible inputs for reported location input) and check if the reported location
#is in the US: **TOOK OUT ABBREVIATIONS
US_Major_locations = c("Alabama", "Alaska", "Buffalo", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "USA", "NY", "LA", "Birmingham", "Anchorage", "Phoenix", "Little Rock", "Los Angeles", "Denver", "Bridgeport", "Wilmington", "Jacksonville", "Atlanta", "Honolulu", "Boise", "Chicago", "Indianapolis", "Des Moines", "Wichita", "Louisville", "New Orleans", "Portland", "Baltimore", "Boston", "Detroit", "Minneapolis", "Jackson", "Kansas City", "Billings", "Omaha", "Las Vegas", "Manchester", "Newark", "Albuquerque", "New York", "Charlotte", "Fargo", "Columbus", "Oklahoma City", "Portland", "Philadelphia", "Providence", "Columbia", "Sioux Falls", "Memphis", "Houston", "Salt Lake City", "Burlington", "Virginia Beach", "Seattle", "Charleston", "Milwaukee", "Cheyenne", "StLouis", "Montgomery", "Juneau", "Phoenix", "Little Rock", "Sacramento", "Denver", "Hartford", "Dover", "Tallahassee", "Atlanta", "Honolulu", "Boise", "Springfield", "Indianapolis", "Baton Rouge", "Augusta", "Annapolis", "Boston", "Lansing", "St. Paul", "Jackson", "Jefferson City", "Helena", "Lincoln", "Carson City", "Concord", "Trenton", "Santa Fe", "Albany", "Raleigh", "Bismarck", "Columbus", "Oklahoma City", "Salem", "Harrisburg", "Providence", "Columbia", "Pierre", "Nashville", "Austin", "Salt Lake City", "Montpelier", "Richmond", "Olympia", "Charleston", "Madison", "Cheyenne", "DC", "Pittsburgh", "St. Louis", "St Louis", "Baltimore", "Philly", "Portland", "New York", "America", "Atlanta", "Chicago", "Baltimore", "Whitehall", "Houston", "Kansas City", "UC Davis", "Milwaukee", "Albuquerque", "Pittsburgh", "Cincinnati", "Santa Monica", "united States", "United States of America")
US_state_abbr = c("US", "U.S", "U.S.A","USA", "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
###Add data attribute: user_has_reported_location_is_US: US, USA, United States, United States of America,?
#str_contains(x, pattern, ignore.case = FALSE, logic = NULL,switch = FALSE)
#5,6,10 = us
#all_ira_samples$user_reported_location_is_US = 0
#help(str_contains)
#for (i in 1:length(all_ira_samples_cleaned[,1])) {
#	is_us = str_contains(all_ira_samples_cleaned$user_reported_location[i], US_state_abbr, ignore.case = TRUE, logic = "OR")
	#is_us_abbr = str_contains(all_ira_samples_cleaned$user_reported_location[i], US_state_abbr, ignore.case = FALSE, logic = "OR")
#	if (is_us == TRUE){
#		print("in US: ")
#		print(all_ira_samples_cleaned$user_reported_location[i])
#		all_ira_samples$user_reported_location_is_US = 1
#	}
#	else{
#		#all_ira_samples_cleaned$user_has_reported_location[i] = 1
		#print(all_ira_samples_cleaned$user_reported_location[i])
#		all_ira_samples$user_reported_location_is_US = 0
#	}
#}
#all_ira_samples_cleaned$user_reported_location_is_US  <- as.numeric(all_ira_samples_cleaned$user_reported_location_is_US)
##DID NOT USE ABOVE (WAS NOT CORRECT)

#account_language == tweet_language?
###Add data attribute: acct_tweet_lang_same (account_lang == tweet_lang?)
###Add data attribute: tweet_lang == 'en'?:English
all_ira_samples_cleaned$acct_tweet_lang_same = 0
all_ira_samples_cleaned$tweet_lang_english = 0
all_ira_samples_cleaned$tweet_lang_russian = 0
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	if (all_ira_samples_cleaned$account_language[i] == all_ira_samples_cleaned$tweet_language[i]){
		all_ira_samples_cleaned$acct_tweet_lang_same[i] = 1
	}
	if(all_ira_samples_cleaned$tweet_language[i] == 'en'){
		all_ira_samples_cleaned$tweet_lang_english[i] = 1
	}
	if(all_ira_samples_cleaned$tweet_language[i] == 'ru'){
		all_ira_samples_cleaned$tweet_lang_russian[i] = 1
	}
}
all_ira_samples_cleaned$tweet_lang_russian  <- as.numeric(all_ira_samples_cleaned$tweet_lang_russian)
all_ira_samples_cleaned$tweet_lang_english  <- as.numeric(all_ira_samples_cleaned$tweet_lang_english)
all_ira_samples_cleaned$acct_tweet_lang_same  <- as.numeric(all_ira_samples_cleaned$acct_tweet_lang_same)

#tweet_text length? - Not useful...
#hashtags
#urls
#user_mentions

###Add data attribute: has hashtags and num hashtags
all_ira_samples_cleaned$has_hashtags = 0
all_ira_samples_cleaned$num_hashtags = 0
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	hashtag_count = 0
	if (is.na(all_ira_samples_cleaned$hashtags[i])||all_ira_samples_cleaned$hashtags[i] == '[]'||all_ira_samples_cleaned$hashtags[i] == ""){
		all_ira_samples_cleaned$has_hashtags[i] = 0
		all_ira_samples_cleaned$num_hashtags[i] = 0
	}
	else{
		all_ira_samples_cleaned$has_hashtags[i] = 1	
		all_hashtags = all_ira_samples_cleaned$hashtags[i]
		num_comma = str_count(all_hashtags, ',')
		hashtag_count = 1 + num_comma
		all_ira_samples_cleaned$num_hashtags[i] = hashtag_count
		}
	
		#all_samples$num_hashtags[i] = hashtag_count	
}
all_ira_samples_cleaned$has_hashtags  <- as.numeric(all_ira_samples_cleaned$has_hashtags)
all_ira_samples_cleaned$num_hashtags  <- as.numeric(all_ira_samples_cleaned$num_hashtags)

###Add data attribute: has urls and number of urls
all_ira_samples$has_urls = 0
all_ira_samples$num_urls = 0
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	url_count = 0
	if (is.na(all_ira_samples_cleaned$urls[i])||all_ira_samples_cleaned$urls[i] == '[]'||all_ira_samples_cleaned$urls[i] == ""){
		all_ira_samples_cleaned$has_urls[i] = 0
		all_ira_samples_cleaned$num_urls[i] = 0
	}
	else{
		all_ira_samples_cleaned$has_urls[i] = 1	
		all_urls = all_ira_samples_cleaned$urls[i]
		num_comma_url = str_count(all_urls, ',')
		url_count = 1 + num_comma_url
		all_ira_samples_cleaned$num_urls[i] = url_count
	}
}
all_ira_samples_cleaned$has_urls  <- as.numeric(all_ira_samples_cleaned$has_urls)
all_ira_samples_cleaned$num_urls  <- as.numeric(all_ira_samples_cleaned$num_urls)

###Add data attribute:has user mentions and number of user mentions
all_ira_samples$has_user_mentions = 0
all_ira_samples$num_user_mentions = 0
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	mention_count = 0
	if (is.na(all_ira_samples_cleaned$user_mentions[i])||all_ira_samples_cleaned$user_mentions[i] == '[]'||all_ira_samples_cleaned$user_mentions[i] == ""){
		all_ira_samples_cleaned$has_user_mentions[i] = 0
		all_ira_samples_cleaned$num_user_mentions[i] = 0
	}
	else{
		all_ira_samples_cleaned$has_user_mentions[i] = 1	
		all_mentions = all_ira_samples_cleaned$user_mentions[i]
		num_mentions = str_count(all_mentions, ',')
		mention_count = 1 + num_mentions
		all_ira_samples_cleaned$num_user_mentions[i] = mention_count
	}
}
all_ira_samples_cleaned$has_user_mentions  <- as.numeric(all_ira_samples_cleaned$has_user_mentions)
all_ira_samples_cleaned$num_user_mentions  <- as.numeric(all_ira_samples_cleaned$num_user_mentions)

###Add data attribute: Time difference between tweet and account creation
all_ira_samples_cleaned$diff_tweet_acct_creation_time <- 0
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	all_ira_samples_cleaned$diff_tweet_acct_creation_time[i] <- difftime(as.POSIXct(all_ira_samples_cleaned[i,]$tweet_date_time), as.POSIXct(all_ira_samples_cleaned[i,]$account_creation_date), units = c("days"))
}
all_ira_samples_cleaned$diff_tweet_acct_creation_time  <- as.numeric(all_ira_samples_cleaned$diff_tweet_acct_creation_time)

###Add data attribute: flag malicious
all_ira_samples_cleaned$malicious <- 1
for (i in 1:length(all_ira_samples_cleaned[,1])) {
	all_ira_samples_cleaned$malicious[i] <- 1
}
all_ira_samples_cleaned$malicious  <- as.numeric(all_ira_samples_cleaned$malicious)
all_ira_samples_cleaned_column_names = colnames(all_ira_samples_cleaned)

#LATER: as factor
	#tweet_lang/user_lang
	#tweet_client*
consumer_key = '9kH1Id2ws479lvqmM4IyHIMy7'  #API KEY
consumer_secret = 'ZkBdWsDXE7envjJH1Fr3BcE1e88Iltx7dFed90dVhWGhVE50j8' #API SECRET KEY
access_token = '1065719128869212164-fVdOivGZg14frli54GSrBi0BSeunIf'   #ACCESS TOKEY
access_secret = '2BOCVcVoxro7VSstgKfbqapTeCeFLLnedkM48kUDXtkKX'   #ACCESS TOKEN SECRET
oath_setup = setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)
getCurRateLimitInfo()
#shows what tweet features I have access to from twitter API account
#searchterm <- "#arsenalfc"
#tweets <- searchTwitter(searchterm,n=5000)
#SETUP RTweet for Twitter API
token <- create_token(
	app = "MisinformationCampaignClassifier",
	consumer_key = "9kH1Id2ws479lvqmM4IyHIMy7",
	consumer_secret = "ZkBdWsDXE7envjJH1Fr3BcE1e88Iltx7dFed90dVhWGhVE50j8",
	access_token = "1065719128869212164-fVdOivGZg14frli54GSrBi0BSeunIf",
	access_secret = "2BOCVcVoxro7VSstgKfbqapTeCeFLLnedkM48kUDXtkKX")
#set_renv = FALSE)
identical(token, get_token())

## stream tweets from london for 60 seconds
#rand_tweets1 <- search_tweets2("vote", n = 9000, type = 'mixed', include_rts = FALSE, retryonratelimit = FALSE, "lang:en")  #n = number of tweeets (max is 18000/15mins)
## quick overview of rtweet functions
vignette("auth", package = "rtweet")
## quick overview of rtweet functions
vignette("intro", package = "rtweet")
## working with the stream
vignette("stream", package = "rtweet")
## working with the stream
vignette("FAQ", package = "rtweet")
##STREAM TWEETS WITH CERTAIN QUERIES FOR 5 MINUTES (300 secs in english)
gov_key_word_query <- "election, vote, trump, hillary, news, fake, 
us, usa, united states, america, alleged, debate,
law, political, politics, campaign, voting, economy,
polling, poll, inflation, unemployment, tax, party,
gov, government, federal, state, elections, large,
or, and, okay, good, bad, when, if, for,
while, but, though, what, who, the, -, !, #,
college, elect, so, that, they, he, she, is,
on, okay, the, for, to, one, can, cannot, will"
#stream_tweets( query, timeout = 300, file_name = rand_tweet_file_names[3], parse = FALSE, "lang:en")
#rand_gov_related_tweet_stream_parsed <- parse_stream(rand_tweet_file_names[3])

#FOR STREAMING TWEETS WITH TRENDING SEARCHES AS QUERIES (300 secs in english)
us_trending_searches <- get_trends("united states")
trending_query = ""

us_trending_searches_queries <- us_trending_searches[1]
for(q in us_trending_searches_queries){
	trending_query = paste(trending_query,q, collapse=' ,')
}
#FOR SEARCHING TWEETS WITH TRENDING SEARCHES AS QUERIES (300 secs in english)
for(q in us_trending_searches_queries){
	trending_query_OR = paste(q, "OR", collapse=' ')
}
hundred_most_common_english_words <- "time,person,year,way,day,thing,man,world,life,hand,part,child,eye,woman,place,work,week,case,point,government,company,number,group,problem,fact,know,take,see,come,think,look,want,give,use,find,tell,ask,work,seem"
hundred_most_common_english_words2<- "feel,try,leave,call,Adjectives,good,new,first,last,long,great,little,own,other,old,right,big,high,different,small,large,next,early,young,important,few,public,bad,same,able,Prepositions,to,of,in,for,on,after"
hundred_most_common_english_words3 <-"Others,the,and,a,that,I,it,not,he,as,you,this,but,his,they,her,she,or,an,will,my,one,all,would,there,their,with,at,by,from,up,about,into,over,Verbs,be,have,do,say,get,make,go"
#five_hundred_most_common_english_words <- "the,of,to,and,a,in,is,it,you,that,he,was,for,on,are,with,as,I,his,they,be,at,one,have,this,from,or,had,by,hot,but,some,what,there,we,can,out,other,were,all,your,when,up,use,word,how,said,an,each,she,which,do,their,time,if,will,way,about,many,then,them,would,write,like,so,these,her,long,make,thing,see,him,two,has,look,more,day,could,go,come,did,my,sound,no,most,number,who,over,know,water,than,call,first,people,may,down,side,been,now,find,any,new,work,part,take,get,place,made,live,where,after,back,little,only,round,man,year,came,show,every,good,me,give,our,Under,very,through,just,form,much,great,think,say,help,low,line,before,turn,cause,same,mean,differ,move,right,boy,old,too,does,tell,sentence,set,three,want,air,well,also,play,small,end,put,home,read,hand,port,large,spell,add,even,land,here,must,big,high,such,follow,act,why,ask,men,change,went,light,kind,off,need,house,picture,try,us,again,animal,point,mother,world,near,build,self,earth,father,head,stand,own,page,should,country,found,answer,school,grow,study,still,learn,plant,cover,food,sun,four,thought,let,keep,eye,never,last,door,between,city,tree,cross,since,hard,start,might,story,saw,far,sea,draw,left,late,run,don't,while,press,close,night,real,life,few,stop,open,seem,together,next,white,children,begin,got,walk,example,ease,paper,often,always,music,those,both,mark,book,letter,until,mile,river,car,feet,care,second,group,carry,took,rain,eat,room,friend,began,idea,fish,mountain,north,once,base,hear,horse,cut,sure,watch,color,face,wood,main,enough,plain,girl,usual,young,ready,above,ever,red,list,though,feel,talk,bird,soon,body,dog,family,direct,pose,leave,song,measure,state,product,black,short,numeral,class,wind,question,happen,complete,ship,area,half,rock,order,fire,south,problem,piece,told,knew,pass,farm,top,whole,king,size,heard,best,hour,better,true .,during,hundred,am,remember,step,early,hold,west,ground,interest,reach,fast,five,sing,listen,six,table,travel,less,morning,box,noun,field,rest,correct,able,pound,done,beauty,drive,stood,contain,front,teach,week,final,gave,green,oh,quick,develop,sleep,warm,free,minute,strong,special,mind,behind,clear,tail,produce,fact,street,inch,lot,nothing,course,stay,wheel,full,force,blue,object,decide,surface,deep,moon,island,foot,yet,busy,test,record,boat,common,gold,possible,plane,age,dry,wonder,laugh,thousand,ago,ran,check,game,shape,yes,hot,miss,brought,heat,snow,bed,bring,sit,perhaps,fill,east,weight,language,among"
##STREAM ALL TWEETS RANDOMLY FOR 60 SECONDS
#rand_tweet_stream1 <- stream_tweets(timeout = 60)
rand_tweet_file_names = c("rand_tweets0.json", "rand_tweets1.json","rand_tweets2.json","rand_tweets3.json" ,"rand_tweets4.json",
				 "rand_tweets5.json","rand_tweets6.json")
##STREAM ALL TWEETS RANDOMLY FOR 180 SECONDS (save to json and then parse)
help("stream_tweets")
time = 300 #in seconds
#search_tweets(hundred_most_common_english_words, n = 9000, type = "mixed", include_rts = FALSE, retryonratelimit = FALSE, parse = FALSE)
stream_tweets( "", timeout = time, file_name = rand_tweet_file_names[1], parse = FALSE)
stream_tweets( "", timeout = 300, file_name = rand_tweet_file_names[2], parse = FALSE)
stream_tweets( gov_key_word_query, timeout = time, file_name = rand_tweet_file_names[3], parse = FALSE)
stream_tweets( "", timeout = 240, file_name = rand_tweet_file_names[4], parse = FALSE)
stream_tweets(hundred_most_common_english_words, timeout = 300, file_name = rand_tweet_file_names[5], parse = FALSE)
stream_tweets( trending_query, timeout = 300, file_name = rand_tweet_file_names[6], parse = FALSE)
stream_tweets(hundred_most_common_english_words2, timeout = 360, file_name = rand_tweet_file_names[7], parse = FALSE)
stream_tweets( trending_query, timeout = 300, file_name = "rand_tweets9.json", parse = FALSE)
stream_tweets(hundred_most_common_english_words3, timeout = 360, file_name = "rand_tweets8.json", parse = FALSE)
stream_tweets("", timeout = 425, file_name = "rand_tweets7.json", parse = FALSE)
stream_tweets("", timeout = 300, file_name = "rand_tweets10.json", parse = FALSE)
stream_tweets("", timeout = 600, file_name = "rand_tweets11.json", parse = FALSE)
stream_tweets2("", timeout = 30, file_name = "rand_tweets10.json", parse = FALSE, append = TRUE) #dir = "rand_tweets11.json", append = TRUE)
#stream_tweets2("", timeout = 20, file_name = "rand_tweets10.json", append = TRUE) #dir = "rand_tweets11.json", append = TRUE)
#stream_tweets("", timeout = 1200, file_name = "rand_tweets12.json")

help("search_tweets")
parsed_tweets_1 = parse_stream(rand_tweet_file_names[1])
parsed_tweets_2 = parse_stream(rand_tweet_file_names[2])
parsed_tweets_3 = parse_stream(rand_tweet_file_names[3])
parsed_tweets_4 = parse_stream(rand_tweet_file_names[4])
parsed_tweets_5 = parse_stream(rand_tweet_file_names[5])
parsed_tweets_6 = parse_stream(rand_tweet_file_names[6])
parsed_tweets_7 = parse_stream(rand_tweet_file_names[7])
parsed_tweets_8 = parse_stream("rand_tweets7.json")
#parsed_tweets_8_verified <- parsed_tweets_8[which(parsed_tweets_8$verified==TRUE),]
parsed_tweets_9 = parse_stream("rand_tweets8.json")
parsed_tweets_10 = parse_stream("rand_tweets9.json")
parsed_tweets_11 = parse_stream("rand_tweets10.json")
parsed_tweets_12 = parse_stream("rand_tweets11.json")
parsed_tweets_13 = parse_stream("rand_eng_tweets_stream2.json")
parsed_tweets_14 = parse_stream("rand_tweets_stream22.json")
parsed_tweets_15 = parse_stream("rand_tweets_stream.json")
#parsed_tweets_16 = parse_stream("rand_tweets12.json")
#parsed_tweets_16 <- parsed_tweets_16[which(parsed_tweets_16$verified==TRUE),]
all_parsed_tweets <- rbind(parsed_tweets_1,parsed_tweets_2,parsed_tweets_3,
													 parsed_tweets_4,parsed_tweets_5,parsed_tweets_6,
													 parsed_tweets_7, parsed_tweets_8,parsed_tweets_9,
													 parsed_tweets_10,parsed_tweets_11,parsed_tweets_12,
													 parsed_tweets_13,parsed_tweets_14,parsed_tweets_15)
#all_parsed_tweets_sample <- ira_tweets_sub1[sample(nrow(ira_tweets_sub1), 1000, replace = FALSE, prob = NULL), c(1:5,7:16,18:31)]
#sample randomly from them? Just need to get right ratio*
#remove useless column attributes
#create necessary attributes
#merge with IRA_tweets

all_parsed_tweets_cleaned = all_parsed_tweets[,c(2,72,76,77,81,3,85,30,5,6,8,11,12,14,13,15,19,28)]
all_ira_samples_cleaned_column_names
colnames(all_parsed_tweets)
parsed_initial_column_names = c("tweetid", "user_reported_location", "follower_count" ,"following_count",            "account_creation_date", "tweet_date_time", "account_language",              
																"tweet_language", "tweet_text", "tweet_client",                 
																"is_reply",  "is_quote", "is_Retweet", "retweet_count",                
																"like_count", "hashtags", "urls", "user_mentions")
colnames(all_parsed_tweets_cleaned) =parsed_initial_column_names
all_parsed_tweets_cleaned$tweetid  <- as.numeric(all_parsed_tweets_cleaned$tweetid)
all_parsed_tweets_cleaned$follower_count  <- as.numeric(all_parsed_tweets_cleaned$follower_count)
all_parsed_tweets_cleaned$following_count  <- as.numeric(all_parsed_tweets_cleaned$following_count)
all_parsed_tweets_cleaned$account_creation_date  <- as.Date(all_parsed_tweets_cleaned$account_creation_date)
all_parsed_tweets_cleaned$tweet_date_time  <- as.Date(all_parsed_tweets_cleaned$tweet_date_time)
all_parsed_tweets_cleaned$retweet_count  <- as.numeric(all_parsed_tweets_cleaned$retweet_count)
all_parsed_tweets_cleaned$like_count  <- as.numeric(all_parsed_tweets_cleaned$like_count)
#colnames(parsed_tweets_7_sample_test)<- new_headers
#test_total_binded <- rbind(parsed_tweets_7_sample_test, all_ira_samples)
#remove unnecessary columns

###Add data attribute: is_quote? to binary (was int id of quoted tweet)
#all_ira_samples$is_quote = 0
#all_samples = cbind(all_ira_samples, all_rand_samples)
for (i in 1:length(all_parsed_tweets_cleaned$is_quote)) {
	if (all_parsed_tweets_cleaned$is_quote[i] == FALSE){
		all_parsed_tweets_cleaned$is_quote[i] = 0
	}
	else{
		all_parsed_tweets_cleaned$is_quote[i] = 1
	}
}
all_parsed_tweets_cleaned$is_quote  <- as.numeric(all_parsed_tweets_cleaned$is_quote)

###Add data attribute: is_reply? to binary (was int id of quoted tweet)
#all_ira_samples$is_reply = 0
for (i in 1:length(all_parsed_tweets_cleaned$is_reply)) {
	if (is.na(all_parsed_tweets_cleaned$is_reply[i])){
		all_parsed_tweets_cleaned$is_reply[i] = 0
	}
	else{
		all_parsed_tweets_cleaned$is_reply[i] = 1
	}
}
all_parsed_tweets_cleaned$is_reply  <- as.numeric(all_parsed_tweets_cleaned$is_reply)

#Change data attribute: is_retweet? to binary (was string)
for (i in 1:length(all_parsed_tweets_cleaned$is_Retweet)) {
	if (all_parsed_tweets_cleaned$is_Retweet[i] == FALSE){
		all_parsed_tweets_cleaned$is_Retweet[i] = 0
	}
	else if(all_parsed_tweets_cleaned$is_Retweet[i] == TRUE){
		all_parsed_tweets_cleaned$is_Retweet[i] = 1
	}
	else{
		print("error:  ")
		print(all_parsed_tweets_cleaned$is_Retweet[i])
	}
}
all_parsed_tweets_cleaned$is_Retweet  <- as.numeric(all_parsed_tweets_cleaned$is_Retweet)

all_parsed_tweets_cleaned$user_has_reported_location = 0
###Add data attribute: user_has_reported_location
for (i in 1:length(all_parsed_tweets_cleaned$user_reported_location)) {
	if (is.na(all_parsed_tweets_cleaned$user_reported_location[i]) || all_parsed_tweets_cleaned$user_reported_location[i] == ""){
		all_parsed_tweets_cleaned$user_has_reported_location[i] = 0
	}
	else{
		all_parsed_tweets_cleaned$user_has_reported_location[i] = 1
		#print(all_ira_samples_cleaned$user_reported_location[i])
	}
}
all_parsed_tweets_cleaned$user_has_reported_location  <- as.numeric(all_parsed_tweets_cleaned$user_has_reported_location)


#account_language == tweet_language?
###Add data attribute: acct_tweet_lang_same (account_lang == tweet_lang?)
###Add data attribute: tweet_lang == 'en'?:English
all_parsed_tweets_cleaned$acct_tweet_lang_same = 0
all_parsed_tweets_cleaned$tweet_lang_english = 0
all_parsed_tweets_cleaned$tweet_lang_russian = 0
for (i in 1:length(all_parsed_tweets_cleaned$account_language)) {
	if (all_parsed_tweets_cleaned$account_language[i] == all_parsed_tweets_cleaned$tweet_language[i]){
		all_parsed_tweets_cleaned$acct_tweet_lang_same[i] = 1
	}
	if(all_parsed_tweets_cleaned$tweet_language[i] == 'en'){
		all_parsed_tweets_cleaned$tweet_lang_english[i] = 1
	}
	if(all_parsed_tweets_cleaned$tweet_language[i] == 'ru'){
		all_parsed_tweets_cleaned$tweet_lang_russian[i] = 1
	}
}
all_parsed_tweets_cleaned$tweet_lang_russian  <- as.numeric(all_parsed_tweets_cleaned$tweet_lang_russian)
all_parsed_tweets_cleaned$tweet_lang_english  <- as.numeric(all_parsed_tweets_cleaned$tweet_lang_english)
all_parsed_tweets_cleaned$acct_tweet_lang_same  <- as.numeric(all_parsed_tweets_cleaned$acct_tweet_lang_same)

#tweet_text length? - Not useful...
#hashtags
#urls
#user_mentions
all_parsed_tweets_cleaned$hashtags = as.character(all_parsed_tweets_cleaned$hashtags)
###Add data attribute: has hashtags and num hashtags
all_parsed_tweets_cleaned$has_hashtags = 0
all_parsed_tweets_cleaned$num_hashtags = 0
for (i in 1:length(all_parsed_tweets_cleaned$hashtags)) {
	hashtag_count = 0
	if (is.na(all_parsed_tweets_cleaned$hashtags[i])){
		all_parsed_tweets_cleaned$has_hashtags[i] = 0
		all_parsed_tweets_cleaned$num_hashtags[i] = 0
	}
	else{
		all_parsed_tweets_cleaned$has_hashtags[i] = 1	
		all_hashtags = all_parsed_tweets_cleaned$hashtags[i]
		num_comma = str_count(all_hashtags, ',')
		hashtag_count = 1 + num_comma
		all_parsed_tweets_cleaned$num_hashtags[i] = hashtag_count
	}
	
	#all_samples$num_hashtags[i] = hashtag_count	
}
all_parsed_tweets_cleaned$has_hashtags  <- as.numeric(all_parsed_tweets_cleaned$has_hashtags)
all_parsed_tweets_cleaned$num_hashtags  <- as.numeric(all_parsed_tweets_cleaned$num_hashtags)
str_count(all_parsed_tweets_cleaned$urls[113883], ',')

###Add data attribute: has urls and number of urls
all_parsed_tweets_cleaned$urls = as.character(all_parsed_tweets_cleaned$urls)
all_parsed_tweets_cleaned$has_urls = 0
all_parsed_tweets_cleaned$num_urls = 0
for (i in 1:length(all_parsed_tweets_cleaned$hashtags)) {
	url_count = 0
	if (is.na(all_parsed_tweets_cleaned$urls[i])){
		all_parsed_tweets_cleaned$has_urls[i] = 0
		all_parsed_tweets_cleaned$num_urls[i] = 0
	}
	else{
		all_parsed_tweets_cleaned$has_urls[i] = 1	
		all_urls = all_parsed_tweets_cleaned$urls[i]
		num_comma_url = str_count(all_urls, ',')
		url_count = 1
		url_count = url_count+num_comma_url
		all_parsed_tweets_cleaned$num_urls[i] = url_count
	}
}
all_parsed_tweets_cleaned$has_urls  <- as.numeric(all_parsed_tweets_cleaned$has_urls)
all_parsed_tweets_cleaned$num_urls  <- as.numeric(all_parsed_tweets_cleaned$num_urls)

###Add data attribute:has user mentions and number of user mentions
all_parsed_tweets_cleaned$user_mentions = as.character(all_parsed_tweets_cleaned$user_mentions)
all_parsed_tweets_cleaned$has_user_mentions = 0
all_parsed_tweets_cleaned$num_user_mentions = 0
for (i in 1:length(all_parsed_tweets_cleaned$hashtags)) {
	mention_count = 0
	if (is.na(all_parsed_tweets_cleaned$user_mentions[i])){
		all_parsed_tweets_cleaned$has_user_mentions[i] = 0
		all_parsed_tweets_cleaned$num_user_mentions[i] = 0
	}
	else{
		all_parsed_tweets_cleaned$has_user_mentions[i] = 1	
		all_mentions = all_parsed_tweets_cleaned$user_mentions[i]
		num_mentions = str_count(all_mentions, ',')
		mention_count = 1 + num_mentions
		all_parsed_tweets_cleaned$num_user_mentions[i] = mention_count
	}
}
all_parsed_tweets_cleaned$has_user_mentions  <- as.numeric(all_parsed_tweets_cleaned$has_user_mentions)
all_parsed_tweets_cleaned$num_user_mentions  <- as.numeric(all_parsed_tweets_cleaned$num_user_mentions)

###Add data attribute: Time difference between tweet and account creation
all_parsed_tweets_cleaned$diff_tweet_acct_creation_time <- 0
for (i in 1:length(all_parsed_tweets_cleaned$hashtags)) {
	all_parsed_tweets_cleaned$diff_tweet_acct_creation_time[i] <- difftime(as.POSIXct(all_parsed_tweets_cleaned[i,]$tweet_date_time), as.POSIXct(all_parsed_tweets_cleaned[i,]$account_creation_date), units = c("days"))
}
all_parsed_tweets_cleaned$diff_tweet_acct_creation_time  <- as.numeric(all_parsed_tweets_cleaned$diff_tweet_acct_creation_time)

###Add data attribute: flag malicious
all_parsed_tweets_cleaned$malicious <- 0
for (i in 1:length(all_parsed_tweets_cleaned$hashtags)) {
	all_parsed_tweets_cleaned$malicious[i] <- 0
}
all_parsed_tweets_cleaned$malicious  <- as.numeric(all_parsed_tweets_cleaned$malicious)
#all_parsed_tweets_cleaned = colnames(all_ira_samples_cleaned)
all_tweets <- rbind(all_parsed_tweets_cleaned,all_ira_samples_cleaned)

write.csv(all_parsed_tweets_cleaned,'all_random_tweets_cleaned.csv')
fwrite(all_parsed_tweets_cleaned,'f_all_random_tweets_cleaned.csv')

write.csv(all_ira_samples_cleaned,'all_ira_tweets_cleaned.csv')
fwrite(all_ira_samples_cleaned,'f_all_ira_tweets_cleaned.csv')

write.csv(all_tweets,'all_tweets.csv')
fwrite(all_tweets,'f_all_tweets.csv')







