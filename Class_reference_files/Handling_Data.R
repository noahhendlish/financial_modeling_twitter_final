



#rm(list=ls(all=TRUE))
options(stringsAsFactors = F)
setwd("~/Desktop/Financial_modeling")  ## This is your working directory. This is the place from where you will be reading and writting data 



### Class 6: Handling Data  #####
# by Santiago Truffa



#list.files()
#########################################################
#set Data
#########################################################
mydata.pirates <- read.dta("~/Desktop/Financial_modeling/Pirates.dta")

names(mydata.pirates) <- c("year","month","day", "capt_date","ac","boat_grt","boat_management","boat_flag","boat_dwt","ap","total_on_board","name","release_status","ransom","year_r","month_r","day_r","release_date","type","time_in_captivity")
mydata.pirates <-mydata.pirates[2:229,]

class(mydata.pirates)

pirates.dt <- as.data.table(mydata.pirates)

## Subsetting 

# Lets look at those data points where we only have positive ransoms, and the boat has positive weight..


# This will allows us to look at this subset of data
pirates.dt[ransom > 0 & boat_grt > 0]

# how many observations do we have?

length(pirates.dt[ransom > 0 & boat_grt > 0]$boat_grt)  ## we look at the length on any given variable

## what data do we have?

names(pirates.dt)

## lets analyse the data format

class(pirates.dt$time_in_captivity)
class(pirates.dt$capt_date)


# so we read all the data as characters...

## Lets start cleaning!

pirates.dt$year <- as.numeric(pirates.dt$year)
pirates.dt$month <- as.numeric(pirates.dt$month)
pirates.dt$day <- as.numeric(pirates.dt$day)

# Date! 
pirates.dt$capt_date <- as.Date(pirates.dt$capt_date , "%m/%d/%y")  ## always look at the data to check format!

pirates.dt$ac ## Look at it.. this seems to be the place where the ship was captured, lets keep it as a character

pirates.dt$boat_grt <- as.numeric(pirates.dt$boat_grt)

## Lets visualize this variable

summary(pirates.dt$boat_grt)

hist(pirates.dt$boat_grt)

## unique(pirates.dt$year)

par(mfrow=c(2,4))
hist(pirates.dt[year == 2005]$boat_grt)
hist(pirates.dt[year == 2006]$boat_grt)
hist(pirates.dt[year == 2007]$boat_grt)
hist(pirates.dt[year == 2008]$boat_grt)
hist(pirates.dt[year == 2009]$boat_grt)
hist(pirates.dt[year == 2010]$boat_grt)
hist(pirates.dt[year == 2011]$boat_grt)
hist(pirates.dt[year == 2012]$boat_grt)


## lets keep on looking at the data

pirates.dt$boat_management  ## This looks like the country they where from

pirates.dt$boat_flag        ## Flag, not necesarily equal to the management

pirates.dt$boat_dwt <- as.numeric(pirates.dt$boat_dwt)

pirates.dt$ap <- as.numeric(pirates.dt$ap)

pirates.dt$total_on_board <- as.numeric(pirates.dt$total_on_board)

pirates.dt$ransom <- as.numeric(pirates.dt$ransom)

# lets see whether the boat size pirates where capturing changed by year

hist(pirates.dt$ransom)

pirates.dt$year_r <- as.numeric(pirates.dt$year_r)
pirates.dt$month_r <- as.numeric(pirates.dt$month_r)
pirates.dt$day_r <- as.numeric(pirates.dt$day_r)


# Date

# Date! 

pirates.dt$release_date <- as.Date(pirates.dt$release_date , "%m/%d/%y")  ## always look at the data to check format!

pirates.dt$type  ## Type of vessel, lets keep it as a character

pirates.dt$time_in_captivity <- as.numeric(pirates.dt$time_in_captivity)

par(mfrow=c(2,4))
hist(pirates.dt[year_r == 2005]$ransom)
hist(pirates.dt[year_r == 2006]$ransom)
hist(pirates.dt[year_r == 2007]$ransom)
hist(pirates.dt[year_r == 2008]$ransom)
hist(pirates.dt[year_r == 2009]$ransom)
hist(pirates.dt[year_r == 2010]$ransom)
hist(pirates.dt[year_r == 2011]$ransom)
hist(pirates.dt[year_r == 2012]$ransom)


par(mfrow=c(1,1))
boxplot(pirates.dt[year_r == 2005]$ransom,pirates.dt[year_r == 2006]$ransom,pirates.dt[year_r == 2007]$ransom,pirates.dt[year_r == 2008]$ransom,pirates.dt[year_r == 2009]$ransom,pirates.dt[year_r == 2010]$ransom,pirates.dt[year_r == 2011]$ransom,pirates.dt[year_r == 2012]$ransom)

boxplot(pirates.dt[year_r == 2005]$time_in_captivity,pirates.dt[year_r == 2006]$time_in_captivity,pirates.dt[year_r == 2007]$time_in_captivity,pirates.dt[year_r == 2008]$time_in_captivity,pirates.dt[year_r == 2009]$time_in_captivity,pirates.dt[year_r == 2010]$time_in_captivity,pirates.dt[year_r == 2011]$time_in_captivity,pirates.dt[year_r == 2012]$time_in_captivity)


par(mfrow=c(1,2))

boxplot(pirates.dt[year_r == 2005]$ransom,pirates.dt[year_r == 2006]$ransom,pirates.dt[year_r == 2007]$ransom,pirates.dt[year_r == 2008]$ransom,pirates.dt[year_r == 2009]$ransom,pirates.dt[year_r == 2010]$ransom,pirates.dt[year_r == 2011]$ransom,pirates.dt[year_r == 2012]$ransom)

boxplot(pirates.dt[year_r == 2005]$time_in_captivity,pirates.dt[year_r == 2006]$time_in_captivity,pirates.dt[year_r == 2007]$time_in_captivity,pirates.dt[year_r == 2008]$time_in_captivity,pirates.dt[year_r == 2009]$time_in_captivity,pirates.dt[year_r == 2010]$time_in_captivity,pirates.dt[year_r == 2011]$time_in_captivity,pirates.dt[year_r == 2012]$time_in_captivity)

par(mfrow=c(1,1))
plot(pirates.dt$ransom, pirates.dt$time_in_captivity, main="Scatterplot Example", 
     xlab="Ransom (in US M) ", ylab="Time in captivity (In days)", pch=19)


