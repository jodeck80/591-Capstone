library(streamR)
library(plyr)
library(ggplot2)
library(grid)
library(maps)
library(RStorm)
load("my_oauth.Rdata")

#Vectors of nicknames for teams
#Doing NBA teams in playoffs
#constant vector of types, 4 buzz words for each team
#namesCavs <- c("cleveland", "cavaliers", "cavs", "Lebron")
#namesHawks <- c("atlanta", "hawks", "horford", "atl")
#namesBulls <- c("chicago", "bulls", "butler", "noah")
#namesWizards <- c("washington", "wizards", "pierce", "Lebron")
#namesWarriors <- c("golden state", "warriors", "curry", "steph")
#namesRockets <- c("houston", "rockets", "harden", "howard")
#namesGrizzlies <- c("memphis", "grizzlies", "randolph", "gasol")
#namesSpurs <- c("san antonio", "spurs", "duncan", "ginobili")

#run these prior to starting loop
#handshake for twitter credentials
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#define globally, used in many functions
tweets.running <- 0

#function to remove tweets having both the words love and hate
filterTweets<-function(a)
{
  # check if text doesn't have both love and hate in it
  if((length(grep("sox",a,ignore.case=TRUE))>0) | 
       (length(grep("yankees",a,ignore.case=TRUE))>0) | 
       (length(grep("tigers",a,ignore.case=TRUE))>0) | 
       (length(grep("dodgers",a,ignore.case=TRUE))>0) | 
       (length(grep("marlins",a,ignore.case=TRUE))>0) |
       (length(grep("cardinals",a,ignore.case=TRUE))>0) |
       (length(grep("MLB",a,ignore.case=TRUE))>0))
  {
    #set league to 1
    a$league = 1
    
    #Hashmap to keep approximation of league count
    words <- GetHash("wordcount")
    if("MLB" %in% words$word){
      words[words$word == "MLB",]$count <- words[words$word == "MLB",]$count + 1
    }else
    {
      words <- rbind(words, data.frame(word = "MLB", count = 1))
    }
    # Store the hashmap
    SetHash("wordcount", words)    
  }
  data.frame(a)
}

#function to remove tweets having both the words love and hate
filterTweetsNBA<-function(a)
{
  # check if text doesn't have both love and hate in it
  if((length(grep("cavs",a,ignore.case=TRUE))>0) | 
       (length(grep("spurs",a,ignore.case=TRUE))>0) | 
       (length(grep("NBA",a,ignore.case=TRUE))>0) | 
       (length(grep("warriors",a,ignore.case=TRUE))>0) | 
       (length(grep("clippers",a,ignore.case=TRUE))>0) | 
       (length(grep("hawks",a,ignore.case=TRUE))>0))
  {
    # return it as data frame
    #data.frame(a)
    a$league = 2
  }

  data.frame(a)
}


updateTweets<-function(a){

#TODO, maybe update to minute
#stream tweets only in US locations
filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 20, oauth = my_oauth)
  
#parse tweets from .json file
tweets.df <- parseTweets("tweetsUS.json", verbose = FALSE)

#delete file once it is stored, to be written to again
file.remove("tweetsUS.json")

#copy parsed tweets, init league to 0
tweets.filter <- tweets.df
tweets.filter$league = 0

# get only tweets if they satisfy filter
tweets.filter<-ddply(tweets.df,.(text),filterTweets)

# get only tweets if they satisfy filter
tweets.filter<-ddply(tweets.filter,.(text),filterTweetsNBA)

#Only look at rows where at least one league was selected
tweets.filter <- subset(tweets.filter, tweets.filter$league!=0 )

#append new filtered tweets to old
tweets.running <- rbind(tweets.running, tweets.filter)

return (tweets.running)
}

#define data frame used for mapping
defineMapPoints <- function(a)
{

#define points with only latitude, longitude, and league class
points1 <- data.frame(x = as.numeric(tweets.running$lon), y = as.numeric(tweets.running$lat), 
                      league = as.numeric(tweets.running$league))

#ensure only points above mexico are considered
points1 <- points1[points1$y > 25, ]

return (points1)
}

mapData <- function(){
  
  #US map
  map.data <- map_data("state")
  
  p <- ggplot(map.data) + 
    geom_map(aes(map_id = region), map = map.data, fill = "white", color = "grey20", size = 0.25) + 
    expand_limits(x = map.data$long, y = map.data$lat) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid.major = element_blank(), plot.background = element_blank(), 
          plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
    geom_point(data = points1, aes(x = x, y = y, color = factor(points1$league)), size = 2 , alpha = 1/2) +
    coord_cartesian(xlim = c(-60, -130))  #trim off unused edges on x axis, not lways needed
  
  return (p)
  
}

#print total counts of each league
printTotals <- function()
{
  #print totals
  print(paste0("Total MLB Tweets: ", sum(tweets.running$league==1, na.rm=TRUE)))
  print(paste0("Total NBA Tweets: ", sum(tweets.running$league==2, na.rm=TRUE)))
}

#determine good length
for (i in 1:3)
{

  #Gather new tweets and update filter
  tweets.running <- updateTweets(tweets.running)
  
  #Prepare tweets to be mapped
  points1 <- defineMapPoints(tweets.running)
  
  #Define map and print it
  map <- mapData()
  print(map)
  
  #Print total counts
  printTotals()
}


#remaining is UNUSED for capstone, only used as reference
#all tweets map
map.data <- map_data("state")

#make data frame to represent coordinates for each tweet
points <- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))

#remove points if latitude isn't greater than 25
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue") + 
  coord_cartesian(xlim = c(-60, -130)) #Make map trim off unused edges on x axis
