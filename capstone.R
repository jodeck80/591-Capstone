library(streamR)
library(plyr)
library(ggplot2)
library(grid)
library(maps)
library(RStorm)
library(tm)
library(stringr)
library(ROAuth)
load("my_oauth.Rdata")

# Color blind friendly colors:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#1 minute tweet duration
TWEET_DURATION <- 60

#number of times to iterate process
ITERATIONS <- 3

MLBTerms <- c("MLB","yankees", "red sox", "orioles", "rays", "blue jays",
              "mets", "braves", "marlins", "phillies", "nationals",
              "tigers", "twins", "indians", "royals", "white sox",
              "cubs", "cardinals", "reds", "pirates", "brewers",
              "mariners", "a's", "astros", "angels", "rangers",
              "dodgers", "giants", "diamondbacks", "padres", "rockies")
NBATerms <- c("NBA","hawks", "cavaliers", "bulls", "raptors", "wizards",
              "bucks", "celtics", "nets", "pacers", "heat",
              "hornets", "pistons", "magic", "76ers", "knicks",
              "warriors", "rockets", "clippers", "spurs", "trail blazers",
              "grizzlies", "mavericks", "pelicans", "thunder", "suns",
              "jazz", "nuggets", "kings", "lakers", "timberwolves")

NFLTerms <- c("NFL","eagles", "giants", "cowboys", "redskins", "bills",
              "patriots", "dolphins", "jets", "bears", "lions",
              "packers", "vikings", "browns", "bengals", "steelers",
              "ravens", "panthers", "saints", "buccaneers", "falcons",
              "titans", "colts", "jaguars", "texans", "49ers",
              "cardinals", "seahawks", "rams", "broncos", "chargers", "raiders", "chiefs")

NHLTerms <- c("NHL","rangers", "canadiens", "lightning", "capitals", "islanders",
              "red wings", "senators", "penguins", "bruins", "panthers",
              "blue jackets", "flyers", "devils", "hurricanes", "maple leafs",
              "sabres", "ducks", "blues", "predators", "blackhawks",
              "canucks", "wild", "jets", "flames", "kings",
              "stars", "avalanche", "sharks", "oilers", "coyotes")
maxMLB<-function(a,b)
{
  max1 = 0
  max2 = 0
  MLB=NULL
  for(i in 2:length(b))
  {
    max1 = 0
    for(j in 1:dim(a)[1]) 
    {
      if(length(grep(b[i],a[j,]$word,ignore.case=TRUE))>0)
      {
        max1 = max1 + a[j,]$count
      }
    }
    if(max2<max1)
    {
      max2 = max1
      MLB = b[i]
    }
  }
  return(MLB)
}

maxMLBcount<-function(a,b)
{
  max1 = 0
  for(i in 1:length(b))
  {
    for(j in 1:dim(a)[1]) 
    {
      if(length(grep(b[i],a[j,]$word,ignore.case=TRUE))>0)
      {
        max1 = max1 + a[j,]$count
      }
    }
  }
  return(max1)
}


# R word counting function:
CountWord <- function(tuple,...){
  # Get the hashmap "word count"
  counts <- GetHash("wordcount")
  if (tuple$word %in% counts$word) {
    # Increment the word count:
    counts[counts$word == tuple$word,]$count <-counts[counts$word == tuple$word,]$count + 1
  } else { 
    # If the word does not exist
    # Add the word with count 1
    counts <- rbind(counts, data.frame(word = tuple$word, count = 1,stringsAsFactors=F))
  }
  # Store the hashmap
  SetHash("wordcount", counts)
}
# and splits it into words:
SplitSentence <- function(tuple,...)
{
  if((tuple$text!="")||(tuple$text!=" "))
  {
    # Split the sentence into words
    words <- unlist(strsplit(as.character(tuple$text), " "))
    # For each word emit a tuple
    for (word in words)
    {
      if (word!="")
      {
        Emit(Tuple(data.frame(word = word,stringsAsFactors=F)),...)
      }  
    } 
  }
}

countMin<-function(tweets.running){
  oneData<-as.data.frame(tweets.running$text)
  oneData[,1]<-as.data.frame(str_replace_all(oneData[,1],"[^[:graph:]]", " "))
  oneData[,1] <- sapply(oneData[,1] ,function(row){ 
    iconv(row, "ISO_8859-2", "ASCII", sub="")
    iconv(row, "latin1", "ASCII", sub="")
    iconv(row, "LATIN2", "ASCII", sub="")
  })
  
  s <- Corpus(VectorSource(oneData[,1]),readerControl=list(language="en"))
  s <- tm_map(s, tolower)
  s <- tm_map(s, removeWords, c(stopwords("english"),"rt","http","retweet"))
  s <- tm_map(s, removePunctuation)
  s <- tm_map(s, PlainTextDocument)
  s <- tm_map(s, stripWhitespace)
  tweets<-data.frame(text=sapply(s, '[[', "content"), stringsAsFactors=FALSE)
  
  #function to pre-process tweet text
  
  topology = Topology(tweets)
  # Add the bolts:
  topology <- AddBolt(
    topology, Bolt(SplitSentence, listen = 0)
  )
  topology <- AddBolt(
    topology, Bolt(CountWord, listen = 1)
  )
  
  # R function that receives a tuple
  # (a sentence in this case)
  
  # Run the stream:
  resultLoc <- RStorm(topology)
  # Obtain results stored in "wordcount"
  
  return (resultLoc)
}
#function to remove tweets having both the words love and hate
filterTweetsMLB<-function(a)
{
  # check if text doesn't have both love and hate in it
  if((length(grep(MLBTerms[1],a,ignore.case=TRUE))>0) | 
       (length(grep(MLBTerms[2],a,ignore.case=TRUE))>0) | 
       (length(grep(MLBTerms[3],a,ignore.case=TRUE))>0) | 
       (length(grep(MLBTerms[4],a,ignore.case=TRUE))>0) | 
       (length(grep(MLBTerms[5],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[6],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[7],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[8],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[9],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[10],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[11],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[12],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[13],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[14],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[15],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[16],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[17],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[18],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[19],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[20],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[21],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[22],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[23],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[24],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[25],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[26],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[27],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[28],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[29],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[30],a,ignore.case=TRUE))>0) |
       (length(grep(MLBTerms[31],a,ignore.case=TRUE))>0))
{
    if(a$league != 0)
    {
      #another league was also mentioned, will remove tweet
      a$league = 5
    }
    else
    {
      #set league to 1 if league wasnt previously set
      a$league = "MLB"
    }
  }
data.frame(a)
}


#function to remove tweets having both the words love and hate
filterTweetsNBA<-function(a)
{
  # check if text doesn't have both love and hate in it
  if((length(grep(NBATerms[1],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[2],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[3],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[4],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[5],a,ignore.case=TRUE))>0) |
       (length(grep(NBATerms[6],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[7],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[8],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[9],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[10],a,ignore.case=TRUE))>0) |
       (length(grep(NBATerms[11],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[12],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[13],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[14],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[15],a,ignore.case=TRUE))>0) |
       (length(grep(NBATerms[16],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[17],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[18],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[19],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[20],a,ignore.case=TRUE))>0) |
       (length(grep(NBATerms[21],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[22],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[23],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[24],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[25],a,ignore.case=TRUE))>0) |
       (length(grep(NBATerms[26],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[27],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[28],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[29],a,ignore.case=TRUE))>0) | 
       (length(grep(NBATerms[30],a,ignore.case=TRUE))>0) )
{
    if(a$league != 0)
    {
      #another league was also mentioned, will remove tweet
      a$league = 5
    }
    else
    {
      #set league to 2 if league wasnt previously set
      a$league = "NBA"
    }
  }
data.frame(a)
}


#function to remove tweets having both the words love and hate
filterTweetsNFL<-function(a)
{
  # check if text doesn't have both love and hate in it
  if((length(grep(NFLTerms[1],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[2],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[3],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[4],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[5],a,ignore.case=TRUE))>0) |
       (length(grep(NFLTerms[6],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[7],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[8],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[9],a,ignore.case=TRUE))>0) |
       (length(grep(NFLTerms[10],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[11],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[12],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[13],a,ignore.case=TRUE))>0) |
       (length(grep(NFLTerms[14],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[15],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[16],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[17],a,ignore.case=TRUE))>0) |
       (length(grep(NFLTerms[18],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[19],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[20],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[21],a,ignore.case=TRUE))>0) |
       (length(grep(NFLTerms[22],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[23],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[24],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[25],a,ignore.case=TRUE))>0) |
       (length(grep(NFLTerms[26],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[27],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[28],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[29],a,ignore.case=TRUE))>0) |
       (length(grep(NFLTerms[30],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[31],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[32],a,ignore.case=TRUE))>0) | 
       (length(grep(NFLTerms[33],a,ignore.case=TRUE))>0) )
{
    if(a$league != 0)
    {
      #another league was also mentioned, will remove tweet
      a$league = 5
    }
    else
    {
      #set league to 3 if league wasnt previously set
      a$league = "NFL"
    }
  }
data.frame(a)
}

#function to remove tweets having both the words love and hate
filterTweetsNHL<-function(a)
{
  # check if text doesn't have both love and hate in it
  if((length(grep(NHLTerms[1],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[2],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[3],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[4],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[5],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[6],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[7],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[8],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[9],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[10],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[11],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[12],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[13],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[14],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[15],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[16],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[17],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[18],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[19],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[20],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[21],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[22],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[23],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[24],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[25],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[26],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[27],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[28],a,ignore.case=TRUE))>0) | 
       (length(grep(NHLTerms[29],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[30],a,ignore.case=TRUE))>0) |
       (length(grep(NHLTerms[31],a,ignore.case=TRUE))>0))
{
    if(a$league != 0)
    {
      #another league was also mentioned, will remove tweet
      a$league = 5
    }
    else
    {
      #set league to 4 if league wasnt previously set
      a$league = "NHL"
    }  
  }
data.frame(a)
}

#Capture new tweets and 
updateTweets<-function(a, firstTime){
  
  #stream tweets only in US locations
  filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = TWEET_DURATION, oauth = my_oauth)
  
  #parse tweets from .json file
  tweets.df <- parseTweets("tweetsUS.json", verbose = FALSE)
  
  #delete file once it is stored, to be written to again
  file.remove("tweetsUS.json")
  
  #copy parsed tweets, init league to 0
  tweets.filter <- tweets.df
  tweets.filter$league = 0
  
  # push tweets through all league filters
  tweets.filter<-ddply(tweets.filter,.(text),filterTweetsMLB)
  tweets.filter<-ddply(tweets.filter,.(text),filterTweetsNBA)
  tweets.filter<-ddply(tweets.filter,.(text),filterTweetsNFL)
  tweets.filter<-ddply(tweets.filter,.(text),filterTweetsNHL)
  
  #Only look at rows where one league was selected
  tweets.filter <- subset(tweets.filter, tweets.filter$league == "MLB" | 
                            tweets.filter$league == "NBA" |
                            tweets.filter$league == "NFL" |
                            tweets.filter$league == "NHL")
  
  #if first time, cant bind yet
  if(firstTime)
  {
    tweets.running <- tweets.filter
  }
  else
  {
    #append new filtered tweets to old
    tweets.running <- rbind(a, tweets.filter)
  }
  
  return (tweets.running)
}

mapData <- function(a){
  
  #US map
  map.data <- map_data("state")
  
  p <- ggplot(map.data) + 
    geom_map(aes(map_id = region), map = map.data, fill = "white", color = "grey20", size = 0.25) + 
    expand_limits(x = map.data$long, y = map.data$lat) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid.major = element_blank(), plot.background = element_blank(), legend.title=element_blank(),
          plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
    geom_point(data = points, aes(x = x, y = y, color = factor(points$league)), size = 2 , alpha = 1/2) +
    scale_colour_manual(values=cbbPalette) + 
    coord_cartesian(xlim = c(-60, -130))  #trim off unused edges on x axis, not lways needed
  
  return (p)
  
}

#print total counts of each league
printTotals <- function()
{  
  #print totals
  print(paste0("Total MLB Tweets: ", sum(tweets.running$league=="MLB", na.rm=TRUE)))
  print(paste0("Total NBA Tweets: ", sum(tweets.running$league=="NBA", na.rm=TRUE)))
  print(paste0("Total NFL Tweets: ", sum(tweets.running$league=="NFL", na.rm=TRUE)))
  print(paste0("Total NHL Tweets: ", sum(tweets.running$league=="NHL", na.rm=TRUE)))
}


#run these prior to starting loop
#handshake for twitter credentials
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#define globally, used in many functions
tweets.running <- data.frame(0)
points <- data.frame(0)
result <- data.frame(0)

#Set hash count
counts <- GetHash("wordcount")
#Gather new tweets and update filter for first time
tweets.running <- updateTweets(tweets.running, TRUE)
result <- countMin(tweets.running)

#determine good length
for (i in 1:ITERATIONS)
{
  
  #Gather new tweets and update filter
  tweets.running <- updateTweets(tweets.running, FALSE)
  result <- countMin(tweets.running)
  
  #define points with only latitude, longitude, and league class
  #update global so it can be used in ggplot
  points <- data.frame(x = as.numeric(tweets.running$lon), y = as.numeric(tweets.running$lat), 
                       league = tweets.running$league)
  
  #Define map and print it
  map <- mapData(points)
  print(map)
  
  #Print total counts
  printTotals()
}
#Prints the most successful franchises in one league
MLB=NULL
pop = NULL
MLBcount = 0
counts <- GetHash("wordcount", result)
MLB = maxMLB(counts,MLBTerms)
if(MLBcount<maxMLBcount(counts,MLBTerms))
{
  MLBcount = maxMLBcount(counts,MLBTerms)
  pop = "MLB"
}
print("Most Tweeted Baseball Team:")
print(MLB)

MLB = maxMLB(counts,NBATerms)
if(MLBcount<maxMLBcount(counts,NBATerms))
{
  MLBcount = maxMLBcount(counts,NBATerms)
  pop = "NBA"
}
print("Most Tweeted Basketball Team:")
print(MLB)

MLB = maxMLB(counts,NHLTerms)
if(MLBcount<maxMLBcount(counts,NHLTerms))
{
  MLBcount = maxMLBcount(counts,NHLTerms)
  pop = "NHL"
}
print("Most Tweeted Hockey Team:")
print(MLB)

MLB = maxMLB(counts,NFLTerms)
if(MLBcount<maxMLBcount(counts,NFLTerms))
{
  MLBcount = maxMLBcount(counts,NFLTerms)
  pop = "NFL"
}
print("Most Tweeted Football Team:")
print(MLB)

print("Most Tweeted League:")
print(pop)
