require(streamR)
require(ROAuth)
require(plyr)
require(stringr)
require(tm)
require(SnowballC)
require(devtools)
require(RMOA)
require(ff)
require(rJava)
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
s <- tm_map(s, removeNumbers)
s <- tm_map(s, removeWords, c(stopwords("english"),"rt","http","retweet"))
s <- tm_map(s, PlainTextDocument)
s<-tm_map(txt.corpus,stripWhitespace)
tweets<-data.frame(text=sapply(s, '[[', "content"), stringsAsFactors=FALSE)

modifiedStripWhiteSpace<-function(a)
{
  if(a!=" ")
  {
    a[,1]=stripWhitespace(a[,1])
    data.frame(a) 
  }
}

newtweets<-ddply(tweets,.(text),modifiedStripWhiteSpace)
tweets<-newtweets
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
# and splits it into words:
SplitSentence <- function(tuple,...)
{
  # Split the sentence into words
  words <- unlist(strsplit(as.character(tuple$text), " "))
  # For each word emit a tuple
  for (word in words)
  {
    if (word!="")
    {
      Emit(Tuple(data.frame(word = word)),...)
    }  
  }
}

# R word counting function:
CountWord <- function(tuple,...){
  # Get the hashmap "word count"
  words <- GetHash("wordcount")
  if (tuple$word %in% words$word) {
    # Increment the word count:
    words[words$word == tuple$word,]$count <-words[words$word == tuple$word,]$count + 1
    print(tuple$word)
  } else { 
    # If the word does not exist
    # Add the word with count 1
    words <- rbind(words, data.frame(word = tuple$word, count = 1))
  }
  # Store the hashmap
  SetHash("wordcount", words)
}

# Run the stream:
result <- RStorm(topology)
# Obtain results stored in "wordcount"
counts <- GetHash("wordcount", result)
