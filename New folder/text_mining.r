{
#installing tm package
#install.packages('tm')
library(tm)
# loading reqiured package

#create corpus
docs<- Corpus(DirSource('Base2/'))
#print(docs)
inspect(docs)

# inspect a particular document
writeLines(as.character(docs[[2]]))

#start preprocessing
toSpace <- content_transformer(function(x, pattern) {return(gsub(pattern, " ",x))})

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, '"')
docs <- tm_map(docs, toSpace, " -")

writeLines(as.character(docs[[2]]))
#Remove punctuation
docs<- tm_map(docs, removePunctuation)

#transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
#Strip digits
docs <- tm_map(docs, removeNumbers)

#Remove stopwords from standard list
docs <- tm_map(docs, removeWords, stopwords("english"))
#Strip whitespace 
docs <- tm_map(docs, stripWhitespace)
#inspect output
writeLines(as.character(docs[[2]]))
}
{
#Need snowballc library for streaming
#install.packages('SnowballC')
library(SnowballC)

docs <- tm_map(docs, content_transformer(gsub), pattern = "activity", replacement ="active")
docs <- tm_map(docs, content_transformer(gsub), pattern = "ting", replacement ="te")
docs <- tm_map(docs, content_transformer(gsub), pattern = "ning", replacement ="n")
docs <- tm_map(docs, content_transformer(gsub), pattern = "stories", replacement ="story")
#Stem document
docs <- tm_map(docs,stemDocument)
#some clean up
docs <- tm_map(docs, content_transformer(gsub), pattern = "challeng", replacement ="challenge")
docs <- tm_map(docs, content_transformer(gsub), pattern = "creativ", replacement ="creative")
docs <- tm_map(docs, content_transformer(gsub), pattern = "stori", replacement ="story")
docs <- tm_map(docs, content_transformer(gsub), pattern = "easi", replacement ="easy")
docs <- tm_map(docs, content_transformer(gsub), pattern = "forc", replacement ="force")
docs <- tm_map(docs, content_transformer(gsub), pattern = "undertaken", replacement ="undertake")
docs <- tm_map(docs, content_transformer(gsub), pattern = "websit", replacement ="website")
docs <- tm_map(docs, content_transformer(gsub), pattern = "comput", replacement ="computer")
docs <- tm_map(docs, content_transformer(gsub), pattern = "electr", replacement ="electric")
docs <- tm_map(docs, content_transformer(gsub), pattern = "messag", replacement ="message")
docs <- tm_map(docs, content_transformer(gsub), pattern = "devic", replacement ="device")
docs <- tm_map(docs, content_transformer(gsub), pattern = "amaz", replacement ="amaze")
docs <- tm_map(docs, content_transformer(gsub), pattern = "outsid", replacement ="outside")
}
{
#inspect
writeLines(as.character(docs[[2]]))

#create document-term matrix
dtm <- DocumentTermMatrix(docs)
#inspect segment of document term matrix
inspect(dtm)

#collapse matrix by summing over columns - this gets total counts(over all docs) for each term
freq <- colSums(as.matrix(dtm))

#length should be total no of terms
length(freq)
print(freq)
#Create sort order (asc)
ord <- order(freq, decreasing=TRUE)
#inspect most frequently occuring terms
freq[head(ord)]
#inspect least frequently occuring terms
freq[tail(ord)]

#remove very frequent and very rare words
dtmr <-DocumentTermMatrix(docs, control=list(wordlengths=c(4,20), bounds = list(global = c(2,27))))

freqr = colSums(as.matrix(dtmr))
#length should be total no of terms
length(freqr)
print(freqr)
#create sort order (asc)
ordr <- order(freqr, decreasing = TRUE)
#inspect most frequently occuring terms
freqr[head(ordr)]
#inspect least frequently occuring terms
freqr[tail(ordr)] 
#list most frequent terms. Lower bound specified as second argument
findFreqTerms(dtmr,lowfreq=7)
}
#correlations
findAssocs(dtmr, "computer" , 0.6)
findAssocs(dtmr,"data",0.6)
findAssocs(dtmr,"device",0.6)

#histogram
dtma <-DocumentTermMatrix(docs, control=list(wordlengths=c(2,20), bounds = list(global = c(5,27))))
m <- as.matrix(dtma)   
m 
Frequency <- sort(colSums(as.matrix(dtma)), decreasing=TRUE)  
wf <- data.frame(Words=names(Frequency), freq=Frequency)   

p <- ggplot(subset(wf, Frequency>1), aes(Words, Frequency))    
p <- p + geom_bar(stat="identity") 
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
p

#wf=data.frame(term=names(freqr),occurences=freqr)
#library(ggplot2)
#p<- ggplot(subset(wf,freqr>100), aes(term, occurences))
#p <- p + geom_bar(stat= "identity")
#p<- p + theme(axis.text.x=element_text(angle=45, hjust=1))
#p

#wordcloud
#install.packages('wordcloud')
library(wordcloud)
set.seed(42)
#limit words by specifying min frequency
#wordcloud (names (freqr), freqr, min.freq=70)
wordcloud (names(freqr), freqr,min.freq=6,colors=brewer.pal (6, "Dark2"))
