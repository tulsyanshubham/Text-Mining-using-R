#installing tm package
#install.packages('tm')
library(tm)
# loading reqiured package

#create corpus
docs<- corpus(DirSource('nm.txt'))
print(docs)
inspect(docs)

# inspect a particular document
writeLines(as.character(docs[[30]]))

#start preprocessing
toSpace <- content_transformer(function(x, pattern) {return(gsub(pattern, " ",x))})
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, "_")
#good practice to check after each step
writeLines(as.character(docs[[30]]))
#Remove punctuation
docs<- tm_map(docs, removePunctuation)

#transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
#Strip digits
docs <- tm_map(docs, removeNumbers)

#Remove stopwords from standard list
docs <- tm_map(docs, removewords, stopwords("english"))
#Strip whitespace 
docs <- tm_map(docs, stripWhitespace)
#inspect output
writeLines(as.character(docs[[30]]))

#Need snowballc library for streaming
install.packages('Snowballc')
library(SnowballC)

#Stem document
docs <- tm_map(docs,stemDocument)
#some clean up
docs <- tm_map(docs, content_transformer(gsub), patten = "organiz", replacement ="organ")
#inspect
writeLines(as.character(docs[[30]]))

#create document-term matrix
dtm <- DocumentTermMatrix(docs)
#inspect segment of document term matrix
inspect(dtm[1:2,1001:1007])

#collapse matrix by summing over columns - this gets total counts(over all docs) for each term
freq <- colsums(as.matrix(dtm))

#length should be total no of terms
length(freq)
#Create sort order (asc)
ord <- order.(freq, decreasing=TRUE)
#inspect most frequently occuring terms
freq[head(ord)]
#inspect least frequently occuring terms
freq[tail(ord)]

#remove very frequent and very rare words
dtmr <-DocumentTermMatrix(docs, control=list(wordlengths=c(4,20), bounds = list(global = c(3,27))))

freqr = colSums(as.matrix(dtmr))
#length should be total no of terms
length(freqr)
#create sort order (asc)
ordr <- order(freqr, decreasing = TRUE)
#inspect most frequently occuring terms
freqr[head(ordr)]
#inspect least frequently occuring terms
freqr[tail(ordr)] 
#list most frequent terms. Lower bound specified as second argument
findFreqTerm(dtmr,lowfreq=80)

#correlations
findAssocs(dtmr, "project" , 0.6)
findAssocs(dtmr,"enterprise",0.6)
findAssocs(dtmr,"system",0.6)

#histogram
wf=data.frame(term=names(freqr),occurences=freqr)
library(gglot2)
p<- ggplot(subset(wf,freqr>100), aes(term, occurences))
p <- p + geom_bar(stat= "identify")
p<- p+ theme(axis.text.x=element_text(angle=45, hjust=1))

