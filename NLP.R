#load libraries
library(NLP)
library(tm)
library(rJava)
library(openNLP) 
library(tm)
library(stringr)
library(readtext)
library(GGally)
library(RColorBrewer)
library(wordcloud)

#functions
getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  return(y2)  
} 

detectPatternOnDocument <- function(doc, pattern) {
  x=as.String(doc)
  res=str_match_all(x,pattern)
  
  dimrow=dim(res[[1]])[1]
  dimcol=dim(res[[1]])[2]
  
  # If there are no rows, no matches have been found
  if (dimrow == 0) {
    return(NA)
  }else{
    if (dimcol > 2){
      # If there are three or more columns, we have to paste all the groups together
      for (i in 1:dimrow) {
        res[[1]][i,2] = paste(res[[1]][i,2:dimcol], collapse = ' ')
      }
    }
    
    # We return all the results found separated by ','
    if (dimcol != 1) {
      result = paste(res[[1]][,2], collapse = ', ')
    }else{
      result = paste(res[[1]][,1], collapse = ', ')
    }
    return(result)
  }
}

detectPatternsInCorpus = function(corpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, 
                                    nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(corpus, detectPatternOnDocument, 
                                     pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

countMatchesPerRow = function (df) {
  entityCountPerFile <- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(entityCountPerFile) <- c("File","Count")
  
  for (i in 1:nrow(df)) {
    entityCountPerFile$File[i] = df$File[i]
    entityCountPerFile$Count[i] = length(Filter(Negate(is.na),df[i,2:length(df[i,])]))
  }
  return (entityCountPerFile[entityCountPerFile[2]!=0,])
}

countMatchesPerColumn = function (df) {
  entityCountPerPattern <- data.frame(matrix(NA, ncol = 2, 
                                             nrow = length(names(df))-1))
  names(entityCountPerPattern) <- c("Entity","Count")
  
  for (i in 2:length(names(df))) {
    entityCountPerPattern$Entity[i-1] = names(df)[i]
    entityCountPerPattern$Count[i-1] = nrow(subset(df, !is.na(df[i])))
  }
  return (entityCountPerPattern)
}

detectPatternOnDocumentWithContext <- function(doc, pattern) {
  txt=as.String(doc)
  number=50
  coord=str_locate(txt,pattern)
  res3=substr(txt,coord[1]-number,coord[2]+number)
  return (res3)
}

printMatchesPerPattern = function (patterns, matches) {
  for (i in 1:length(patterns)){
    print(paste("PATTERN: ",patterns[i]))
    strings = matches[,i+1][!is.na(unlist(matches[,i+1]))]
    print(strings)
    print(" ") 
  }
}

#set working directory
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

#load text
source = DirSource("corpus/", encoding = "UTF-8")
corpus_0 = Corpus(source)
corpus <- tm_map(corpus_0, content_transformer(tolower)) #important, remark on report
inspect(corpus[[1]]) #to check if the text is in lowercase

#First part: word analysis

#default term matrix
tdm = TermDocumentMatrix(corpus)

freq=rowSums(as.matrix(tdm))
tail(sort(freq),n=10) #most frequent words
sum(freq == 1) #number of words that appear only one time
plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency") #word frequency plot

#custom stop words, "without" appeared 14 times and it is not interesting in medical field
myStopWords = c(stopwords(),"without")

tdm = TermDocumentMatrix(corpus,
                         control=list(stopwords = myStopWords,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))
length(dimnames(tdm)$Terms) #number of terms that have been identified
freq=rowSums(as.matrix(tdm))
tail(sort(freq),n=10) #most frequent words

plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency") #word frequency plot

#bar plot

freq=rowSums(as.matrix(tdm))
high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")

#word cloud
pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]
set.seed(1234)

corpus.ngrams = VCorpus(source)

tdm.unigram = TermDocumentMatrix(corpus.ngrams,
                                 control=list(stopwords = c(myStopWords),
                                              removePunctuation = T, 
                                              removeNumbers = T)) 
freq = sort(rowSums(as.matrix(tdm.unigram)), decreasing = T)

word.cloud=wordcloud(words=names(freq), freq=freq,
                     min.freq=20, random.order=F, colors=pal)

#Second part: analyze texts by patterns

#1. Find patterns: Allergy
pattern0=c("allergy")
pattern0=c(pattern0,"asthma")
pattern0=c(pattern0,"nasal")
pattern0=c(pattern0,"allergic")

matches0 = detectPatternsInCorpus(corpus, pattern0)

countMatchesPerRow(matches0) #number of times that the patterns appear in each text
countMatchesPerColumn(matches0) #number of times the pattern appears

for (i in 1:length(pattern0)){
  print(paste("PATTERN: ",pattern0[i]))
  strings = lapply(corpus, detectPatternOnDocumentWithContext, pattern=pattern0[i])
  print(unlist(strings[!is.na(unlist(strings))]))
  print(" ")
}

#2. Find patterns: Urology
pattern1=c("urology")
pattern1=c(pattern1,"vasectomy")
pattern1=c(pattern1,"scrotal")
pattern1=c(pattern1,"prostate")

matches1= detectPatternsInCorpus(corpus, pattern1)

countMatchesPerRow(matches1) #number of times that the patterns appear in each text
countMatchesPerColumn(matches1) #number of times that the patterns appear in each text

for (i in 1:length(pattern1)){
  print(paste("PATTERN: ",pattern1[i]))
  strings = lapply(corpus, detectPatternOnDocumentWithContext, pattern=pattern1[i])
  print(unlist(strings[!is.na(unlist(strings))]))
  print(" ")
}

#3. For radiology text, which exam have been done to the patient?
pattern2=c("exam: , ([A-z]* [A-z]*)")

matches2= detectPatternsInCorpus(corpus, pattern2)

countMatchesPerRow(matches2) #number of times that the patterns appear in each text
countMatchesPerColumn(matches2) #number of times that the patterns appear in each text
printMatchesPerPattern(pattern2, matches2) #Exams that have been done to patients

for (i in 1:length(pattern2)){
  print(paste("PATTERN: ",pattern2[i]))
  strings = lapply(corpus, detectPatternOnDocumentWithContext, pattern=pattern2[i])
  print(unlist(strings[!is.na(unlist(strings))]))
  print(" ")
}
