##############################################
##TD-IDF
## http://stats.stackexchange.com/questions/78321/term-frequency-inverse-document-frequency-tf-idf-weighting

## http://stackoverflow.com/questions/26264981/using-bag-of-words-classifier-on-a-out-of-sample-dataset
#library('caTools')
library(tm)
library(RTextTools)

training.size <- 700000
validation.size <- 100000

#training.sample.size <- 350000
## Training
training <- read.csv("data/2015s2-mo444-assignment-02.csv", header = TRUE, sep = ",", nrows = training.size)

## In order to limit the usage of memory use only part of the training.
##
## Group the training$Category into the 39 categories.        
## We are interesting only on Category and Descript
training.cat.desc <- select(training, Category, Descript)
## Now create a corpus for each category keeping only the column Descript
train.arson <- select(filter(training.cat.desc, Category == "ARSON"), Descript)
corpus.arson <- Corpus(VectorSource(train.arson$Descript))

train.ASSAULT <- select(filter(training.cat.desc, Category == "ASSAULT"), Descript)
corpus.ASSAULT <- Corpus(VectorSource(train.ASSAULT$Descript))

train.BAD.CHECKS <- select(filter(training.cat.desc, Category == "BAD CHECKS"), Descript)
corpus.BAD.CHECKS <- Corpus(VectorSource(train.BAD.CHECKS$Descript))

train.BRIBERY <- select(filter(training.cat.desc, Category == "BRIBERY"), Descript)
corpus.BRIBERY <- Corpus(VectorSource(train.BRIBERY$Descript))

train.BURGLARY <- select(filter(training.cat.desc, Category == "BURGLARY"), Descript)
corpus.BURGLARY <- Corpus(VectorSource(train.BURGLARY$Descript))

train.DISORDERLY.CONDUCT <- select(filter(training.cat.desc, Category == "DISORDERLY CONDUCT"), Descript)
corpus.DISORDERLY.CONDUCT <- Corpus(VectorSource(train.DISORDERLY.CONDUCT$Descript))

train.DRIVING.UNDER.THE.INFLUENCE <- select(filter(training.cat.desc, Category == "DRIVING UNDER THE INFLUENCE"), Descript)
corpus.DRIVING.UNDER.THE.INFLUENCE <- Corpus(VectorSource(train.DRIVING.UNDER.THE.INFLUENCE$Descript))

train.DRUG.NARCOTIC <- select(filter(training.cat.desc, Category == "DRUG/NARCOTIC"), Descript)
corpus.DRUG.NARCOTIC <- Corpus(VectorSource(train.DRUG.NARCOTIC$Descript))

train.DRUNKENNESS <- select(filter(training.cat.desc, Category == "DRUNKENNESS"), Descript)
corpus.DRUNKENNESS <- Corpus(VectorSource(train.DRUNKENNESS$Descript))

train.EMBEZZLEMENT <- select(filter(training.cat.desc, Category == "EMBEZZLEMENT"), Descript)
corpus.EMBEZZLEMENT <- Corpus(VectorSource(train.EMBEZZLEMENT$Descript))

train.EXTORTION <- select(filter(training.cat.desc, Category == "EXTORTION"), Descript)
corpus.EXTORTION <- Corpus(VectorSource(train.EXTORTION$Descript))

train.FAMILY.OFFENSES <- select(filter(training.cat.desc, Category == "FAMILY OFFENSES"), Descript)
corpus.FAMILY.OFFENSES <- Corpus(VectorSource(train.FAMILY.OFFENSES$Descript))

train.FORGERY.COUNTERFEITING <- select(filter(training.cat.desc, Category == "FORGERY/COUNTERFEITING"), Descript)
corpus.FORGERY.COUNTERFEITING <- Corpus(VectorSource(train.FORGERY.COUNTERFEITING$Descript))

train.FRAUD <- select(filter(training.cat.desc, Category == "FRAUD"), Descript)
corpus.FRAUD <- Corpus(VectorSource(train.FRAUD$Descript))

train.GAMBLING <- select(filter(training.cat.desc, Category == "GAMBLING"), Descript)
corpus.GAMBLING <- Corpus(VectorSource(train.GAMBLING$Descript))

train.KIDNAPPING <- select(filter(training.cat.desc, Category == "KIDNAPPING"), Descript)
corpus.KIDNAPPING <- Corpus(VectorSource(train.KIDNAPPING$Descript))

train.LARCENY.THEFT <- select(filter(training.cat.desc, Category == "LARCENY/THEFT"), Descript)
corpus.LARCENY.THEFT <- Corpus(VectorSource(train.LARCENY.THEFT$Descript$Descript))

train.LIQUOR.LAWS <- select(filter(training.cat.desc, Category == "LIQUOR LAWS"), Descript)
corpus.LIQUOR.LAWS <- Corpus(VectorSource(train.LIQUOR.LAWS$Descript))

train.LOITERING <- select(filter(training.cat.desc, Category == "LOITERING"), Descript)
corpus.LOITERING <- Corpus(VectorSource(train.LOITERING$Descript))

train.MISSING.PERSON <- select(filter(training.cat.desc, Category == "MISSING PERSON"), Descript)
corpus.MISSING.PERSON <- Corpus(VectorSource(train.MISSING.PERSON$Descript))

train.NON.CRIMINAL <- select(filter(training.cat.desc, Category == "NON-CRIMINAL"), Descript)
corpus.NON.CRIMINAL <- Corpus(VectorSource(train.NON.CRIMINAL$Descript))

train.OTHER.OFFENSES <- select(filter(training.cat.desc, Category == "OTHER OFFENSES"), Descript)
corpus.OTHER.OFFENSES <- Corpus(VectorSource(train.OTHER.OFFENSES$Descript))

train.PORNOGRAPHY.OBSCENE.MAT <- select(filter(training.cat.desc, Category == "PORNOGRAPHY/OBSCENE MAT"), Descript)
corpus.PORNOGRAPHY.OBSCENE.MAT <- Corpus(VectorSource(train.PORNOGRAPHY.OBSCENE.MAT$Descript))

train.PROSTITUTION <- select(filter(training.cat.desc, Category == "PROSTITUTION"), Descript)
corpus.PROSTITUTION <- Corpus(VectorSource(train.PROSTITUTION$Descript))

train.RECOVERED.VEHICLE <- select(filter(training.cat.desc, Category == "RECOVERED VEHICLE"), Descript)
corpus.RECOVERED.VEHICLE <- Corpus(VectorSource(train.RECOVERED.VEHICLE$Descript))

train.ROBBERY <- select(filter(training.cat.desc, Category == "ROBBERY"), Descript)
corpus.ROBBERY <- Corpus(VectorSource(train.ROBBERY$Descript))

train.RUNAWAY <- select(filter(training.cat.desc, Category == "RUNAWAY"), Descript)
corpus.RUNAWAY <- Corpus(VectorSource(train.RUNAWAY$Descript))

train.SECONDARY.CODES <- select(filter(training.cat.desc, Category == "SECONDARY CODES"), Descript)
corpus.SECONDARY.CODES <- Corpus(VectorSource(train.SECONDARY.CODES$Descript))

train.SEX.OFFENSES.FORCIBLE <- select(filter(training.cat.desc, Category == "SEX OFFENSES FORCIBLE"), Descript)
corpus.SEX.OFFENSES.FORCIBLE <- Corpus(VectorSource(train.SEX.OFFENSES.FORCIBLE$Descript))

train.SEX.OFFENSES.NON.FORCIBLE <- select(filter(training.cat.desc, Category == "SEX OFFENSES NON FORCIBLE"), Descript)
corpus.SEX.OFFENSES.NON.FORCIBLE <- Corpus(VectorSource(train.SEX.OFFENSES.NON.FORCIBLE$Descript))

train.STOLEN.PROPERTY <- select(filter(training.cat.desc, Category == "STOLEN PROPERTY"), Descript)
corpus.STOLEN.PROPERTY <- Corpus(VectorSource(train.STOLEN.PROPERTY$Descript))

train.SUICIDE <- select(filter(training.cat.desc, Category == "SUICIDE"), Descript)
corpus.SUICIDE <- Corpus(VectorSource(train.SUICIDE$Descript))

train.SUSPICIOUS.OCC <- select(filter(training.cat.desc, Category == "SUSPICIOUS OCC"), Descript)
corpus.SUSPICIOUS.OCC <- Corpus(VectorSource(train.SUSPICIOUS.OCC$Descript))

train.TREA <- select(filter(training.cat.desc, Category == "TREA"), Descript)
corpus.TREA <- Corpus(VectorSource(train.TREA$Descript))

train.TRESPASS <- select(filter(training.cat.desc, Category == "TRESPASS"), Descript)
corpus.TRESPASS <- Corpus(VectorSource(train.TRESPASS$Descript))

train.VANDALISM <- select(filter(training.cat.desc, Category == "VANDALISM"), Descript)
corpus.VANDALISM <- Corpus(VectorSource(train.VANDALISM$Descript))

train.VEHICLE.THEFT <- select(filter(training.cat.desc, Category == "VEHICLE THEFT"), Descript)
corpus.VEHICLE.THEFT <- Corpus(VectorSource(train.VEHICLE.THEFT$Descript))

train.WARRANTS <- select(filter(training.cat.desc, Category == "WARRANTS"), Descript)
corpus.WARRANTS <- Corpus(VectorSource(train.WARRANTS$Descript))

train.WEAPON.LAWS <- select(filter(training.cat.desc, Category == "WEAPON LAWS"), Descript)
corpus.WEAPON.LAWS <- Corpus(VectorSource(train.WEAPON.LAWS$Descript))


##
##lapply(mget(ls(pattern = "^train\\.")), function(x) x <- x[1] * 2)

##
# lowfreq A numeric for the lower frequency bound.
# highfreq A numeric for the upper frequency bound.
# A character vector of terms in x which occur more or equal often 
# than lowfreq times and less or equal often than highfreq times.
##TD-IDF, short for term frequency–inverse document frequency, is a 
##numerical statistic that is intended to reflect how important a word is to
##a document in a collection or corpus. The tf-idf value increases 
## proportionally to the number of times a word appears in the document, 
##but is offset by the frequency of the word in the corpus, which helps to adjust 
##for the fact that some words appear more frequently in general

corpus.arson.copy <- corpus.arson # store a copy to be used in stem complete
corpus.arson	<-	tm_map(corpus.arson,	stripWhitespace)
corpus.arson	<-	tm_map(corpus.arson,	content_transformer(removePunctuation))
corpus.arson	<-	tm_map(corpus.arson,	content_transformer(tolower))
corpus.arson	<-	tm_map(corpus.arson,	stemDocument)
corpus.arson	<-	tm_map(corpus.arson,	removeWords, stopwords("english"))
#corpus.arson	<-	tm_map(corpus.arson,stemCompletion, dictionary = corpus.arson.copy)

dtm.arson <- DocumentTermMatrix(corpus.arson, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
## Most frequent. Find those terms that occur at least x times,
most.frequent.words <- findFreqTerms(dtm.arson, lowfreq = 100) ## occur >= 10000
## occur more or equal often than lowfreq times and less or equal often than highfreq
med.freq.words <- findFreqTerms(dtm.arson, lowfreq = 50, highfreq = 100) ##  10 <= occur < 100
## less frequent. Find those terms that occur maximum x times,
less.freq.words <- findFreqTerms(dtm.arson, highfreq = 50) ## occur < 100

##############
#corpus.LARCENY.THEFT.copy <- corpus.LARCENY.THEFT # store a copy to be used in stem complete

corpus.LARCENY.THEFT	<-	tm_map(corpus.LARCENY.THEFT,	stripWhitespace)
corpus.LARCENY.THEFT	<-	tm_map(corpus.LARCENY.THEFT,	content_transformer(removePunctuation))
corpus.LARCENY.THEFT	<-	tm_map(corpus.LARCENY.THEFT,	content_transformer(tolower))
corpus.LARCENY.THEFT	<-	tm_map(corpus.LARCENY.THEFT,	stemDocument)
corpus.LARCENY.THEFT	<-	tm_map(corpus.LARCENY.THEFT,	removeWords, stopwords("english"))
#corpus.LARCENY.THEFT	<-	tm_map(corpus.arson,stemCompletion, dictionary = corpus.LARCENY.THEFT.copy)

dtm.LARCENY.THEFT <- DocumentTermMatrix(corpus.LARCENY.THEFT, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
## Most frequent. Find those terms that occur at least x times,
most.frequent.words <- findFreqTerms(dtm.LARCENY.THEFT, lowfreq = 100) ## occur >= 10000
## occur more or equal often than lowfreq times and less or equal often than highfreq
med.freq.words <- findFreqTerms(dtm.LARCENY.THEFT, lowfreq = 50, highfreq = 100) ##  10 <= occur < 100
## less frequent. Find those terms that occur maximum x times,
less.freq.words <- findFreqTerms(dtm.LARCENY.THEFT, highfreq = 50) ## occur < 100

#######




#data= read.csv('comments.csv', stringsAsFactors = FALSE)
#corpus = Corpus(VectorSource(training$Descript))

# Pre-process data
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

saveRDS(corpus, file = "corpus.rds")
#corpus <- readRDS(file = "corpus.rds")

# Create matrix
dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))

saveRDS(dtm, file = "dtm.rds")
# dtm <- readRDS(file = "dtm.rds")
# Remove sparse terms
#dtm <- removeSparseTerms(dtm, 0.96)
inspect(dtm)

## Most frequent. Find those terms that occur at least x times,
most.frequent.words <- findFreqTerms(dtm, lowfreq = 30000) ## occur >= 10000
## occur more or equal often than lowfreq times and less or equal often than highfreq
med.freq.words <- findFreqTerms(dtm, lowfreq = 10, highfreq = 30000) ##  10 <= occur < 100
## less frequent. Find those terms that occur maximum x times,
less.freq.words <- findFreqTerms(dtm, highfreq = 10) ## occur < 100

## Frequent words and count#
termFreq <- colSums(as.matrix(dtm))

dtm.matrix <- as.matrix(dtm)



# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))
## Or import from an already prepared dataframe


# Add in the outcome variable
labeledTerms$IsImp = data$IsImp 

