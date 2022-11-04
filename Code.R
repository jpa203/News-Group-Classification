library(tm)
library(class)

# Loading Files

red.autos <- system.file("texts", "20Newsgroups", "20news-bydate-test", "rec.autos", package = 'tm')
sci.space <- system.file("texts","20Newsgroups", "20news-bydate-test", "sci.space", package = 'tm')


Temp1 <- DirSource(system.file("texts", "20Newsgroups", "20news-bydate-train", "sci.space", package = 'tm'))
Temp2 <- DirSource(system.file("texts", "20Newsgroups", "20news-bydate-test", "sci.space", package = 'tm'))
Temp3 <- DirSource(system.file("texts", "20Newsgroups", "20news-bydate-train", "rec.autos", package = 'tm'))
Temp4 <- DirSource(system.file("texts", "20Newsgroups", "20news-bydate-test", "rec.autos", package = 'tm'))

# Transferring to Corpus

Doc1.Train <- Corpus(URISource(Temp1$filelist[1:100]),readerControl=list(reader=readPlain))
Doc1.Test <- Corpus(URISource(Temp2$filelist[1:100]),readerControl=list(reader=readPlain))
Doc2.Train <- Corpus(URISource(Temp3$filelist[1:100]),readerControl=list(reader=readPlain))
Doc2.Test <- Corpus(URISource(Temp4$filelist[1:100]),readerControl=list(reader=readPlain))


corpus <- c(Doc1.Train, Doc1.Test, Doc2.Train, Doc2.Test)

# Pre Processing 

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
transform.chr <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, transform.chr, "@|#")
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Creating Document-Term Matrix

dtm <- as.matrix(DocumentTermMatrix(corpus, control=list(wordLengths=c(2,Inf), bounds=list(global=c(5,Inf)))))



# kNN

train <- dtm[c(201:300, 1:100), ]
test <- dtm[c(301:400, 101:200), ]
tags <- factor(c(rep("Positive",100), rep("Negative",100))) # Tags - Correct answers for the training dataset - I think this needs to be sci/rec
tags <- factor(tags, levels = c('Positive', 'Negative'))

set.seed(0)

prob.test<- knn(train, test, tags, k = 2, prob=TRUE) # k-number of neighbors considered

a <- 1:length(prob.test)

b <- levels(prob.test)[prob.test]

c <- attributes(prob.test)$prob

d <- tags

result <- data.frame(Doc = a, Predict = b, Prob = c, Correct = (b == d))

result

# correct tags

sum(prob.test==tags)/length(tags)

# TP, FN, FP, TN

RecClassified <- (prob.test == tags)[1:100]

TP <- sum(RecClassified == 'TRUE')

FN <- sum(RecClassified == 'FALSE')

SciClassified <- (prob.test == tags)[101:200]

FP <- sum(SciClassified == 'TRUE')

TN <- sum(SciClassified == 'FALSE')

# Precision 

P <- TP / (TP + FP)

# Recall

R <- TP / (TP + FN)

# confusion matrix

CM <- data.frame(Rec = c(TP, FN), Sci = c(FP,TN), row.names = c('Rec', 'Sci'))

# F Score

fscore <- 2 * (P*R) / (P+R)
