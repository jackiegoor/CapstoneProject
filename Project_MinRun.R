r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)

packages <- c("rJava","tm","NLP","openNLP","ngram", "ggplot2", "stringr", "markovchain", "sqldf") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(rJava,quietly=TRUE,warn.conflicts=FALSE)
library(tm,quietly=TRUE,warn.conflicts=FALSE)
library(NLP,quietly=TRUE,warn.conflicts=FALSE)
library(openNLP,quietly=TRUE,warn.conflicts=FALSE)
##library(wordnet,quietly=TRUE,warn.conflicts=FALSE)
library(ngram,quietly=TRUE,warn.conflicts=FALSE)
library(ggplot2,quietly=TRUE,warn.conflicts=FALSE)
library(stringr,quietly=TRUE,warn.conflicts=FALSE)
library(markovchain,quietly=TRUE,warn.conflicts=FALSE)
library(sqldf,quietly=TRUE,warn.conflicts=FALSE)

##filePath = "\\\\sherlock.usa.tribune.com\\qlikviewdocs$\\Temp\\CapstoneProject\\"
filePath = "E:/Coursera/"
#dir.create('E:/Coursera', showWarnings = FALSE)
#setwd('E:/Coursera')
dir.create(filePath, showWarnings = FALSE)
setwd(filePath)

if(!file.exists(paste(filePath,"Coursera-SwiftKey.zip"))) {
        download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',paste(filePath,"Coursera-SwiftKey.zip"))
        unzip(paste(filePath,"Coursera-SwiftKey.zip"),exdir='.')
}

stats.twitter <- as.numeric(shell(paste("wc ", filePath, "final/en_US/en_US.twitter.txt | gawk '{print $1; print $2; print $3}'", sep=""), ignore.stderr=TRUE, intern=TRUE))
twitterNumLines10Pct <- stats.twitter[1] * 0.10
twitterNumLines3Pct <- stats.twitter[1]  * 0.03
twitterNumLines1Pct <- stats.twitter[1]  * 0.01
con <- file(paste(filePath, "final\\en_US\\en_US.twitter.txt", sep=""), "r")
twitterData <- readLines(con, warn=FALSE, n=twitterNumLines3Pct)
close(con) 

sTwitterData <- toString(twitterData)
sTwitterDataProcessed <- preprocess(sTwitterData, case = "lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
sTwitterDataProcessed <- str_replace_all(sTwitterDataProcessed, "[^[:alpha:]\']", " ")
twitter1grams <- ngram(sTwitterDataProcessed, 1)
twitter2grams <- ngram(sTwitterDataProcessed, 2)
twitter3grams <- ngram(sTwitterDataProcessed, 3)
s1gramTable <- get.phrasetable(twitter1grams)
s1gramTableSmall <- s1gramTable[1:20,]
s2gramTable <- get.phrasetable(twitter2grams)
s2gramTableSmall <- s2gramTable[1:20,]
s3gramTable <- get.phrasetable(twitter3grams)
s3gramTableSmall <- s3gramTable[1:20,]
dftwitter1grams <- as.data.frame(s1gramTableSmall)
dftwitter1grams$numericfreq <- as.numeric(dftwitter1grams$freq)
dftwitter2grams <- as.data.frame(s2gramTableSmall)
dftwitter2grams$numericfreq <- as.numeric(dftwitter2grams$freq)
dftwitter3grams <- as.data.frame(s3gramTableSmall)
dftwitter3grams$numericfreq <- as.numeric(dftwitter3grams$freq)

dfSplit2Grams <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                        ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                        ,numericfreq 
                      FROM dftwitter2grams")
dfSumFreq2Grams <- sqldf("SELECT First, SUM(numericfreq) totFreq
                        FROM dfSplit2Grams
                        GROUP BY First
                        ORDER BY First")
dfProb2Grams <- sqldf("SELECT sp.First
                        ,sp.Second
                        ,sp.numericfreq freq
                        ,sm.totFreq totfreq
                       FROM dfSplit2Grams sp
                       INNER JOIN dfSumFreq2Grams sm
                         ON sp.First = sm.First
                       ORDER BY sp.First")
# I had to trim trailing spaces so that grep wouldn't match words at the end of the line (even though I
# am using the ^ start-of-line anchor).
write.csv(str_trim(s1gramTableSmall[,1]), file="s1gramTableSmall.txt", row.names=FALSE, quote=FALSE)
write.csv(str_trim(s2gramTableSmall[,1]), file="s2gramTableSmall.txt", row.names=FALSE, quote=FALSE)
write.csv(str_trim(s1gramTable[,1]), file="s1gramTable.txt", row.names=FALSE, quote=FALSE)
write.csv(str_trim(s2gramTable[,1]), file="s2gramTable.txt", row.names=FALSE, quote=FALSE)

# For each unigram:
#   1. Need to sum the frequency of that unigram at the start of all bigrams
#   2. For each bigram that starts with that unigram, its probability will be
#      its frequency divided by 1.  
# EXAMPLE: sum of frequency of bigrams that start with "the " = 61931
#          frequency of "the cat " = 1960
#          probability in cell["the ", "cat "] = (1960 / 61931) = 0.031648125
#
#     -- Need to get individual probabilities for EACH bigram

# read all unigrams
# read all bigrams
# loop thru all unigrams
# I need to create a matrix with: --> this is the transitionMatrix
# -- Should I add smoothing? --> Not yet
#   - s1gramTableSmall[i,1] as the matrix row.names (make a list())
#   - matrix cell is probability = (bigram freq) / (unigram freq) 
#   - second word in bigram as col.names (make a list())
# initialize lists to # of rows, # of cols
# NOTE: Unigrams should include all words in bigrams - using this fact to create matrix
numRows <- length(s1gramTableSmall[,1])
numCols<- numRows
rowNames_unigrams <- vector("list", numRows)
colNames_bigram2ndword <- vector("list", numCols)
rowNames_unigrams <- s1gramTableSmall[,1]
colNames_bigram2ndword <- s1gramTableSmall[,1]
# initialize transitionMatrix (numeric sets all cells to 0)
transitionMatrix <- matrix(numeric(numRows^2),
                          byrow=TRUE,
                          nrow=numRows,
                          dimnames=list(rowNames_unigrams,
                                        colNames_bigram2ndword))

for (i in 1:length(s1gramTableSmall[,1]))
{
  print(i)
  print(s1gramTableSmall[i,1])
  print(s1gramTableSmall[i,2])
  # freq should be the count of this bigram
  # prob should be freq divided by sum of frequency of bigrams where this is the first word in the bigram
  # For example, if there are 2 bigrams that start with "in " and they are "in the " with a frequency of 27
  # and "in my " with a frequency of 13 then the probability in the transitionMatrix of "in " followed by
  # "the " should be 27/(27+13) = 0.675.
  freq <- s2gramTableSmall[]
  freq <- as.numeric(shell(paste('grep ', '"^',s1gramTableSmall[i,1], '"', ' s2gramTableSmall.txt | wc -l', sep=""), ignore.stderr=TRUE, intern=TRUE))
  #rowNames_unigrams[[i]] <- s1gramTableSmall[i,1]
  #rowNames_unigrams[[i]] <- new__col_element
  print(freq)
  transitionMatrix[s1gramTableSmall[i,1],] <- freq / s1gramTableSmall[i,2]
  print(freq/s1gramTableSmall[i,2])
  # unigram freq in s1gramTableSmall[i,2]
  #transitionMatrix[s1gramTableSmall[i,1], ] <- 
}

fileName = paste(filePath,"s1gramTableSmall.txt",sep="")
unigram = "it"
as.numeric(shell("wc -l c:\\users\\jgoor\\en_US.twitter.txt"))
setwd("C:\\Coursera\\")
shell(paste("grep '^",unigram," ' s2gramTableSmall.txt | wc -l", sep=""), ignore.stderr=TRUE, intern=TRUE)
as.numeric(shell(paste("wc -l | grep '^it'", fileName), ignore.stderr=TRUE, intern=TRUE))
paste("grep '^",unigram,"'", fileName, " | wc -l")


tMatrixExample <- load(file="E:\\Training_HowTo\\Coursera\\DataScientist\\CapstoneProject\\SampleProject1\\transitionMatrix.Rdata")




