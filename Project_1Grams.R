source("./ProcessRawData.R")

s2gramTableSmall <- s2gramTable[1:20,]

twitter1grams <- ngram(sTwitterDataProcessed, 1)
blogs1grams <- ngram(sBlogsDataProcessed, 1)
news1grams <- ngram(sNewsDataProcessed, 1)

s1gramTableTwitter <- get.phrasetable(twitter1grams)
s1gramTableBlogs <- get.phrasetable(blogs1grams)
s1gramTableNews <- get.phrasetable(news1grams)

save(s1gramTableTwitter,file="s1gramTableTwitter10pct.RData")
save(s1gramTableBlogs,file="s1gramTableBlogs10pct.RData")
save(s1gramTableNews,file="s1gramTableNews10pct.RData")
load("s1gramTableTwitter.RData")
load("s1gramTableBlogs.RData")
load("s1gramTableNews.RData")

dftwitter1grams <- as.data.frame(s1gramTableTwitter)
dftwitter1grams$numericfreq <- as.numeric(dftwitter1grams$freq)
dfblogs1grams <- as.data.frame(s1gramTableBlogs)
dfblogs1grams$numericfreq <- as.numeric(dfblogs1grams$freq)
dfnews1grams <- as.data.frame(s1gramTableNews)
dfnews1grams$numericfreq <- as.numeric(dfnews1grams$freq)

save(dftwitter1grams,file="dftwitter1grams10pct.RData")
save(dfblogs1grams,file="dfblogs1grams10pct.RData")
save(dfnews1grams,file="dfnews1grams10pct.RData")
load("dftwitter1grams.RData")
load("dfblogs1grams.RData")
load("dfnews1grams.RData")

grep("thanks for ",dftwitter1grams,perl=TRUE)

# This splits First then Second_Third word
dfSplit1GramsTwitter <- sqldf("SELECT RTRIM(ngrams) First
                       ,numericfreq 
                       FROM dftwitter1grams")

dfProb1GramsTwitter_1 <- sqldf("SELECT First Predictor
                             ,First Predicted
                             ,numericfreq freqPredictor
                             ,numericfreq freqPredicted
                             FROM dfSplit1GramsTwitter
                             ")
###
# This splits First then Second_Third word
dfSplit1GramsNews <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                            ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                            ,numericfreq 
                            FROM dfnews1grams")
dfProb1GramsNews <- sqldf("SELECT sp.First
                           ,SUM(sp.numericfreq) freqPredictor
                           FROM dfSplit1GramsNews sp
                           GROUP BY sp.First
                           ")
dfProb1GramsNews_2 <- sqldf("SELECT sp.First Predictor
                             ,sp.Second Predicted
                             ,sm.freqPredictor
                             ,sp.numericfreq freqPredicted
                             FROM dfSplit1GramsNews sp
                             INNER JOIN dfProb1GramsNews sm
                             ON sp.First = sm.First
                             ")
###
# This splits First then Second_Third word
dfSplit1GramsBlogs <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                           ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                           ,numericfreq 
                           FROM dfblogs1grams")
dfProb1GramsBlogs <- sqldf("SELECT sp.First
                      ,SUM(sp.numericfreq) freqPredictor
                             FROM dfSplit1GramsBlogs sp
                             GROUP BY sp.First
                             ")
dfProb1GramsBlogs_2 <- sqldf("SELECT sp.First Predictor
                               ,sp.Second Predicted
                               ,sm.freqPredictor
                               ,sp.numericfreq freqPredicted
                               FROM dfSplit1GramsBlogs sp
                               INNER JOIN dfProb1GramsBlogs sm
                               ON sp.First = sm.First
                               ")

save(dfProb1GramsTwitter_1,file="dfProb1GramsTwitter_1_10pct.RData")
save(dfProb1GramsNews_2,file="dfProb1GramsNews_2_10pct.RData")
save(dfProb1GramsBlogs_2,file="dfProb1GramsBlogs_2_10pct.RData")
load("dfProb1GramsTwitter_2.RData")
load("dfProb1GramsNews_2.RData")
load("dfProb1GramsBlogs_2.RData")

sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
      FROM dfProb1GramsTwitter_2
      WHERE Predictor='thanks for' AND Predicted='the'")

dfAllProb1Grams <- sqldf("SELECT Predictor, Predicted
                            ,SUM(freqPredictor) freqPredictor
                            ,SUM(freqPredicted) freqPredicted
                          FROM
                                  (SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb1GramsTwitter_2
                                  UNION ALL
                                  SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb1GramsBlogs_2
                                  UNION ALL
                                  SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb1GramsNews_2)
                           GROUP BY Predictor, Predicted
                         ")

dfAllProb1Grams <- sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
                          FROM dfAllProb1Grams 
                          WHERE freqPredicted > 5
                          ORDER BY Predictor, Predicted, freqPredicted")

save(dfAllProb1Grams, file="dfAllProb1Grams.Rdata")
load("dfAllProb1Grams.Rdata")

sqldf("SELECT First, Second, freq, totfreq
       FROM dfAllProb1Grams
      WHERE First='this'
      AND freq > 150
      ORDER BY freq")

predictor = "thanks for"
sqlStmt = paste("SELECT Predicted, freqPredicted FROM dfAllProb1Grams WHERE Predictor='",predictor,"' ","ORDER BY freqPredicted DESC",sep="")
results <- sqldf(sqlStmt)

grep("thanks for",dfProb1GramsBlogs_2,perl=TRUE)

