source("./ProcessRawData.R")

s2gramTableSmall <- s2gramTable[1:20,]

twitter2grams <- ngram(sTwitterDataProcessed, 2)
blogs2grams <- ngram(sBlogsDataProcessed, 2)
news2grams <- ngram(sNewsDataProcessed, 2)

s2gramTableTwitter <- get.phrasetable(twitter2grams)
s2gramTableBlogs <- get.phrasetable(blogs2grams)
s2gramTableNews <- get.phrasetable(news2grams)

save(s2gramTableTwitter,file="s2gramTableTwitter.RData")
save(s2gramTableBlogs,file="s2gramTableBlogs.RData")
save(s2gramTableNews,file="s2gramTableNews.RData")
load("s2gramTableTwitter.RData")
load("s2gramTableBlogs.RData")
load("s2gramTableNews.RData")

dftwitter2grams <- as.data.frame(s2gramTableTwitter)
dftwitter2grams$numericfreq <- as.numeric(dftwitter2grams$freq)
dfblogs2grams <- as.data.frame(s2gramTableBlogs)
dfblogs2grams$numericfreq <- as.numeric(dfblogs2grams$freq)
dfnews2grams <- as.data.frame(s2gramTableNews)
dfnews2grams$numericfreq <- as.numeric(dfnews2grams$freq)

save(dftwitter2grams,file="dftwitter2grams.RData")
save(dfblogs2grams,file="dfblogs2grams.RData")
save(dfnews2grams,file="dfnews2grams.RData")
load("dftwitter2grams.RData")
load("dfblogs2grams.RData")
load("dfnews2grams.RData")

grep("thanks for ",dftwitter2grams,perl=TRUE)

# This splits First then Second_Third word
dfSplit2GramsTwitter <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                        ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                       ,numericfreq 
                       FROM dftwitter2grams")

# Put first, second together, separate third. Need freq of all 2 (freqPost) and freq of first two (freqPre)
dfSplit2GramsTwitter_1 <- sqldf("SELECT First 
                        ,Second
                        ,numericfreq freqPost
                         FROM dfSplit2GramsTwitter")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfProb2GramsTwitter <- sqldf("SELECT sp.First
                      ,SUM(sp.freqPost) freqPredictor
                      FROM dfSplit2GramsTwitter_1 sp
                      GROUP BY sp.First
                      ")
dfProb2GramsTwitter_2 <- sqldf("SELECT sp.First Predictor
                             ,sp.Second Predicted
                             ,sm.freqPredictor
                             ,sp.freqPost freqPredicted
                             FROM dfSplit2GramsTwitter_1 sp
                             INNER JOIN dfProb2GramsTwitter sm
                             ON sp.First = sm.First
                             ")
###
# This splits First then Second_Third word
dfSplit2GramsNews <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                            ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                            ,numericfreq 
                            FROM dfnews2grams")
dfProb2GramsNews <- sqldf("SELECT sp.First
                           ,SUM(sp.numericfreq) freqPredictor
                           FROM dfSplit2GramsNews sp
                           GROUP BY sp.First
                           ")
dfProb2GramsNews_2 <- sqldf("SELECT sp.First Predictor
                             ,sp.Second Predicted
                             ,sm.freqPredictor
                             ,sp.numericfreq freqPredicted
                             FROM dfSplit2GramsNews sp
                             INNER JOIN dfProb2GramsNews sm
                             ON sp.First = sm.First
                             ")
###
# This splits First then Second_Third word
dfSplit2GramsBlogs <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                           ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                           ,numericfreq 
                           FROM dfblogs2grams")
dfProb2GramsBlogs <- sqldf("SELECT sp.First
                      ,SUM(sp.numericfreq) freqPredictor
                             FROM dfSplit2GramsBlogs sp
                             GROUP BY sp.First
                             ")
dfProb2GramsBlogs_2 <- sqldf("SELECT sp.First Predictor
                               ,sp.Second Predicted
                               ,sm.freqPredictor
                               ,sp.numericfreq freqPredicted
                               FROM dfSplit2GramsBlogs sp
                               INNER JOIN dfProb2GramsBlogs sm
                               ON sp.First = sm.First
                               ")

save(dfProb2GramsTwitter_2,file="dfProb2GramsTwitter_2.RData")
save(dfProb2GramsNews_2,file="dfProb2GramsNews_2.RData")
save(dfProb2GramsBlogs_2,file="dfProb2GramsBlogs_2.RData")
load("dfProb2GramsTwitter_2.RData")
load("dfProb2GramsNews_2.RData")
load("dfProb2GramsBlogs_2.RData")

sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
      FROM dfProb2GramsTwitter_2
      WHERE Predictor='thanks for' AND Predicted='the'")

dfAllProb2Grams <- sqldf("SELECT Predictor, Predicted
                            ,SUM(freqPredictor) freqPredictor
                            ,SUM(freqPredicted) freqPredicted
                          FROM
                                  (SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb2GramsTwitter_2
                                  UNION ALL
                                  SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb2GramsBlogs_2
                                  UNION ALL
                                  SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb2GramsNews_2)
                           GROUP BY Predictor, Predicted
                         ")

dfAllProb2Grams <- sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
                          FROM dfAllProb2Grams 
                          WHERE freqPredicted > 5
                          ORDER BY Predictor, Predicted, freqPredicted")

save(dfAllProb2Grams, file="dfAllProb2Grams.Rdata")
load("dfAllProb2Grams.Rdata")

sqldf("SELECT First, Second, freq, totfreq
       FROM dfAllProb2Grams
      WHERE First='this'
      AND freq > 150
      ORDER BY freq")

predictor = "thanks for"
sqlStmt = paste("SELECT Predicted, freqPredicted FROM dfAllProb2Grams WHERE Predictor='",predictor,"' ","ORDER BY freqPredicted DESC",sep="")
results <- sqldf(sqlStmt)

grep("thanks for",dfProb2GramsBlogs_2,perl=TRUE)

