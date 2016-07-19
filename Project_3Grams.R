source("./ProcessRawData.R")

s3gramTableSmall <- s3gramTable[1:20,]

twitter3grams <- ngram(sTwitterDataProcessed, 3)
blogs3grams <- ngram(sBlogsDataProcessed, 3)
news3grams <- ngram(sNewsDataProcessed, 3)

s3gramTableTwitter <- get.phrasetable(twitter3grams)
s3gramTableBlogs <- get.phrasetable(blogs3grams)
s3gramTableNews <- get.phrasetable(news3grams)

save(s3gramTableTwitter,file="s3gramTableTwitter.RData")
save(s3gramTableBlogs,file="s3gramTableBlogs.RData")
save(s3gramTableNews,file="s3gramTableNews.RData")
load("s3gramTableTwitter.RData")
load("s3gramTableBlogs.RData")
load("s3gramTableNews.RData")

dftwitter3grams <- as.data.frame(s3gramTableTwitter)
dftwitter3grams$numericfreq <- as.numeric(dftwitter3grams$freq)
dfblogs3grams <- as.data.frame(s3gramTableBlogs)
dfblogs3grams$numericfreq <- as.numeric(dfblogs3grams$freq)
dfnews3grams <- as.data.frame(s3gramTableNews)
dfnews3grams$numericfreq <- as.numeric(dfnews3grams$freq)

save(dftwitter3grams,file="dftwitter3grams.RData")
save(dfblogs3grams,file="dfblogs3grams.RData")
save(dfnews3grams,file="dfnews3grams.RData")
load("dftwitter3grams.RData")
load("dfblogs3grams.RData")
load("dfnews3grams.RData")

sqldf("SELECT ngrams, freq
      FROM s3gramTableTwitter
      WHERE ngrams='a a a '")

grep("thanks for ",dftwitter3grams)

# This splits First then Second_Third word
dfSplit3GramsTwitter <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                        ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                       ,numericfreq 
                       FROM dftwitter3grams")
# Splits first, second, third
dfSplit3GramsTwitter <- sqldf("SELECT First
                        ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                       ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                       ,numericfreq
                       FROM dfSplit3GramsTwitter")
# Put first, second together, separate third. Need freq of all 3 (freqPost) and freq of first two (freqPre)
dfSplit3GramsTwitter_1 <- sqldf("SELECT (First || ' ' || Second) AS First
                        ,Third AS Second
                        ,numericfreq AS freqPost
                         FROM dfSplit3GramsTwitter")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfProb3GramsTwitter <- sqldf("SELECT sp.First
                      ,SUM(sp.freqPost) AS freqPredictor
                      FROM dfSplit3GramsTwitter_1 sp
                      GROUP BY sp.First
                      ")
dfProb3GramsTwitter_2 <- sqldf("SELECT sp.First AS Predictor
                             ,sp.Second AS Predicted
                             ,sm.freqPredictor
                             ,sp.freqPost AS freqPredicted
                             FROM dfSplit3GramsTwitter_1 sp
                             INNER JOIN dfProb3GramsTwitter sm
                             ON sp.First = sm.First
                             ")
###
# This splits First then Second_Third word
dfSplit3GramsNews <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                              ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                              ,numericfreq 
                              FROM dfnews3grams")
# Splits first, second, third
dfSplit3GramsNews <- sqldf("SELECT First
                              ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                              ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                              ,numericfreq
                              FROM dfSplit3GramsNews")
# Put first, second together, separate third. Need freq of all 3 (freqPost) and freq of first two (freqPre)
dfSplit3GramsNews_1 <- sqldf("SELECT (First || ' ' || Second) AS First
                                ,Third AS Second
                                ,numericfreq AS freqPost
                                FROM dfSplit3GramsNews")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfProb3GramsNews <- sqldf("SELECT sp.First
                             ,SUM(sp.freqPost) AS freqPredictor
                             FROM dfSplit3GramsNews_1 sp
                             GROUP BY sp.First
                             ")
dfProb3GramsNews_2 <- sqldf("SELECT sp.First AS Predictor
                               ,sp.Second AS Predicted
                               ,sm.freqPredictor
                               ,sp.freqPost AS freqPredicted
                               FROM dfSplit3GramsNews_1 sp
                               INNER JOIN dfProb3GramsNews sm
                               ON sp.First = sm.First
                               ")
###
# This splits First then Second_Third word
dfSplit3GramsBlogs <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                           ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                           ,numericfreq 
                           FROM dfblogs3grams")
# Splits first, second, third
dfSplit3GramsBlogs <- sqldf("SELECT First
                           ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                           ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                           ,numericfreq
                           FROM dfSplit3GramsBlogs")
# Put first, second together, separate third. Need freq of all 3 (freqPost) and freq of first two (freqPre)
dfSplit3GramsBlogs_1 <- sqldf("SELECT (First || ' ' || Second) AS First
                             ,Third AS Second
                             ,numericfreq AS freqPost
                             FROM dfSplit3GramsBlogs")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfProb3GramsBlogs <- sqldf("SELECT sp.First
                          ,SUM(sp.freqPost) AS freqPredictor
                          FROM dfSplit3GramsBlogs_1 sp
                          GROUP BY sp.First
                          ")
dfProb3GramsBlogs_2 <- sqldf("SELECT sp.First AS Predictor
                            ,sp.Second AS Predicted
                            ,sm.freqPredictor
                            ,sp.freqPost AS freqPredicted
                            FROM dfSplit3GramsBlogs_1 sp
                            INNER JOIN dfProb3GramsBlogs sm
                            ON sp.First = sm.First
                            ")

save(dfProb3GramsTwitter_2,file="dfProb3GramsTwitter_2.RData")
save(dfProb3GramsNews_2,file="dfProb3GramsNews_2.RData")
save(dfProb3GramsBlogs_2,file="dfProb3GramsBlogs_2.RData")
load("dfProb3GramsTwitter_2.RData")
load("dfProb3GramsNews_2.RData")
load("dfProb3GramsBlogs_2.RData")

sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
      FROM dfProb3GramsTwitter_2
      WHERE Predictor='a a' AND Predicted='a'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
      FROM dfProb3GramsNews_2
      WHERE Predictor='a a' AND Predicted='a'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfProb3GramsBlogs_2
      WHERE Predictor='a a' AND Predicted='a'")


dfAllProb3Grams <- sqldf("SELECT Predictor, Predicted
                            ,SUM(freqPredictor) freqPredictor
                            ,SUM(freqPredicted) freqPredicted
                          FROM
                                  (SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb3GramsTwitter_2
                                  UNION ALL
                                  SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb3GramsBlogs_2
                                  UNION ALL
                                  SELECT Predictor, Predicted, freqPredictor, freqPredicted
                                  FROM dfProb3GramsNews_2)
                           GROUP BY Predictor, Predicted
                         ")


save(dfAllProb3Grams, file="dfAllProb3Grams.Rdata")
load("dfAllProb3Grams.Rdata")

sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfAllProb3Grams
      WHERE Predictor='monkeys this'")

predictor = "thanks for"
sqlStmt = paste("SELECT Predicted, freqPredicted FROM dfAllProb3Grams WHERE Predictor='",predictor,"' ","ORDER BY freqPredicted DESC",sep="")
results <- sqldf(sqlStmt)

grep("thanks for",dfProb3GramsBlogs_2,perl=TRUE)

