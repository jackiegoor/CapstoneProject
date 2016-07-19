source("./ProcessRawData.R")

s4gramTableSmall <- s4gramTable[1:20,]

twitter4grams <- ngram(sTwitterDataProcessed, 4)
blogs4grams <- ngram(sBlogsDataProcessed, 4)
news4grams <- ngram(sNewsDataProcessed, 4)

s4gramTableTwitter <- get.phrasetable(twitter4grams)
s4gramTableBlogs <- get.phrasetable(blogs4grams)
s4gramTableNews <- get.phrasetable(news4grams)

save(s4gramTableTwitter,file="s4gramTableTwitter.RData")
save(s4gramTableBlogs,file="s4gramTableBlogs.RData")
save(s4gramTableNews,file="s4gramTableNews.RData")
load("s4gramTableTwitter.RData")
load("s4gramTableBlogs.RData")
load("s4gramTableNews.RData")

dftwitter4grams <- as.data.frame(s4gramTableTwitter)
dftwitter4grams$numericfreq <- as.numeric(dftwitter4grams$freq)
dfblogs4grams <- as.data.frame(s4gramTableBlogs)
dfblogs4grams$numericfreq <- as.numeric(dfblogs4grams$freq)
dfnews4grams <- as.data.frame(s4gramTableNews)
dfnews4grams$numericfreq <- as.numeric(dfnews4grams$freq)

save(dftwitter4grams,file="dftwitter4grams.RData")
save(dfblogs4grams,file="dfblogs4grams.RData")
save(dfnews4grams,file="dfnews4grams.RData")
load("dftwitter4grams.RData")
load("dfblogs4grams.RData")
load("dfnews4grams.RData")

sqldf("SELECT ngrams, freq
      FROM s4gramTableTwitter
      WHERE ngrams='a a a a '")

# This splits First then Second_Third word
dfSplit4GramsTwitter <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                        ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                       ,numericfreq 
                       FROM dftwitter4grams")
# Splits first, second, third
dfSplit4GramsTwitter <- sqldf("SELECT First
                        ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                       ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                       ,numericfreq
                       FROM dfSplit4GramsTwitter")
dfSplit4GramsTwitter_1 <- sqldf("SELECT First
                              ,Second
                              ,RTRIM(SUBSTR(Third,1,CHARINDEX(' ',Third))) Third
                              ,RTRIM(LTRIM(SUBSTR(Third,CHARINDEX(' ',Third),LENGTH(Third)))) Fourth
                              ,numericfreq
                              FROM dfSplit4GramsTwitter")
# Put first, second, third together, separate fourth Need freq of all 4 (freqPost) and freq of first two (freqPre)
dfSplit4GramsTwitter_1 <- sqldf("SELECT (First || ' ' || Second || ' ' || Third) First
                        ,Fourth Second
                        ,numericfreq
                         FROM dfSplit4GramsTwitter_1")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfSplit4GramsTwitter_2 <- sqldf("SELECT sp.First
                      ,SUM(sp.numericfreq) freqPredictor
                      FROM dfSplit4GramsTwitter_1 sp
                      GROUP BY sp.First
                      ")
dfSplit4GramsTwitter_3 <- sqldf("SELECT sp.First Predictor
                             ,sm.Second Predicted
                             ,sp.freqPredictor
                             ,sm.numericfreq freqPredicted
                             FROM dfSplit4GramsTwitter_2 sp
                             INNER JOIN dfSplit4GramsTwitter_1 sm
                             ON sp.First = sm.First
                             ")
###
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
      FROM dfSplit4GramsTwitter_3
      WHERE Predictor='thanks for the'
      ORDER BY freqPredicted")

predictor = "thanks for"
sqlStmt = paste("SELECT Predicted, freqPredicted FROM dfAllProb4Grams WHERE Predictor='",predictor,"' ","ORDER BY freqPredicted DESC",sep="")
results <- sqldf(sqlStmt)

grep("thanks for",dfProb4GramsBlogs_2,perl=TRUE)

######################
# This splits First then Second_Third word
dfSplit4GramsBlogs <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                              ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                              ,numericfreq 
                              FROM dfblogs4grams")
# Splits first, second, third
dfSplit4GramsBlogs <- sqldf("SELECT First
                              ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                              ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                              ,numericfreq
                              FROM dfSplit4GramsBlogs")
dfSplit4GramsBlogs_1 <- sqldf("SELECT First
                                ,Second
                                ,RTRIM(SUBSTR(Third,1,CHARINDEX(' ',Third))) Third
                                ,RTRIM(LTRIM(SUBSTR(Third,CHARINDEX(' ',Third),LENGTH(Third)))) Fourth
                                ,numericfreq
                                FROM dfSplit4GramsBlogs")
# Put first, second, third together, separate fourth Need freq of all 4 (freqPost) and freq of first two (freqPre)
dfSplit4GramsBlogs_1 <- sqldf("SELECT (First || ' ' || Second || ' ' || Third) First
                                ,Fourth Second
                                ,numericfreq
                                FROM dfSplit4GramsBlogs_1")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfSplit4GramsBlogs_2 <- sqldf("SELECT sp.First
                                ,SUM(sp.numericfreq) freqPredictor
                                FROM dfSplit4GramsBlogs_1 sp
                                GROUP BY sp.First
                                ")
dfSplit4GramsBlogs_3 <- sqldf("SELECT sp.First Predictor
                                ,sm.Second Predicted
                                ,sp.freqPredictor
                                ,sm.numericfreq freqPredicted
                                FROM dfSplit4GramsBlogs_2 sp
                                INNER JOIN dfSplit4GramsBlogs_1 sm
                                ON sp.First = sm.First
                                ")
######################
# This splits First then Second_Third word
dfSplit4GramsNews <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                            ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                            ,numericfreq 
                            FROM dfnews4grams")
# Splits first, second, third
dfSplit4GramsNews <- sqldf("SELECT First
                            ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                            ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                            ,numericfreq
                            FROM dfSplit4GramsNews")
dfSplit4GramsNews_1 <- sqldf("SELECT First
                              ,Second
                              ,RTRIM(SUBSTR(Third,1,CHARINDEX(' ',Third))) Third
                              ,RTRIM(LTRIM(SUBSTR(Third,CHARINDEX(' ',Third),LENGTH(Third)))) Fourth
                              ,numericfreq
                              FROM dfSplit4GramsNews")
# Put first, second, third together, separate fourth Need freq of all 4 (freqPost) and freq of first two (freqPre)
dfSplit4GramsNews_1 <- sqldf("SELECT (First || ' ' || Second || ' ' || Third) First
                              ,Fourth Second
                              ,numericfreq
                              FROM dfSplit4GramsNews_1")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfSplit4GramsNews_2 <- sqldf("SELECT sp.First
                              ,SUM(sp.numericfreq) freqPredictor
                              FROM dfSplit4GramsNews_1 sp
                              GROUP BY sp.First
                              ")
dfSplit4GramsNews_3 <- sqldf("SELECT sp.First Predictor
                              ,sm.Second Predicted
                              ,sp.freqPredictor
                              ,sm.numericfreq freqPredicted
                              FROM dfSplit4GramsNews_2 sp
                              INNER JOIN dfSplit4GramsNews_1 sm
                              ON sp.First = sm.First
                              ")
save(dfSplit4GramsTwitter_3,file="dfSplit4GramsTwitter_3.RData")
save(dfSplit4GramsBlogs_3,file="dfSplit4GramsBlogs_3.RData")
save(dfSplit4GramsNews_3,file="dfSplit4GramsNews_3.RData")
load("dfSplit4GramsTwitter_3.RData")
load("dfSplit4GramsBlogs_3.RData")
load("dfSplit4GramsNews_3.RData")

dfAllProb4Grams <- sqldf("SELECT Predictor, Predicted
                            ,SUM(freqPredictor) freqPredictor
                         ,SUM(freqPredicted) freqPredicted
                         FROM
                         (SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit4GramsTwitter_3
                         UNION ALL
                         SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit4GramsBlogs_3
                         UNION ALL
                         SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit4GramsNews_3)
                         GROUP BY Predictor, Predicted
                         ")


save(dfAllProb4Grams, file="dfAllProb4Grams.Rdata")
load("dfAllProb4Grams.Rdata")

sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfSplit4GramsTwitter_3
      WHERE Predictor='arctic monkeys this'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfSplit4GramsBlogs_3
      WHERE Predictor='arctic monkeys this'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfSplit4GramsNews_3
      WHERE Predictor='arctic monkeys this'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfAllProb4Grams
      WHERE Predictor='arctic monkeys this'")

