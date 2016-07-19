source("./ProcessRawData.R")

s5gramTableSmall <- s5gramTable[1:20,]

twitter5grams <- ngram(sTwitterDataProcessed, 5)
blogs5grams <- ngram(sBlogsDataProcessed, 5)
news5grams <- ngram(sNewsDataProcessed, 5)

s5gramTableTwitter <- get.phrasetable(twitter5grams)
s5gramTableBlogs <- get.phrasetable(blogs5grams)
s5gramTableNews <- get.phrasetable(news5grams)

save(s5gramTableTwitter,file="s5gramTableTwitter.RData")
save(s5gramTableBlogs,file="s5gramTableBlogs.RData")
save(s5gramTableNews,file="s5gramTableNews.RData")
load("s5gramTableTwitter.RData")
load("s5gramTableBlogs.RData")
load("s5gramTableNews.RData")

dftwitter5grams <- as.data.frame(s5gramTableTwitter)
dftwitter5grams$numericfreq <- as.numeric(dftwitter5grams$freq)
dfblogs5grams <- as.data.frame(s5gramTableBlogs)
dfblogs5grams$numericfreq <- as.numeric(dfblogs5grams$freq)
dfnews5grams <- as.data.frame(s5gramTableNews)
dfnews5grams$numericfreq <- as.numeric(dfnews5grams$freq)

save(dftwitter5grams,file="dftwitter5grams.RData")
save(dfblogs5grams,file="dfblogs5grams.RData")
save(dfnews5grams,file="dfnews5grams.RData")
load("dftwitter5grams.RData")
load("dfblogs5grams.RData")
load("dfnews5grams.RData")

sqldf("SELECT ngrams, freq
      FROM s5gramTableTwitter
      WHERE ngrams='a a a a '")

# This splits First then Second_Third word
dfSplit5GramsTwitter <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                        ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                       ,numericfreq 
                       FROM dftwitter5grams")
# Splits first, second, third
dfSplit5GramsTwitter <- sqldf("SELECT First
                        ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                       ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                       ,numericfreq
                       FROM dfSplit5GramsTwitter")
dfSplit5GramsTwitter_1 <- sqldf("SELECT First
                              ,Second
                              ,RTRIM(SUBSTR(Third,1,CHARINDEX(' ',Third))) Third
                              ,RTRIM(LTRIM(SUBSTR(Third,CHARINDEX(' ',Third),LENGTH(Third)))) Fourth
                              ,numericfreq
                              FROM dfSplit5GramsTwitter")
dfSplit5GramsTwitter_2 <- sqldf("SELECT First
                                ,Second
                                ,Third
                                ,RTRIM(SUBSTR(Fourth,1,CHARINDEX(' ',Fourth))) Fourth
                                ,RTRIM(LTRIM(SUBSTR(Fourth,CHARINDEX(' ',Fourth),LENGTH(Fourth)))) Fifth
                                ,numericfreq
                                FROM dfSplit5GramsTwitter_1")# Put first, second, third together, separate fourth Need freq of all 5 (freqPost) and freq of first two (freqPre)
dfSplit5GramsTwitter_3 <- sqldf("SELECT (First || ' ' || Second || ' ' || Third || ' ' || Fourth) First
                        ,Fifth Second
                        ,numericfreq
                         FROM dfSplit5GramsTwitter_2")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfSplit5GramsTwitter_4 <- sqldf("SELECT sp.First
                      ,SUM(sp.numericfreq) freqPredictor
                      FROM dfSplit5GramsTwitter_3 sp
                      GROUP BY sp.First
                      ")
dfSplit5GramsTwitter_5 <- sqldf("SELECT sp.First Predictor
                             ,sm.Second Predicted
                             ,sp.freqPredictor
                             ,sm.numericfreq freqPredicted
                             FROM dfSplit5GramsTwitter_4 sp
                             INNER JOIN dfSplit5GramsTwitter_3 sm
                             ON sp.First = sm.First
                             ")
###
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
      FROM dfSplit5GramsTwitter_3
      WHERE Predictor='thanks for the'
      ORDER BY freqPredicted")

predictor = "thanks for"
sqlStmt = paste("SELECT Predicted, freqPredicted FROM dfAllProb5Grams WHERE Predictor='",predictor,"' ","ORDER BY freqPredicted DESC",sep="")
results <- sqldf(sqlStmt)

grep("thanks for",dfProb5GramsBlogs_2,perl=TRUE)

######################
# This splits First then Second_Third word
dfSplit5GramsBlogs <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                              ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                              ,numericfreq 
                              FROM dfblogs5grams")
# Splits first, second, third
dfSplit5GramsBlogs <- sqldf("SELECT First
                              ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                              ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                              ,numericfreq
                              FROM dfSplit5GramsBlogs")
dfSplit5GramsBlogs_1 <- sqldf("SELECT First
                                ,Second
                                ,RTRIM(SUBSTR(Third,1,CHARINDEX(' ',Third))) Third
                                ,RTRIM(LTRIM(SUBSTR(Third,CHARINDEX(' ',Third),LENGTH(Third)))) Fourth
                                ,numericfreq
                                FROM dfSplit5GramsBlogs")
dfSplit5GramsBlogs_2 <- sqldf("SELECT First
                                ,Second
                                ,Third
                                ,RTRIM(SUBSTR(Fourth,1,CHARINDEX(' ',Fourth))) Fourth
                                ,RTRIM(LTRIM(SUBSTR(Fourth,CHARINDEX(' ',Fourth),LENGTH(Fourth)))) Fifth
                                ,numericfreq
                                FROM dfSplit5GramsBlogs_1")
# Put first, second, third together, separate fourth Need freq of all 5 (freqPost) and freq of first two (freqPre)
dfSplit5GramsBlogs_3 <- sqldf("SELECT (First || ' ' || Second || ' ' || Third || ' ' || Fourth) First
                                ,Fifth Second
                                ,numericfreq
                                FROM dfSplit5GramsBlogs_2")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfSplit5GramsBlogs_4 <- sqldf("SELECT sp.First
                                ,SUM(sp.numericfreq) freqPredictor
                                FROM dfSplit5GramsBlogs_3 sp
                                GROUP BY sp.First
                                ")
dfSplit5GramsBlogs_5 <- sqldf("SELECT sp.First Predictor
                                ,sm.Second Predicted
                                ,sp.freqPredictor
                                ,sm.numericfreq freqPredicted
                                FROM dfSplit5GramsBlogs_4 sp
                                INNER JOIN dfSplit5GramsBlogs_3 sm
                                ON sp.First = sm.First
                                ")
######################
# This splits First then Second_Third word
dfSplit5GramsNews <- sqldf("SELECT RTRIM(SUBSTR(ngrams,1,CHARINDEX(' ',ngrams))) First
                            ,RTRIM(LTRIM(SUBSTR(ngrams,CHARINDEX(' ',ngrams),LENGTH(ngrams)))) Second
                            ,numericfreq 
                            FROM dfnews5grams")
# Splits first, second, third
dfSplit5GramsNews <- sqldf("SELECT First
                            ,RTRIM(SUBSTR(Second,1,CHARINDEX(' ',Second))) Second
                            ,RTRIM(LTRIM(SUBSTR(Second,CHARINDEX(' ',Second),LENGTH(Second)))) Third
                            ,numericfreq
                            FROM dfSplit5GramsNews")
dfSplit5GramsNews_1 <- sqldf("SELECT First
                              ,Second
                              ,RTRIM(SUBSTR(Third,1,CHARINDEX(' ',Third))) Third
                              ,RTRIM(LTRIM(SUBSTR(Third,CHARINDEX(' ',Third),LENGTH(Third)))) Fourth
                              ,numericfreq
                              FROM dfSplit5GramsNews")
dfSplit5GramsNews_2 <- sqldf("SELECT First
                              ,Second
                              ,Third
                              ,RTRIM(SUBSTR(Fourth,1,CHARINDEX(' ',Fourth))) Fourth
                              ,RTRIM(LTRIM(SUBSTR(Fourth,CHARINDEX(' ',Fourth),LENGTH(Fourth)))) Fifth
                              ,numericfreq
                              FROM dfSplit5GramsNews_1")
# Put first, second, third together, separate fourth Need freq of all 5 (freqPost) and freq of first two (freqPre)
dfSplit5GramsNews_3 <- sqldf("SELECT (First || ' ' || Second || ' ' || Third || ' ' || Fourth) First
                              ,Fifth Second
                              ,numericfreq
                              FROM dfSplit5GramsNews_2")
# Now I need to get the frequency of First (predictor) while keeping the freqPost (for the First + Second freq).
dfSplit5GramsNews_4 <- sqldf("SELECT sp.First
                              ,SUM(sp.numericfreq) freqPredictor
                              FROM dfSplit5GramsNews_3 sp
                              GROUP BY sp.First
                              ")
dfSplit5GramsNews_5 <- sqldf("SELECT sp.First Predictor
                              ,sm.Second Predicted
                              ,sp.freqPredictor
                              ,sm.numericfreq freqPredicted
                              FROM dfSplit5GramsNews_4 sp
                              INNER JOIN dfSplit5GramsNews_3 sm
                              ON sp.First = sm.First
                              ")
save(dfSplit5GramsTwitter_5,file="dfSplit5GramsTwitter_5.RData")
save(dfSplit5GramsBlogs_5,file="dfSplit5GramsBlogs_5.RData")
save(dfSplit5GramsNews_5,file="dfSplit5GramsNews_5.RData")
load("dfSplit5GramsTwitter_5.RData")
load("dfSplit5GramsBlogs_5.RData")
load("dfSplit5GramsNews_5.RData")

dfAllProb5Grams <- sqldf("SELECT Predictor, Predicted
                            ,SUM(freqPredictor) freqPredictor
                         ,SUM(freqPredicted) freqPredicted
                         FROM
                         (SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit5GramsTwitter_3
                         UNION ALL
                         SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit5GramsBlogs_3
                         UNION ALL
                         SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit5GramsNews_3)
                         GROUP BY Predictor, Predicted
                         ")


save(dfAllProb5Grams, file="dfAllProb5Grams.Rdata")
load("dfAllProb5Grams.Rdata")

sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfSplit5GramsTwitter_3
      WHERE Predictor='arctic monkeys this'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfSplit5GramsBlogs_3
      WHERE Predictor='arctic monkeys this'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfSplit5GramsNews_3
      WHERE Predictor='arctic monkeys this'")
sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
       FROM dfAllProb5Grams
      WHERE Predictor='arctic monkeys this'")

