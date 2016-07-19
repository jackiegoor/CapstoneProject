rm(list=(ls(all=TRUE)))
gc()
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)

packages <- c("sqldf", "data.table") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(sqldf,quietly=TRUE,warn.conflicts=FALSE)
library(data.table,quietly=TRUE,warn.conflicts=FALSE)

#filePath = "\\\\sherlock.usa.tribune.com\\qlikviewdocs$\\Temp\\CapstoneProject\\"
#filePath = "E:/Coursera/"
filePath = "D:/Diversions/CourseraCourses/DataScientist/CapstoneProject/"
#dir.create('E:/Coursera', showWarnings = FALSE)
#setwd('E:/Coursera')
dir.create(filePath, showWarnings = FALSE)
setwd(filePath)

# Merge twitter, blogs, news 1-grams thru 5-grams.
load("dfSplit5GramsTwitter_5.RData")
load("dfSplit5GramsBlogs_5.RData")
load("dfSplit5GramsNews_5.RData")

# What I really want are the top 3 (by frequency) entries for each 5-gram.
dfAllProb5Grams <- sqldf("SELECT Predictor, Predicted
                            ,SUM(freqPredictor) freqPredictor
                            ,SUM(freqPredicted) freqPredicted
                         FROM
                         (SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit5GramsTwitter_5
                         UNION ALL
                         SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit5GramsBlogs_5
                         UNION ALL
                         SELECT Predictor, Predicted, freqPredictor, freqPredicted
                         FROM dfSplit5GramsNews_5)
                         GROUP BY Predictor, Predicted
                         ")
save(dfAllProb5Grams, file="dfAllProb5Grams.Rdata")
load("dfAllProb5Grams.Rdata")

# Get rid of one-offs
dfAllProb5Grams_1 <- sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
                          FROM dfAllProb5Grams 
                         WHERE freqPredicted > 1
                         ORDER BY Predictor, Predicted, freqPredicted")

save(dfAllProb5Grams_1, file="dfAllProb5GramsNoOneOffs.Rdata")
load("dfAllProb5GramsNoOneOffs.Rdata")

DTAllProb5Grams <- as.data.table(dfAllProb5Grams_1)
DTAllProb5Grams$Predictor <- as.character(DTAllProb5Grams$Predictor)
DTAllProb5Grams$Predicted <- as.character(DTAllProb5Grams$Predicted)

write.table(DTAllProb5Grams,file="DTAllProb5Grams.csv")
DTAllProb5Grams <- read.table("DTAllProb5Grams.csv")

DTAllProb5Grams[,freqRank:=data.table::frank(DTAllProb5Grams, -freqPredicted), by=Predictor]

dfAllProb5Grams_1 <- rank(x=-paste(Predictor, " ", Predicted), ties.method ="min")

dfAllProb5Grams_1 <- sqldf("SELECT Predictor, Predicted,
                                @predicted_rank := IF(@current_freq = freqPredicted, @predicted_rank + 1, 1) predicted_rank,
                                @current_freq := freqPredicted 
                           FROM dfAllProb5Grams
                           ORDER BY Predictor, freqPredicted DESC")

dfAllProb5Grams_2 <- sqldf("SELECT Predictor, Predicted
                                FROM dfAllProb5Grams_1
                                WHERE RowNo < 4
                                ORDER BY Predictor")

# dfAllProb5Grams_1 <- sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
#                           FROM dfAllProb5Grams 
#                          WHERE freqPredicted > 5
#                          ORDER BY Predictor, Predicted, freqPredicted")

save(dfAllProb5Grams_1, file="dfAllProb5Grams_1.Rdata")
load("dfAllProb5Grams_1.Rdata")

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

dfAllProb4Grams <- sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
                          FROM dfAllProb4Grams 
                         WHERE freqPredicted > 5
                         ORDER BY Predictor, Predicted, freqPredicted")

save(dfAllProb4Grams, file="dfAllProb4Grams.Rdata")
load("dfAllProb4Grams.Rdata")

load("dfProb3GramsTwitter_2.RData")
load("dfProb3GramsNews_2.RData")
load("dfProb3GramsBlogs_2.RData")
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
dfAllProb3Grams <- sqldf("SELECT Predictor, Predicted, freqPredictor, freqPredicted
                          FROM dfAllProb3Grams 
                         WHERE freqPredicted > 5
                         ORDER BY Predictor, Predicted, freqPredicted")

save(dfAllProb3Grams, file="dfAllProb3Grams.Rdata")
load("dfAllProb3Grams.Rdata")

load("dfProb2GramsTwitter_2.RData")
load("dfProb2GramsNews_2.RData")
load("dfProb2GramsBlogs_2.RData")
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


load("dfProb1GramsTwitter_2.RData")
load("dfProb1GramsNews_2.RData")
load("dfProb1GramsBlogs_2.RData")
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

