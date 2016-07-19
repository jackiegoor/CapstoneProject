options(shiny.maxRequestSize=95*1024^2)

rm(list=(ls(all=TRUE)))
gc()
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)

packages <- c("shiny","shinyIncubator","rJava","RWeka","R.utils", "stringi", "stringr", "textcat", "sqldf", "tcltk", "tm") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(rJava,quietly=TRUE,warn.conflicts=FALSE)
library(shiny,quietly=TRUE,warn.conflicts=FALSEv)
library(shinyIncubator,quietly=TRUE,warn.conflicts=FALSE)
library(rJava,quietly=TRUE,warn.conflicts=FALSE)
library(RWeka,quietly=TRUE,warn.conflicts=FALSE)
library(R.utils,quietly=TRUE,warn.conflicts=FALSE)
library(stringi,quietly=TRUE,warn.conflicts=FALSE)
library(stringr,quietly=TRUE,warn.conflicts=FALSE)
library(textcat,quietly=TRUE,warn.conflicts=FALSE)
library(tm,quietly=TRUE,warn.conflicts=FALSE)
library(sqldf,quietly=TRUE,warn.conflicts=FALSE)
library(ngram,quietly=TRUE,warn.conflicts=FALSE)

load("sorted5GramsRank3OrLess.RData")
load("sorted4GramsRank3OrLess.RData")
load("sorted3GramsRank3OrLess.RData")
load("sorted2GramsRank3OrLess.RData")
load("sorted1GramsRank3OrLess.RData")

# Read all the necessary files before starting
shinyServer(function(input, output, session) {
        
        
        # Read data
        readData = reactive({  
                
                # Read data sets created during data processing
                # These files contain the Predictor word(s) and the 3 most likely Predicted next words (Rank3OrLess).
                # For the 5 grams, the Predictor will contain 4 words.
                # For the 4 grams, the Predictor will contain 3 words.
                # For the 3 grams, the Predictor will contain 2 words.
                # For the 2 grams, the Predictor will contain 1 words.
                # For the 1 grams, there is no Predictor. This simply contains the 3 most common Predicted
                # next words.
#                  load("sorted5GramsRank3OrLess.RData")
#                  load("sorted4GramsRank3OrLess.RData")
#                  load("sorted3GramsRank3OrLess.RData")
#                  load("sorted2GramsRank3OrLess.RData")
#                  load("sorted1GramsRank3OrLess.RData")

                return
        });

        ####################################################################################################
        ####################################### Prediction #################################################
        ####################################################################################################
        predictWord = function(updateProgress = NULL){tryCatch({
                
                # Initialize 
                updateProgress(detail = "Working very hard...")      
                data = readData()
                unacceptableWords <- read.delim(file="UnacceptableWords.txt", header=TRUE)
                
                myInput <- stringi::stri_trans_general(input$textInput, "latin-ascii")    
                myInput <- str_replace_all(myInput, "[^[:alpha:]\']", " ")
                myInput <- ngram::preprocess(myInput, case = "lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
                myInput <- str_trim(myInput)
                # The data files were read in when the app started.
                # Get a count of the words entered.
                splitWords <- stringi::stri_split_regex(myInput," ",simplify=TRUE)
                numWords <- sum(lengths(splitWords)) # Need to sum since they're in separate lists
                #if (numWords >= 4)
                #{
                        # Only use last 4 words.
                        myInput <- paste(splitWords[1,numWords-3],splitWords[1,numWords-2],splitWords[1,numWords-1],splitWords[1,numWords],sep=" ")
                        # Get the last 4 and select from 5 grams
                        myNext <- sqldf(paste("SELECT Predicted FROM sorted5GramsRank3OrLess WHERE Predictor = '",myInput,"'",sep=""))
                        if (lengths(myNext) == 0)
                        {
                                # Only use last 3 words.
                                myInput <- paste(splitWords[1,numWords-2],splitWords[1,numWords-1],splitWords[1,numWords],sep=" ")
                                # Get the last 3 and select from 5 grams
                                myNext <- sqldf(paste("SELECT Predicted FROM sorted4GramsRank3OrLess WHERE Predictor = '",myInput,"'",sep=""))
                                if (lengths(myNext) == 0)
                                {
                                        # Only use last 2 words.
                                        myInput <- paste(splitWords[1,numWords-1],splitWords[1,numWords],sep=" ")
                                        # Get the last 2 and select from 5 grams
                                        myNext <- sqldf(paste("SELECT Predicted FROM sorted3GramsRank3OrLess WHERE Predictor = '",myInput,"'",sep=""))
                                        if (lengths(myNext) == 0)
                                        {
                                                # Only use last word.
                                                myInput <- paste(splitWords[1,numWords],sep=" ")
                                                myNext <- sqldf(paste("SELECT Predicted FROM sorted2GramsRank3OrLess WHERE Predictor = '",myInput,"'",sep=""))
                                                if (lengths(myNext) == 0)
                                                {
                                                        myNext <- sqldf(paste("SELECT Predicted FROM sorted1GramsRank3OrLess",sep=""))
                                                }
                                        }
                                }
                        }
#                 } else if (numWords == 3)
#                 {
#                         # Only use last 3 words.
#                         myInput <- paste(splitWords[1,numWords-2],splitWords[1,numWords-1],splitWords[1,numWords],sep=" ")
#                         # Get the last 3 and select from 5 grams
#                         myNext <- sqldf(paste("SELECT Predicted FROM sorted4GramsRank3OrLess WHERE Predictor = '",myInput,"'",sep=""))
#                 } else if (numWords == 2)
#                 {
#                         # Only use last 2 words.
#                         myInput <- paste(splitWords[1,numWords-1],splitWords[1,numWords],sep=" ")
#                         # Get the last 2 and select from 5 grams
#                         myNext <- sqldf(paste("SELECT Predicted FROM sorted3GramsRank3OrLess WHERE Predictor = '",myInput,"'",sep=""))
#                 } else if (numWords == 1)
#                 {
#                         # Only use last word.
#                         myInput <- paste(splitWords[1,numWords],sep=" ")
#                         myNext <- sqldf(paste("SELECT Predicted FROM sorted2GramsRank3OrLess WHERE Predictor = '",myInput,"'",sep=""))
#                 } else
#                 {
#                         myNext <- sqldf(paste("SELECT Predicted FROM sorted1GramsRank3OrLess",sep=""))
#                 }
#                 
                if (lengths(myNext) == 3)
                {
                    return (paste(myNext[1,1],myNext[2,1],myNext[3,1],sep=" "))
                } else if (lengths(myNext) == 2)
                {
                        return (paste(myNext[1,1],myNext[2,1],sep=" "))
                } else if (lengths(myNext) == 1)
                {
                        return (myNext[1,1])
                }
                
        }, error = function(err){    
                return (paste("Couldn't predict next word: ", err))
        }, finally = {} 
        )}
        
        ## Render results
        output$result = renderText({    
                
                # Respond to 'predict' button click only
                input$predict
                
                # User isolate to avoid dependency on other inputs
                isolate({       
                        # Create a Progress object
                        progress <- shiny::Progress$new()
                        progress$set(message = "", value = 0)
                        
                        # Close the progress when this reactive exits (even if there's an error)
                        on.exit(progress$close())
                        
                        updateProgress <- function(value = NULL, detail = NULL) {
                                if (is.null(value)) {
                                        value <- progress$getValue()
                                        value <- value + (progress$getMax() - value) / 5
                                }
                                progress$set(value = value, detail = detail)
                        }
                        
                        # Create a closure to update progress.
                        # Find the next word
                        predictWord(updateProgress)     
                }) 
                
                
        })
})