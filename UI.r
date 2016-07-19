rm(list=(ls(all=TRUE)))
gc()
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)

packages <- c("shiny","shinyIncubator") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(shiny,quietly=TRUE,warn.conflicts=FALSEv)
library(shinyIncubator,quietly=TRUE,warn.conflicts=FALSE)

#filePath = "\\\\sherlock.usa.tribune.com\\qlikviewdocs$\\Temp\\CapstoneProject\\"
#filePath = "E:/Coursera/"
filePath = "D:/Diversions/CourseraCourses/DataScientist/CapstoneProject/ToPublish/"
#dir.create('E:/Coursera', showWarnings = FALSE)
#setwd('E:/Coursera')
dir.create(filePath, showWarnings = FALSE)
setwd(filePath)

unacceptableWords <- read.delim(file="UnacceptableWords.txt", header=TRUE)

# Define UI for application
shinyUI(fluidPage(

  # Application title
  titlePanel("Word Prediction"),
  helpText("This application will predict the next word based on text entered by the user."),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(      
      helpText("Enter text then push the Predict button below."),
      
      textInput("textInput", "Text", value = ""),           
      br(),   
      actionButton("predict", "Predict"),      
      br()   
    )
    ,

    # Main panel
    mainPanel(
      h3("Prediction"),
      br(),
      br(),
      verbatimTextOutput("result")      
    )
  )
))