# script to run some basic Praat analysis in R. Requires that you have already 
# loaded usefulPraatFunctions.R. To use this script, set your working directory 
# to where you have your TextFiles and Wav files and run the sections of code 
# relevent to you. Also assumes that what you're interested in is in the second
# tier of your TextGrids.
# 
# WARNING: PraatR will crash if any file paths or names have spaces in them

# load PraatR
library("PraatR")

# set working directory and use a function to make it easier to refer to files
setwd("/yourFilePath/")
FullPath = function(FileName){ 
  return( paste(getwd(), FileName, sep="/")) }

# get a list of all .wav and .TextGrid files from that directory
fileList <- list.files()
# check out those sweet RegExs! So elegent ^^
wavList <- subset(fileList, grepl("(wav)$", fileList))
textGridList <- subset(fileList, grepl("(TextGrid)$", fileList))

# praat commands -- note that you may need to update Praat if you get error text
# that begins like this:
#
# Error: Unknown function «do» in formula.
# Script line 10 not performed or completed:
# « do ( "Read from file...", input$ ) »

praat( command="Play", input=FullPath(wavList[1]) )

# This sections calls a number of useful functions and runs them so that we have
# a handful of useful variables and vectors to use in our analysis later on. It
# requires usefulPraatRFunctions.

# get number of intervals in each textgrid as a vector
numInt <- numberOfIntervals(textGridList, tier = 2)

# for each file in your list of textgrids, get the duration of each labelled
# section and saves it as a table with the label 
#
#######

# create an empty table that will contain our durations(eventually)
colClasses = c("character", "character", "character")
col.names = c("File", "Word", "Duration")
durationTable <- read.table(text = "",
                            colClasses = colClasses,
                            col.names = col.names)
# step through each text grid, and if the label interval isn't empty, find the
# duration of the interval and append it, the file name and the intervial label
# to a table
for (i in 1:length(textGridList)){
  # list all the interval labels (including blanks) in the text grid
  labelList <- labelOfIntervals(textGridList[i],numInt[i], tier = 2 )
  # look thorugh each label and if it's not blank find the duration and then add
  # it to the table along with basic info
  for (j in 1:length(labelList)){
    if (labelList[j] != "" ){
      duration <- durationOfInterval(file = textGridList[i], 
                                     numberOfInterval = j, 
                                     tier = 2)
      newrow <- data.frame("File" = textGridList[i],
                           "Word" = labelList[j],
                           "Duration" = duration, 
                           stringsAsFactors = F) 
      durationTable <- rbind(durationTable, newrow)
    }
  }
}


# extra useful code bits: ignore
FullPath(wavList[1:5])
textGridList [1:5]

for (m in 1:5){
 print(m)
}
