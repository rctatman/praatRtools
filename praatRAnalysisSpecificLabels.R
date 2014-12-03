# Introduction to the script
# 
# Script to run some basic analysis of vowels using Praat in R. This script 
# takes time-alinged textgrid files that contain only vowels and .wav sound 
# files and returns your choice of duration, intensity, pitch, F1 and F2. 
# (Pitch, F1 and F2 will be taken from the point of greatest intensity.) It will
# also create assosciated praat objects for each measurement you choose to 
# include. Note that some of these processes are really resource intensive and, 
# depending on the number of files you have to analyze and how fast your 
# computers is, may take several hours or even days (for very large data sets).
# 
# Requires that you have already loaded usefulPraatFunctions.R. To use this 
# script, set your working directory to where you have your TextFiles and Wav 
# files and run the sections of code relevent to you. Also assumes that what 
# you're interested in is in the second tier of your TextGrids.
# 
# WARNING: PraatR will crash if any file paths or names have spaces in them
# 
## Contact and maintainer info.
#
# Code produced by Rachael Tatman (rctatman@uw.edu) at the University of
# Washington. Work supported by the National Science Foundation Graduate
# Research Fellowship Program.

## Collect information on which pieces of analysis are needed. 
#
# These variables determine which measurements the script will include. Set the
# value next to every measurement you're not interested in to FALSE. This will
# greatly reduce the time it takes to run the analysis. 

findDuration = TRUE
findIntensity = TRUE
findPitch = TRUE
findF1 =  TRUE
findF2 = TRUE

# this lets you load in a .csv of labels that you're interested in and then get
# information for only the labelled intervals where the the label of the
# interval is on that list. Case sensative
tablesOfLabelsOfInterest = read.csv(file.choose(), header = FALSE)
labelsOfInterest = tablesOfLabelsOfInterest[[1]]

## Load required packages
#
# load PraatR. PraatR is NOT in CRAN (as of 11/25/2014) but is avalible for
# download here: http://www.aaronalbin.com/praatr/index.htm
library("PraatR")
library(stringr)

## Set and check our working directory 
#
# set working directory and use a function to make it easier to refer to files
# for later: 
setwd("/Users/labuser/Documents/labusers/Rachael/passageTask/")
setwd("/Users/labuser/Documents/labusers/Rachael/wordTask/")
# start with Convo
setwd("/Users/labuser/Documents/labusers/Rachael/convoTask/")
FullPath = function(FileName){
  return( paste(getwd(), FileName, sep="/")) }

# get a list of all .wav and .TextGrid files from that directory
fileList <- list.files()

# find all the .wav and .TextGrid files in the current directory 
wavList <- subset(fileList, grepl("(wav)$", fileList))
textGridList <- subset(fileList, grepl("(TextGrid)$", fileList))

# check to make sure that we have the same number of .wav and .TextGrid files. 
if(length(wavList)==length(textGridList)){
  print("Everything's ok.")
}else{
  print(c("Something's wrong! Make sure you have a .TextGrid file", 
          "for every .Wav file in your folder."))
}

## Set up a datafram for our data
#
# get number of intervals in the second tier of each textgrid as a vector. (This
# is the only function in usefulPraatRFunctions that loops thorugh your
# directory for you)
numInt <- numberOfIntervals(textGridList, tier = 1)

# Now we create an empty table for our measurements. 
colClasses = c("character", "character")
col.names = c("File", "Word")
measurementTable <- read.table(text = "",
                               colClasses = colClasses,
                               col.names = col.names)
# populate that empty table with the labels of all non-empty intervals in a tier
# we're interested in
for (i in 1:length(textGridList)){
  # list all the interval labels (including blanks) in the text grid
  labelList <- labelOfIntervals(textGridList[i],numInt[i], tier = 1 )
  # look thorugh each label and if it's not blank find the duration and then
  # add it to our variable
  for (j in 1:length(labelList)){
    label = labelList[j]
    if (match(label, labelsOfInterest, nomatch = 0)){
      newrow <- data.frame("File" = textGridList[i],
                           "Word" = labelList[j],
                           stringsAsFactors = F) 
      measurementTable <- rbind(measurementTable, newrow)
    }
  }
}

# save our file in case we need it later
write.csv(measurementTable, "mesasurementTable.csv")

## Find duration 
#
# check to see that we actually want to find duration and then do so
if (findDuration == TRUE){
  # create an empty variable for duration measurements
  Duration <- NULL
  # loop through each text grid and ilst the duration of all non-blank intervals
  for (i in 1:length(textGridList)){
    # list all the interval labels (including blanks) in the text grid
    labelList <- labelOfIntervals(textGridList[i],numInt[i], tier = 1 )
    # look thorugh each label and if it's not blank find the duration and then
    # add it to our variable
    for (j in 1:length(labelList)){
      if (match(labelList[j], labelsOfInterest, nomatch = 0)){
        newVal <- durationOfInterval(file = textGridList[i], 
                                     numberOfInterval = j, 
                                     tier = 1)
        Duration <- c(duration, newVal)
      }
    }
  }
  # add Duration to our measurement table and then save it to disk
  measurementTable <- cbind(measurementTable, Duration, 
                            stringsAsFactors = FALSE, deparse.level = 1)
  write.csv(measurementTable, "measurementTableDur.csv")
}


## Find the time of max intensity (if needed)
#
# now check to see if any of the measurements that require the time of max
# intensity are needed. If so, create intensity object and find the pointof max
# intensity
if ((findIntensity + findPitch + findF1 + findF2) > 0){
  # make intensity ojects
  for (i in 1:length(wavList)){
    toIntensity(file = wavList[i])
  }
  
  # create a list of all of our intenisty files for easy reference 
  fileList <- list.files()
  intensityList <- subset(fileList, grepl("(.intensity)$", fileList))
  
  # create a place to store our points of max intensity 
  timeMaxInt <- NULL
  
  # find the point of max intensity and add it to our file
  for (i in 1:length(textGridList)){
      # list all the interval labels (including blanks) in the text grid
    labelList <- labelOfIntervals(textGridList[i],numInt[i], tier = 1 )
    # look through each label and if it's not blank find the point of max intensity
    for (j in 1:length(labelList)){
      if (match(labelList[j], labelsOfInterest, nomatch = 0)){
        times <- timePointsOfInterval(file = textGridList[i], 
                                      numberOfInterval = j, 
                                      tier = 1) 
        newVal <- intensityMaximumTime(file = intensityList[i], 
                                       times[1], times[2])  
        timeMaxInt <- c(timeMaxInt, newVal)
      }
    }
  }
  # add our measurements to the table and save the file
  measurementTable <- cbind(measurementTable, timeMaxInt, 
                            stringsAsFactors = FALSE, deparse.level = 1)
  write.csv(measurementTable, "measurementTableIntTime.csv")
}

## Find max intensity 
#
# now find the max intensity (if needed) and add it to our table 
if (findIntensity == TRUE){
  # create a place to store our points of max intensity 
  Intensity <- NULL
  for (i in 1:length(textGridList)){
    # list all the interval labels (including blanks) in the text grid
    labelList <- labelOfIntervals(textGridList[i],numInt[i], tier = 1 )
    # look through each label and if it's not blank find the point of max intensity
    for (j in 1:length(labelList)){
      if (match(labelList[j], labelsOfInterest, nomatch = 0)){
        times <- timePointsOfInterval(file = textGridList[i], 
                                      numberOfInterval = j, 
                                      tier = 1) 
        newVal <- intensityMaximum(file = intensityList[i], 
                                   timeLow = times[1], 
                                   timeHigh = times[2])
        Intensity <- c(Intensity, newVal)
      }
    }
  }
  # add our intensity measurements to the table and save the file
  measurementTable <- cbind(measurementTable, Intensity, 
                            stringsAsFactors = FALSE, deparse.level = 1)
  write.csv(measurementTable, "measurementTableInt.csv")
}

## Find max pitch 
#
# now find the max pitch (if needed) and add it to our table 
if (findPitch == TRUE){
  # create a pitch object for every .wav file in our folder. This is another
  # really hefty one.
  for (i in 1:length(wavList)){
    toPitch(file = wavList[i])
  }
  fileList <- list.files()
  pitchList <- subset(fileList, grepl("(.pitch)$", fileList))
  # now create a pitch tier for each of those pitch objects 
  for (i in 1:length(pitchList)){
    toPitchTier(file = pitchList[i])
  }
  # now take the pitch from that pitch tier at the point of maximum intensity
  # (taken from our table)
  Pitch <- NULL
  for(i in 1:dim(measurementTable)[1]){
    newVal <- pitchFromPitchTier(paste(gsub(".TextGrid", '' , measurementTable$File[i]), 
                                       ".pitchTier", sep = "") , measurementTable$timeMaxInt[i])
    Pitch <- c(Pitch, newVal)
  }
  # now we can bind that list of values as a column to our table
  measurementTable <- cbind(measurementTable, Pitch, 
                            stringsAsFactors = FALSE, deparse.level = 1)
  # and save it out
  write.csv(measurementTable, "measurementTablePitch.csv")
}

# Create formant objects
# 
# create formant objects if we need them. This particular function takes a
# really long time
if (findF1 + findF2 > 0){
  for (i in 1:length(wavList)){
    toFormant(file = wavList[i])
  }
}

# Find F1
# 
# find the first formant (if needed) and add it to our file. This will take
# quite a while.
if (findF1 == TRUE){
  F1 = NULL
  for(i in 1:dim(measurementTable)[1]){
    fileName <- paste(gsub(".TextGrid", '' , measurementTable$File[i]), 
                      ".formant", sep = "")
    newVal <- formantAtTime(file = fileName,
                            formantNumber = 1,
                            time = measurementTable$Time_Max_Intensity[i])
    F1 <- c(F1, newVal)
  }
  # now we can bind that list of values as a column to our table
  measurementTable <- cbind(measurementTable, F1, 
                            stringsAsFactors = FALSE, deparse.level = 1)
  # and save it out
  write.csv(measurementTable, "measurementTableF1.csv")
}

## Find F2
#
# find the second formant (if needed) and add it to our file
if (findF2 == TRUE){
  F2 = NULL
  for(i in 1:dim(measurementTable)[1]){
    fileName <- paste(gsub(".TextGrid", '' , measurementTable$File[i]), 
                      ".formant", sep = "")
    newVal <- formantAtTime(file = fileName,
                            formantNumber = 2,
                            time = measurementTable$Time_Max_Intensity[i])
    F2 <- c(F2, newVal)
  }
  # now we can bind that list of values as a column to our table
  measurementTable <- cbind(measurementTable, F2, 
                            stringsAsFactors = FALSE, deparse.level = 1)
  # and save it out
  write.csv(measurementTable, "measurementTableF2.csv")
}
