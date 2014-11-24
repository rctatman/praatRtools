# script to run some basic Praat analysis in R. Requires that you have already 
# loaded usefulPraatFunctions.R. To use this script, set your working directory 
# to where you have your TextGrid and Wav files and run the sections of code 
# relevent to you. Also assumes that what you're interested in is in the second
# tier of your TextGrids.
# 
# WARNING: PraatR will crash if any file paths or names have spaces in them

# load PraatR
library("PraatR")

# set working directory and use a function to make it easier to refer to files
setwd("/home/rachael/Dropbox/attentionData/ParticipantData/memoryTasks/")
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


# plot a histogram of our density table to make sure that there's nothing really
# weird going on
hist(durationTable$Duration)
plot(durationTable$Duration)

# subset the duration table based on first and second tasks. note: very specific
# to this task and analysis, but useful data manipulation for other instances as 
# well

# find all rows in the table where the file name contians a "1", extract
# those rows and append a new "Task" column to the dataframe with the value 1 in
# it
memTask1 <- subset(durationTable, grepl("[1]", durationTable$File))
newCol <- data.frame("Task" = 1)
memTask1 <- cbind(memTask1, newCol)

# same as above but with the second task
memTask2 <- subset(durationTable, grepl("[2]", durationTable$File))
newCol <- data.frame("Task" = 2)
memTask2 <- cbind(memTask2, newCol)

# create a new table including a column with the task number
memTask <- rbind(memTask1, memTask2)
# add a "person" row made from the first three letters of each filename
person <- data.frame("Person" = tolower(substr(memTask[,1], 1, 3)))
memTask <- cbind(memTask, person)
# now let's save it, just in case
write.csv(file=FullPath("memTaskDurations.csv"), x=memTask)

# basic discriptive statistics and plots
summary(memTask1)
summary(memTask2)
summary(memTask)
boxplot(memTask1$Duration, memTask2$Duration)
boxplot(memTask1$Duration, memTask2$Duration, notch = TRUE)

t.test(memTask1$Duration, memTask2$Duration)
# HOly CARP IT'S SIGNIFICIATN. But they're not really independent and I'm
# interested in more than just which task is at play; I also want to account for
# individiual variation and weird tokens as well. To do that I'm going to use a
# linear mixed effects model.
library(lme4)
model <- lmer(Duration ~ Task + (1|Person), data = memTask)
summary(model)
# Now I'm going to construct another liner mixed effects model that looks
# excludes task and compare it to the model that incldes it to see if there
# really a significnat effect of task.
null.model <- lmer(Duration ~ (1|Person), data = memTask, REML=FALSE)
anova(model, null.model)
# Huzzah, there is! This is only looking at the effect of repeating a task,
# however. I want to know if there's an effect of intervening task as well.

# now, for each labelled interval, we want to find the point where we have the 
# highest intensity: we'll use this to measure both formants and pitch so that 
# we have a simple, reproducable way to make sure that our measurements are 
# equivilent. (Note that this function  inassumes that you've only labelled the 
# vowels here in the second tier)
# 
# This section of code finds the point of the highest intensity in a labelled
# interval and then creates a table with 1) the label of the file, 2) the label
# of the invertal and 3) the point of highest intensity for each labelled
# interval

# first, we need to create an intensity object for each of the .wav files in our
# working directory. 
for (i in 1:length(wavList)){
  toIntensity(file = wavList[i])
}
# at this point, if you look in your working directory you should see a file
# with a .intensity extension for every .wav file in your directory. Now let's
# make a list of all those files and check that it's the same length as our list
# of .wav files to makes sure that's the case
fileList <- list.files()
intensityList <- subset(fileList, grepl("(.intensity)$", fileList))
if (length(intensityList) == length(wavList)){
  print("Everything's fine!")
} else {
  print("Something's wrong!")
}
# now that we know everything's ok, we can run this bit of code which looks 
# through each textgrid, finds the labeled intervals, finds the start and end 
# points of each labelled intervals and then finds the time point of the maximum
# ampilitude within those two points as well as the time of that point. For good
# measure, I'm putting the duration data from above in this table as well. 
# create an empty table that will contain our measurement values
colClasses = c("character", "character", "character", 
               "character", "character")
col.names = c("File", "Word", "Duration",
              "Max_Intensity", "Time_Max_Intensity")
measurementsTable <-  read.table(text = "",
                                 colClasses = colClasses,
                                 col.names = col.names)
# step through each text grid, and if the label interval isn't empty, find the 
# duration of the interval, the maximum intensity and the time of the maximum 
# intensity and append it, then add the file name and the interval label to a
# table
# 
# Fair warning: this bit will take a while. You might want to go get tea or do
# some yoga or something.
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
      times <- timePointsOfInterval(file = textGridList[i], 
                                    numberOfInterval = j, 
                                    tier = 2) 
      Max_Intensity <- intensityMaximum(file = intensityList[i], times[1], times[2])
      Time_Max_Intensity <- intensityMaximumTime(file = intensityList[i], times[1], times[2])
      newrow <- data.frame("File" = textGridList[i],
                           "Word" = labelList[j],
                           "Duration" = duration,
                           "Max_Intensity" = Max_Intensity,
                           "Time_Max_Intensity" = Time_Max_Intensity, 
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

praat( command="Play", input=FullPath(wavList[1]) )
