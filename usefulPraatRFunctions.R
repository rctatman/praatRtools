#useful functions in PraatR note that you may need to update Praat if you get 
#error text that begins like this:
#
#Error: Unknown function «do» in formula. Script line 10 not performed or 
#completed: « do ( "Read from file...", input$ ) »
#
##################################################


# Number of Intervals
# 
# function that takes a list of textgrids and a tier number and returns a vector
# of the number of intervals in each. Requires that you are in the same working
# directory as your files.
numberOfIntervals <- function(files,tier = 1){
  # function that a list of text grid files names and gives us the complete file
  # path requires that you're in the same working directory as your files
  FullPath = function(FileName){ 
    return( paste(getwd(), FileName, sep="/")) }
  
  # create some variables we'll use in the loop
  numInt <- NULL
  tierArg = list(tier)
  
  # loops thorugh every textgrid in your file list nand
  for (i in 1:length(files)) {
    j <- praat( command = "Get number of intervals...",
           arguments = tierArg,
           input=FullPath(files[i]))
    numInt = c(numInt, j)
  }
  return(numInt)
}

# a function to get an interval label from a selected Praat Textgrid file.
# Requires a single file, the nubmer of intervals (can be got using the above
# function) and the tier of interest (1 by default).
labelOfIntervals <- function(file, numberOfIntervals, tierNumber = 1){
  labelList <- NULL
   for (i in 1:numberOfIntervals){
    intervalNumber <- i
    labelArgs <- list(tierNumber, intervalNumber)
    label<- praat(command = "Get label of interval...",
                  arguments = labelArgs, 
                  input=FullPath(file))
    labelList <- c(labelList, label)
  }
  return(labelList)
}

# This function takes a file, the index of an interval and a tier number and 
# returns the duration of that interval in milliseconds
# 
# It requries the package stringr; if you haven't installed it, uncomment the
# next line.
# install(stringr)
library(stringr)
durationOfInterval <- function(file, numberOfInterval, tierNumber = 1){
  argsPoint <- list(tierNumber, numberOfInterval)
  start <- praat(command = "Get start point...",
                 arguments = argsPoint,
                 input=FullPath(file))
  end <-  praat(command = "Get end point...",
                arguments = argsPoint,
                input=FullPath(file))
  # convert this output to floats we can do math on
  start <- as.numeric(str_extract(start, "[0-9]*.[0-9]*"))
  end <- as.numeric(str_extract(end, "[0-9]*.[0-9]*"))
  duration = (end - start) * 1000
  return(duration)
}

# function returns the start and end points of a textgrid interval given a file,
# the index of an interval and a tier number
timePointsOfInterval <- function(file, numberOfInterval, tierNumber = 1){
  argsPoint <- list(tierNumber, numberOfInterval)
  start <- praat(command = "Get start point...",
                 arguments = argsPoint,
                 input=FullPath(file))
  end <-  praat(command = "Get end point...",
                arguments = argsPoint,
                input=FullPath(file))
  start <- as.numeric(str_extract(start, "[0-9]*.[0-9]*"))
  end <- as.numeric(str_extract(end, "[0-9]*.[0-9]*"))
  points <- c(start, end)
  return(points)
}

# This funciton creates an intensity object given a .wav file and saves it to the
# working directory
toIntensity <- function(file, pitchMin = 100, timeStep = 0, subtractMean = "yes"){
  argsIntensity <- list(
    pitchMin = pitchMin,
    timeStep = timeStep, 
    subtractMean = subtractMean)
  fileName <- paste(substr(file, 1, nchar(file)-4),".intensity", sep = "")
  praat(command = "To Intensity...",
        arguments = argsIntensity,
        input=FullPath(file),
        output =FullPath(fileName))
}


# This function takes an intensity grid and two time points and returns the
# point in that interval with the highest intensity. 
intensityMaximum <- function(file, timeLow, timeHigh, Interpolation = "Parabolic"){
  argsIntMax <- list(timeLow, timeHigh, Interpolation)
  intMax <- praat(command = "Get maximum...",
                  arguments = argsIntMax,
                  input = FullPath(file))
  intMax <- as.numeric(str_extract(end, "[0-9]*.[0-9]*"))
  return(intMax)
}

# This function takes an intensity grid and two time points and returns the
# value of the highest intensity between those two points
intensityMaximumTime <- function(file, timeLow, timeHigh, Interpolation = "Parabolic"){
  argsIntMaxTime <- list(timeLow, timeHigh, Interpolation)
  intMaxTime <- praat(command = "Get time of maximum...",
                  arguments = argsIntMaxTime,
                  input = FullPath(file))
  intMaxTime <- as.numeric(str_extract(intMaxTime, "[0-9]*.[0-9]*"))
  return(intMaxTime)	
}

# This function creates a pitch object given a .wav file and saves it to the
# working directory
toPitch <- function(file, timeStep = 0, pitchMin = 75, pitchMax = 600){
  argsPitch <- list(timeStep,pitchMin,pitchMax)
  fileName <- paste(substr(file, 1, nchar(file)-4),".pitch", sep = "")
  praat(command = "To Pitch...",
        arguments = argsPitch,
        input=FullPath(file),
        output =FullPath(fileName))
}

# This function is supposed to extract pitch from a pitch object at a given 
# point: at the moment it appears to be broken. It returns "undefinined" 
# regardless of whether there is a pitch track present at the point where it is 
# called or not. I've circumvented this by creating pitchtiers from the pitch 
# objects and then using them but it seems a little inefficient.  
pitchAtMaxIntensity <- function(file, time){
  argsPitchMax <- list(time, "Hertz", "Linear")
  pitch <-  praat(command = "Get value at time...",
        arguments = argsPitchMax,
        input=FullPath(file))
  pitch <- as.numeric(str_extract(pitch, "[0-9]*.[0-9]*"))
  return(pitch)
}

# creates a pitchtier given a pitch object
toPitchTier <- function(file){
  fileName <- paste(substr(file, 1, nchar(file)-6),".pitchTier", sep = "")
  praat(command = "Down to PitchTier",
        input=FullPath(file),
        output =FullPath(fileName))
}

# gives the pitch value at a specific time point given both a pitch ovbject and
# a time value
pitchFromPitchTier <- function(file, time){
  argsPitchTier <- list(time)
  pitch <-  praat(command = "Get value at time...",
                  arguments = argsPitchTier,
                  input=FullPath(file))
  pitch <- as.numeric(str_extract(pitch, "[0-9]*.[0-9]*"))
  return(pitch)
}

# this function creates a formant object. Taken from Aaron Albin's example on
# the PraatR page, here: http://www.aaronalbin.com/praatr/ExampleApplication.r
toFormant <- function(file){
  fileName <- paste(substr(file, 1, nchar(file)-4),".formant", sep = "")
  FormantArguments = list( 0.001, # Time step (s)
                           5,     # Max. number of formants
                           5500,  # Maximum formant (Hz)
                           0.025, # Window length (s)
                           50    )# Pre-emphasis from (Hz)
  praat( "To Formant (burg)...",
       arguments = FormantArguments,
       input = FullPath(file),
       output = FullPath(fileName))
}

# this function outputs a formant given a .formant object, formant number (1 = 
# f1, 2 = f2, etc.) and time of the measturement. You can also specify units 
# (Hertz or Bark, Hertz by default) and interpolation method (linear by 
# default). More disucssion of this Praat function here: 
# http://www.fon.hum.uva.nl/praat/manual/Formant__Get_value_at_time___.html
# 
# Please note that there may be pretty severe pitch tracking errors--make sure
# you check your measurements!
formantAtTime <- function(file, formantNumber, time, 
                          unit = "Hertz", interpolation = "linear"){
 formantArgs <- list(formantNumber, time, unit, interpolation)
 formant <- praat(command = "Get value at time...",
                  arguments = formantArgs,
                  input = FullPath(file))
 formant <- as.numeric(str_extract(formant, "[0-9]*.[0-9]*"))
 return(formant)
}


# junky test code: ignore
formantAtTime(file = formantList[1], 
              formantNumber = 2,  
              time = measureNew$Time_Max_Intensity[3])

junk <- "junkjunk"
paste(substr(junk, 0, nchar(junk)-4),"_jaaank.dot", sep="")

pitchObj <- toPitch(wavList[1])
times <- timePointsOfInterval(file = textGridList[1], 
                   numberOfInterval = 2, 
                   tier = 2) 
intensityMaximum(file = intensityList[1], times[1], times[2])
intensityMaximumTime(file = intensityList[1], times[1], times[2])

time = measureNew$Time_Max_Intensity[1]/1000
pitchAtMaxIntensity(pitchList[1], time = 69.442904)

pitchTier(pitchList[1])

file = pitchList[1]
time =69.442904 
  argsPitchMax <- list(time, "Hertz", "Linear")
  pitch <-  praat(command = "Get value at time...",
                  arguments = argsPitchMax,
                  input=FullPath(file))

pitchFromPitchTier(file = "007_mem1.pitchTier", time = measureNew$Time_Max_Intensity[1])

toFormant(wavList[1])
