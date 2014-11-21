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
