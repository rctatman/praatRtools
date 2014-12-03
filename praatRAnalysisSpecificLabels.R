if (findF1 == TRUE){
  F1 = NULL
  for(i in 1:dim(measurementTable)[1]){
    fileName <- paste(gsub(".TextGrid", '' , measurementTable$File[i]), 
                      ".formant", sep = "")
    newVal <- formantAtTime(file = fileName,
                            formantNumber = 1,
                            time = measurementTable$timeMaxInt[i])
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
                            time = measurementTable$timeMaxInt[i])
    F2 <- c(F2, newVal)
  }
  # now we can bind that list of values as a column to our table
  measurementTable <- cbind(measurementTable, F2, 
                            stringsAsFactors = FALSE, deparse.level = 1)
  # and save it out
  write.csv(measurementTable, "measurementTableF2.csv")
}
