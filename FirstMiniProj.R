#First Mini Project
#Code by Jett Adriel M. Rabe
#CMSC 197-2 Data Science

########
#1.)

#Creates a function named pollutantmean, takes in parameters:
#directory -> pathway to file
#pollutant -> name of pollutant
#id -> an integer vector
pollutantmean <- function(directory, pollutant, id=1:332){
  
  #Creates a list of files, full.names set to true to get the folder name as well
  files_list <- list.files(directory, full.names = TRUE)
  
  #This creates an empty data frame
  mydataframe <- data.frame()
  
  #for loop that loops through the files_list, made possible with id
  for (i in id){
    #Extends the data frame by using rbind to bind the succeeding csv files
    mydataframe <- rbind(mydataframe, read.csv(files_list[i]))
  }
  
  #takes the mean of pollutant specified from within the dataframe and stores it
  #na.rm = TRUE allows us to ignore NA values
  resultmean <- mean(mydataframe[, pollutant], na.rm=TRUE)
  
  #Simply returns the result stored
  return (resultmean)
}

#Samples

pollutantmean("specdata", "sulfate", 1:10) #Ans: 4.064128

#For this particular sample, I am quite certain that the answer is
#different as opposed to what is shown in the example output given to us
pollutantmean("specdata", "nitrate", 70:72) #Ans: 1.706047

pollutantmean("specdata", "nitrate", 23) #Ans: 1.280833

###############################################


########
#2.)

#Creates a function named complete, takes in parameters:
#directory -> pathway to file
#id -> an integer vector
complete <- function(directory, id=1:332){
  
  #Creates a list of files, full.names set to true to get the folder name as well
  files_list <- list.files(directory, full.names = TRUE)
  nobs <- 0
  #This creates an empty data frame
  finalframe <- data.frame()
  
  #for loop that loops through the files_list, made possible with id
  for (i in id){
    mydataframe <- data.frame()
    mydataframe <- rbind(mydataframe, read.csv(files_list[i]))
    datasubset <- mydataframe[which(!is.na(mydataframe[, "sulfate"]) & !is.na(mydataframe[, "nitrate"])),]
    tempdf <- data.frame(i, nrow(datasubset))
    finalframe <- rbind(finalframe, tempdf)
  }
  
  
  
  return (finalframe)
  
  #Simply returns the result stored

}


#Samples

complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 1)
