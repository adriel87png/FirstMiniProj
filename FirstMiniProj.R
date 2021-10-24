#First Mini Project
#Code by Jett Adriel M. Rabe
#CMSC 197-2 Data Science
#Finished 10/24/2021 15:16

########
#1.) POLLUTANT MEAN

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
#2.) COMPLETE CASES

#Creates a function named complete, takes in parameters:
#directory -> pathway to file
#id -> an integer vector
complete <- function(directory, id=1:332){
  
  #Creates a list of files, full.names set to true to get the folder name as well
  files_list <- list.files(directory, full.names = TRUE)
  
  #Creates an empty data frame for the final output
  finalframe <- data.frame()
  
  #for loop that loops through the files_list, made possible with id
  for (i in id){
    #recreates empty data frame for evaluation of the next csv file
    mydataframe <- data.frame()
    #binds the csv file to empty data frame
    mydataframe <- rbind(mydataframe, read.csv(files_list[i]))
    
    #compares sulfate and nitrate columns if they both have values that aren't NA
    #will be left with a subset that contains both values for both columns
    datasubset <- mydataframe[which(!is.na(mydataframe[, "sulfate"]) & !is.na(mydataframe[, "nitrate"])),]
    
    #creates temporary data frame to store the id and num of observations (got by using nrow in datasubset)
    tempdf <- data.frame(i, nrow(datasubset))
    #rbinds the finalframe and the temporary dataframe
    finalframe <- rbind(finalframe, tempdf)
  }
  
  #renaming of the columns
  colnames(finalframe)[1] <- "id"
  colnames(finalframe)[2] <- "nobs"
  
  #returns the result frame
  return (finalframe)
  


}


#Samples

complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 30:25)


####################################################
########
#3.) CORRELATION

#Creates a function named corr, takes in parameters:
#directory -> pathway to file
#threshold -> number of complete observations one must have (more like a passing score, must hit this mark)
corr <- function(directory, threshold = 0){
  
  #Creates a list of files, full.names set to true to get the folder name as well
  files_list <- list.files(directory, full.names = TRUE)
  
  #will create empty vector for storing results later
  myvector <- c()

  
  #for loop that loops through the files_list, there are 332 csv files, hence 1:332
  for (i in 1:332){
    
    #This creates an empty data frame
    mydataframe <- data.frame()
    
    #fills in the empty dataframe with data from the current csv file
    mydataframe <- rbind(mydataframe, read.csv(files_list[i]))
    

    #I couldve used my number 2, but in case of independent checking, I used complete.cases
    #makes sure the rows/data have both sulfate and nitrate in them
    #We will use this as opposed to mydataframe
    completedata <- mydataframe[complete.cases(mydataframe),]
    
    #need this to check if it appeases the threshold
    rownumber <- nrow(completedata)
    
    #the rownumber indicates how much observations there are
    #if the number od observations is enough, we proceed
    if (rownumber > threshold){
      
      #Using the cor built-in function as indicated by instructions
      #we get the correlation of nitrate and sulfate
      correlationresult <- cor(completedata$nitrate, completedata$sulfate)
      
      #store the result of this by concatenating it to myvector
      myvector <- c(myvector, correlationresult)
    }
    
  }

  #returns the result vector
  return (myvector)
  
}

#sample
cr <- corr("specdata", 150)
head(cr);summary(cr)

cr <- corr("specdata", 400)
head(cr);summary(cr)

cr <- corr("specdata", 5000)
head(cr);summary(cr);length(cr)

cr <- corr("specdata")
head(cr);summary(cr);length(cr)


#######################
########
#4.) HISTOGRAM

#reads and stores contents of the csv file into outcome
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
#just to check the top contents
head(outcome)

#colClasses were character earlier so need to coerce as numeric column 11
#Since we will be using that specific column
outcome[, 11] <- as.numeric(outcome[, 11])

#rename the main title, changed color to skyblue and renamed the label on x-axis
#this creates a histogram
hist(outcome[, 11], main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", xlab = "Deaths", col = "skyblue")
