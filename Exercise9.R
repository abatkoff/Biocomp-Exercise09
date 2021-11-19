### Exercise 9 
### Abigayle Batkoff

##create a function that computes the coefficients of variation for a specified column across files in a specified directory

#specify the variable dir as the directory
dir <- "~/Users/abigaylebatkoff/Downloads/MBA Year 1/Biocomputing/wages" 

#specify column variable
column <- 2


myfunction <- function(dir, column, override = FALSE){
  
  #generate the list of files in given directory 
  files <- list.files(path=dir, full.names = TRUE)
  unlist(files) #this makes the list of files a vector
  
  #create vector to hold coefficients of variation
  coeffvar <- numeric(length=length(files))
  
  #go through each file and compute the coefficient of variation if the file meets the criteria
  for(i in 1:length(files)){
    
    #convert csv file to a data frame
    dframe<-read.table(file = files[i], header=TRUE, sep=",", stringsAsFactors = FALSE)
    
    #print warning if < 50 obs and override=TRUE, otherwise print error and print NA for that coef of var
    if(nrow(dframe) < 50 || nrow(dframe)-sum(is.na(dframe[,column])) < 50){ 
      
      if(override==FALSE){
        print(files[i])
        print("Error, this file contains less than 50 observations. A coefficient of variation will not be computed and the output vector will contain 'NA' for this file's computation, which can be found in the following output:")
        print(i)
        coeffvar[i]<-NA
      } else if(override==TRUE){
        
        if(sum(is.na(dframe[,column])) > 0 && nrow(dframe)-sum(is.na(dframe[,column])) < 50){
          print(files[i])
          print("Warning, this file contains less than 50 observations after ignoring the NA values present in the designated column. The NA vaues were ignored in the computation and the resulting coefficient of variation can be found in the following output:")
          print(i)
          dframe<- complete.cases(dframe[,column])
          coeffvar[i]=sd(dframe)/mean(dframe)
        } else {
          print(files[i])
          print("Warning, this file contains less than 50 observations. The computed coefficient of variation for this file will be less reliable.")
          print("The coefficient of variation for this file can be found in the following output:")
          print(i)
          coeffvar[i]=sd(dframe[,column])/mean(dframe[,column])
        }
      }
      
    } else if(nrow(dframe) >= 50) {
      
      if(sum(is.na(dframe[,column])) > 0){
        print(files[i])
        print("This file has NA values in the designated column, they were ignored in the computation and the resulting coefficient of variation can be found in the following output:")
        print(i)
        dframe<- complete.cases(dframe[,column]) 
        coeffvar[i]=sd(dframe)/mean(dframe)
        
      } else {
        coeffvar[i]=sd(dframe[,column])/mean(dframe[,column])
      }
    } 
    
  } 
  return(coeffvar)
}


