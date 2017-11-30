# read all the data in the specdata directory into a large list using lapply.

readdata <- function(directory){
  
  files <- list.files(path=directory,pattern="*.csv",full.names=TRUE)  # a list of all the file names
  df_list <- lapply(files,read.csv)  # all the data is in a list called df_list
  names(df_list) <- gsub(".csv","",list.files(directory,full.names=FALSE),fixed = TRUE) # get rid of the ".csv" in the list entry names
  
  df_list  #return
  
}

# now the three functions described in the week 2 assignment

pollutemean <- function(df_list, pollutant, id=1:332){

  temp <- lapply(df_list[id], function(x){x[pollutant][!is.na(x[pollutant])]}) 
  
  mean(unlist(temp,use.names=FALSE))
  
}



complete <- function(df_list, id=1:332){
  
  temp <- df_list[id]
  
  totals <- sapply(temp, function(x){sum(!is.na(x["sulfate"])&!is.na(x["nitrate"]))})
  
  data.frame(id=id, nobs=totals)
  
}



corr <- function(df_list, threshold=0){
  
  result<-numeric()
  
  for(i in 1:332){
    
    if(complete(df_list,i)["nobs"] > threshold){
      
      idx <- sapply(df_list[i], function(x){!is.na(x["sulfate"])&!is.na(x["nitrate"])})  # determine which IDs have slufate and nitrate data
      result <- c(result,cor(df_list[[i]][idx,"sulfate"],df_list[[i]][idx,"nitrate"]))   # append the correlation to the results vector
      
    }
    
  }
  
  result
}

