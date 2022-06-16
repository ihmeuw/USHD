##############################
# These are functions for checking specific variables and data sets for missingness
# And square dimensionality (same number of observations by age, year, sex, etc).
##############################
library(plyr)

check_missingness <-function(data) {
  print("**************** Checking Data For Missingness **************************",quote=FALSE)
  #Checking your data set for missingness:
  #this will also tell you the names of the columns in the data set
  
  #Generating a data set to check for missing values
  missing<-data.table(columns=names(data)) #initializing a data table with columns names for rows
  missing$missing_values<-data.table(sapply(data, function(x) sum(is.na(x))))$V1
  
  if (sum(missing$missing_values)==0){
    print("      No Missingness Detected in your data set! :]      ",quote=FALSE)
    
  }else{
    #If there is missingness somewhere in the data set...  
    print(missing)
    for (each_column in length(missing)) {
      column_name<-missing$columns[each_column]
      n_missing<-missing$missing_values[each_column]
      
      if (n_missing>0) {
        print(paste0("Whoah! You've got ",n_missing," missing values in the ",column_name," column!"),quote=FALSE)
      }#if missingness WAS detected (sum of missing values greater than 0)
    }# for each column in the data set where missingness was detected
    rm(n_missing,column_name) #cleaning up variables created within function
  }# else-loop initiated if there was missingness detected in the data set
  rm(missing) #cleaning up data frame that checks for missing values  
  
  
  print("**************** Checking Data For Infinite Values **************************",quote=FALSE)
  #Checking your data set for infiniteness:
  #this will also tell you the names of the columns in the data set
  
  #Generating a data set to check for infinite values
  infinite<-data.table(columns=names(data)) #initializing a data table with columns names for rows
  infinite$infinite_values<-data.table(sapply(data, function(x) sum(is.infinite(x))))$V1
  
  if (sum(infinite$infinite_values)==0){
    print("      No Infinite Values Detected in your data set! :]      ",quote=FALSE)
    
  }else{
    #If there is infiniteness somewhere in the data set...  
    print(infinite)
    for (each_column in length(infinite)) {
      column_name<-infinite$columns[each_column]
      n_infinite<-infinite$infinite_values[each_column]
      
      if (n_infinite>0) {
        print(paste0("Whoah! You've got ",n_infinite," infinite values in the ",column_name," column!"),quote=FALSE)
      }#if infiniteness WAS detected (sum of infinite values greater than 0)
    }# for each column in the data set where infiniteness was detected
    rm(n_infinite,column_name) #cleaning up variables created within function
  }# else-loop initiated if there was infiniteness detected in the data set
  rm(infinite) #cleaning up data frame that checks for infinite values  
  print("**************** Checking Data For Duplicate Values **************************",quote=FALSE)
  data_unkeyed<-copy(data)
  setkey(data_unkeyed)
  discrepancy<-nrow(data_unkeyed)-nrow(unique(data_unkeyed))
  if (discrepancy>0){print(paste0("WHOAH, you have " ,discrepancy," duplicate values in this data set"))}else{
    print("      No Duplicate Values Detected in your data set! :]      ")
  }
}#closing functions

check_var<-function(data,var){
  print(paste0("***********************    Checking ",var,"      *************************"),quote=FALSE)
  if(var %in% names(data)){
    unique_var<-sort(unique(data[[var]]))
    print(paste0("      There are ",length(unique_var)," unique values of the variable ",var," in the data set.      "),quote=FALSE)
    freq_per_var<-data.frame(table((data)[[var]]))
    freq_per_var<-plyr::rename(freq_per_var, c("Var1" = "var", "Freq" = "n_observations"))
    
    #testing for whether there are the same number of observations for each category
    identicalValue <- function(x,y) if (identical(x,y)) x else "! whoah, not the same number! of "
    n_obs<-Reduce(identicalValue,freq_per_var$n_observations)
    print(paste0("      There are ",n_obs," observations for each level of the variable.      "),quote=FALSE)
    print("      The levels of this variable are:    ",quote=FALSE)
    if (length(unique_var)<50){print(unique_var)}
    rm(n_obs,identicalValue,unique_var,freq_per_var)
  }else{
    print("      The variable you desginated doesn't seem to esist in this data set.      ",quote=FALSE)
  }#closing else-loop if  year does not exist in data set  
} #closing function


