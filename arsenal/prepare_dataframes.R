library(arsenal)
library(dplyr)


iter_prepare_dataframe_cols <- function(df_list, use_cols = NULL,
                                        exclude_cols = NULL){
  ####The input of the function is:
  ##df_list : A list of dataframes
  ##use_cols: vector of characters, name of columns to use
  ##exclude_cols: vector of characters, name of columns to exclude
  ##Returns: a List, where its first element is a list containing the
  ##dataframes prepared (df_proc) and the second element is a list containing the
  ##colnames of the dataframes (des_vars)
  
  #"""Iterates through the data list and calls _prepare_dataframe_cols()."""
  
  ##r variable is a list that contains the dataframes
  ##list_colnames variable is a list that contains the colnames of the dataframes
  r <- list()
  list_colnames <- list()
  for (i in 1:length(df_list)){
    df_i <- as.data.frame(df_list[i])
    df_prepared <- prepare_dataframe_cols(df_i,use_cols, exclude_cols)
    r <- c(list(df_prepared), r)
    #list_colnames <- c(list(colnames(df_prepared)), list_colnames)
  }
  ###r contains the list of dataframes
  #final_list <- list(r, list_colnames)
  return(r)
  
}


prepare_dataframe_cols <- function(df, usecols = NULL, excludecols = NULL){
  #"""Excludes/Includes required columns in the dataset"""
  if (is.null(usecols)){
    if(is.null(excludecols)){
      return(df)
    } else {
      return(df[, !(colnames(df) %in% excludecols)])
    }
  } else if (is.null(excludecols)){
    return(df[, (colnames(df) %in% usecols)])
  } else {
    stop("ValueError: Both use_cols and exclude_cols cannot take values")
  }
}


#####

#Define _get-default_names function
get_default_names <- function(n){
  #This function returns the default names of the cohort
  cohort_list <- c()
  for (i in 1:n){
    cohort <- paste0("Cohort-", i)
    cohort_list<- append(cohort_list, cohort)
  }
  return(cohort_list)
}

#Define assign_cohort_name function
assign_cohort_name <- function(cnames, n){
  #Assign cohort names if they are not specified
  if (is.null(cnames)){
    cohort_names <- get_default_names(n)
  } else {
    cohort_names <- cnames
  }
  return(cohort_names)
}

prepare_dataframe <- function(list_dataframes,cohorts_names = NULL,
                              cohort_col = NULL){
  ##This function receives a list dataframes and rbinds them. If the user
  ##provides the vector cohorts_names with the same length as the list_datarames
  ##the function will add a column indicating the cohort of each dataframe
  ##If the list_dataframe only conatains a single dataframe and no 
  ##cohorts_names , the function will add a column with a default cohort name
  ##The column added to the dataframes specifyng the name of the cohort will be
  ##COHORT_ASSIGNED
  
  
  if(class(list_dataframes) == "data.frame"){
    list_dataframes <- list(list_dataframes)
  }
  
  if(is.null(cohorts_names)){
    cohorts_names <- get_default_names(length(list_dataframes))
  }
  if((length(list_dataframes) != length(cohorts_names)) &&
     !is.null(cohorts_names) && !is.null(cohort_col)){
    stop("The number of dataframes is different")
  }
  #We first check if there is a single dataframe
  if(length(list_dataframes) == 1){
    finalDF <- as.data.frame(list_dataframes[1])
    if(is.null(cohort_col)){
      print("i")
      finalDF$COHORT_ASSIGNED<- get_default_names(1)
    }
    
  } else {
    for(i_df in 1:length(list_dataframes)){
      tmp_df <- as.data.frame(list_dataframes[i_df])
      if(is.null(cohort_col)){
        tmp_df$COHORT_ASSIGNED<- cohorts_names[i_df]
      }
      
      if(!exists("finalDF")){
        finalDF <- tmp_df
      } else{
        finalDF <- rbind(finalDF, tmp_df)
      }
    }
  }
  
  return(finalDF)
  
  
  
}



#############TESTING############
###############################
#df1 <- read.csv("data4.csv", row.names = 1)
#df2 <- read.csv("data4.csv", row.names = 1)


#list_test <- list(df1, df2)
#list_test <- list(df1)

#test <- prepare_dataframe(df1)








