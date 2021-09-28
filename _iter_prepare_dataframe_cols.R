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
    list_colnames <- c(list(colnames(df_prepared)), list_colnames)
  }
  ###final_list is a list containing 2 lists: r and list_colnames
  final_list <- list(r, list_colnames)
  return(final_list)
  
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



###Testing dataframe

#test_list <- list(read.csv("df_list.csv", row.names = 1))
#test <- iter_prepare_dataframe_cols(test_list)
