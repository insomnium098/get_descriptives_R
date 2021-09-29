library(collections)


build_ordinal_type <- function(values){
  ##Builds customized category datatype. Needed to indicate ordinal values.
  ##Returns as.factor() ordered
  return(factor(values, ordered=TRUE))
}

###Python:
# Mapping the data columns to specified dtype if specified
#df_proc, df_dtypes = _iter_map_dtypes(df_proc, column_dtypes)

###df_proc is the first list resulting from iter_prepare_dataframe_cols
iter_map_dtypes <- function(df_list, col_dtypes = NULL){
  #"""Iterates through the data list and calls _map_dtypes()."""
  ###By how R functions, this function will only allow
  ##to process a column as an factor
  ##dtypes is a character vector indicating which columns should be converted 
  ##in factor
  mapped <- list()
  
  for (df in df_list){
    mapped <- c(mapped, list(map_dtypes(df, col_dtypes)))
  }
  
  ##The second element is the type of the columns
  ###sapply(data4, class)
  return(list(mapped, cohort_names))

}

map_dtypes <- function(df, col_dtypes = NULL){
  if(is.null(col_dtypes)){
    return(df)
  } else{
    df <- data.frame(df)
    
    for (i in 1:ncol(df)){
      if(colnames(df)[i] %in% col_dtypes){
        df[,i] <- build_ordinal_type(df[,i])
      }
    }
    
  }
  return(df)
}











test_list <- list(read.csv("df_list.csv", row.names = 1))
test <- iter_prepare_dataframe_cols(test_list)
df_proc <- test[1]

###Test function
iter_map_dtypes(df_proc)
coltypes <- c("birth_year", "gender")
test2 <- iter_map_dtypes(df_proc, coltypes)
test3 <- as.data.frame(test2[1])

####Factors example
#birth_factor <- build_ordinal_type(data4$birth_year)
#birth_dict <- dict(levels(birth_factor),"birth_year")


