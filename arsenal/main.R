library(arsenal)

source("prepare_dataframes.R")
get_descriptives <- function(list_dataframes,
                             cohort_col = NULL,
                             cohort_names = NULL,
                             use_cols = NULL,
                             exclude_cols = NULL){
  
  ####Function to obtain descriptIve statistics of a given list of dataframes
  ###INPUT:
  ###list_dataframes: a single dataframe or a list of dataframes
  ###cohort_col: character vector indicating the colname of list_dataframes to
  ###be used for cohort assignment
  ###cohort_names: character vector indicating the names of the cohorts
  ###use_cols: character vector indicating the names of the columns to be used
  ###excludecols: character vector indicating the names of the columns to be 
  ###excluded
  
  
  ###We first prepare de dataframe with the use and exclude columns
  df <- iter_prepare_dataframe_cols(list_dataframes,use_cols, exclude_cols)
  
  ####Next we prepare the dataframe with the cohorts_names
  df <- prepare_dataframe(df, cohort_names, cohort_col)
  
  return(df)
  
  
}






#############TESTING############
###############################
df1 <- read.csv("data4.csv", row.names = 1)
df2 <- read.csv("data4.csv", row.names = 1)
list_test <- list(df1, df2)
test_df <- get_descriptives(list_test, exclude_cols = c("race", "gender"),
                            cohort_col = "cohort.label")

test_df_no_cohort <- get_descriptives(list_test, exclude_cols = c("race", "gender"))
