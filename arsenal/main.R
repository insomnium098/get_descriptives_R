library(arsenal)

source("prepare_dataframes.R")
source("stats_analysis.R")
get_descriptives <- function(list_dataframes,
                             cohort_col = NULL,
                             cohort_names = NULL,
                             use_cols = NULL,
                             exclude_cols = NULL,
                             continuous_stat_agg ="both"){
  
  ####Function to obtain descriptIve statistics of a given list of dataframes
  ###INPUT:
  ###list_dataframes: a single dataframe or a list of dataframes
  ###cohort_col: character vector indicating the colname of list_dataframes to
  ###be used for cohort assignment
  ###cohort_names: character vector indicating the names of the cohorts
  ###use_cols: character vector indicating the names of the columns to be used
  ###excludecols: character vector indicating the names of the columns to be 
  ###excluded
  ##continuous_stat_agg: character indicating in the continous variables
  ##should be summarized with the mean, median(IQR), or both. Possible values:
  ##"mean", "median", "both"
  
  
  ###We first prepare de dataframe with the use and exclude columns
  df <- iter_prepare_dataframe_cols(list_dataframes,use_cols, exclude_cols)
  
  ####Next we prepare the dataframe with the cohorts_names
  df <- prepare_dataframe(df, cohort_names, cohort_col)
  
  ###Next we run the stats analysis
  
  df <- run_arsenal(df, cohort_col, continuous_stat_agg)
  
  return(df)
}






#############TESTING############
###############################
df1 <- read.csv("data4.csv", row.names = 1)
df2 <- read.csv("data4.csv", row.names = 1)
list_test <- list(df1, df2)



####The test should be giving warnings in Wilcox.test as we are using the same
###table duplicated for the analysis
####Making the result as data.frame
test_df <- as.data.frame(get_descriptives(list_test, exclude_cols = c("race", "gender"),
                            cohort_col = "cohort.label"))

###Making the result as summary

summary(get_descriptives(list_test, exclude_cols = c("race", "gender")))


