library(arsenal)
library(dplyr)
library(naniar)

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

  ##Case when its a single dataframe
  if(class(df_list) == "data.frame"){
    r <- prepare_dataframe_cols(df_list,use_cols, exclude_cols)
  } else {
    for (i in 1:length(df_list)){
      df_i <- as.data.frame(df_list[i])
      df_prepared <- prepare_dataframe_cols(df_i,use_cols, exclude_cols)
      r <- c(list(df_prepared), r)
      #list_colnames <- c(list(colnames(df_prepared)), list_colnames)
    }

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

#Define nan_policy function
nan_policy<-function(df, nan_decision){

  #Write out all the strings associated with missing values
  na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available",
                  "NOt available", "UNKNOWN", "Unknown")

  #Replace all na_strings for NAs
  df <- as.data.frame(replace_with_na_all(data=df, condition = ~.x %in%
                                            c("NA", "N A", "N / A", "N/A",
                                              "N/ A", "Not Available",
                                              "NOt available", "UNKNOWN",
                                              "Unknown")))

  #Make decision on what to do with missing values
  if (nan_decision == 'keep'){
    df <- df
  } else {
    print("Dropping all rows that contain missing values")
    df <- na.omit(df)
    df
  }
  return(df)
}


run_arsenal <- function(df, cohort_col = NULL, continous_stat_agg, nan_decision,
                        dig){


  formula <- get_formula(df, cohort_col)

  cont_agg <- get_continous_stat_agg(continous_stat_agg)

  #Make decision on what to do with missing values
  df <- nan_policy(df, nan_decision)

  ##WT stands for Wilcoxon-test(alias Mann-Whitney)
  tab_results <- tableby(formula,data=df, numeric.test = "wt", cat.test = "chisq",
                         numeric.stats = cont_agg)


  output <- summary(tab_results, digits = dig, dig.count = 2, dig.pct = 2, dig.p = 2, text=TRUE)

  return(output)

}

get_continous_stat_agg <- function(var){

  if(var == "both"){
    stat <- c("meansd", "median")
    return(stat)
  } else if (var == "mean") {
    return("meansd")
  } else {
    return(var)
  }
}

get_formula <- function(df,cohort_col = NULL ){

  if(is.null(cohort_col) && (length(which(colnames(df) == "COHORT_ASSIGNED")) == 0)){
    formula <- formulize("",".")
  } else if((length(which(colnames(df) == "COHORT_ASSIGNED")) != 0)) {
    formula <-formulize("COHORT_ASSIGNED",".")
  } else {
    ## The formulize function does the paste and as.formula steps
    formula <- formulize(cohort_col,".")
  }
  return(formula)

}


get_descriptives <- function(list_dataframes,
                             cohort_col = NULL,
                             cohort_names = NULL,
                             use_cols = NULL,
                             exclude_cols = NULL,
                             continous_stat_agg ="both",
                             nan_decision = "keep",
                             dig = 2){

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
  ###nan_decision: String indicating if the user wants to keep NAs or drop them,
  ##Possible values: "keep". Any other string besides from keep will drop NAs
  ###dig: Integer indicating the number of decimal places to be shown.


  ###We first prepare de dataframe with the use and exclude columns
  df <- iter_prepare_dataframe_cols(list_dataframes,use_cols, exclude_cols)

  ####Next we prepare the dataframe with the cohorts_names
  df <- prepare_dataframe(df, cohort_names, cohort_col)

  ###Next we run the stats analysis

  df <- run_arsenal(df, cohort_col, continous_stat_agg, nan_decision, dig)

  return(df)
}

