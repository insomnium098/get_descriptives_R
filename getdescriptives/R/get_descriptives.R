library(arsenal)
library(dplyr)
library(naniar)
library(NeuroBlu)

# -------------Median IQR-----------#
# By default, Arsenal doesn't implement the Median (IQR) function.
# To overcome this, we created the function and then we added
# it to arsenal namespace
medianIQR <- function (x, na.rm = TRUE, weights = NULL, ...)
{
  y <- if (na.rm && allNA(x)) {
    NA_real_
  }
  else {
    m <- wtd.quantile(x, weights = weights, probs = c(0.5),
                      na.rm = na.rm)
    iqr_a <- diff(wtd.quantile(x, weights = weights,
                               probs = c(0.25,0.75), na.rm = na.rm))
    if (is.Date(x))
      list(as.character(m), as.difftime(s, units = "days"))
    else c(m, iqr_a)
  }
  as.tbstat(y, parens = c("(", ")"))
}

# We assign the medianIQR function to arsenal
environment(medianIQR) <- asNamespace('arsenal')

# ---------------------------------#

.iter_prepare_dataframe_cols <- function(df_list, use_cols = NULL,
                                        exclude_cols = NULL){
  # The input of the function is:
  # df_list : A list of dataframes
  # use_cols: vector of characters, name of columns to use
  # exclude_cols: vector of characters, name of columns to exclude
  # Returns: a List, where its first element is a list containing the
  # dataframes prepared (df_proc) and the second element is a list containing the
  # colnames of the dataframes (des_vars)

  # """Iterates through the data list and calls _prepare_dataframe_cols()."""

  # r variable is a list that contains the dataframes
  # list_colnames variable is a list that contains the colnames of the dataframes
  r <- list()
  list_colnames <- list()

  # Case when its a single dataframe
  if(class(df_list) == "data.frame"){
    r <- .prepare_dataframe_cols(df_list,use_cols, exclude_cols)
  } else {
    for (i in 1:length(df_list)){
      df_i <- as.data.frame(df_list[i])
      df_prepared <- .prepare_dataframe_cols(df_i,use_cols, exclude_cols)
      r <- c(list(df_prepared), r)
    }

  }

  # r contains the list of dataframes
  return(r)

}


.prepare_dataframe_cols <- function(df_var, usecols = NULL, excludecols = NULL){
  # """Excludes/Includes required columns in the dataset"""
  if (is.null(usecols)){
    if(is.null(excludecols)){
      return(df_var)
    } else {
      return(df_var[, !(colnames(df_var) %in% excludecols)])
    }
  } else if (is.null(excludecols)){
    return(df_var[, (colnames(df_var) %in% usecols)])
  } else {
    stop("ValueError: Both use_cols and exclude_cols cannot take values")
  }
}



.get_default_names <- function(n){
  # This function returns the default names of the cohort
  cohort_list <- c()
  for (i in 1:n){
    cohort <- paste0("Cohort-", i)
    cohort_list<- append(cohort_list, cohort)
  }
  return(cohort_list)
}

.prepare_dataframe <- function(list_dataframes,cohorts_names = NULL,
                              cohort_col = NULL){
  # This function receives a list dataframes and rbinds them. If the user
  # provides the vector cohorts_names with the same length as the list_datarames
  # the function will add a column indicating the cohort of each dataframe
  # If the list_dataframe only conatains a single dataframe and no
  # cohorts_names , the function will add a column with a default cohort name
  # The column added to the dataframes specifyng the name of the cohort will be
  # COHORT_ASSIGNED


  if(class(list_dataframes) == "data.frame"){
    list_dataframes <- list(list_dataframes)
  }

  if(is.null(cohorts_names)){
    cohorts_names <- .get_default_names(length(list_dataframes))
  }
  if(is.null(cohort_col) && length(list_dataframes) == 1){
    warning("UserWarning: Name of the cohort column is not specified.
            The data is assumed to be from a single cohort",
            call. = F)
  }
  if((length(list_dataframes) != length(cohorts_names)) &&
     !is.null(cohorts_names) && !is.null(cohort_col)){
    stop("The number of dataframes is different")
  }
  # We first check if there is a single dataframe
  if(length(list_dataframes) == 1){
    finalDF <- as.data.frame(list_dataframes[1])
    if(!is.null(cohorts_names) && is.null(cohort_col)){
      if (length(cohorts_names) > 1){
        finalDF$COHORT_ASSIGNED <- cohorts_names[1]
      } else {
        finalDF$COHORT_ASSIGNED <- cohorts_names
      }
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

# Define .nan_policy function
.nan_policy<-function(df_var, nan_decision){

  # Write out all the strings associated with missing values
  na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available",
                  "NOt available", "UNKNOWN", "Unknown")

  # Replace all na_strings for NAs
  df_var <- as.data.frame(replace_with_na_all(data=df_var, condition = ~.x %in%
                                            c("NA", "N A", "N / A", "N/A",
                                              "N/ A", "Not Available",
                                              "NOt available", "UNKNOWN",
                                              "Unknown")))

  # Make decision on what to do with missing values
  if (nan_decision == 'keep'){
    df_var <- df_var
  } else {
    print("Dropping all rows that contain missing values")
    df_var <- na.omit(df_var)
    df_var
  }
  return(df_var)
}


.run_arsenal <- function(df_var, cohort_col = NULL, continous_stat_agg, dig,
                         continous_test, categorical_test){


  formula <- .get_formula(df_var, cohort_col)

  cont_agg <- .get_continous_stat_agg(continous_stat_agg)

  # Obtain the labels of the stats
  stats_labels <- .get_stats_labels(cont_agg)

  # Make decision on what to do with missing values
  # df_var <- .nan_policy(df_var, nan_decision)

  # Obtaninig the numeric test
  numeric_test <- .get_numeric_test(df_var, cohort_col, continous_test)

  cat_test <- .get_categorical_test(df_var, categorical_test)

  # WT stands for Wilcoxon-test(alias Mann-Whitney)
  tab_results <- suppressWarnings(tableby(formula,data=df_var,
                                          numeric.test = numeric_test,
                                          cat.test = cat_test,
                         numeric.stats = cont_agg, total = FALSE,
                         cat.stats=c("countpct"), stats.labels = stats_labels))


  output <- summary(tab_results, digits = dig, dig.count = 2, dig.pct = 2,
                    dig.p = 2, text=TRUE, pfootnote=TRUE)

  return(output)

}

.get_numeric_test <- function(df_var, cohort_col, continous_test){

  if(continous_test == "auto"){
    return(.determine_numeric_test(df_var, cohort_col))
  } else {
    return(continous_test)
  }

}

.get_categorical_test <- function(df_var, categorical_test){

  if(categorical_test == "auto"){
    return("chisq")
  } else {
    return(categorical_test)
  }

}

.get_stats_labels <- function(labels){
  # Function that returns the labels that are displayed in the summarized table
  labels_stats <- list(meansd="Mean (SD)", medianIQR = "Median (IQR)")
  return(labels_stats[labels])

}

.get_continous_stat_agg <- function(var){

  if(var == "both"){
    stat <- c("meansd", "medianIQR")
    return(stat)
  } else if (var == "mean") {
    return("meansd")
  } else if (var == "median"){
    return("medianIQR")
  }
}

.get_formula <- function(df_var,cohort_col = NULL ){

  if(is.null(cohort_col) && (length(which(colnames(df_var) == "COHORT_ASSIGNED")) == 0)){
    formula <- formulize("",".")
  } else if((length(which(colnames(df_var) == "COHORT_ASSIGNED")) != 0)) {
    formula <-formulize("COHORT_ASSIGNED",".")
  } else {
    # The formulize function does the paste and as.formula steps
    formula <- formulize(cohort_col,".")
  }
  return(formula)

}

.determine_numeric_test <- function(df_var, cohort_col){
  # Check the number of groups. If < 2 then Wilcox-test is used for
  # numerical variables, else anova is used

  if(is.null(cohort_col)){
    indexCohortCol <- which(colnames(df_var) == "COHORT_ASSIGNED")
    if(length(unique(df_var[,indexCohortCol])) > 2){
      return("anova")
    } else{
      return("wt")
    }
  } else{
    indexCohortCol <- which(colnames(df_var) == cohort_col)
    if(length(unique(df_var[,indexCohortCol])) > 2){
      return("anova")
    } else{
      return("wt")
    }

  }
}

.demographics_df <- function(cohort){
  # This function takes in a cohort name and gets the corresponding patient IDs
  # to extract demographic variables and returns a dataframe ready to use in
  # get_descriptives()

  # Extract demographic variables
  birth_year = getBirthYear(cohort) #Birth year
  gender = getGender(cohort) #Gender
  city = getCity(cohort) #City
  education_years = getEducationYears(cohort) #Education years
  employment = getEmployment(cohort) #Employment
  ethnicity = getEthnicity(cohort) #Ethnicity
  marital = getMarital(cohort) #Marital status
  race = getRace(cohort) #Race
  state = getState(cohort) #State

  # List of variables to merge
  demographics <- list(birth_year, gender, city, education_years,
                       employment, ethnicity,
                       marital, race, state)

  # Merge all remaining variables
  for (i in demographics){
    i <- as.data.frame(i) # Change lists to dataframes
    if(!(exists("df_final"))){
      df_final<-i
    } else {
      df_final<-merge(df_final, i, by="person_id", all=TRUE)
    }
  }
  df_final <- df_final[,-c(1)] # Remove column 'person_id', which is the first in the dataframe
  return(df_final)
}

.check_input <- function(user_input){
  # Function that verifies if the input is an
  # integer, list of integers or dataframe
  # If the input is integer or list of integer
  # it will parse them to obtain the demographics dataframe
  if(class(user_input) == "numeric"){
    return(.demographics_df(user_input))
  } else if (class(user_input) == "data.frame"){
    return(user_input)
  } else if (class(user_input[[1]]) == "numeric"){
    return(.parse_demographic_list(user_input))
  } else {
    return(user_input)
  }
}

.parse_demographic_list <- function(user_input){
  # Function that receives a list of integers (person_id)
  # Making a Dataframe of demographics for each of them
  final_list <- list()
  for (x in user_input){
    df_demo <- .demographics_df(x)
    final_list  <- c(list(df_demo), final_list)
  }

  return(final_list)
}

#' Function to obtain descriptive statistics of a given list of dataframes
#' or patients.
#' @param list_dataframes a single dataframe or a list of dataframes
#' @param cohort_col character vector indicating the colname of list_dataframes to
#' be used for cohort assignment
#' @param cohort_names character vector indicating the names of the cohorts
#' @param use_cols character vector indicating the names of the columns to be used
#' @param exclude_cols character vector indicating the names of the columns to be excluded
#' @param continuous_stat_agg character indicating in the continous variables
#' should be summarized with the Mean (STD), Median (IQR), or both in 2 rows.
#' Possible values: "mean", "median", "both"
#' @param continous_test character indicating the test for the continous variables
#' possible values: "auto", "anova" , "wt".
#' @param categorical_test character indicating the test for the continous variables
#' possible values: "auto", "chisq" , "fe".
#' @param dig Integer indicating the number of decimal places to be shown.
#' @param csv csv: Boolean, indicating if the output should be a CSV. By default
#' the output is in Markdown
#' @return A table with the described statistics
#' @examples
#' get_descriptives(mtcars)
#' get_descriptives(list(list(mtcars[1:10,]), mtcars[11:20,]))
#' @export

get_descriptives <- function(list_dataframes,
                             cohort_col = NULL,
                             cohort_names = NULL,
                             use_cols = NULL,
                             exclude_cols = NULL,
                             continous_stat_agg ="both",
                             continous_test = "auto",
                             categorical_test = "auto",
                             dig = 2,
                             csv = F){


  # We first check if the user provided ids of patients or a dataframe
  df_var <- .check_input(list_dataframes)


  # Next we prepare de dataframe with the use and exclude columns
  df_var <- .iter_prepare_dataframe_cols(df_var,use_cols, exclude_cols)

  # Next we prepare the dataframe with the cohorts_names
  df_var <- .prepare_dataframe(df_var, cohort_names, cohort_col)

  # Next we run the stats analysis

  df_var <- .run_arsenal(df_var, cohort_col, continous_stat_agg, dig,
                         continous_test, categorical_test)

  if( csv == T) {
    # We now format df_var to export it as csv file
    df_var <- as.data.frame(df_var)
    df_var <- write.csv(df_var, row.names=F)
  }



  return(df_var)
}
