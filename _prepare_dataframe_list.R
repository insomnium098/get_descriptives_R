#Environment
library(readxl)
getwd()
setwd("/Users/luis_/OneDrive/Documentos/Holmusk/get_descriptives/")

#Load data
data1 <- read_excel("data1.xlsx", sheet="Sheet1")
data2 <- read_excel("data2.xlsx", sheet="Sheet1")
data3 <- read_excel("data3.xlsx", sheet="Sheet1")
data4 <- read_excel("data4.xlsx", sheet="Sheet1")

#Define _get-default_names function
get_default_names <- function(n){
  #This function returns the default names of the cohort
  cohort_list <- c()
  for (i in 1:n){
    cohort <- paste("Cohort-", i)
    cohort_list<- append(cohort_list, cohort)
  }
  return(cohort_list)
}

#Try get_default_names function
n = 5 #Define cname
get_default_names(n)

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

#Try assign_cohort_name function
cnames <- c('Condition A', 'Condition B') #Define cname
assign_cohort_name(cnames,5) #Function with a defined cnames
assign_cohort_name(NULL,5) #Function with NULL cnames

#Define prepare_dataframe function
prepare_dataframe <- function(df, colname, cnames){
  #Converts the dataframe to a list of dataframes for each cohort. This functions feed to prepare_dataframe_list
  list_df <- c()
  if (is.null(colname)){
    print("Name of the cohort column is not specified. Therefore, the data is assumed to be from a single cohort")
    cohort_names <- assign_cohort_name(cnames, 1)
    list_df <- c(df)
  } else {
    indexColname <- grep(colname, colnames(df))
    cohort_names <- unique(df[,indexColname])
    for (i in cohort_names){
      tmp <- df[df[, indexColname] == i,]
      tmp$indexColname <- NULL
      append(list_df, tmp)
    }
  }
  return(list(list_df, cohort_names))
}

#Try prepare_dataframe function
cnames <- c('Condition A', 'Condition B') #Define cname
colname <- 'cohort-label' #Define name of the column with the cohort
prepare_dataframe(data4, NULL, cnames) #Null colname
prepare_dataframe(data4, colname, cnames) #Null colname

