#Load data
data4 <- read.csv("data4.csv", row.names = 1)

#Define _get_default_names function
get_default_names <- function(n){
  #This function returns the default names of the cohort
  cohort_list <- c()
  for (i in 1:n){
    cohort <- paste0("Cohort-", i)
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
  list_df <- list()
  
  if (is.null(colname)){
    warning("Name of the cohort column is not specified. Therefore, the data is assumed to be from a single cohort")
    cohort_names <- assign_cohort_name(cnames, 1)
    cohort_names <- strsplit(cohort_names, " ")
    list_df <- (list(df))
  } else {
    ##We find the column specified,#We need to make a case where the column doesnt exist
    indexColname <- which(colnames(df) == colname)
    cohort_names <- unique(df[,indexColname])
    tmp_cohort_name <- list()
    
    for (i in cohort_names){
      tmp <- df[df[, indexColname] == i,]
      tmp <- tmp[-c(indexColname)]
      #append(list_df, tmp)
      list_df <- c(list_df, list(tmp))
      tmp_cohort_name <- c(tmp_cohort_name, i)
      
    }
    cohort_names <- tmp_cohort_name
  }
  return(list(list_df, cohort_names))
}

#Try prepare_dataframe function
cnames <- c('Condition A', 'Condition B') #Define cname
colname <- 'cohort-label' #Define name of the column that has the cohorts of interest
prepare_dataframe(data4, NULL, cnames) #Null colname
prepare_dataframe(data4, colname, cnames) #Null colname

#Define validate_length function
#THIS FUNCTIO WAS MODIFIED SIGNIFICANTLY FROM THE ONE IN PYTHON DUE TO DIFFERENCES IN THE FUNCTIONS NROW() AND LENGTH() USED IN R
validate_length<-function(var1, var2){
  #Check if the length of two variables is the same
  if (is.null(nrow(var1))){
    if(length(var1) == nrow(var2)){
      return(TRUE)
    } else {
      warning("Lengths of data and column names do not match. Assigning default values.")
      return(FALSE)
    }
  }
  if (is.null(nrow(var2))){
    if(nrow(var1) == length(var2)){
      return(TRUE)
    } else {
      warning("Lengths of data and column names do not match. Assigning default values.")
      return(FALSE)
    }
  }
  if (nrow(var1) == nrow(var2)){
    return(TRUE)
  } else {
    warning("Lengths of data and column names do not match. Assigning default values.")
    return(FALSE)
  }
}


#Try validate_length function
validate_length(data3, data1) #Returns TRUE
validate_length(data3, data2) #Returns FALSE

#Define get_cohort_names function
get_cohort_names<-function(data, cnames){
  #Return cohort names for the dataframes
  n = nrow(data) #CHECK IF WE SHOULD USE NROW OR LENGTH, PYTHON CODE USES LEN() WHICH IS EQUIVALENT TO NROW IN R
  if (is.null(cnames)){
    return(get_default_names(n))
  }
  if (validate_length(data, cnames) == TRUE){
    cohort_names <- cnames
  } else {
    cohort_names <- get_default_names(n)
  }
  return(cohort_names)
}

#Try get_cohort_names function
cnames <- c('Condition A', 'Condition B', 'Condition C')
get_cohort_names(data4, NULL) #Returns default cohort names
get_cohort_names(data2, cnames) #Returns provided cnames
get_cohort_names(data4, cnames) #Returns default cohort names because lengths do not match

#Define prepare_dataframe_list
prepare_dataframe_list<-function(df,pid,colname,cnames){
  #Converts the inputs to a list of dataframes for each cohort.
  if (is.null(df)){
    if (is.null(pid)){
      stop("At least dataframe or patient_id must be Null")
    }
    #Validate if pid is a list of lists
    if (typeof(pid) == 'list'){
      print("An IF / ELSE statement is missing here using fetch function from neuroblu")
    } else {
      stop("patient_id must be of type list")
      cohort_names <- get_cohort_names(list_df, cnames)
    }
  } else {
    if (is.null(pid) == FALSE){
      stop("Both use_cols and exclude_cols cannot take values.")
    }
    #"Another IF statement goes here"
    if(typeof(df) == 'list'){
      list_df <- df
      cohort_names <- assign_cohort_name(cnames, length(list_df)) #CHECK IF WE SHOULD USE LENGTH OR NROW
    } else {
      prepare_dataframe(df, colname, cnames)
    }
  } #return()
}

#Try prepare_dataframe_list
cnames <- c('Condition A', 'Condition B') #Define cname
colname <- 'cohort-label' #Define name of the column that has the cohorts of interest 
x <- prepare_dataframe_list(data4, NULL, colname, cnames)
x

