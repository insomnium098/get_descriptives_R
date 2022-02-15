# -------------Median IQR-----------#
# By default, Arsenal doesn't implement the Median (IQR) function.
# To overcome this, we created the function and then we added
# it to arsenal namespace
medianIQR <- function(x, na.rm = TRUE, weights = NULL, ...) {

  y <- if (na.rm && allNA(x)) {
    NA_real_
  }
  else {
    m <- wtd.quantile(x, weights = weights, probs = c(0.5),
                      na.rm = na.rm)
    iqrA <- diff(wtd.quantile(x, weights = weights,
                              probs = c(0.25, 0.75), na.rm = na.rm))
    if (is.Date(x))
      list(as.character(m), as.difftime(s, units = "days"))
    else c(m, iqrA)
  }
  as.tbstat(y, parens = c("(", ")"))
}

# We assign the medianIQR function to arsenal
environment(medianIQR) <- asNamespace("arsenal")

# Modifying also the wilcoxon test (Mann whitney) so it can support ordinal variables
# The original wilcoxon test needs the values to be as.numerical(), however in the ordinal
# variables, the values are as.factor. We added a line to convert the values
# to as.numerical() before testing
wtOrdinal <- function(x, x.by, ..., wilcox.correct = FALSE, wilcox.exact = NULL, test.always = FALSE) {
  x <- as.numeric(x)
  tab <- table(is.na(x), x.by)
  if(ncol(tab) != 2) stop("The Wilcoxon Rank Sum test must have exactly two groups")

  if(!test.always && (any(tab[1, ] == 0) || any(colSums(tab) == 0))) {
    return(list(p.value=NA_real_, statistic.F=NA_real_, method="Wilcoxon rank sum test"))
  }

  stats::wilcox.test(x ~ as.factor(x.by), correct = wilcox.correct, exact = wilcox.exact)
}

environment(wtOrdinal) <- asNamespace("arsenal")
assignInNamespace("wt", wtOrdinal, ns = "arsenal")


# ---------------------------------#

.iterPrepareDataframeCols <- function(dfList, useCols = NULL,
                                      excludeCols = NULL,
                                      cohortCol = NULL) {
  # The input of the function is:
  # dfList : A list of dataframes
  # useCols: vector of characters, name of columns to use
  # excludeCols: vector of characters, name of columns to exclude
  # Returns: a List, where its first element is a list containing the
  # dataframes prepared (dfProc) and the second element is a list containing the
  # colnames of the dataframes (desVars)

  # """Iterates through the data list and calls .prepareDataframeCols()."""

  # r variable is a list that contains the dataframes
  # listColnames variable is a list that contains the colnames of the dataframes
  r <- list()
  listColnames <- list()

  # Case when its a single dataframe
  if (class(dfList) == "data.frame") {
    r <- .prepareDataframeCols(dfList, useCols, excludeCols, cohortCol)
  } else {
    for (i in 1:length(dfList)) {
      dfI <- as.data.frame(dfList[i])
      dfPrepared <- .prepareDataframeCols(dfI, useCols, excludeCols, cohortCol)
      r <- c(list(dfPrepared), r)
    }

  }

  # r contains the list of dataframes
  return(r)

}


.prepareDataframeCols <- function(dfVar, useCols = NULL, excludeCols = NULL,
                                  cohortCol = NULL) {
  # """Excludes/Includes required columns in the dataset"""
  if (is.null(useCols)) {
    if (is.null(excludeCols)) {
      return(dfVar)
    } else {
      return(dfVar[, !(colnames(dfVar) %in% excludeCols)])
    }
  } else if (is.null(excludeCols)) {
    if(is.null(cohortCol)){
      return(dfVar[, (colnames(dfVar) %in% useCols)])
    } else {
      return(dfVar[, (colnames(dfVar) %in%
                        c(useCols,cohortCol ))])
    }

  } else {
    stop("ValueError: Both useCols and excludeCols cannot take values")
  }
}



.getDefaultNames <- function(n) {
  # This function returns the default names of the cohort
  cohortList <- c()
  for (i in 1:n) {
    cohort <- paste0("Cohort-", i)
    cohortList <- append(cohortList, cohort)
  }
  return(cohortList)
}

.prepareDataframe <- function(listDataframes, cohortsNames = NULL,
                              cohortCol = NULL) {
  # This function receives a list dataframes and rbinds them. If the user
  # provides the vector cohortsNames with the same length as the list_datarames
  # the function will add a column indicating the cohort of each dataframe
  # If the listDataframes only conatains a single dataframe and no
  # cohortsNames , the function will add a column with a default cohort name
  # The column added to the dataframes specifyng the name of the cohort will be
  # COHORT_ASSIGNED


  if (class(listDataframes) == "data.frame") {
    listDataframes <- list(listDataframes)
  }

  listDataframes <- rev(listDataframes)

  if (is.null(cohortsNames)) {
    cohortsNames <- .getDefaultNames(length(listDataframes))
  }
  if (is.null(cohortCol) && length(listDataframes) == 1) {
    warning("UserWarning: Name of the cohort column is not specified.
            The data is assumed to be from a single cohort",
            call. = F)
  }
  if ((length(listDataframes) != length(cohortsNames)) &&
      !is.null(cohortsNames) && !is.null(cohortCol)) {
    stop("The number of dataframes is different")
  }
  # We first check if there is a single dataframe
  if (length(listDataframes) == 1) {
    finalDF <- as.data.frame(listDataframes[1])
    if (!is.null(cohortsNames) && is.null(cohortCol)) {
      if (length(cohortsNames) > 1) {
        finalDF$COHORT_ASSIGNED <- cohortsNames[1]
      } else {
        finalDF$COHORT_ASSIGNED <- cohortsNames
      }
    }

  } else {
    for (dfI in 1:length(listDataframes)) {
      dfTemp <- as.data.frame(listDataframes[dfI])
      if (is.null(cohortCol)) {
        dfTemp$COHORT_ASSIGNED<- cohortsNames[dfI]
      }

      if (!exists("finalDF")) {
        finalDF <- dfTemp
      } else{
        finalDF <- rbind(finalDF, dfTemp)
      }
    }
  }

  return(finalDF)
}


.runArsenal <- function(dfVar, cohortCol = NULL, continousStatAgg, dig,
                        continousTest, categoricalTest) {


  formula <- .getFormula(dfVar, cohortCol)

  contAgg <- .getContinousStatAgg(continousStatAgg)

  # Obtain the labels of the stats
  statsLabels <- .getStatsLabels(contAgg)

  # Obtaninig the numeric test
  numericTest <- .getNumericTest(dfVar, cohortCol, continousTest)

  catTest <- .getCategoricalTest(dfVar, categoricalTest)

  ordinalTest <- .getOrdinalTest(numericTest)


  # WT stands for Wilcoxon-test(alias Mann-Whitney)
  tabResults <- suppressWarnings(arsenal::tableby(formula,data = dfVar,
                                                  numeric.test = numericTest,
                                                  cat.test = catTest,
                                                  ordered.test = ordinalTest,
                                                  numeric.stats = contAgg, total = FALSE,
                                                  cat.stats = c("countpct"), stats.labels = statsLabels))


  output <- summary(tabResults, digits = dig, dig.count = 2, dig.pct = 2,
                    dig.p = 2, text = TRUE, pfootnote = TRUE)

  return(output)

}

.getNumericTest <- function(dfVar, cohortCol, continousTest) {

  if (continousTest == "auto") {
    return(.determineNumericTest(dfVar, cohortCol))
  } else {
    return(continousTest)
  }

}

.getCategoricalTest <- function(dfVar, categoricalTest) {

  if (categoricalTest == "auto") {
    return("chisq")
  } else {
    return(categoricalTest)
  }

}

.getOrdinalTest <- function(numericTest){

  if(numericTest == "wt"){
    return("wtOrdinal")
  } else {
    return("kwt")
  }


}

.getStatsLabels <- function(labels) {
  # Function that returns the labels that are displayed in the summarized table
  labelsStats <- list(meansd = "Mean (SD)", medianIQR = "Median (IQR)")
  return(labelsStats[labels])

}

.getContinousStatAgg <- function(var) {

  if (var == "both") {
    stat <- c("meansd", "medianIQR")
    return(stat)
  } else if (var == "mean") {
    return("meansd")
  } else if (var == "median") {
    return("medianIQR")
  }
}

.getFormula <- function(dfVar, cohortCol = NULL) {

  if(is.null(cohortCol) && (length(which(colnames(dfVar) == "COHORT_ASSIGNED")) == 0)) {
    formula <- arsenal::formulize("", ".")
  } else if((length(which(colnames(dfVar) == "COHORT_ASSIGNED")) != 0)) {
    formula <- arsenal::formulize("COHORT_ASSIGNED",".")
  } else {
    # The arsenal::formulize function does the paste and as.formula steps
    formula <- arsenal::formulize(cohortCol, ".")
  }
  return(formula)

}

.determineNumericTest <- function(dfVar, cohortCol) {
  # Check the number of groups. If < 2 then Wilcox-test is used for
  # numerical variables, else anova is used

  if (is.null(cohortCol)) {
    indexCohortCol <- which(colnames(dfVar) == "COHORT_ASSIGNED")
    if (length(unique(dfVar[, indexCohortCol])) > 2) {
      return("anova")
    } else{
      return("wt")
    }
  } else{
    indexCohortCol <- which(colnames(dfVar) == cohortCol)
    if (length(unique(dfVar[, indexCohortCol])) > 2) {
      return("anova")
    } else{
      return("wt")
    }

  }
}

.demographicsDf <- function(cohort) {
  # This function takes in a cohort name and gets the corresponding patient IDs
  # to extract demographic variables and returns a dataframe ready to use in
  # generateDescriptives

  # Extract demographic variables
  birthYear <- getBirthYear(cohort)
  gender <- getGender(cohort)
  city <- getCity(cohort)
  educationYears <- getEducationYears(cohort)
  employment <- getEmployment(cohort)
  ethnicity <- getEthnicity(cohort)
  marital <- getMarital(cohort)
  race <- getRace(cohort)
  state <- getState(cohort)

  # List of variables to merge
  demographics <- list(birthYear, gender, city, educationYears,
                       employment, ethnicity,
                       marital, race, state)

  # Merge all remaining variables
  for (i in demographics) {
    i <- as.data.frame(i) # Change lists to dataframes
    if (!(exists("dfFinal"))) {
      dfFinal <- i
    } else {
      dfFinal <- merge(dfFinal, i, by = "person_id", all = TRUE)
    }
  }
  dfFinal <- dfFinal[, -c(1)] # Remove column 'person_id', which is the first in the dataframe
  return(dfFinal)
}

.checkInput <- function(userInput) {
  # Function that verifies if the input is an
  # integer, list of integers or dataframe
  # If the input is integer or list of integer
  # it will parse them to obtain the demographics dataframe
  if (class(userInput) == "numeric") {
    return(.demographicsDf(userInput))
  } else if ('tbl_df' %in% class(userInput)) {
    return(as.data.frame(userInput))
  } else if ('tbl'  %in% class(userInput)) {
    return(as.data.frame(userInput))
  } else if ('data.frame'  %in% class(starwars)) {
    return(userInput)
  } else if (class(userInput[[1]]) == "numeric") {
    return(.parseDemographicList(userInput))
  } else {
    return(userInput)
  }
}

.parseDemographicList <- function(userInput) {
  # Function that receives a list of integers (person_id)
  # Making a Dataframe of demographics for each of them
  finalList <- list()
  for (x in userInput) {
    dfDemo <- .demographicsDf(x)
    finalList  <- c(list(dfDemo), finalList)
  }

  return(finalList)
}

.checkColumnExist <- function(dfVar, column_name){
  # Function that checks if a column name exists in a dataframe
  # Returns a boolean
  var_exists <- grep(column_name, colnames(dfVar))
  if(length(var_exists) == 0){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

.getColumnIndex <- function(dfVar, column_name){
  # Function that returns the index of a column name
  # Returns the index of the column
  var_exists <- grep(column_name, colnames(dfVar))
  return(var_exists)
}



.prepareColumnTypes <- function(dfVar, column_types){
  # Function that prepares the columns as the column_types specified
  if(is.null(column_types)){
    return(dfVar)
  }

  for (i in 1:length(column_types)){
    # Check if the column exists
    if(.checkColumnExist(dfVar, names(column_types)[i])){
      # Get the index of the column in the df
      index_col <- .getColumnIndex(dfVar, names(column_types)[i])
      # get the values of the list
      val_list <- column_types[i]
      # replace the values in the columns
      dfVar[,index_col] <- val_list
    }

    return(dfVar)

  }


}

#' Build an ordinal type.
#'
#' @description
#' This function returns a column of a dataframe as an ordered factor (ordinal)
#' @param df_column A vector such as the column of a dataframe
#' @return A vector as an ordered factor
#' @examples
#' build_ordinal_type(mtcars$mpg)
#' @export
build_ordinal_type <- function(df_column){
  return(ordered(df_column))
}

#' Generates descriptive table for the specified list of dataframes.
#'
#' @description
#' This function is used to obtain descriptive statistics for a given list of dataframes
#' or patient ids. The summary table created can be viewed as a markdown table on the stdout or optionally export to csv.
#' @param listDataframes a single dataframe or a list of dataframes
#' @param cohortCol character vector indicating the column name of listDataframes to
#' be used for cohort assignment
#' @param cohortNames character vector indicating the names of the cohorts
#' @param useCols character vector indicating the names of the columns to be used
#' @param excludeCols character vector indicating the names of the columns to be excluded
#' @param column_types List indicating the names of the columns and the type of the variables
#' @param continuousStatAgg character indicating in the continous variables
#' to be summarized with the Mean (SD), Median (IQR), or both in consecutive rows.
#' Possible values: "mean", "median", "both"
#' @param continousTest character indicating the test for the continous variables
#' possible values: "auto", "anova" , "wt".
#' @param categoricalTest character indicating the test for the categorical variables
#' possible values: "auto", "chisq" , "fe".
#' @param dig Integer indicating the number of decimal places to be displayed.
#' @param csv csv: Boolean, indicating if the output should be exported as a CSV. By default
#' the output is not exported.
#' @return A table with the described statistics
#' @examples
#' library(NeuroBlu)
#' library(survival)
#' generateDescriptives(diabetic, cohortCol = "trt", useCols = c("age", "eye", "risk", "time", "trt"), csv = T)
#'
#' For ordinal variables:
#'
#'test <- subset(mockstudy, select = c(sex, age))
#'age_ordinal <- build_ordinal_type(test$age)
#'list_types <- list("age" = age_ordinal)
#'generateDescriptives(test, cohortCol = "sex",column_types = list_types)
#' @export

generateDescriptives <- function(listDataframes,
                                 cohortCol = NULL,
                                 cohortNames = NULL,
                                 useCols = NULL,
                                 excludeCols = NULL,
                                 column_types = NULL,
                                 continousStatAgg ="both",
                                 continousTest = "auto",
                                 categoricalTest = "auto",
                                 dig = 2,
                                 csv = F) {


  # We first check if the user provided ids of patients or a dataframe
  dfVar <- .checkInput(listDataframes)


  # Next we prepare de dataframe with the use and exclude columns
  dfVar <- .iterPrepareDataframeCols(dfVar, useCols, excludeCols, cohortCol)

  # Next we prepare the dataframe with the cohortsNames
  dfVar <- .prepareDataframe(dfVar, cohortNames, cohortCol)

  # Process the column types
  dfVar <- .prepareColumnTypes(dfVar, column_types)

  # Next we run the stats analysis

  dfVar <- .runArsenal(dfVar, cohortCol, continousStatAgg, dig,
                       continousTest, categoricalTest)

  if (csv == T) {
    # We now format dfVar to export it as csv file
    write.csv(as.data.frame(dfVar), 'statTable.csv', row.names = F)
  }



  return(dfVar)
}
