library(arsenal)


run_arsenal <- function(df, cohort_col = NULL){

  
  formula <- get_formula(df, cohort_col)
  
  ##WT stands for Wilcoxon-test(alias Mann-Whitney)
  tab_results <- tableby(formula,data=df, numeric.test = "wt", cat.test = "chisq")
  return(tab_results)
  
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