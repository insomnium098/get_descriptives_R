library(arsenal)


run_arsenal <- function(df, cohort_col = NULL, continous_stat_agg){

  
  formula <- get_formula(df, cohort_col)
  
  cont_agg <- get_continous_stat_agg(continous_stat_agg)
  
  ##WT stands for Wilcoxon-test(alias Mann-Whitney)
  tab_results <- tableby(formula,data=df, numeric.test = "wt", cat.test = "chisq",
                         numeric.stats = cont_agg)
  return(tab_results)
  
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