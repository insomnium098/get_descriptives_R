library(dplyr)
data("PlantGrowth")

##Function to export as csv

export_csv <- function(df){
  return(write.csv(as.data.frame(df), row.names=F))
}

test_that("Single dataframe with numerical variables", {
  var_output <- get_descriptives(mtcars)
  var_expected <- summary(tableby(~., data = mtcars,numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = c("meansd", "median"), total = FALSE,
                                  cat.stats=c("countpct")), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(export_csv(var_expected), var_output)
})

test_that("Single dataframe with both numerical and categorical variables", {
  var_output <- get_descriptives(PlantGrowth)
  var_expected <- summary(tableby(~., data = PlantGrowth,numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = c("meansd", "median"), total = FALSE,
                                  cat.stats=c("countpct")), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(export_csv(var_expected), var_output)
})


test_that("Single dataframe with cohort_col", {
  plants <- filter(PlantGrowth, group %in% c("trt1", "trt2"))
  var_output <- get_descriptives(plants, cohort_col = "group")
  var_expected <- summary(tableby(group~., data = plants,numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = c("meansd", "median"), total = FALSE,
                                  cat.stats=c("countpct")), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(export_csv(var_expected), var_output)
})

test_that("Single dataframe with cohort_col and > 2 cohort groups", {

  var_output <- get_descriptives(PlantGrowth, cohort_col = "group")
  var_expected <- summary(tableby(group~., data = PlantGrowth,
                                  cat.test = "chisq",
                                  numeric.stats = c("meansd", "median"), total = FALSE,
                                  cat.stats=c("countpct")), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(export_csv(var_expected), var_output)
})

test_that("Two dataframes with cohort_col", {
  plants <- filter(PlantGrowth, group %in% c("trt1", "trt2"))
  var_output <- get_descriptives(list(list(plants), plants), cohort_col = "group")
  var_expected <- summary(tableby(group~., data = rbind(plants, plants),numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = c("meansd", "median"), total = FALSE,
                                  cat.stats=c("countpct")), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(export_csv(var_expected), var_output)
})

test_that("Two dataframes with numerical variables, no cohort names", {
  mtcars_one <- mtcars
  mtcars_two <- mtcars
  mtcars_one$COHORT_ASSIGNED <- "Cohort-1"
  mtcars_two$COHORT_ASSIGNED <- "Cohort-2"

  var_output <- get_descriptives(list(list(mtcars), mtcars))
  var_expected <- summary(tableby(COHORT_ASSIGNED ~., data = rbind(mtcars_one,
                                                                   mtcars_two),numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = c("meansd", "median"), total = FALSE,
                                  cat.stats=c("countpct")), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(export_csv(var_expected), var_output)
})

test_that("Two dataframes with numerical variables, cohort names defined", {
  mtcars_one <- mtcars
  mtcars_two <- mtcars
  mtcars_one$COHORT_ASSIGNED <- "Test1"
  mtcars_two$COHORT_ASSIGNED <- "Test2"

  var_output <- get_descriptives(list(list(mtcars), mtcars), cohort_names = c("Test1", "Test2"))
  var_expected <- summary(tableby(COHORT_ASSIGNED ~., data = rbind(mtcars_one,
                                                                   mtcars_two),numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = c("meansd", "median"), total = FALSE,
                                  cat.stats=c("countpct")), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(export_csv(var_expected), var_output)
})

test_that("Check_input single dataframe", {
  expect_identical(check_input(PlantGrowth), PlantGrowth)
})

