library(dplyr)
data("PlantGrowth")

##Function to export as csv

export_csv <- function(df){
  return(write.csv(as.data.frame(df), row.names=F))
}

n_stats <- c("meansd", "medianIQR")
labels_stats <- list(meansd="Mean (SD)", medianIQR = "Median (IQR)")
##Supress the warnings derived from testing the same tables
options(warn=-1)
test_that("Single dataframe with numerical variables", {
  var_output <- get_descriptives(mtcars)
  cars <- mtcars
  cars$COHORT_ASSIGNED <- "Cohort-1"
  var_expected <- summary(tableby(COHORT_ASSIGNED~., data = cars,numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = n_stats , total = FALSE,
                                  cat.stats=c("countpct"),
                                  stats.labels = labels_stats), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(var_expected, var_output)
})

test_that("Single dataframe with both numerical and categorical variables", {
  var_output <- get_descriptives(PlantGrowth)
  plants_df <- PlantGrowth
  plants_df$COHORT_ASSIGNED <- "Cohort-1"
  var_expected <- summary(tableby(COHORT_ASSIGNED~., data = plants_df,numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = n_stats, total = FALSE,
                                  cat.stats=c("countpct"),
                                  stats.labels = labels_stats), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(var_expected, var_output)
})


test_that("Single dataframe with cohort_col", {
  plants <- filter(PlantGrowth, group %in% c("trt1", "trt2"))
  var_output <- get_descriptives(plants, cohort_col = "group")
  var_expected <- summary(tableby(group~., data = plants,numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = n_stats , total = FALSE,
                                  cat.stats=c("countpct"),
                                  stats.labels = labels_stats), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(var_expected, var_output)
})

test_that("Single dataframe with cohort_col and > 2 cohort groups", {

  var_output <- get_descriptives(PlantGrowth, cohort_col = "group")
  var_expected <- summary(tableby(group~., data = PlantGrowth,
                                  cat.test = "chisq",
                                  numeric.stats = n_stats, total = FALSE,
                                  cat.stats=c("countpct"),
                                  stats.labels = labels_stats), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(var_expected, var_output)
})

test_that("Two dataframes with cohort_col", {
  plants <- filter(PlantGrowth, group %in% c("trt1", "trt2"))
  var_output <- get_descriptives(list(list(plants), plants), cohort_col = "group")
  var_expected <- summary(tableby(group~., data = rbind(plants, plants),numeric.test = "wt",
                                  cat.test = "chisq",
                                  numeric.stats = n_stats, total = FALSE,
                                  cat.stats=c("countpct"),
                                  stats.labels = labels_stats), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(var_expected, var_output)
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
                                  numeric.stats = n_stats, total = FALSE,
                                  cat.stats=c("countpct"),
                                  stats.labels = labels_stats), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(var_expected, var_output)
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
                                  numeric.stats = n_stats , total = FALSE,
                                  cat.stats=c("countpct"),
                                  stats.labels = labels_stats), digits = 2,
                          dig.count = 2, dig.pct = 2,
                          dig.p = 2, text=TRUE,
                          pfootnote=TRUE)
  expect_identical(var_expected, var_output)
})

test_that("Check_input single dataframe", {
  expect_identical(check_input(PlantGrowth), PlantGrowth)
})

