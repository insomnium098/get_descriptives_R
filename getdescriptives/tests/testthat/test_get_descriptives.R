library(dplyr)
data("PlantGrowth")

source("../../R/get_descriptives.R")

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
  expect_identical(.check_input(PlantGrowth), PlantGrowth)
})

test_that("iter_prepare_dataframe_cols single dataframe no params", {
  expect_identical(.iter_prepare_dataframe_cols(PlantGrowth), PlantGrowth)
})

test_that("iter_prepare_dataframe_cols single dataframe use_cols", {
  expect_identical(.iter_prepare_dataframe_cols(mtcars,
                                                use_cols = c("mpg", "cyl")),
                   mtcars[,1:2])
})

test_that("iter_prepare_dataframe_cols single dataframe exclude_cols", {
  mtcars_exclude <- subset(mtcars, select = -c(mpg, cyl))
  expect_identical(.iter_prepare_dataframe_cols(mtcars,
                                                exclude_cols = c("mpg", "cyl")),
                   mtcars_exclude)
})

test_that("iter_prepare_dataframe_cols multiple dataframes no params", {
  list_dfs <- list(list(mtcars), mtcars)
  m_df <- as.data.frame(list_dfs[2])
  actual_var <- .iter_prepare_dataframe_cols(list_dfs)
  expect_identical(actual_var[[2]],
                   m_df)
})

test_that("iter_prepare_dataframe_cols multiple dataframes use_cols", {
  df_include <- mtcars[,1:2]
  list_dfs <- list(list(df_include), df_include)
  m_df <- as.data.frame(list_dfs[2])
  actual_var <- .iter_prepare_dataframe_cols(list_dfs,
                                             use_cols = c("mpg", "cyl"))
  expect_identical(actual_var[[2]],
                   m_df)
})

test_that("iter_prepare_dataframe_cols multiple dataframes exclude_cols", {
  mtcars_exclude <- subset(mtcars, select = -c(mpg, cyl))
  list_dfs <- list(list(mtcars_exclude), mtcars_exclude)
  m_df <- as.data.frame(list_dfs[2])
  actual_var <- .iter_prepare_dataframe_cols(list_dfs,
                                             exclude_cols = c("mpg", "cyl"))
  expect_identical(actual_var[[2]],
                   m_df)
})

test_that(".get_default_names", {
  var_expected <- c("Cohort-1", "Cohort-2")
  expect_identical(.get_default_names(2),
                   var_expected)
})

test_that(".prepare_dataframe no params", {
  cars1 <- mtcars
  cars1$COHORT_ASSIGNED <- c("Cohort-1")
  cars2 <- mtcars
  cars2$COHORT_ASSIGNED <- c("Cohort-2")
  cars_rbind <- rbind(cars1, cars2)
  list_dfs <- list(list(mtcars), mtcars)

  expect_identical(.prepare_dataframe(list_dfs),
                   cars_rbind)
})

test_that(".determine_numeric_test cohort_col null", {
  cars_rbind <- rbind(mtcars, mtcars)
  list_dfs <- list(list(mtcars), mtcars)

  expect_identical(.prepare_dataframe(list_dfs, cohort_col = "gear"),
                   cars_rbind)
})

test_that(".determine_numeric_test cohort_col NULL ", {
  expect_identical(.determine_numeric_test(mtcars, cohort_col = NULL),
                   "wt")
})

test_that(".determine_numeric_test cohort_col NULL and COHORT_ASSIGNED two groups ", {
  cars1 <- mtcars
  cars1$COHORT_ASSIGNED <- c("Cohort-1")
  cars2 <- mtcars
  cars2$COHORT_ASSIGNED <- c("Cohort-2")
  cars3 <- mtcars
  cars3$COHORT_ASSIGNED <- c("Cohort-3")
  cars_rbind <- rbind(cars1, cars2)
  cars_rbind <- rbind(cars_rbind, cars3)

  expect_identical(.determine_numeric_test(cars_rbind, cohort_col = NULL),
                   "anova")
})

test_that(".determine_numeric_test cohort_col > 2 groups ", {
  expect_identical(.determine_numeric_test(mtcars, cohort_col = "gear"),
                   "anova")
})

test_that(".determine_numeric_test cohort_col < 2 groups ", {
  expect_identical(.determine_numeric_test(mtcars, cohort_col = "gear"),
                   "anova")
})

test_that(".determine_numeric_test cohort_col < 2 groups ", {
  expect_identical(.determine_numeric_test(mtcars, cohort_col = "am"),
                   "wt")
})

test_that(".get_continous_stat_agg", {
  expect_identical(.get_continous_stat_agg("both"),
                   c("meansd", "medianIQR"))
  expect_identical(.get_continous_stat_agg("mean"),
                   c("meansd"))
  expect_identical(.get_continous_stat_agg("median"),
                   c("medianIQR"))

})

test_that(".get_formula", {

  expect_identical(class(.get_formula(mtcars, cohort_col = "gear")),
                   "formula")
  expect_identical(as.character(.get_formula(mtcars, cohort_col = "gear")),
                   c("~", "gear", "."))
  expect_identical(as.character(.get_formula(mtcars)),
                   c("~", "."))

  cars1 <- mtcars
  cars1$COHORT_ASSIGNED <- c("Cohort-1")
  cars2 <- mtcars
  cars2$COHORT_ASSIGNED <- c("Cohort-2")
  cars3 <- mtcars
  cars_rbind <- rbind(cars1, cars2)

  expect_identical(as.character(.get_formula(cars_rbind)),
                   c("~", "COHORT_ASSIGNED", "."))

})




