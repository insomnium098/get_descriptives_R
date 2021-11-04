library(dplyr)
data("PlantGrowth")

source("../../R/get-descriptives.R")

nStats <- c("meansd", "medianIQR")
labelsStats <- list(meansd = "Mean (SD)", medianIQR = "Median (IQR)")

# Supress the warnings derived from testing the same tables
options(warn = -1)

test_that("Single dataframe with numerical variables", {
  varOutput <- generateDescriptives(mtcars)
  cars <- mtcars
  cars$COHORT_ASSIGNED <- "Cohort-1"
  varExpected <- summary(tableby(COHORT_ASSIGNED~., data = cars, numeric.test = "wt",
                                 cat.test = "chisq",
                                 numeric.stats = nStats, total = FALSE,
                                 cat.stats = c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text = TRUE,
                         pfootnote = TRUE)
  expect_identical(varExpected, varOutput)
})

test_that("Single dataframe with both numerical and categorical variables", {
  varOutput <- generateDescriptives(PlantGrowth)
  plantsDf <- PlantGrowth
  plantsDf$COHORT_ASSIGNED <- "Cohort-1"
  varExpected <- summary(tableby(COHORT_ASSIGNED~., data = plantsDf, numeric.test = "wt",
                                 cat.test = "chisq",
                                 numeric.stats = nStats, total = FALSE,
                                 cat.stats = c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text = TRUE,
                         pfootnote = TRUE)
  expect_identical(varExpected, varOutput)
})


test_that("Single dataframe with cohortCol", {
  plants <- filter(PlantGrowth, group %in% c("trt1", "trt2"))
  varOutput <- generateDescriptives(plants, cohortCol = "group")
  varExpected <- summary(tableby(group~., data = plants, numeric.test = "wt",
                                 cat.test = "chisq",
                                 numeric.stats = nStats, total = FALSE,
                                 cat.stats = c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text = TRUE,
                         pfootnote = TRUE)
  expect_identical(varExpected, varOutput)
})

test_that("Single dataframe with cohortCol and > 2 cohort groups", {

  varOutput <- generateDescriptives(PlantGrowth, cohortCol = "group")
  varExpected <- summary(tableby(group~., data = PlantGrowth,
                                 cat.test = "chisq",
                                 numeric.stats = nStats, total = FALSE,
                                 cat.stats = c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text = TRUE,
                         pfootnote = TRUE)
  expect_identical(varExpected, varOutput)
})

test_that("Two dataframes with cohortCol", {
  plants <- filter(PlantGrowth, group %in% c("trt1", "trt2"))
  varOutput <- generateDescriptives(list(list(plants), plants), cohortCol = "group")
  varExpected <- summary(tableby(group~., data = rbind(plants, plants), numeric.test = "wt",
                                 cat.test = "chisq",
                                 numeric.stats = nStats, total = FALSE,
                                 cat.stats = c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text = TRUE,
                         pfootnote = TRUE)
  expect_identical(varExpected, varOutput)
})

test_that("Two dataframes with numerical variables, no cohort names", {
  mtcarsOne <- mtcars
  mtcarsTwo <- mtcars
  mtcarsOne$COHORT_ASSIGNED <- "Cohort-1"
  mtcarsTwo$COHORT_ASSIGNED <- "Cohort-2"

  varOutput <- generateDescriptives(list(list(mtcars), mtcars))
  varExpected <- summary(tableby(COHORT_ASSIGNED ~., data = rbind(mtcarsOne,
                                                                  mtcarsTwo), numeric.test = "wt",
                                 cat.test = "chisq",
                                 numeric.stats = nStats, total = FALSE,
                                 cat.stats = c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text = TRUE,
                         pfootnote = TRUE)
  expect_identical(varExpected, varOutput)
})

test_that("Two dataframes with numerical variables, cohort names defined", {
  mtcarsOne <- mtcars
  mtcarsTwo <- mtcars
  mtcarsOne$COHORT_ASSIGNED <- "Test1"
  mtcarsTwo$COHORT_ASSIGNED <- "Test2"

  varOutput <- generateDescriptives(list(list(mtcars), mtcars), cohortNames = c("Test1", "Test2"))
  varExpected <- summary(tableby(COHORT_ASSIGNED ~., data = rbind(mtcarsOne,
                                                                  mtcarsTwo), numeric.test = "wt",
                                 cat.test = "chisq",
                                 numeric.stats = nStats, total = FALSE,
                                 cat.stats = c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text = TRUE,
                         pfootnote = TRUE)
  expect_identical(varExpected, varOutput)
})

test_that("Single list of patient ids", {
  patientsDf <- getDemographics(c(100380024, 148050025, 168270025))
  columnsFilter <- c('gender', 'birth_year', 'city', 'marital_status', 'years_in_education')
  varOutput <- generateDescriptives(patientsDf, useCols = columnsFilter)
  patientsDfFiltered <- subset(patientsDf, select = columnsFilter)
  patientsDfFiltered$COHORT_ASSIGNED <- "Cohort-1"
  varExpected <- summary(tableby(COHORT_ASSIGNED~., data = patientsDfFiltered,numeric.test = "wt",
                                 cat.test = "chisq",
                                 numeric.stats = nStats , total = FALSE,
                                 cat.stats=c("countpct"),
                                 stats.labels = labelsStats), digits = 2,
                         dig.count = 2, dig.pct = 2,
                         dig.p = 2, text=TRUE,
                         pfootnote=TRUE)

  expect_identical(varExpected, varOutput)
  expect_identical(varExpected, varOutput)
})

test_that("CheckInput single dataframe", {
  expect_identical(.checkInput(PlantGrowth), PlantGrowth)
})

test_that("iterPrepareDataframeCols single dataframe no params", {
  expect_identical(.iterPrepareDataframeCols(PlantGrowth), PlantGrowth)
})

test_that("iterPrepareDataframeCols single dataframe useCols", {
  expect_identical(.iterPrepareDataframeCols(mtcars,
                                             useCols = c("mpg", "cyl")),
                   mtcars[, 1:2])
})

test_that("iterPrepareDataframeCols single dataframe excludeCols", {
  mtcarsExclude <- subset(mtcars, select = -c(mpg, cyl))
  expect_identical(.iterPrepareDataframeCols(mtcars,
                                             excludeCols = c("mpg", "cyl")),
                   mtcarsExclude)
})

test_that("iterPrepareDataframeCols multiple dataframes no params", {
  listDfs <- list(list(mtcars), mtcars)
  mDf <- as.data.frame(listDfs[2])
  actualVar <- .iterPrepareDataframeCols(listDfs)
  expect_identical(actualVar[[2]],
                   mDf)
})

test_that("iterPrepareDataframeCols multiple dataframes useCols", {
  dfInclude <- mtcars[, 1:2]
  listDfs <- list(list(dfInclude), dfInclude)
  mDf <- as.data.frame(listDfs[2])
  actualVar <- .iterPrepareDataframeCols(listDfs,
                                         useCols = c("mpg", "cyl"))
  expect_identical(actualVar[[2]],
                   mDf)
})

test_that("iterPrepareDataframeCols multiple dataframes excludeCols", {
  mtcarsExclude <- subset(mtcars, select = -c(mpg, cyl))
  listDfs <- list(list(mtcarsExclude), mtcarsExclude)
  mDf <- as.data.frame(listDfs[2])
  actualVar <- .iterPrepareDataframeCols(listDfs,
                                         excludeCols = c("mpg", "cyl"))
  expect_identical(actualVar[[2]],
                   mDf)
})

test_that(".getDefaultNames", {
  varExpected <- c("Cohort-1", "Cohort-2")
  expect_identical(.getDefaultNames(2),
                   varExpected)
})

test_that(".prepareDataframe no params", {
  cars1 <- mtcars
  cars1$COHORT_ASSIGNED <- c("Cohort-1")
  cars2 <- mtcars
  cars2$COHORT_ASSIGNED <- c("Cohort-2")
  carsRbind <- rbind(cars1, cars2)
  listDfs <- list(list(mtcars), mtcars)

  expect_identical(.prepareDataframe(listDfs),
                   carsRbind)
})

test_that(".determineNumericTest cohortCol null", {
  carsRbind <- rbind(mtcars, mtcars)
  listDfs <- list(list(mtcars), mtcars)

  expect_identical(.prepareDataframe(listDfs, cohortCol = "gear"),
                   carsRbind)
})

test_that(".determineNumericTest cohortCol NULL ", {
  expect_identical(.determineNumericTest(mtcars, cohortCol = NULL),
                   "wt")
})

test_that(".determineNumericTest cohortCol NULL and COHORT_ASSIGNED two groups ", {
  cars1 <- mtcars
  cars1$COHORT_ASSIGNED <- c("Cohort-1")
  cars2 <- mtcars
  cars2$COHORT_ASSIGNED <- c("Cohort-2")
  cars3 <- mtcars
  cars3$COHORT_ASSIGNED <- c("Cohort-3")
  carsRbind <- rbind(cars1, cars2)
  carsRbind <- rbind(carsRbind, cars3)

  expect_identical(.determineNumericTest(carsRbind, cohortCol = NULL),
                   "anova")
})

test_that(".determineNumericTest cohortCol > 2 groups ", {
  expect_identical(.determineNumericTest(mtcars, cohortCol = "gear"),
                   "anova")
})

test_that(".determineNumericTest cohortCol < 2 groups ", {
  expect_identical(.determineNumericTest(mtcars, cohortCol = "gear"),
                   "anova")
})

test_that(".determineNumericTest cohortCol < 2 groups ", {
  expect_identical(.determineNumericTest(mtcars, cohortCol = "am"),
                   "wt")
})

test_that(".getContinousStatAgg", {
  expect_identical(.getContinousStatAgg("both"),
                   c("meansd", "medianIQR"))
  expect_identical(.getContinousStatAgg("mean"),
                   c("meansd"))
  expect_identical(.getContinousStatAgg("median"),
                   c("medianIQR"))

})

test_that(".getFormula", {

  expect_identical(class(.getFormula(mtcars, cohortCol = "gear")),
                   "formula")
  expect_identical(as.character(.getFormula(mtcars, cohortCol = "gear")),
                   c("~", "gear", "."))
  expect_identical(as.character(.getFormula(mtcars)),
                   c("~", "."))

  cars1 <- mtcars
  cars1$COHORT_ASSIGNED <- c("Cohort-1")
  cars2 <- mtcars
  cars2$COHORT_ASSIGNED <- c("Cohort-2")
  cars3 <- mtcars
  carsRbind <- rbind(cars1, cars2)

  expect_identical(as.character(.getFormula(carsRbind)),
                   c("~", "COHORT_ASSIGNED", "."))

})



