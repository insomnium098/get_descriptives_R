# R version of get_descriptives for NeuroBlu

### General Info
***
This package mimics the python version of get_descriptives of NeuroBlu.
It receives a single dataframe or a list of dataframes and generates 
a table with the descriptive stats of the variables.

Continuous are compared between cohorts using Mann-Whitney U test while
Categorical and ordinal variables with Chi-squared test.

### Installation

```
devtools::install_github("insomnium098/get_descriptives_R/getdescriptives")
```

### Example

#### Single cohort

```
get_descriptives(mtcars)
```

```

|             | Cohort-1 (N=32) |  Total (N=32)   |
|:------------|:---------------:|:---------------:|
|mpg          |                 |                 |
|-  Mean (SD) |  20.09 (6.03)   |  20.09 (6.03)   |
|-  Median    |      19.20      |      19.20      |
|cyl          |                 |                 |
|-  Mean (SD) |   6.19 (1.79)   |   6.19 (1.79)   |
|-  Median    |      6.00       |      6.00       |
|disp         |                 |                 |
|-  Mean (SD) | 230.72 (123.94) | 230.72 (123.94) |
|-  Median    |     196.30      |     196.30      |
|hp           |                 |                 |
|-  Mean (SD) | 146.69 (68.56)  | 146.69 (68.56)  |
|-  Median    |     123.00      |     123.00      |
|drat         |                 |                 |
|-  Mean (SD) |   3.60 (0.53)   |   3.60 (0.53)   |
|-  Median    |      3.70       |      3.70       |
|wt           |                 |                 |
|-  Mean (SD) |   3.22 (0.98)   |   3.22 (0.98)   |
|-  Median    |      3.33       |      3.33       |
|qsec         |                 |                 |
|-  Mean (SD) |  17.85 (1.79)   |  17.85 (1.79)   |
|-  Median    |      17.71      |      17.71      |
|vs           |                 |                 |
|-  Mean (SD) |   0.44 (0.50)   |   0.44 (0.50)   |
|-  Median    |      0.00       |      0.00       |
|am           |                 |                 |
|-  Mean (SD) |   0.41 (0.50)   |   0.41 (0.50)   |
|-  Median    |      0.00       |      0.00       |
|gear         |                 |                 |
|-  Mean (SD) |   3.69 (0.74)   |   3.69 (0.74)   |
|-  Median    |      4.00       |      4.00       |
|carb         |                 |                 |
|-  Mean (SD) |   2.81 (1.62)   |   2.81 (1.62)   |
|-  Median    |      2.00       |      2.00       |
```

#### Two cohorts


```
get_descriptives(list(list(mtcars[1:10,]), mtcars[11:20,]))
```
```
|             | Cohort-1 (N=10) | Cohort-2 (N=10) |  Total (N=20)   | p value|
|:------------|:---------------:|:---------------:|:---------------:|-------:|
|mpg          |                 |                 |                 |   0.256|
|-  Mean (SD) |  19.89 (8.92)   |  20.37 (2.91)   |  20.13 (6.46)   |        |
|-  Median    |      16.85      |      21.00      |      18.95      |        |
|cyl          |                 |                 |                 |   0.260|
|-  Mean (SD) |   6.60 (1.90)   |   5.80 (1.48)   |   6.20 (1.70)   |        |
|-  Median    |      8.00       |      6.00       |      6.00       |        |
|disp         |                 |                 |                 |   0.472|
|-  Mean (SD) | 259.25 (159.56) | 208.61 (90.37)  | 233.93 (128.86) |        |
|-  Median    |     275.80      |     163.80      |     196.30      |        |
|hp           |                 |                 |                 |   0.306|
|-  Mean (SD) | 149.60 (67.46)  | 122.80 (51.45)  | 136.20 (59.99)  |        |
|-  Median    |     180.00      |     110.00      |     116.50      |        |
|drat         |                 |                 |                 |   0.879|
|-  Mean (SD) |   3.55 (0.69)   |   3.54 (0.44)   |   3.54 (0.56)   |        |
|-  Median    |      3.15       |      3.77       |      3.46       |        |
|wt           |                 |                 |                 |   0.198|
|-  Mean (SD) |   3.67 (1.43)   |   3.13 (0.41)   |   3.40 (1.06)   |        |
|-  Median    |      3.75       |      3.20       |      3.44       |        |
|qsec         |                 |                 |                 |   0.940|
|-  Mean (SD) |  18.30 (0.87)   |  18.58 (2.14)   |  18.44 (1.59)   |        |
|-  Median    |      17.99      |      18.45      |      18.15      |        |
|vs           |                 |                 |                 |   0.383|
|-  Mean (SD) |   0.40 (0.52)   |   0.60 (0.52)   |   0.50 (0.51)   |        |
|-  Median    |      0.00       |      1.00       |      0.50       |        |
|am           |                 |                 |                 |   1.000|
|-  Mean (SD) |   0.30 (0.48)   |   0.30 (0.48)   |   0.30 (0.47)   |        |
|-  Median    |      0.00       |      0.00       |      0.00       |        |
|gear         |                 |                 |                 |   0.383|
|-  Mean (SD) |   3.40 (0.52)   |   3.60 (0.52)   |   3.50 (0.51)   |        |
|-  Median    |      3.00       |      4.00       |      3.50       |        |
|carb         |                 |                 |                 |   0.553|
|-  Mean (SD) |   2.90 (1.20)   |   2.50 (1.35)   |   2.70 (1.26)   |        |
|-  Median    |      3.00       |      2.00       |      3.00       |        |
```
