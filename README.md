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


|                | Cohort-1 (N=32) |
|:---------------|:---------------:|
|mpg             |                 |
|-  Mean (SD)    |  20.09 (6.03)   |
|-  Median (IQR) |  19.20 (7.38)   |
|cyl             |                 |
|-  Mean (SD)    |   6.19 (1.79)   |
|-  Median (IQR) |   6.00 (4.00)   |
|disp            |                 |
|-  Mean (SD)    | 230.72 (123.94) |
|-  Median (IQR) | 196.30 (205.18) |
|hp              |                 |
|-  Mean (SD)    | 146.69 (68.56)  |
|-  Median (IQR) | 123.00 (83.50)  |
|drat            |                 |
|-  Mean (SD)    |   3.60 (0.53)   |
|-  Median (IQR) |   3.70 (0.84)   |
|wt              |                 |
|-  Mean (SD)    |   3.22 (0.98)   |
|-  Median (IQR) |   3.33 (1.03)   |
|qsec            |                 |
|-  Mean (SD)    |  17.85 (1.79)   |
|-  Median (IQR) |  17.71 (2.01)   |
|vs              |                 |
|-  Mean (SD)    |   0.44 (0.50)   |
|-  Median (IQR) |   0.00 (1.00)   |
|am              |                 |
|-  Mean (SD)    |   0.41 (0.50)   |
|-  Median (IQR) |   0.00 (1.00)   |
|gear            |                 |
|-  Mean (SD)    |   3.69 (0.74)   |
|-  Median (IQR) |   4.00 (1.00)   |
|carb            |                 |
|-  Mean (SD)    |   2.81 (1.62)   |
|-  Median (IQR) |   2.00 (2.00)   |


```

```
data("PlantGrowth")
get_descriptives(PlantGrowth)
```

```
|                | Cohort-1 (N=30) |
|:---------------|:---------------:|
|weight          |                 |
|-  Mean (SD)    |   5.07 (0.70)   |
|-  Median (IQR) |   5.15 (0.98)   |
|group           |                 |
|-  ctrl         |   10 (33.3%)    |
|-  trt1         |   10 (33.3%)    |
|-  trt2         |   10 (33.3%)    |
```
#### Single cohort, > 2 groups
```
get_descriptives(PlantGrowth, cohort_col = "group")
```

```
|                | ctrl (N=10) | trt1 (N=10) | trt2 (N=10) |   p value|
|:---------------|:-----------:|:-----------:|:-----------:|---------:|
|weight          |             |             |             | 0.016 (1)|
|-  Mean (SD)    | 5.03 (0.58) | 4.66 (0.79) | 5.53 (0.44) |          |
|-  Median (IQR) | 5.15 (0.74) | 4.55 (0.66) | 5.44 (0.47) |          |
1. Linear Model ANOVA
```

#### Two cohorts


```
get_descriptives(list(list(mtcars[1:10,]), mtcars[11:20,]))
```
```
|                | Cohort-1 (N=10) | Cohort-2 (N=10) |   p value|
|:---------------|:---------------:|:---------------:|---------:|
|mpg             |                 |                 | 0.256 (1)|
|-  Mean (SD)    |  19.89 (8.92)   |  20.37 (2.91)   |          |
|-  Median (IQR) |  16.85 (12.42)  |  21.00 (3.63)   |          |
|cyl             |                 |                 | 0.260 (1)|
|-  Mean (SD)    |   6.60 (1.90)   |   5.80 (1.48)   |          |
|-  Median (IQR) |   8.00 (3.50)   |   6.00 (1.50)   |          |
|disp            |                 |                 | 0.472 (1)|
|-  Mean (SD)    | 259.25 (159.56) | 208.61 (90.37)  |          |
|-  Median (IQR) | 275.80 (298.02) | 163.80 (99.73)  |          |
|hp              |                 |                 | 0.306 (1)|
|-  Mean (SD)    | 149.60 (67.46)  | 122.80 (51.45)  |          |
|-  Median (IQR) | 180.00 (118.50) | 110.00 (22.25)  |          |
|drat            |                 |                 | 0.879 (1)|
|-  Mean (SD)    |   3.55 (0.69)   |   3.54 (0.44)   |          |
|-  Median (IQR) |   3.15 (0.97)   |   3.77 (0.73)   |          |
|wt              |                 |                 | 0.198 (1)|
|-  Mean (SD)    |   3.67 (1.43)   |   3.13 (0.41)   |          |
|-  Median (IQR) |   3.75 (2.44)   |   3.20 (0.50)   |          |
|qsec            |                 |                 | 0.940 (1)|
|-  Mean (SD)    |  18.30 (0.87)   |  18.58 (2.14)   |          |
|-  Median (IQR) |  17.99 (1.15)   |  18.45 (2.84)   |          |
|vs              |                 |                 | 0.383 (1)|
|-  Mean (SD)    |   0.40 (0.52)   |   0.60 (0.52)   |          |
|-  Median (IQR) |   0.00 (1.00)   |   1.00 (1.00)   |          |
|am              |                 |                 | 1.000 (1)|
|-  Mean (SD)    |   0.30 (0.48)   |   0.30 (0.48)   |          |
|-  Median (IQR) |   0.00 (0.75)   |   0.00 (0.75)   |          |
|gear            |                 |                 | 0.383 (1)|
|-  Mean (SD)    |   3.40 (0.52)   |   3.60 (0.52)   |          |
|-  Median (IQR) |   3.00 (1.00)   |   4.00 (1.00)   |          |
|carb            |                 |                 | 0.553 (1)|
|-  Mean (SD)    |   2.90 (1.20)   |   2.50 (1.35)   |          |
|-  Median (IQR) |   3.00 (1.75)   |   2.00 (2.75)   |          |
1. Wilcoxon rank sum test
```

```
get_descriptives(list(list(PlantGrowth), PlantGrowth), cohort_names = c("plant1", "plant2"))
```

```
|                | plant1 (N=30) | plant2 (N=30) |   p value|
|:---------------|:-------------:|:-------------:|---------:|
|weight          |               |               | 1.000 (1)|
|-  Mean (SD)    |  5.07 (0.70)  |  5.07 (0.70)  |          |
|-  Median (IQR) |  5.15 (0.98)  |  5.15 (0.98)  |          |
|group           |               |               | 1.000 (2)|
|-  ctrl         |  10 (33.3%)   |  10 (33.3%)   |          |
|-  trt1         |  10 (33.3%)   |  10 (33.3%)   |          |
|-  trt2         |  10 (33.3%)   |  10 (33.3%)   |          |
1. Wilcoxon rank sum test
2. Pearson's Chi-squared test
```
