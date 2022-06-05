MSDS_6372_Project1
================
Miguel Bonilla, Reuven Derner, Milan Patel, Tamas Toth
2022-05-27

#### Loading the necessary R libraries for the analysis

``` r
# Load the necessary libraries
library(knitr)
library(rmarkdown)
library(ggpubr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(e1071)
library(class)
library(caret)
library(stringr)
library(sjPlot)
library(data.table)
library(reshape2)
library(corrplot)
library(naivebayes)
library(car)
library(egg)
library(rworldmap)
library(Hmisc)
library(DataExplorer)
library(selectiveInference)
library(dlookr)
```

``` r
# Turn off scientific notation
options(scipen = 100, digits = 4)
```

#### Read the data

``` r
#Read the data
#setwd('/Users/ttoth76/Downloads/SMU/Semester_2/DS 6372 Applied Statistics_Inference & Modeling/FLS/Project1_Summer2022/GitContent/LifeExpectancy')
LifeExp = read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Life_Expectancy_Data.csv', header = TRUE, sep = ",")
# take a sample of 15 from the dataframe
LifeExp_sample = sample_n(LifeExp, 5)
knitr::kable(LifeExp_sample, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Country
</th>
<th style="text-align:right;">
Year
</th>
<th style="text-align:left;">
Status
</th>
<th style="text-align:right;">
Life.expectancy
</th>
<th style="text-align:right;">
Adult.Mortality
</th>
<th style="text-align:right;">
infant.deaths
</th>
<th style="text-align:right;">
Alcohol
</th>
<th style="text-align:right;">
percentage.expenditure
</th>
<th style="text-align:right;">
Hepatitis.B
</th>
<th style="text-align:right;">
Measles
</th>
<th style="text-align:right;">
BMI
</th>
<th style="text-align:right;">
under.five.deaths
</th>
<th style="text-align:right;">
Polio
</th>
<th style="text-align:right;">
Total.expenditure
</th>
<th style="text-align:right;">
Diphtheria
</th>
<th style="text-align:right;">
HIV.AIDS
</th>
<th style="text-align:right;">
GDP
</th>
<th style="text-align:right;">
Population
</th>
<th style="text-align:right;">
thinness..1.19.years
</th>
<th style="text-align:right;">
thinness.5.9.years
</th>
<th style="text-align:right;">
Income.composition.of.resources
</th>
<th style="text-align:right;">
Schooling
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Lesotho
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
47.8
</td>
<td style="text-align:right;">
592
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2.75
</td>
<td style="text-align:right;">
91.854
</td>
<td style="text-align:right;">
88
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
28.8
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
8.85
</td>
<td style="text-align:right;">
88
</td>
<td style="text-align:right;">
27.3
</td>
<td style="text-align:right;">
934.43
</td>
<td style="text-align:right;">
199993
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
0.447
</td>
<td style="text-align:right;">
10.7
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:right;">
2011
</td>
<td style="text-align:left;">
Developed
</td>
<td style="text-align:right;">
82.0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
6.98
</td>
<td style="text-align:right;">
5439.692
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
5189
</td>
<td style="text-align:right;">
61.5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
9.27
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
38334.68
</td>
<td style="text-align:right;">
59379449
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
0.872
</td>
<td style="text-align:right;">
16.4
</td>
</tr>
<tr>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
55.0
</td>
<td style="text-align:right;">
388
</td>
<td style="text-align:right;">
542
</td>
<td style="text-align:right;">
9.55
</td>
<td style="text-align:right;">
104.475
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
2613
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
863
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
4.47
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
5.2
</td>
<td style="text-align:right;">
1136.83
</td>
<td style="text-align:right;">
14641724
</td>
<td style="text-align:right;">
12.3
</td>
<td style="text-align:right;">
12.3
</td>
<td style="text-align:right;">
0.477
</td>
<td style="text-align:right;">
9.1
</td>
</tr>
<tr>
<td style="text-align:left;">
Estonia
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
71.2
</td>
<td style="text-align:right;">
211
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11.48
</td>
<td style="text-align:right;">
7.806
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
54.6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
4.84
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
538.35
</td>
<td style="text-align:right;">
137935
</td>
<td style="text-align:right;">
2.3
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
0.791
</td>
<td style="text-align:right;">
15.4
</td>
</tr>
<tr>
<td style="text-align:left;">
Senegal
</td>
<td style="text-align:right;">
2010
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
64.3
</td>
<td style="text-align:right;">
212
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
1.020
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
428
</td>
<td style="text-align:right;">
21.7
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
4.62
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
0.4
</td>
<td style="text-align:right;">
11.63
</td>
<td style="text-align:right;">
12916229
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
0.449
</td>
<td style="text-align:right;">
7.7
</td>
</tr>
</tbody>
</table>

#### Address the missing values in each column (NA as well as empty strings).

``` r
# Address the missing values in each column (NA as well as empty strings).
missing_df = as.data.frame(sapply(LifeExp, function(x) sum(is.na(x))))
colnames(missing_df) = c("variable missing")
knitr::kable(missing_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
variable missing
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Country
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Year
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Status
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Life.expectancy
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Adult.Mortality
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
infant.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Alcohol
</td>
<td style="text-align:right;">
194
</td>
</tr>
<tr>
<td style="text-align:left;">
percentage.expenditure
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hepatitis.B
</td>
<td style="text-align:right;">
553
</td>
</tr>
<tr>
<td style="text-align:left;">
Measles
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
BMI
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
under.five.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Polio
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Total.expenditure
</td>
<td style="text-align:right;">
226
</td>
</tr>
<tr>
<td style="text-align:left;">
Diphtheria
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
HIV.AIDS
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
GDP
</td>
<td style="text-align:right;">
448
</td>
</tr>
<tr>
<td style="text-align:left;">
Population
</td>
<td style="text-align:right;">
652
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness..1.19.years
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness.5.9.years
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
Income.composition.of.resources
</td>
<td style="text-align:right;">
167
</td>
</tr>
<tr>
<td style="text-align:left;">
Schooling
</td>
<td style="text-align:right;">
163
</td>
</tr>
</tbody>
</table>

``` r
empty_string_df = as.data.frame(sapply(LifeExp, function(x) sum(x == "")))
colnames(empty_string_df) = c("variable empty")
knitr::kable(empty_string_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
variable empty
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Country
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Year
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Status
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Life.expectancy
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Adult.Mortality
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
infant.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Alcohol
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
percentage.expenditure
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hepatitis.B
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Measles
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
BMI
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
under.five.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Polio
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Total.expenditure
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Diphtheria
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
HIV.AIDS
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
GDP
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Population
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness..1.19.years
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness.5.9.years
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Income.composition.of.resources
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Schooling
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

``` r
#set random seed
set.seed(329)
```

``` r
# Function to Identify different characteristics of the data frame 
# Getting a concise summary of the dataframe: str()
# Listing the column labels of the dataframe: colnames()
# Size of the dataset: dim()
# # Verify if there is any negative values in the dataset
dfinfo = function(df_name)
  {
  df_structure = str(df_name)
  df_colnames = colnames(df_name)
  df_dimensions = dim(df_name)
  df_neg = print(paste("Negative values in the Data Frame:", 
                       sapply(df_name, function(x) sum(x < 0))))
  outparam = list(df_structure, df_colnames, df_dimensions, df_neg)
  return (outparam)
}
```

``` r
dfinfo(LifeExp)
```

    ## 'data.frame':    2938 obs. of  22 variables:
    ##  $ Country                        : chr  "Afghanistan" "Afghanistan" "Afghanistan" "Afghanistan" ...
    ##  $ Year                           : int  2015 2014 2013 2012 2011 2010 2009 2008 2007 2006 ...
    ##  $ Status                         : chr  "Developing" "Developing" "Developing" "Developing" ...
    ##  $ Life.expectancy                : num  65 59.9 59.9 59.5 59.2 58.8 58.6 58.1 57.5 57.3 ...
    ##  $ Adult.Mortality                : int  263 271 268 272 275 279 281 287 295 295 ...
    ##  $ infant.deaths                  : int  62 64 66 69 71 74 77 80 82 84 ...
    ##  $ Alcohol                        : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.03 0.02 0.03 ...
    ##  $ percentage.expenditure         : num  71.3 73.5 73.2 78.2 7.1 ...
    ##  $ Hepatitis.B                    : int  65 62 64 67 68 66 63 64 63 64 ...
    ##  $ Measles                        : int  1154 492 430 2787 3013 1989 2861 1599 1141 1990 ...
    ##  $ BMI                            : num  19.1 18.6 18.1 17.6 17.2 16.7 16.2 15.7 15.2 14.7 ...
    ##  $ under.five.deaths              : int  83 86 89 93 97 102 106 110 113 116 ...
    ##  $ Polio                          : int  6 58 62 67 68 66 63 64 63 58 ...
    ##  $ Total.expenditure              : num  8.16 8.18 8.13 8.52 7.87 9.2 9.42 8.33 6.73 7.43 ...
    ##  $ Diphtheria                     : int  65 62 64 67 68 66 63 64 63 58 ...
    ##  $ HIV.AIDS                       : num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
    ##  $ GDP                            : num  584.3 612.7 631.7 670 63.5 ...
    ##  $ Population                     : num  33736494 327582 31731688 3696958 2978599 ...
    ##  $ thinness..1.19.years           : num  17.2 17.5 17.7 17.9 18.2 18.4 18.6 18.8 19 19.2 ...
    ##  $ thinness.5.9.years             : num  17.3 17.5 17.7 18 18.2 18.4 18.7 18.9 19.1 19.3 ...
    ##  $ Income.composition.of.resources: num  0.479 0.476 0.47 0.463 0.454 0.448 0.434 0.433 0.415 0.405 ...
    ##  $ Schooling                      : num  10.1 10 9.9 9.8 9.5 9.2 8.9 8.7 8.4 8.1 ...
    ##  [1] "Negative values in the Data Frame: 0" 
    ##  [2] "Negative values in the Data Frame: 0" 
    ##  [3] "Negative values in the Data Frame: 0" 
    ##  [4] "Negative values in the Data Frame: NA"
    ##  [5] "Negative values in the Data Frame: NA"
    ##  [6] "Negative values in the Data Frame: 0" 
    ##  [7] "Negative values in the Data Frame: NA"
    ##  [8] "Negative values in the Data Frame: 0" 
    ##  [9] "Negative values in the Data Frame: NA"
    ## [10] "Negative values in the Data Frame: 0" 
    ## [11] "Negative values in the Data Frame: NA"
    ## [12] "Negative values in the Data Frame: 0" 
    ## [13] "Negative values in the Data Frame: NA"
    ## [14] "Negative values in the Data Frame: NA"
    ## [15] "Negative values in the Data Frame: NA"
    ## [16] "Negative values in the Data Frame: 0" 
    ## [17] "Negative values in the Data Frame: NA"
    ## [18] "Negative values in the Data Frame: NA"
    ## [19] "Negative values in the Data Frame: NA"
    ## [20] "Negative values in the Data Frame: NA"
    ## [21] "Negative values in the Data Frame: NA"
    ## [22] "Negative values in the Data Frame: NA"

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ##  [1] "Country"                         "Year"                           
    ##  [3] "Status"                          "Life.expectancy"                
    ##  [5] "Adult.Mortality"                 "infant.deaths"                  
    ##  [7] "Alcohol"                         "percentage.expenditure"         
    ##  [9] "Hepatitis.B"                     "Measles"                        
    ## [11] "BMI"                             "under.five.deaths"              
    ## [13] "Polio"                           "Total.expenditure"              
    ## [15] "Diphtheria"                      "HIV.AIDS"                       
    ## [17] "GDP"                             "Population"                     
    ## [19] "thinness..1.19.years"            "thinness.5.9.years"             
    ## [21] "Income.composition.of.resources" "Schooling"                      
    ## 
    ## [[3]]
    ## [1] 2938   22
    ## 
    ## [[4]]
    ##  [1] "Negative values in the Data Frame: 0" 
    ##  [2] "Negative values in the Data Frame: 0" 
    ##  [3] "Negative values in the Data Frame: 0" 
    ##  [4] "Negative values in the Data Frame: NA"
    ##  [5] "Negative values in the Data Frame: NA"
    ##  [6] "Negative values in the Data Frame: 0" 
    ##  [7] "Negative values in the Data Frame: NA"
    ##  [8] "Negative values in the Data Frame: 0" 
    ##  [9] "Negative values in the Data Frame: NA"
    ## [10] "Negative values in the Data Frame: 0" 
    ## [11] "Negative values in the Data Frame: NA"
    ## [12] "Negative values in the Data Frame: 0" 
    ## [13] "Negative values in the Data Frame: NA"
    ## [14] "Negative values in the Data Frame: NA"
    ## [15] "Negative values in the Data Frame: NA"
    ## [16] "Negative values in the Data Frame: 0" 
    ## [17] "Negative values in the Data Frame: NA"
    ## [18] "Negative values in the Data Frame: NA"
    ## [19] "Negative values in the Data Frame: NA"
    ## [20] "Negative values in the Data Frame: NA"
    ## [21] "Negative values in the Data Frame: NA"
    ## [22] "Negative values in the Data Frame: NA"

#### Generate summary statistics

``` r
# Generate summary statistics
summary(LifeExp)
```

    ##    Country               Year         Status          Life.expectancy
    ##  Length:2938        Min.   :2000   Length:2938        Min.   :36.3   
    ##  Class :character   1st Qu.:2004   Class :character   1st Qu.:63.1   
    ##  Mode  :character   Median :2008   Mode  :character   Median :72.1   
    ##                     Mean   :2008                      Mean   :69.2   
    ##                     3rd Qu.:2012                      3rd Qu.:75.7   
    ##                     Max.   :2015                      Max.   :89.0   
    ##                                                       NA's   :10     
    ##  Adult.Mortality infant.deaths       Alcohol      percentage.expenditure
    ##  Min.   :  1     Min.   :   0.0   Min.   : 0.01   Min.   :    0         
    ##  1st Qu.: 74     1st Qu.:   0.0   1st Qu.: 0.88   1st Qu.:    5         
    ##  Median :144     Median :   3.0   Median : 3.76   Median :   65         
    ##  Mean   :165     Mean   :  30.3   Mean   : 4.60   Mean   :  738         
    ##  3rd Qu.:228     3rd Qu.:  22.0   3rd Qu.: 7.70   3rd Qu.:  442         
    ##  Max.   :723     Max.   :1800.0   Max.   :17.87   Max.   :19480         
    ##  NA's   :10                       NA's   :194                           
    ##   Hepatitis.B      Measles            BMI       under.five.deaths
    ##  Min.   : 1.0   Min.   :     0   Min.   : 1.0   Min.   :   0     
    ##  1st Qu.:77.0   1st Qu.:     0   1st Qu.:19.3   1st Qu.:   0     
    ##  Median :92.0   Median :    17   Median :43.5   Median :   4     
    ##  Mean   :80.9   Mean   :  2420   Mean   :38.3   Mean   :  42     
    ##  3rd Qu.:97.0   3rd Qu.:   360   3rd Qu.:56.2   3rd Qu.:  28     
    ##  Max.   :99.0   Max.   :212183   Max.   :87.3   Max.   :2500     
    ##  NA's   :553                     NA's   :34                      
    ##      Polio      Total.expenditure   Diphtheria      HIV.AIDS    
    ##  Min.   : 3.0   Min.   : 0.37     Min.   : 2.0   Min.   : 0.10  
    ##  1st Qu.:78.0   1st Qu.: 4.26     1st Qu.:78.0   1st Qu.: 0.10  
    ##  Median :93.0   Median : 5.76     Median :93.0   Median : 0.10  
    ##  Mean   :82.5   Mean   : 5.94     Mean   :82.3   Mean   : 1.74  
    ##  3rd Qu.:97.0   3rd Qu.: 7.49     3rd Qu.:97.0   3rd Qu.: 0.80  
    ##  Max.   :99.0   Max.   :17.60     Max.   :99.0   Max.   :50.60  
    ##  NA's   :19     NA's   :226       NA's   :19                    
    ##       GDP           Population         thinness..1.19.years thinness.5.9.years
    ##  Min.   :     2   Min.   :        34   Min.   : 0.10        Min.   : 0.10     
    ##  1st Qu.:   464   1st Qu.:    195793   1st Qu.: 1.60        1st Qu.: 1.50     
    ##  Median :  1767   Median :   1386542   Median : 3.30        Median : 3.30     
    ##  Mean   :  7483   Mean   :  12753375   Mean   : 4.84        Mean   : 4.87     
    ##  3rd Qu.:  5911   3rd Qu.:   7420359   3rd Qu.: 7.20        3rd Qu.: 7.20     
    ##  Max.   :119173   Max.   :1293859294   Max.   :27.70        Max.   :28.60     
    ##  NA's   :448      NA's   :652          NA's   :34           NA's   :34        
    ##  Income.composition.of.resources   Schooling   
    ##  Min.   :0.00                    Min.   : 0.0  
    ##  1st Qu.:0.49                    1st Qu.:10.1  
    ##  Median :0.68                    Median :12.3  
    ##  Mean   :0.63                    Mean   :12.0  
    ##  3rd Qu.:0.78                    3rd Qu.:14.3  
    ##  Max.   :0.95                    Max.   :20.7  
    ##  NA's   :167                     NA's   :163

### Observations:

-   The dataset is comprised of 2938 observations and 22 variables
-   There are numerical and categorical variables (Country and Status)
    in the dataset
-   Column names have spaces and special characters that has been
    replaced by R with “.”
-   There are missing values or empty strings in the dataset
-   No duplicated records
-   ‘Life.expectancy’ is the dependent variable - There are 10 missing
    observations in the dependent variable
-   We need to predict Salary however there is no salary variable in the
    dataset but MonthlyIncome variable seems to be sufficient for this
    purpose.

### Scatterplots

``` r
#####################################################################################
#                        Scatter plots for checking linearity                       #
#####################################################################################

################### Linear - Linear ###################
LifeExp$Status = as.factor(LifeExp$Status)
#num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
#MLR_num_LE = LifeExp[, num_cols]
pairs(Life.expectancy~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-1.png)<!-- -->

``` r
pairs(Life.expectancy~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-2.png)<!-- -->

``` r
pairs(Life.expectancy~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-3.png)<!-- -->

``` r
pairs(Life.expectancy~HIV.AIDS+GDP+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-4.png)<!-- -->

``` r
pairs(Life.expectancy~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-5.png)<!-- -->

``` r
################### Linear - Log transformation ###################
LifeExp$Status = as.factor(LifeExp$Status)
#num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
#MLR_num_LE = LifeExp[, num_cols]
pairs(Life.expectancy~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-6.png)<!-- -->

``` r
pairs(Life.expectancy~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-7.png)<!-- -->

``` r
pairs(Life.expectancy~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-8.png)<!-- -->

``` r
pairs(Life.expectancy~log(HIV.AIDS)+log(GDP)+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-9.png)<!-- -->

``` r
pairs(Life.expectancy~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-10.png)<!-- -->

``` r
################### Log - Log transformation ###################
LifeExp$Status = as.factor(LifeExp$Status)
#num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
#MLR_num_LE = LifeExp[, num_cols]
pairs(log(Life.expectancy)~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-11.png)<!-- -->

``` r
pairs(log(Life.expectancy)~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-12.png)<!-- -->

``` r
pairs(log(Life.expectancy)~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-13.png)<!-- -->

``` r
pairs(log(Life.expectancy)~log(HIV.AIDS)+log(GDP)+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-14.png)<!-- -->

``` r
pairs(log(Life.expectancy)~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-15.png)<!-- -->

### Observations

We can observe linear relationship between Life Expectancy and the
following variables

-   Income composition of resources
-   Schooling
-   Log(HIV.AIDS)
-   Log(GDP)
-   Log(percentage.expenditure)
-   BMI
-   Year
-   Adult.Mortality

\#adding region column to do regional imputation

``` r
###rename ivory coast and remove parenthesis from country names
LifeExp$Country <- replace(LifeExp$Country,LifeExp$Country == "Côte d'Ivoire","Ivory Coast")
LifeExp$Country <-  str_replace(LifeExp$Country,"\\(.*\\)","")
LifeExp$Country <- trimws(LifeExp$Country,"right")

### add region using rworldmap package
regions <- rworldmap::countryRegions
regions <- regions[,c(2,6)]
colnames(regions) <- c("Country","Region")
regions$Country <- replace(regions$Country, regions$Country == "The Bahamas", "Bahamas")
regions$Country <- replace(regions$Country, regions$Country == "Brunei", "Brunei Darussalam")
regions$Country <- replace(regions$Country, regions$Country == "Cape Verde", "Cabo Verde")
regions$Country <- replace(regions$Country, regions$Country == "Republic of the Congo", "Congo")
regions$Country <- replace(regions$Country, regions$Country == "Czech Republic", "Czechia")
regions$Country <- replace(regions$Country, regions$Country == "North Korea", "Democratic People's Republic of Korea")
regions$Country <- replace(regions$Country, regions$Country == "Guinea Bissau", "Guinea-Bissau")
regions$Country <- replace(regions$Country, regions$Country == "Laos","Lao People's Democratic Republic")
regions$Country <- replace(regions$Country, regions$Country == "Federated States of Micronesia", "Micronesia")
regions$Country <- replace(regions$Country, regions$Country == "South Korea", "Republic of Korea")
regions$Country <- replace(regions$Country, regions$Country == "Moldova", "Republic of Moldova")
regions$Country <- replace(regions$Country, regions$Country == "Russia", "Russian Federation")
regions$Country <- replace(regions$Country, regions$Country == "Republic of Serbia", "Serbia")
regions$Country <- replace(regions$Country, regions$Country == "Syria", "Syrian Arab Republic")
regions$Country <- replace(regions$Country, regions$Country == "Macedonia", "The former Yugoslav republic of Macedonia")
regions$Country <- replace(regions$Country, regions$Country == "East Timor", "Timor-Leste")
regions$Country <- replace(regions$Country, regions$Country == "United Kingdom", "United Kingdom of Great Britain and Northern Ireland")
regions$Country <- replace(regions$Country, regions$Country == "Vietnam", "Viet Nam")

LifeExp <-  join(LifeExp,regions,by = "Country", type = 'left')
LifeExp$Country <- replace(LifeExp$Country, LifeExp$Country == "Micronesia", "Micronesia (Federated States of)")
LifeExp$Region <- as.factor(LifeExp$Region)
```

## Fixing the missing values by replacing with median

``` r
# Drop missing values from the dependent variable
LifeExp = LifeExp[!(is.na(LifeExp$Life.expectancy)),]

na_list = colnames(LifeExp)[apply(LifeExp, 2, anyNA)]
# LifeExp_db = apply(LifeExp[,colnames(LifeExp) %in% na_list],2,median,na.rm =  TRUE)
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Alcohol = replace(Alcohol,is.na(Alcohol), median(Alcohol, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Hepatitis.B = replace(Hepatitis.B,is.na(Hepatitis.B), median(Hepatitis.B, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(BMI = replace(BMI,is.na(BMI), median(BMI, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Polio = replace(Polio,is.na(Polio), median(Polio, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Total.expenditure = replace(Total.expenditure,is.na(Total.expenditure), median(Total.expenditure, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Diphtheria = replace(Diphtheria,is.na(Diphtheria), median(Diphtheria, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(GDP = replace(GDP,is.na(GDP), median(GDP, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Population = replace(Population,is.na(Population), median(Population, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(thinness..1.19.years = replace(thinness..1.19.years,is.na(thinness..1.19.years), median(thinness..1.19.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(thinness.5.9.years = replace(thinness.5.9.years,is.na(thinness.5.9.years), median(thinness.5.9.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Income.composition.of.resources = replace(Income.composition.of.resources,is.na(Income.composition.of.resources), median(Income.composition.of.resources, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Schooling = replace(Schooling,is.na(Schooling), median(Schooling, na.rm = TRUE)))

#repeat grouping by Region for countries which have only NAs
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Alcohol = replace(Alcohol,is.na(Alcohol), median(Alcohol, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Hepatitis.B = replace(Hepatitis.B,is.na(Hepatitis.B), median(Hepatitis.B, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(BMI = replace(BMI,is.na(BMI), median(BMI, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Polio = replace(Polio,is.na(Polio), median(Polio, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Total.expenditure = replace(Total.expenditure,is.na(Total.expenditure), median(Total.expenditure, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Diphtheria = replace(Diphtheria,is.na(Diphtheria), median(Diphtheria, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(GDP = replace(GDP,is.na(GDP), median(GDP, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Population = replace(Population,is.na(Population), median(Population, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(thinness..1.19.years = replace(thinness..1.19.years,is.na(thinness..1.19.years), median(thinness..1.19.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(thinness.5.9.years = replace(thinness.5.9.years,is.na(thinness.5.9.years), median(thinness.5.9.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Income.composition.of.resources = replace(Income.composition.of.resources,is.na(Income.composition.of.resources), median(Income.composition.of.resources, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Schooling = replace(Schooling,is.na(Schooling), median(Schooling, na.rm = TRUE)))
# convert the tibble to data frame
LifeExp = as.data.frame(LifeExp)

#impute USA values
#USA GDP
US_GDP <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4150786.csv',header = FALSE)
colnames(US_GDP) <- US_GDP[3,]
US_GDP <- rename(US_GDP,c("Country Name"="Country"))
US_GDP <- US_GDP[US_GDP$Country == "United States",c(1,45:60)]
US_GDP <- US_GDP %>% pivot_longer(!Country,names_to = "Year",values_to = "GDP2")
US_GDP$Country <- replace(US_GDP$Country, US_GDP$Country == "United States", "United States of America")
US_GDP$Year <- as.integer(US_GDP$Year)

LifeExp <- left_join(LifeExp,US_GDP, by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate(GDP = ifelse(LifeExp$Country == "United States of America", LifeExp$GDP2, LifeExp$GDP))

#US Schooling
US_Scho <- read.csv(file = "https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Expected%20years%20of%20schooling%20(years).csv",skip = 6,header = FALSE)
colnames(US_Scho) <- US_Scho[1,]
US_Scho$Country <- trimws(US_Scho$Country, which = "both")
US_Scho <- US_Scho[US_Scho$Country == "United States", colSums(is.na(US_Scho)) !=nrow(US_Scho)]
US_Scho <- US_Scho[,c(2,13:28)]
US_Scho <- US_Scho %>% pivot_longer(!Country,names_to = "Year",values_to = "Schooling2")
US_Scho$Year <- as.integer(US_Scho$Year)
US_Scho$Schooling2 <- as.numeric(US_Scho$Schooling2)
US_Scho$Country <- replace(US_Scho$Country, US_Scho$Country == "United States", "United States of America")


LifeExp <- left_join(LifeExp,US_Scho,by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate(Schooling =ifelse(LifeExp$Country == "United States of America",LifeExp$Schooling2,LifeExp$Schooling))

#US income composition
US_Inc <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Income%20index.csv',skip = 5, header = FALSE)
colnames(US_Inc) <- US_Inc[1,]
US_Inc$Country <- trimws(US_Inc$Country,which = "both")
US_Inc <- US_Inc[US_Inc$Country == "United States",colSums(is.na(US_Inc)) !=nrow(US_Inc)]
US_Inc <- US_Inc[,c(2,13:28)]
US_Inc <- US_Inc %>%  pivot_longer(!Country,names_to = "Year", values_to = "comp2")
US_Inc$Year <- as.integer(US_Inc$Year)
US_Inc$comp2 <- as.numeric(US_Inc$comp2)
US_Inc$Country <- replace(US_Inc$Country,US_Inc$Country == "United States", "United States of America")

LifeExp <- left_join(LifeExp,US_Inc,by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate("Income.composition.of.resources" = ifelse(LifeExp$Country == "United States of America", LifeExp$comp2,LifeExp$Income.composition.of.resources))

#Add population from external source
#data from UN in thousands 
pop_all <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/WPP2019_TotalPopulationBySex.csv')
pop_all <- pop_all %>% dplyr::select(Country = Location, Year = Time, Population2 = PopTotal) %>% mutate(Population2 = Population2*1000) %>% filter(Year %in% c(2000:2015))

#clean country names to match before merge
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Bolivia (Plurinational State of)", "Bolivia")
pop_all$Country <- replace(pop_all$Country,pop_all$Country == "Côte d'Ivoire", "Ivory Coast")
pop_all$Country <- replace(pop_all$Country,pop_all$Country == "Dem. People's Republic of Korea","Democratic People's Republic of Korea")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Iran (Islamic Republic of)", "Iran")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Micronesia (Fed. States of)", "Micronesia (Federated States of)")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Eswatini", "Swaziland")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "North Macedonia", "The former Yugoslav republic of Macedonia")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "United Kingdom", "United Kingdom of Great Britain and Northern Ireland")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Venezuela (Bolivarian Republic of)", "Venezuela")
LifeExp<- left_join(LifeExp,pop_all,by=c("Country","Year")) %>% mutate(Population = Population2)

#replace adult.mortality since there are clear mistakes with the data that could
#not be resolved with transformation of the variable
adlt_mort <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Adult_mort.csv',header = TRUE)
adlt_mort <- adlt_mort %>% dplyr::select("Country" = Location,"Year" = Period,"Adult.Mort2" = Value)
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Côte d’Ivoire","Ivory Coast")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Bolivia (Plurinational State of)","Bolivia")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Venezuela (Bolivarian Republic of)","Venezuela")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Iran (Islamic Republic of)", "Iran")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Eswatini", "Swaziland")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "The former Yugoslav Republic of Macedonia", "The former Yugoslav republic of Macedonia")

LifeExp <- left_join(LifeExp,adlt_mort, by=c("Country","Year"))
LifeExp <- LifeExp %>% 
  dplyr::mutate("Adult.Mortality" = Adult.Mort2)
#drop new variables
drop = c("GDP2","Schooling2","comp2", "Population2","Adult.Mort2")
LifeExp <- LifeExp[,!colnames(LifeExp) %in% drop]

### Transform countries to continents
library(countrycode)
LifeExp$Continent = countrycode(sourcevar = LifeExp[, "Country"], origin = "country.name", destination = "continent")
LifeExp$Continent = as.factor(LifeExp$Continent)

#LifeExpKNN = LifeExp
```

### Full Correlation Matrix for Linear Regression (Life.expectancy)

``` r
#####################################################################################
#      Full Correlation Matrix for Linear Regression (Life.expectancy)              #
#####################################################################################
# Filter for data to be included
#num_cols = LifeExp %>% dplyr::select(where(is.numeric)) %>% colnames()
#LifeExpcorr = LifeExp[,num_cols]
#corrplot(cor(LifeExpcorr), method = 'square', order = 'AOE', addCoef.col = 'black', 
#         cl.pos = 'n', col = COL2('BrBG'))

plot_correlate(LifeExp)
```

    ## Warning: 'plot_correlate' is deprecated.
    ## Use 'plot.correlate' instead.
    ## See help("Deprecated")

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-9-1.png" angle=90 style="display: block; margin: auto;" />

### Observations

-   Under five death and infant death are perfectly correlated. They are
    describing the same thing. One of the variable is redundant.
-   GDP and percentage expenditure are perfectly correlated. They are
    describing the same thing. One of the variable is redundant.
-   Schooling and Income Composition of Resources have a strong positive
    correlation
-   Life Expectancy and Adult Mortality are highly negatively correlated
-   Life Expectancy and HIV.AIDS are moderately correlated
-   Life Expectancy and BMI are moderately correlated
-   Life Expectancy and Schooling are highly correlated
-   Life Expectancy and Income Composition of Resources are highly
    correlated

## Uni-variate analysis

``` r
#####################################################################################
#                               Uni-variate analysis                                #
#####################################################################################
# Let's plot the summary statistics
# Univariate analysis
num_cols = LifeExp %>% dplyr::select(where(is.numeric)) %>% colnames()
num_cols_exclude = c('Year')
num_cols_plots = noquote(unlist(num_cols[!( num_cols %in% num_cols_exclude)]))
nrows = length(num_cols_plots)
for (i in num_cols_plots)
{
box_p = LifeExp %>%
  ggplot(aes(x="", y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  coord_flip() + theme_classic() + xlab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylab(i)
hist_p = LifeExp %>%
  ggplot() +
  geom_histogram(aes(x = .data[[i]], y = (..count..)/sum(..count..)),
                 position = "identity", bins = 30, 
                 fill = "sandybrown", color = "black") +
  ylab("Relative Frequency") +
  theme_classic() + xlab(i) + ggtitle(paste(i, "- Univariate Analysis")) + 
  theme(plot.title = element_text(hjust = 0.5))
egg::ggarrange(hist_p, box_p, heights = 2:1) 
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-19.png" angle=90 style="display: block; margin: auto;" />

### Categorical data plots

``` r
#####################################################################################
#                               Categorical data plots                              #
#####################################################################################
num_var = LifeExp %>% dplyr::select(where(is.numeric)) %>% colnames()
cat_cols = LifeExp %>% dplyr::select(where(is.factor)) %>% colnames()
num_ex = c('Year')
num_var_plots = noquote(unlist(num_var[!( num_var %in% num_ex)]))
# Plot all categorical variables
for (c in cat_cols)
{
  cat_plot = LifeExp %>% ggplot(aes(x= .data[[c]], group = 1)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent") +
    scale_y_continuous(labels = scales::percent) + theme(legend.position = "none") +
    ggtitle(paste(c, "Categorical Analysis")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # +
    #scale_fill_brewer(palette="Oranges")
    egg::ggarrange(cat_plot, ncol=2) 
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-3.png" angle=90 style="display: block; margin: auto;" />

## Bi-variate analysis with Status variable

``` r
#####################################################################################
#                     Bi-variate analysis with Status variable                      #
#####################################################################################
for (i in num_var_plots)
{
multibox = LifeExp %>%
  ggplot(aes(x=Status, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  xlab("Status") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Status bi-variate analysis")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-19.png" angle=90 style="display: block; margin: auto;" />

## Bi-variate analysis with Region variable

``` r
#####################################################################################
#                     Bi-variate analysis with Region variable                   #
#####################################################################################
for (i in num_var_plots)
{
multibox = LifeExp %>%
  ggplot(aes(x=Region, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  xlab("Region") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Region bi-variate analysis")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_fill_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-19.png" angle=90 style="display: block; margin: auto;" />
\## Overall life expectancy over time

``` r
#####################################################################################
#                          Overall life expectancy over time                        #
#####################################################################################

mean_LifeExp = LifeExp %>% group_by(Year) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))
mean_LifeExp_reg = LifeExp %>% group_by(Year, Region) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))
mean_LifeExp_cont = LifeExp %>% group_by(Year, Continent) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))

#Overall life expectancy
ggplot(data=mean_LifeExp, aes(x=Year, y=meanle)) +
  geom_line()+
  geom_point() +
   ggtitle("Mean life expactancy by year") +
  theme(plot.title = element_text(hjust = 0.5))+
   xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-14-1.png" angle=90 style="display: block; margin: auto;" />

``` r
#Overall life expectancy by Region
ggplot(data=mean_LifeExp_reg, aes(x=Year, y=meanle, group = Region)) +
  geom_line(aes(color=Region))+
  geom_point(aes(color=Region)) +
  ggtitle("Mean life expactancy by year by region") +
theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-14-2.png" angle=90 style="display: block; margin: auto;" />

``` r
#Overall life expectancy by Continent
ggplot(data=mean_LifeExp_cont, aes(x=Year, y=meanle, group = Continent)) +
  geom_line(aes(color=Continent))+
  geom_point(aes(color=Continent)) +
  ggtitle("Mean life expactancy by year by continent") +
theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-14-3.png" angle=90 style="display: block; margin: auto;" />

``` r
#####################################################################################
#                                     Data Preparation                              #
#####################################################################################

### Log Transform variables
LifeExp = mutate(LifeExp, log.HIV.AIDS = ifelse(HIV.AIDS==0, log(HIV.AIDS+1), log(HIV.AIDS)))
LifeExp = mutate(LifeExp, log.GDP = ifelse(GDP==0, log(GDP+1), log(GDP)))
LifeExp = mutate(LifeExp, log.percentage.expenditure = ifelse(percentage.expenditure==0, log(percentage.expenditure+1), log(percentage.expenditure)))



### Make country as factor
LifeExp$Country = as.factor(LifeExp$Country)
```

``` r
#####################################################################################
#                    Split the Data to Train and Test sets (85%-15%)               #
#####################################################################################
index<-sample(1:dim(LifeExp)[1],round(dim(LifeExp)[1]*0.85),replace=F)
train = LifeExp[index,]
test = LifeExp[-index,]

# Create training and test set for regression models
# Droping the variables which were log transformed, as well as country and region or countinent
#drop_for_reg = c(-1, -8, -16, -17, -27)
drop_for_reg = c("Country", "HIV.AIDS", "GDP", "percentage.expenditure", "Region")
rtrain = train[,!colnames(train) %in% drop_for_reg]
rtest = test[,!colnames(train) %in% drop_for_reg]
x=model.matrix(Life.expectancy~.,rtrain)[,-1]
y=rtrain$Life.expectancy
xtest = model.matrix(Life.expectancy~.,rtest)[,-1]
ytest = rtest$Life.expectancy

# Create training and test set for KNN model
drop_for_knn = c("log.HIV.AIDS", "log.GDP", "log.percentage.expenditure", "Region" ,"Continent")
ktrain = train[,!colnames(train) %in% drop_for_knn]
ktest = test[,!colnames(train) %in% drop_for_knn]
```

``` r
#####################################################################################
#                             EDA on Train sets                                    #
#####################################################################################

describe(rtrain)
```

    ## # A tibble: 20 × 26
    ##    described_variables         n    na     mean      sd se_mean     IQR skewness
    ##    <chr>                   <int> <int>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
    ##  1 Year                     2489     0  2.01e+3 4.59e+0 9.20e-2 7   e+0 -0.00819
    ##  2 Life.expectancy          2489     0  6.91e+1 9.61e+0 1.93e-1 1.27e+1 -0.629  
    ##  3 Adult.Mortality          2489     0  1.96e+2 1.16e+2 2.33e+0 1.46e+2  1.33   
    ##  4 infant.deaths            2489     0  3.18e+1 1.22e+2 2.44e+0 2.3 e+1  9.33   
    ##  5 Alcohol                  2489     0  4.56e+0 4.04e+0 8.09e-2 6.71e+0  0.621  
    ##  6 Hepatitis.B              2489     0  8.02e+1 2.45e+1 4.91e-1 2.1 e+1 -1.82   
    ##  7 Measles                  2489     0  2.54e+3 1.18e+4 2.37e+2 4.1 e+2  9.27   
    ##  8 BMI                      2489     0  3.80e+1 2.00e+1 4.01e-1 3.67e+1 -0.227  
    ##  9 under.five.deaths        2489     0  4.41e+1 1.66e+2 3.33e+0 3   e+1  9.09   
    ## 10 Polio                    2489     0  8.23e+1 2.35e+1 4.71e-1 2   e+1 -2.06   
    ## 11 Total.expenditure        2489     0  5.91e+0 2.46e+0 4.93e-2 3.18e+0  0.595  
    ## 12 Diphtheria               2489     0  8.22e+1 2.36e+1 4.74e-1 1.9 e+1 -2.06   
    ## 13 Population               2489     0  3.83e+7 1.43e+8 2.87e+6 2.30e+7  7.98   
    ## 14 thinness..1.19.years     2489     0  4.92e+0 4.45e+0 8.92e-2 5.6 e+0  1.70   
    ## 15 thinness.5.9.years       2489     0  4.93e+0 4.53e+0 9.08e-2 5.6 e+0  1.78   
    ## 16 Income.composition.of.…  2489     0  6.28e-1 2.09e-1 4.19e-3 2.88e-1 -1.08   
    ## 17 Schooling                2489     0  1.20e+1 3.35e+0 6.71e-2 4.3 e+0 -0.570  
    ## 18 log.HIV.AIDS             2489     0 -1.21e+0 1.62e+0 3.26e-2 2.08e+0  1.27   
    ## 19 log.GDP                  2489     0  7.47e+0 1.79e+0 3.60e-2 2.29e+0 -0.196  
    ## 20 log.percentage.expendi…  2489     0  3.92e+0 2.75e+0 5.52e-2 4.59e+0 -0.0746 
    ## # … with 18 more variables: kurtosis <dbl>, p00 <dbl>, p01 <dbl>, p05 <dbl>,
    ## #   p10 <dbl>, p20 <dbl>, p25 <dbl>, p30 <dbl>, p40 <dbl>, p50 <dbl>,
    ## #   p60 <dbl>, p70 <dbl>, p75 <dbl>, p80 <dbl>, p90 <dbl>, p95 <dbl>,
    ## #   p99 <dbl>, p100 <dbl>

## Observations

\*\* A High degree of skewness can be identified in Infant Deaths \*\* A
High degree of skewness can be identified in Measles \*\* A High degree
of skewness can be identified in under.five.deaths \*\* A High degree of
skewness can be identified in Population

``` r
normality(rtrain) 
```

    ## # A tibble: 20 × 4
    ##    vars                            statistic  p_value sample
    ##    <chr>                               <dbl>    <dbl>  <dbl>
    ##  1 Year                                0.948 7.20e-29   2489
    ##  2 Life.expectancy                     0.956 9.84e-27   2489
    ##  3 Adult.Mortality                     0.890 7.28e-39   2489
    ##  4 infant.deaths                       0.239 2.12e-72   2489
    ##  5 Alcohol                             0.908 2.33e-36   2489
    ##  6 Hepatitis.B                         0.723 1.44e-53   2489
    ##  7 Measles                             0.212 4.17e-73   2489
    ##  8 BMI                                 0.926 2.73e-33   2489
    ##  9 under.five.deaths                   0.247 3.40e-72   2489
    ## 10 Polio                               0.692 2.12e-55   2489
    ## 11 Total.expenditure                   0.978 3.71e-19   2489
    ## 12 Diphtheria                          0.690 1.58e-55   2489
    ## 13 Population                          0.226 9.42e-73   2489
    ## 14 thinness..1.19.years                0.841 1.64e-44   2489
    ## 15 thinness.5.9.years                  0.836 5.27e-45   2489
    ## 16 Income.composition.of.resources     0.916 4.14e-35   2489
    ## 17 Schooling                           0.980 5.30e-18   2489
    ## 18 log.HIV.AIDS                        0.712 3.11e-54   2489
    ## 19 log.GDP                             0.992 1.21e-10   2489
    ## 20 log.percentage.expenditure          0.940 1.09e-30   2489

``` r
#Runs a Shapario-Wilk Tests, if the p-value is >= .05 then the data is normally distrusted, if <0.05 the data is not normally distrusted.

#Find Features that are not normally distributed 

train %>%
  normality() %>%
  filter(p_value < 0.05) %>%
  arrange(abs(p_value))
```

    ## # A tibble: 23 × 4
    ##    vars                   statistic  p_value sample
    ##    <chr>                      <dbl>    <dbl>  <dbl>
    ##  1 Measles                    0.212 4.17e-73   2489
    ##  2 Population                 0.226 9.42e-73   2489
    ##  3 infant.deaths              0.239 2.12e-72   2489
    ##  4 under.five.deaths          0.247 3.40e-72   2489
    ##  5 HIV.AIDS                   0.359 5.46e-69   2489
    ##  6 percentage.expenditure     0.399 9.65e-68   2489
    ##  7 GDP                        0.543 1.47e-62   2489
    ##  8 Diphtheria                 0.690 1.58e-55   2489
    ##  9 Polio                      0.692 2.12e-55   2489
    ## 10 log.HIV.AIDS               0.712 3.11e-54   2489
    ## # … with 13 more rows

``` r
# Verify non normality and transformation options of the variability 
plot_normality(rtrain)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-1.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-2.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-3.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-4.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-5.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-6.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-7.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-8.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-9.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-10.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-11.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-12.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-13.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-14.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-15.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-16.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-17.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-18.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-19.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-20.png)<!-- -->

## Observations

\*\* The p-value of the Population is less than 0.0001 signifying it is
non normally and should be transformed if utilized in the following
models.  
\*\* The p-value of the Measles is less than 0.0001 signifying it is non
normally and should be transformed if utilized in the following models.
\*\* The p-value of the infant.deaths is less than 0.0001 signifying it
is non normally and should be transformed if utilized in the following
models. \*\* The p-value of the under.five.deaths is less than 0.0001
signifying it is non normally and should be transformed if utilized in
the following models. \*\* The p-value of the Hepatitis.B is less than
0.0001 signifying it is non normally and should be transformed if
utilized in the following models. \*\* The p-value of the Diphtheria is
less than 0.0001 signifying it is non normally and should be transformed
if utilized in the following models. \*\* The p-value of the Polio is
less than 0.0001 signifying it is non normally and should be transformed
if utilized in the following models. \*\* The p-value of the
log.HIV.AIDS is less than 0.0001 signifying it is non normally and
should be transformed if utilized in the following models. \*\* The
p-value of the thinness.5.9.years is less than 0.0001 signifying it is
non normally and should be transformed if utilized in the following
models. \*\* The p-value of the thinness..1.19.years is less than 0.0001
signifying it is non normally and should be transformed if utilized in
the following models.

##################################################################################### 

# Modeling

##################################################################################### 

``` r
#Prediction function
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

### Evaluation Data Frame
### test_ASE, R-squared/Adjusted R-squared
eval_train_df = data.frame(model_name = character(11), MSE=numeric(11), R_Squared = numeric(11), AdjR_Squared = numeric(11), RMSE = numeric(11))
#eval_train_df = eval_train_df %>% dplyr::mutate(ID = row_number())

eval_test_df = data.frame(model_name = character(11), MSE=numeric(11), R_Squared = numeric(11), AdjR_Squared = numeric(11), RMSE = numeric(11))
#eval_test_df = eval_test_df %>% dplyr::mutate(ID = row_number())


#####################################################################################
#                                       Lasso                                       #
#####################################################################################
library(glmnet)

grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(x,y,alpha=1, lambda =grid)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-1.png)<!-- -->

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
lasso.pred=predict(lasso.mod ,s=bestlambda ,newx=xtest)

testMSE_LASSO<-mean((ytest-lasso.pred)^2)
testMSE_LASSO
```

    ## [1] 6.109

``` r
coef(lasso.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                              s1
    ## (Intercept)                     38.913158907625
    ## Year                             0.015210240258
    ## StatusDeveloping                -0.682427202646
    ## Adult.Mortality                 -0.055330690370
    ## infant.deaths                    .             
    ## Alcohol                          0.068195557874
    ## Hepatitis.B                     -0.004353091479
    ## Measles                         -0.000015002591
    ## BMI                              0.009516994883
    ## under.five.deaths               -0.002159911431
    ## Polio                            0.012583053007
    ## Total.expenditure                .             
    ## Diphtheria                       0.015037712650
    ## Population                       0.000000001792
    ## thinness..1.19.years            -0.002961333914
    ## thinness.5.9.years              -0.012291062869
    ## Income.composition.of.resources  2.254910145188
    ## Schooling                        0.405398343412
    ## ContinentAmericas                2.554524807036
    ## ContinentAsia                    0.983989599495
    ## ContinentEurope                  1.868664377329
    ## ContinentOceania                 0.788391884258
    ## log.HIV.AIDS                     .             
    ## log.GDP                          0.134116940276
    ## log.percentage.expenditure       0.062280687460

``` r
lasso_residuals = (ytest - lasso.pred)
hist(lasso_residuals, main = "Histogram of Residuals (LASSO)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-2.png)<!-- -->

``` r
plot(lasso_residuals, main = "Residuals plot (LASSO)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-3.png)<!-- -->

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = lasso.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   2.4717   0.9252   1.7455

``` r
##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
#fit.lasso.lm = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + #Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + #log.percentage.expenditure, data = train_lasso)

### Continent included
fit.lasso.lm = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Diphtheria + Population + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, data = rtrain)
summary(fit.lasso.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + Alcohol + 
    ##     Hepatitis.B + Measles + BMI + under.five.deaths + Polio + 
    ##     Diphtheria + Population + thinness..1.19.years + thinness.5.9.years + 
    ##     Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, 
    ##     data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.423  -1.592  -0.324   1.313  11.001 
    ## 
    ## Coefficients:
    ##                                       Estimate     Std. Error t value
    ## (Intercept)                     71.72412075037 24.28963230217    2.95
    ## Year                            -0.00086511486  0.01214067889   -0.07
    ## Adult.Mortality                 -0.05876804423  0.00064871388  -90.59
    ## Alcohol                          0.17087342098  0.01699616336   10.05
    ## Hepatitis.B                     -0.00661790601  0.00265926580   -2.49
    ## Measles                         -0.00002307951  0.00000536028   -4.31
    ## BMI                              0.01461271593  0.00337832608    4.33
    ## under.five.deaths               -0.00214694352  0.00049818829   -4.31
    ## Polio                            0.01165443879  0.00310372893    3.75
    ## Diphtheria                       0.01780219327  0.00334287918    5.33
    ## Population                       0.00000000236  0.00000000054    4.37
    ## thinness..1.19.years            -0.02328214244  0.03528439109   -0.66
    ## thinness.5.9.years              -0.02332357531  0.03454830111   -0.68
    ## Income.composition.of.resources  2.27179031933  0.45032265892    5.04
    ## Schooling                        0.41926017342  0.03083507702   13.60
    ## log.GDP                          0.14840654019  0.04267701648    3.48
    ## log.percentage.expenditure       0.05208147975  0.02320813005    2.24
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.00318 ** 
    ## Year                                         0.94320    
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## Hepatitis.B                                  0.01289 *  
    ## Measles                                   0.00001730 ***
    ## BMI                                       0.00001583 ***
    ## under.five.deaths                         0.00001700 ***
    ## Polio                                        0.00018 ***
    ## Diphtheria                                0.00000011 ***
    ## Population                                0.00001305 ***
    ## thinness..1.19.years                         0.50942    
    ## thinness.5.9.years                           0.49968    
    ## Income.composition.of.resources           0.00000049 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                      0.00051 ***
    ## log.percentage.expenditure                   0.02491 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.58 on 2472 degrees of freedom
    ## Multiple R-squared:  0.928,  Adjusted R-squared:  0.928 
    ## F-statistic: 2e+03 on 16 and 2472 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.lasso.lm_VIF = vif(fit.lasso.lm)
barplot(fit.lasso.lm_VIF, main = 'VIF Values (LASSO)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-4.png)<!-- -->

``` r
### Fit linear model with factors
#fit.lasso.lm3 = lm(Life.expectancy ~ Status + Region + Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, data = train_lasso)

### Continent included
fit.lasso.lm3 = lm(Life.expectancy ~ Status + Continent + Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Diphtheria + Population + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, data = rtrain)

#### Hypothesis testing ####
summary(fit.lasso.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Status + Continent + Year + Adult.Mortality + 
    ##     Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + 
    ##     Polio + Diphtheria + Population + thinness..1.19.years + 
    ##     thinness.5.9.years + Income.composition.of.resources + Schooling + 
    ##     log.GDP + log.percentage.expenditure, data = rtrain)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -12.41  -1.50  -0.29   1.14  10.15 
    ## 
    ## Coefficients:
    ##                                        Estimate      Std. Error t value
    ## (Intercept)                     26.701702526520 24.042250034403    1.11
    ## StatusDeveloping                -0.731227025190  0.207556926596   -3.52
    ## ContinentAmericas                2.737744817628  0.207040167773   13.22
    ## ContinentAsia                    1.148916818553  0.178604538637    6.43
    ## ContinentEurope                  2.052569171618  0.250700876160    8.19
    ## ContinentOceania                 0.992539269468  0.276200569704    3.59
    ## Year                             0.021282736363  0.012014794441    1.77
    ## Adult.Mortality                 -0.054955070137  0.000729411659  -75.34
    ## Alcohol                          0.068153244868  0.021010789789    3.24
    ## Hepatitis.B                     -0.005792826269  0.002583081966   -2.24
    ## Measles                         -0.000016148459  0.000005208101   -3.10
    ## BMI                              0.009385770611  0.003377710366    2.78
    ## under.five.deaths               -0.002447758320  0.000485215599   -5.04
    ## Polio                            0.013026077820  0.003006133091    4.33
    ## Diphtheria                       0.015639225010  0.003245372742    4.82
    ## Population                       0.000000002110  0.000000000528    4.00
    ## thinness..1.19.years             0.001066950114  0.034242755071    0.03
    ## thinness.5.9.years              -0.013689549029  0.033842314209   -0.40
    ## Income.composition.of.resources  2.214555341527  0.443608409009    4.99
    ## Schooling                        0.398911026518  0.030181787447   13.22
    ## log.GDP                          0.129272221073  0.041668306039    3.10
    ## log.percentage.expenditure       0.069164565191  0.022583379583    3.06
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.26684    
    ## StatusDeveloping                             0.00043 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                    0.00000000015001429 ***
    ## ContinentEurope                  0.00000000000000042 ***
    ## ContinentOceania                             0.00033 ***
    ## Year                                         0.07662 .  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                      0.00120 ** 
    ## Hepatitis.B                                  0.02501 *  
    ## Measles                                      0.00195 ** 
    ## BMI                                          0.00550 ** 
    ## under.five.deaths                0.00000048733102222 ***
    ## Polio                            0.00001528655356616 ***
    ## Diphtheria                       0.00000153088929503 ***
    ## Population                       0.00006576862056827 ***
    ## thinness..1.19.years                         0.97515    
    ## thinness.5.9.years                           0.68587    
    ## Income.composition.of.resources  0.00000063881649327 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                      0.00194 ** 
    ## log.percentage.expenditure                   0.00222 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.49 on 2467 degrees of freedom
    ## Multiple R-squared:  0.933,  Adjusted R-squared:  0.933 
    ## F-statistic: 1.64e+03 on 21 and 2467 DF,  p-value: <0.0000000000000002

``` r
# At alpha = 0.05 the following variables are not significant therefore don't contribute to the model performance:
# Year, Thinness aged 5 to 9 years old, Thinness aged 10 to 19 years old

# Predicting
train_pred = predict(fit.lasso.lm3, rtrain)
test_pred = predict(fit.lasso.lm3, rtest)

# Scoring the final model on Training and Test set
residuals = resid(fit.lasso.lm3)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)
sm = summary(fit.lasso.lm3)
mse_trn = mean(sm$residuals^2)

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared


## Test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.lasso.lm3$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

### Checking Multiple Liner Regression model assumptions
confint(fit.lasso.lm3)
```

    ##                                            2.5 %          97.5 %
    ## (Intercept)                     -20.443371869109 73.846776922150
    ## StatusDeveloping                 -1.138230809433 -0.324223240947
    ## ContinentAmericas                 2.331754359223  3.143735276032
    ## ContinentAsia                     0.798686526073  1.499147111033
    ## ContinentEurope                   1.560963292771  2.544175050464
    ## ContinentOceania                  0.450930377281  1.534148161655
    ## Year                             -0.002277387043  0.044842859769
    ## Adult.Mortality                  -0.056385392460 -0.053524747814
    ## Alcohol                           0.026952639838  0.109353849897
    ## Hepatitis.B                      -0.010858058985 -0.000727593552
    ## Measles                          -0.000026361159 -0.000005935759
    ## BMI                               0.002762330365  0.016009210856
    ## under.five.deaths                -0.003399230229 -0.001496286412
    ## Polio                             0.007131273131  0.018920882509
    ## Diphtheria                        0.009275289058  0.022003160962
    ## Population                        0.000000001075  0.000000003145
    ## thinness..1.19.years             -0.066080560331  0.068214460560
    ## thinness.5.9.years               -0.080051824557  0.052672726500
    ## Income.composition.of.resources   1.344672056255  3.084438626798
    ## Schooling                         0.339726773274  0.458095279762
    ## log.GDP                           0.047563754298  0.210980687848
    ## log.percentage.expenditure        0.024880227867  0.113448902516

``` r
hist(residuals, main = "Histogram of Residuals (Lasso MLR fit)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-5.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot (Lasso MLR fit)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-6.png)<!-- -->

``` r
plot(fit.lasso.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-7.png)<!-- -->

``` r
plot(fit.lasso.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-8.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_lasso = 1:dim(xtest)[1]
plot(x_lasso, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (LASSO)", ylab="Life expectancy")
lines(x_lasso, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("Original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-9.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (LASSO MLR fit)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-10.png)<!-- -->

``` r
### Evaluation Data Frame
### test_ASE, R-squared/Adjusted R-squared, RMSE
eval_test_df = data.frame(model_name = 'LASSO', MSE=format(round(mse,4),nsmall=4), R_Squared=format(round(rsqd,4),nsmall=4), AdjR_Squared=format(round(adjrsqd,4),nsmall=4), RMSE = format(round(rmse,4),nsmall=4))

### train_ASE, R-squared/Adjusted R-squared, RMSE
eval_train_df = data.frame(model_name = 'LASSO', MSE=format(round(mse_trndf,4),nsmall=4), R_Squared=format(round(rsqd_trn,4),nsmall=4), AdjR_Squared=format(round(adjrsqd_trn,4),nsmall=4), RMSE = format(round(rmse_trn,4),nsmall=4))


#####################################################################################
#                                  Forward Selection                                #
#####################################################################################
library(leaps)

mlr.fwd=regsubsets(Life.expectancy~.,data=rtrain,method="forward",nvmax=24)
testASE<-c()
for (i in 1:24){
  predictions = predict.regsubsets(object=mlr.fwd,newdata=rtest,id=i) 
  testASE[i] = mean((rtest$Life.expectancy-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:24,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE", main='Forward Selection plot')
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.fwd)$rss
lines(1:24,rss/dim(rtrain)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-11.png)<!-- -->

``` r
mlr.fwd.final=regsubsets(Life.expectancy~.,data=LifeExp[,!colnames(train) %in% drop_for_reg],method="forward",nvmax=24)
coef(mlr.fwd.final,14)
```

    ##                     (Intercept)                StatusDeveloping 
    ##                        68.85409                        -0.72160 
    ##                 Adult.Mortality                   infant.deaths 
    ##                        -0.05489                         0.02599 
    ##                         Alcohol               under.five.deaths 
    ##                         0.05509                        -0.02092 
    ##                           Polio                      Diphtheria 
    ##                         0.01125                         0.01423 
    ## Income.composition.of.resources                       Schooling 
    ##                         2.41027                         0.41296 
    ##               ContinentAmericas                   ContinentAsia 
    ##                         2.83007                         1.02407 
    ##                 ContinentEurope                ContinentOceania 
    ##                         2.20075                         1.23535 
    ##                         log.GDP 
    ##                         0.21963

``` r
summary(mlr.fwd.final)
```

    ## Subset selection object
    ## Call: regsubsets.formula(Life.expectancy ~ ., data = LifeExp[, !colnames(train) %in% 
    ##     drop_for_reg], method = "forward", nvmax = 24)
    ## 24 Variables  (and intercept)
    ##                                 Forced in Forced out
    ## Year                                FALSE      FALSE
    ## StatusDeveloping                    FALSE      FALSE
    ## Adult.Mortality                     FALSE      FALSE
    ## infant.deaths                       FALSE      FALSE
    ## Alcohol                             FALSE      FALSE
    ## Hepatitis.B                         FALSE      FALSE
    ## Measles                             FALSE      FALSE
    ## BMI                                 FALSE      FALSE
    ## under.five.deaths                   FALSE      FALSE
    ## Polio                               FALSE      FALSE
    ## Total.expenditure                   FALSE      FALSE
    ## Diphtheria                          FALSE      FALSE
    ## Population                          FALSE      FALSE
    ## thinness..1.19.years                FALSE      FALSE
    ## thinness.5.9.years                  FALSE      FALSE
    ## Income.composition.of.resources     FALSE      FALSE
    ## Schooling                           FALSE      FALSE
    ## ContinentAmericas                   FALSE      FALSE
    ## ContinentAsia                       FALSE      FALSE
    ## ContinentEurope                     FALSE      FALSE
    ## ContinentOceania                    FALSE      FALSE
    ## log.HIV.AIDS                        FALSE      FALSE
    ## log.GDP                             FALSE      FALSE
    ## log.percentage.expenditure          FALSE      FALSE
    ## 1 subsets of each size up to 24
    ## Selection Algorithm: forward
    ##           Year StatusDeveloping Adult.Mortality infant.deaths Alcohol
    ## 1  ( 1 )  " "  " "              "*"             " "           " "    
    ## 2  ( 1 )  " "  " "              "*"             " "           " "    
    ## 3  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 4  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 5  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 6  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 7  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 8  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 9  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 10  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 11  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 12  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 13  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 14  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 15  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 16  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 17  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 18  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 19  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 20  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 21  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 22  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 23  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 24  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ##           Hepatitis.B Measles BMI under.five.deaths Polio Total.expenditure
    ## 1  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 2  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 3  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 4  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 5  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 6  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 7  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 8  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 9  ( 1 )  " "         " "     " " "*"               " "   " "              
    ## 10  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 11  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 12  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 13  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 14  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 15  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 16  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 17  ( 1 ) "*"         " "     "*" "*"               "*"   " "              
    ## 18  ( 1 ) "*"         " "     "*" "*"               "*"   " "              
    ## 19  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 20  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 21  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 22  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 23  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 24  ( 1 ) "*"         "*"     "*" "*"               "*"   "*"              
    ##           Diphtheria Population thinness..1.19.years thinness.5.9.years
    ## 1  ( 1 )  " "        " "        " "                  " "               
    ## 2  ( 1 )  " "        " "        " "                  " "               
    ## 3  ( 1 )  " "        " "        " "                  " "               
    ## 4  ( 1 )  "*"        " "        " "                  " "               
    ## 5  ( 1 )  "*"        " "        " "                  " "               
    ## 6  ( 1 )  "*"        " "        " "                  " "               
    ## 7  ( 1 )  "*"        " "        " "                  " "               
    ## 8  ( 1 )  "*"        " "        " "                  " "               
    ## 9  ( 1 )  "*"        " "        " "                  " "               
    ## 10  ( 1 ) "*"        " "        " "                  " "               
    ## 11  ( 1 ) "*"        " "        " "                  " "               
    ## 12  ( 1 ) "*"        " "        " "                  " "               
    ## 13  ( 1 ) "*"        " "        " "                  " "               
    ## 14  ( 1 ) "*"        " "        " "                  " "               
    ## 15  ( 1 ) "*"        " "        " "                  " "               
    ## 16  ( 1 ) "*"        " "        " "                  " "               
    ## 17  ( 1 ) "*"        " "        " "                  " "               
    ## 18  ( 1 ) "*"        " "        " "                  " "               
    ## 19  ( 1 ) "*"        " "        " "                  " "               
    ## 20  ( 1 ) "*"        "*"        " "                  " "               
    ## 21  ( 1 ) "*"        "*"        "*"                  " "               
    ## 22  ( 1 ) "*"        "*"        "*"                  " "               
    ## 23  ( 1 ) "*"        "*"        "*"                  "*"               
    ## 24  ( 1 ) "*"        "*"        "*"                  "*"               
    ##           Income.composition.of.resources Schooling ContinentAmericas
    ## 1  ( 1 )  " "                             " "       " "              
    ## 2  ( 1 )  " "                             "*"       " "              
    ## 3  ( 1 )  " "                             "*"       " "              
    ## 4  ( 1 )  " "                             "*"       " "              
    ## 5  ( 1 )  " "                             "*"       "*"              
    ## 6  ( 1 )  " "                             "*"       "*"              
    ## 7  ( 1 )  " "                             "*"       "*"              
    ## 8  ( 1 )  "*"                             "*"       "*"              
    ## 9  ( 1 )  "*"                             "*"       "*"              
    ## 10  ( 1 ) "*"                             "*"       "*"              
    ## 11  ( 1 ) "*"                             "*"       "*"              
    ## 12  ( 1 ) "*"                             "*"       "*"              
    ## 13  ( 1 ) "*"                             "*"       "*"              
    ## 14  ( 1 ) "*"                             "*"       "*"              
    ## 15  ( 1 ) "*"                             "*"       "*"              
    ## 16  ( 1 ) "*"                             "*"       "*"              
    ## 17  ( 1 ) "*"                             "*"       "*"              
    ## 18  ( 1 ) "*"                             "*"       "*"              
    ## 19  ( 1 ) "*"                             "*"       "*"              
    ## 20  ( 1 ) "*"                             "*"       "*"              
    ## 21  ( 1 ) "*"                             "*"       "*"              
    ## 22  ( 1 ) "*"                             "*"       "*"              
    ## 23  ( 1 ) "*"                             "*"       "*"              
    ## 24  ( 1 ) "*"                             "*"       "*"              
    ##           ContinentAsia ContinentEurope ContinentOceania log.HIV.AIDS log.GDP
    ## 1  ( 1 )  " "           " "             " "              " "          " "    
    ## 2  ( 1 )  " "           " "             " "              " "          " "    
    ## 3  ( 1 )  " "           " "             " "              " "          " "    
    ## 4  ( 1 )  " "           " "             " "              " "          " "    
    ## 5  ( 1 )  " "           " "             " "              " "          " "    
    ## 6  ( 1 )  " "           "*"             " "              " "          " "    
    ## 7  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 8  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 9  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 10  ( 1 ) " "           "*"             " "              " "          "*"    
    ## 11  ( 1 ) "*"           "*"             " "              " "          "*"    
    ## 12  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 13  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 14  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 15  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 16  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 17  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 18  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 19  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 20  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 21  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 22  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 23  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 24  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ##           log.percentage.expenditure
    ## 1  ( 1 )  " "                       
    ## 2  ( 1 )  " "                       
    ## 3  ( 1 )  " "                       
    ## 4  ( 1 )  " "                       
    ## 5  ( 1 )  " "                       
    ## 6  ( 1 )  " "                       
    ## 7  ( 1 )  " "                       
    ## 8  ( 1 )  " "                       
    ## 9  ( 1 )  " "                       
    ## 10  ( 1 ) " "                       
    ## 11  ( 1 ) " "                       
    ## 12  ( 1 ) " "                       
    ## 13  ( 1 ) " "                       
    ## 14  ( 1 ) " "                       
    ## 15  ( 1 ) " "                       
    ## 16  ( 1 ) "*"                       
    ## 17  ( 1 ) "*"                       
    ## 18  ( 1 ) "*"                       
    ## 19  ( 1 ) "*"                       
    ## 20  ( 1 ) "*"                       
    ## 21  ( 1 ) "*"                       
    ## 22  ( 1 ) "*"                       
    ## 23  ( 1 ) "*"                       
    ## 24  ( 1 ) "*"

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##    2.476    0.925    1.755

``` r
##### Fit Linear Model based on Forward Selection without factors to measure VIF####
fit.fwd.lm = lm(Life.expectancy ~ Adult.Mortality+infant.deaths+Alcohol+under.five.deaths+Polio+
                                  Diphtheria + Income.composition.of.resources +
                                  Schooling + log.GDP, data = rtrain)
summary(fit.fwd.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Adult.Mortality + infant.deaths + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.763  -1.562  -0.302   1.250  10.321 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.544649   0.458119  151.80
    ## Adult.Mortality                 -0.059220   0.000642  -92.30
    ## infant.deaths                    0.035306   0.005666    6.23
    ## Alcohol                          0.203073   0.016361   12.41
    ## under.five.deaths               -0.028038   0.004177   -6.71
    ## Polio                            0.010223   0.003091    3.31
    ## Diphtheria                       0.013694   0.003126    4.38
    ## Income.composition.of.resources  2.497496   0.447785    5.58
    ## Schooling                        0.434116   0.030573   14.20
    ## log.GDP                          0.224843   0.038249    5.88
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                         0.000000000541 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                     0.000000000024 ***
    ## Polio                                        0.00096 ***
    ## Diphtheria                            0.000012300054 ***
    ## Income.composition.of.resources       0.000000027054 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000004701 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.6 on 2479 degrees of freedom
    ## Multiple R-squared:  0.927,  Adjusted R-squared:  0.927 
    ## F-statistic: 3.5e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.fwd.lm_VIF = vif(fit.fwd.lm)
barplot(fit.fwd.lm_VIF, main = 'VIF Values (FWD selection)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-12.png)<!-- -->

``` r
# We can see that Forward Selection did not remove infant.deaths or under.five.deaths as those are perfectly correlated. Let's remove the one with the smallest coefficient (infant.deaths).

##### Fit Linear Model based on FWD selection without factors to measure VIF####
fit.fwd.lm2 = lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                  Diphtheria + Income.composition.of.resources +
                                  Schooling + log.GDP, data = rtrain)
summary(fit.fwd.lm2)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Adult.Mortality + Alcohol + under.five.deaths + 
    ##     Polio + Diphtheria + Income.composition.of.resources + Schooling + 
    ##     log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.976  -1.582  -0.291   1.285  10.411 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.412920   0.461109  150.53
    ## Adult.Mortality                 -0.059888   0.000637  -93.95
    ## Alcohol                          0.186374   0.016263   11.46
    ## under.five.deaths               -0.002090   0.000327   -6.39
    ## Polio                            0.011049   0.003112    3.55
    ## Diphtheria                       0.015588   0.003135    4.97
    ## Income.composition.of.resources  2.663278   0.450391    5.91
    ## Schooling                        0.437343   0.030801   14.20
    ## log.GDP                          0.218220   0.038525    5.66
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                       0.0000000002 ***
    ## Polio                                        0.00039 ***
    ## Diphtheria                              0.0000007056 ***
    ## Income.composition.of.resources         0.0000000038 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                 0.0000000165 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.62 on 2480 degrees of freedom
    ## Multiple R-squared:  0.926,  Adjusted R-squared:  0.926 
    ## F-statistic: 3.88e+03 on 8 and 2480 DF,  p-value: <0.0000000000000002

``` r
### re-run Visualize VIF
fit.fwd.lm2_VIF = vif(fit.fwd.lm2)
barplot(fit.fwd.lm2_VIF, main = 'Re-test of VIF Values (FWD selection)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-13.png)<!-- -->

``` r
##### Fit Linear Model based on Forward Selection regularization and removed multicollinearity and categorical variables####
fit.fwd.lm3 = lm(Life.expectancy ~ Status+Continent+Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                  Diphtheria + Income.composition.of.resources +
                                  Schooling + log.GDP, data = rtrain)

#### Hypothesis testing ####
summary(fit.fwd.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Status + Continent + Adult.Mortality + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.532  -1.497  -0.249   1.110  10.073 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     68.714463   0.513675  133.77
    ## StatusDeveloping                -0.659861   0.206801   -3.19
    ## ContinentAmericas                2.889912   0.198793   14.54
    ## ContinentAsia                    1.110255   0.172118    6.45
    ## ContinentEurope                  2.248488   0.242507    9.27
    ## ContinentOceania                 1.289408   0.260456    4.95
    ## Adult.Mortality                 -0.055438   0.000724  -76.58
    ## Alcohol                          0.057913   0.020809    2.78
    ## under.five.deaths               -0.001786   0.000319   -5.59
    ## Polio                            0.012628   0.002993    4.22
    ## Diphtheria                       0.014414   0.003029    4.76
    ## Income.composition.of.resources  2.658460   0.436928    6.08
    ## Schooling                        0.407231   0.030031   13.56
    ## log.GDP                          0.207836   0.037172    5.59
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                              0.0014 ** 
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                          0.00000000013 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                       0.00000078972 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                       0.0054 ** 
    ## under.five.deaths                      0.00000002498 ***
    ## Polio                                  0.00002544692 ***
    ## Diphtheria                             0.00000205863 ***
    ## Income.composition.of.resources        0.00000000135 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                0.00000002502 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.51 on 2475 degrees of freedom
    ## Multiple R-squared:  0.932,  Adjusted R-squared:  0.932 
    ## F-statistic: 2.61e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

``` r
# At alpha = 0.05 the following variables are not significant therefore don't contribute to the model performance:
# thinness.5.9.years, Measles, BMI, Total.expenditure.

# Predicting
train_pred = predict(fit.fwd.lm3, rtrain)
test_pred = predict(fit.fwd.lm3, rtest)

# Scoring the final model on Training and Test set
residuals = resid(fit.fwd.lm3)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)
sm = summary(fit.fwd.lm3)
mse_trn = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.fwd.lm3)
```

    ##                                     2.5 %    97.5 %
    ## (Intercept)                     67.707187 69.721740
    ## StatusDeveloping                -1.065383 -0.254339
    ## ContinentAmericas                2.500095  3.279730
    ## ContinentAsia                    0.772745  1.447766
    ## ContinentEurope                  1.772950  2.724025
    ## ContinentOceania                 0.778674  1.800141
    ## Adult.Mortality                 -0.056858 -0.054019
    ## Alcohol                          0.017109  0.098717
    ## under.five.deaths               -0.002412 -0.001159
    ## Polio                            0.006759  0.018498
    ## Diphtheria                       0.008475  0.020353
    ## Income.composition.of.resources  1.801679  3.515242
    ## Schooling                        0.348342  0.466119
    ## log.GDP                          0.134945  0.280727

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-14.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-15.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-16.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-17.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_fwd = 1:dim(xtest)[1]
plot(x_fwd, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (FWD selection)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-18.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (FWD Selection fit)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-19.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.lasso.lm3$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('FWD Selection', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('FWD Selection', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
#####################################################################################
#                               Backward Elimination                                #
#####################################################################################

mlr.bck=regsubsets(Life.expectancy~.,data=rtrain,method="backward",nvmax=24)
testASE<-c()
for (i in 1:24){

  predictions = predict.regsubsets(object=mlr.bck,newdata=test,id=i) 
  testASE[i] = mean((test$Life.expectancy-predictions)^2)
}

par(mfrow=c(1,1))
plot(1:24,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.bck)$rss
lines(1:24,rss/dim(rtrain)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-20.png)<!-- -->

``` r
mlr.bck.final=regsubsets(Life.expectancy~.,data=LifeExp[,!colnames(train) %in% drop_for_reg],method="backward",nvmax=24)
coef(mlr.bck.final,14)
```

    ##                     (Intercept)                StatusDeveloping 
    ##                        68.85409                        -0.72160 
    ##                 Adult.Mortality                   infant.deaths 
    ##                        -0.05489                         0.02599 
    ##                         Alcohol               under.five.deaths 
    ##                         0.05509                        -0.02092 
    ##                           Polio                      Diphtheria 
    ##                         0.01125                         0.01423 
    ## Income.composition.of.resources                       Schooling 
    ##                         2.41027                         0.41296 
    ##               ContinentAmericas                   ContinentAsia 
    ##                         2.83007                         1.02407 
    ##                 ContinentEurope                ContinentOceania 
    ##                         2.20075                         1.23535 
    ##                         log.GDP 
    ##                         0.21963

``` r
summary(mlr.bck.final)
```

    ## Subset selection object
    ## Call: regsubsets.formula(Life.expectancy ~ ., data = LifeExp[, !colnames(train) %in% 
    ##     drop_for_reg], method = "backward", nvmax = 24)
    ## 24 Variables  (and intercept)
    ##                                 Forced in Forced out
    ## Year                                FALSE      FALSE
    ## StatusDeveloping                    FALSE      FALSE
    ## Adult.Mortality                     FALSE      FALSE
    ## infant.deaths                       FALSE      FALSE
    ## Alcohol                             FALSE      FALSE
    ## Hepatitis.B                         FALSE      FALSE
    ## Measles                             FALSE      FALSE
    ## BMI                                 FALSE      FALSE
    ## under.five.deaths                   FALSE      FALSE
    ## Polio                               FALSE      FALSE
    ## Total.expenditure                   FALSE      FALSE
    ## Diphtheria                          FALSE      FALSE
    ## Population                          FALSE      FALSE
    ## thinness..1.19.years                FALSE      FALSE
    ## thinness.5.9.years                  FALSE      FALSE
    ## Income.composition.of.resources     FALSE      FALSE
    ## Schooling                           FALSE      FALSE
    ## ContinentAmericas                   FALSE      FALSE
    ## ContinentAsia                       FALSE      FALSE
    ## ContinentEurope                     FALSE      FALSE
    ## ContinentOceania                    FALSE      FALSE
    ## log.HIV.AIDS                        FALSE      FALSE
    ## log.GDP                             FALSE      FALSE
    ## log.percentage.expenditure          FALSE      FALSE
    ## 1 subsets of each size up to 24
    ## Selection Algorithm: backward
    ##           Year StatusDeveloping Adult.Mortality infant.deaths Alcohol
    ## 1  ( 1 )  " "  " "              "*"             " "           " "    
    ## 2  ( 1 )  " "  " "              "*"             " "           " "    
    ## 3  ( 1 )  " "  " "              "*"             " "           " "    
    ## 4  ( 1 )  " "  " "              "*"             " "           " "    
    ## 5  ( 1 )  " "  " "              "*"             " "           " "    
    ## 6  ( 1 )  " "  " "              "*"             " "           " "    
    ## 7  ( 1 )  " "  " "              "*"             " "           " "    
    ## 8  ( 1 )  " "  " "              "*"             " "           " "    
    ## 9  ( 1 )  " "  " "              "*"             " "           " "    
    ## 10  ( 1 ) " "  " "              "*"             " "           " "    
    ## 11  ( 1 ) " "  " "              "*"             "*"           " "    
    ## 12  ( 1 ) " "  "*"              "*"             "*"           " "    
    ## 13  ( 1 ) " "  "*"              "*"             "*"           " "    
    ## 14  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 15  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 16  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 17  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 18  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 19  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 20  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 21  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 22  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 23  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 24  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ##           Hepatitis.B Measles BMI under.five.deaths Polio Total.expenditure
    ## 1  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 2  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 3  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 4  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 5  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 6  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 7  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 8  ( 1 )  " "         " "     " " "*"               " "   " "              
    ## 9  ( 1 )  " "         " "     " " "*"               " "   " "              
    ## 10  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 11  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 12  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 13  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 14  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 15  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 16  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 17  ( 1 ) "*"         " "     "*" "*"               "*"   " "              
    ## 18  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 19  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 20  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 21  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 22  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 23  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 24  ( 1 ) "*"         "*"     "*" "*"               "*"   "*"              
    ##           Diphtheria Population thinness..1.19.years thinness.5.9.years
    ## 1  ( 1 )  " "        " "        " "                  " "               
    ## 2  ( 1 )  " "        " "        " "                  " "               
    ## 3  ( 1 )  " "        " "        " "                  " "               
    ## 4  ( 1 )  " "        " "        " "                  " "               
    ## 5  ( 1 )  "*"        " "        " "                  " "               
    ## 6  ( 1 )  "*"        " "        " "                  " "               
    ## 7  ( 1 )  "*"        " "        " "                  " "               
    ## 8  ( 1 )  "*"        " "        " "                  " "               
    ## 9  ( 1 )  "*"        " "        " "                  " "               
    ## 10  ( 1 ) "*"        " "        " "                  " "               
    ## 11  ( 1 ) "*"        " "        " "                  " "               
    ## 12  ( 1 ) "*"        " "        " "                  " "               
    ## 13  ( 1 ) "*"        " "        " "                  " "               
    ## 14  ( 1 ) "*"        " "        " "                  " "               
    ## 15  ( 1 ) "*"        " "        " "                  " "               
    ## 16  ( 1 ) "*"        " "        " "                  " "               
    ## 17  ( 1 ) "*"        " "        " "                  " "               
    ## 18  ( 1 ) "*"        " "        " "                  " "               
    ## 19  ( 1 ) "*"        "*"        " "                  " "               
    ## 20  ( 1 ) "*"        "*"        " "                  " "               
    ## 21  ( 1 ) "*"        "*"        "*"                  " "               
    ## 22  ( 1 ) "*"        "*"        "*"                  " "               
    ## 23  ( 1 ) "*"        "*"        "*"                  "*"               
    ## 24  ( 1 ) "*"        "*"        "*"                  "*"               
    ##           Income.composition.of.resources Schooling ContinentAmericas
    ## 1  ( 1 )  " "                             " "       " "              
    ## 2  ( 1 )  " "                             "*"       " "              
    ## 3  ( 1 )  " "                             "*"       "*"              
    ## 4  ( 1 )  " "                             "*"       "*"              
    ## 5  ( 1 )  " "                             "*"       "*"              
    ## 6  ( 1 )  " "                             "*"       "*"              
    ## 7  ( 1 )  "*"                             "*"       "*"              
    ## 8  ( 1 )  "*"                             "*"       "*"              
    ## 9  ( 1 )  "*"                             "*"       "*"              
    ## 10  ( 1 ) "*"                             "*"       "*"              
    ## 11  ( 1 ) "*"                             "*"       "*"              
    ## 12  ( 1 ) "*"                             "*"       "*"              
    ## 13  ( 1 ) "*"                             "*"       "*"              
    ## 14  ( 1 ) "*"                             "*"       "*"              
    ## 15  ( 1 ) "*"                             "*"       "*"              
    ## 16  ( 1 ) "*"                             "*"       "*"              
    ## 17  ( 1 ) "*"                             "*"       "*"              
    ## 18  ( 1 ) "*"                             "*"       "*"              
    ## 19  ( 1 ) "*"                             "*"       "*"              
    ## 20  ( 1 ) "*"                             "*"       "*"              
    ## 21  ( 1 ) "*"                             "*"       "*"              
    ## 22  ( 1 ) "*"                             "*"       "*"              
    ## 23  ( 1 ) "*"                             "*"       "*"              
    ## 24  ( 1 ) "*"                             "*"       "*"              
    ##           ContinentAsia ContinentEurope ContinentOceania log.HIV.AIDS log.GDP
    ## 1  ( 1 )  " "           " "             " "              " "          " "    
    ## 2  ( 1 )  " "           " "             " "              " "          " "    
    ## 3  ( 1 )  " "           " "             " "              " "          " "    
    ## 4  ( 1 )  " "           "*"             " "              " "          " "    
    ## 5  ( 1 )  " "           "*"             " "              " "          " "    
    ## 6  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 7  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 8  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 9  ( 1 )  "*"           "*"             " "              " "          "*"    
    ## 10  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 11  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 12  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 13  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 14  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 15  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 16  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 17  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 18  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 19  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 20  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 21  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 22  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 23  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 24  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ##           log.percentage.expenditure
    ## 1  ( 1 )  " "                       
    ## 2  ( 1 )  " "                       
    ## 3  ( 1 )  " "                       
    ## 4  ( 1 )  " "                       
    ## 5  ( 1 )  " "                       
    ## 6  ( 1 )  " "                       
    ## 7  ( 1 )  " "                       
    ## 8  ( 1 )  " "                       
    ## 9  ( 1 )  " "                       
    ## 10  ( 1 ) " "                       
    ## 11  ( 1 ) " "                       
    ## 12  ( 1 ) " "                       
    ## 13  ( 1 ) " "                       
    ## 14  ( 1 ) " "                       
    ## 15  ( 1 ) " "                       
    ## 16  ( 1 ) "*"                       
    ## 17  ( 1 ) "*"                       
    ## 18  ( 1 ) "*"                       
    ## 19  ( 1 ) "*"                       
    ## 20  ( 1 ) "*"                       
    ## 21  ( 1 ) "*"                       
    ## 22  ( 1 ) "*"                       
    ## 23  ( 1 ) "*"                       
    ## 24  ( 1 ) "*"

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##    2.476    0.925    1.755

``` r
##different result now, will repeat steps from earlier model
### Continent
fit.bck.lm <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+Alcohol+under.five.deaths+Polio+
                                   Diphtheria+Income.composition.of.resources + Schooling + log.GDP
                 ,data = rtrain)

summary(fit.bck.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Adult.Mortality + infant.deaths + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.763  -1.562  -0.302   1.250  10.321 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.544649   0.458119  151.80
    ## Adult.Mortality                 -0.059220   0.000642  -92.30
    ## infant.deaths                    0.035306   0.005666    6.23
    ## Alcohol                          0.203073   0.016361   12.41
    ## under.five.deaths               -0.028038   0.004177   -6.71
    ## Polio                            0.010223   0.003091    3.31
    ## Diphtheria                       0.013694   0.003126    4.38
    ## Income.composition.of.resources  2.497496   0.447785    5.58
    ## Schooling                        0.434116   0.030573   14.20
    ## log.GDP                          0.224843   0.038249    5.88
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                         0.000000000541 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                     0.000000000024 ***
    ## Polio                                        0.00096 ***
    ## Diphtheria                            0.000012300054 ***
    ## Income.composition.of.resources       0.000000027054 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000004701 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.6 on 2479 degrees of freedom
    ## Multiple R-squared:  0.927,  Adjusted R-squared:  0.927 
    ## F-statistic: 3.5e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.bck.lm_VIF = vif(fit.bck.lm)
barplot(fit.bck.lm_VIF, main = 'VIF Values (Backward elimination)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-21.png)<!-- -->

``` r
#two are correlated and giving a high VIF it's under.five and infant.deaths
fit.bck.lm2 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                   Diphtheria+Income.composition.of.resources + Schooling + log.GDP,data = rtrain)


fit.bck.lm2_VIF = vif(fit.bck.lm2)
barplot(fit.bck.lm2_VIF, main = 'VIF Values (Backward elimination)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-22.png)<!-- -->

``` r
##fit model with categorical variables and removed colinearity
# Continent
fit.bck.lm3 = lm(Life.expectancy ~ Status + Continent + Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                   Diphtheria+Income.composition.of.resources + Schooling + log.GDP,data = rtrain)


#### Hypothesis testing ####
summary(fit.bck.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Status + Continent + Adult.Mortality + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.532  -1.497  -0.249   1.110  10.073 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     68.714463   0.513675  133.77
    ## StatusDeveloping                -0.659861   0.206801   -3.19
    ## ContinentAmericas                2.889912   0.198793   14.54
    ## ContinentAsia                    1.110255   0.172118    6.45
    ## ContinentEurope                  2.248488   0.242507    9.27
    ## ContinentOceania                 1.289408   0.260456    4.95
    ## Adult.Mortality                 -0.055438   0.000724  -76.58
    ## Alcohol                          0.057913   0.020809    2.78
    ## under.five.deaths               -0.001786   0.000319   -5.59
    ## Polio                            0.012628   0.002993    4.22
    ## Diphtheria                       0.014414   0.003029    4.76
    ## Income.composition.of.resources  2.658460   0.436928    6.08
    ## Schooling                        0.407231   0.030031   13.56
    ## log.GDP                          0.207836   0.037172    5.59
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                              0.0014 ** 
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                          0.00000000013 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                       0.00000078972 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                       0.0054 ** 
    ## under.five.deaths                      0.00000002498 ***
    ## Polio                                  0.00002544692 ***
    ## Diphtheria                             0.00000205863 ***
    ## Income.composition.of.resources        0.00000000135 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                0.00000002502 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.51 on 2475 degrees of freedom
    ## Multiple R-squared:  0.932,  Adjusted R-squared:  0.932 
    ## F-statistic: 2.61e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

``` r
# Predicting
train_pred = predict(fit.bck.lm3, rtrain)
test_pred = predict(fit.bck.lm3, rtest)

# Scoring the final model on Training and Test set
residuals = resid(fit.bck.lm3)
train_score = postResample(pred = train_pred, obs = train$Life.expectancy)
test_score = postResample(pred = test_pred, obs = test$Life.expectancy)
sm = summary(fit.bck.lm3)
mse_trn = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.bck.lm3)
```

    ##                                     2.5 %    97.5 %
    ## (Intercept)                     67.707187 69.721740
    ## StatusDeveloping                -1.065383 -0.254339
    ## ContinentAmericas                2.500095  3.279730
    ## ContinentAsia                    0.772745  1.447766
    ## ContinentEurope                  1.772950  2.724025
    ## ContinentOceania                 0.778674  1.800141
    ## Adult.Mortality                 -0.056858 -0.054019
    ## Alcohol                          0.017109  0.098717
    ## under.five.deaths               -0.002412 -0.001159
    ## Polio                            0.006759  0.018498
    ## Diphtheria                       0.008475  0.020353
    ## Income.composition.of.resources  1.801679  3.515242
    ## Schooling                        0.348342  0.466119
    ## log.GDP                          0.134945  0.280727

``` r
hist(residuals, main = "Histogram of Residuals (Backward elimination)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-23.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot (Backward elimination)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-24.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-25.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-26.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_fwd = 1:dim(xtest)[1]
plot(x_fwd, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Backward elimination)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-27.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Backward elimination)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-28.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.bck.lm3$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared




eval_test_df = rbind(eval_test_df, c('Backward Elim.', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('Backward Elim.', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))


#####################################################################################
#                                  Ridge regression                                 #
#####################################################################################

grid=10^seq(10,-2, length =100)
ridge.mod=glmnet(x,y,alpha=0, lambda =grid, family = 'gaussian') # alpha is 0 for Ridge
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-29.png)<!-- -->

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
ridge.pred_trn=predict(ridge.mod ,s=bestlambda ,newx=x)
ridge.pred=predict(ridge.mod ,s=bestlambda ,newx=xtest)

testMSE_RIDGE<-mean((ytest-ridge.pred)^2)
testMSE_RIDGE
```

    ## [1] 6.308

``` r
coef(ridge.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                              s1
    ## (Intercept)                     -6.953245823684
    ## Year                             0.035785530923
    ## StatusDeveloping                -1.194643319721
    ## Adult.Mortality                 -0.040190310300
    ## infant.deaths                   -0.000797400323
    ## Alcohol                          0.050596274481
    ## Hepatitis.B                     -0.003481492104
    ## Measles                         -0.000014728467
    ## BMI                              0.012643399859
    ## under.five.deaths               -0.001562761653
    ## Polio                            0.015097198409
    ## Total.expenditure                0.001651533509
    ## Diphtheria                       0.016998064261
    ## Population                       0.000000002233
    ## thinness..1.19.years            -0.017902225075
    ## thinness.5.9.years              -0.029773042974
    ## Income.composition.of.resources  3.598357691516
    ## Schooling                        0.365014675731
    ## ContinentAmericas                2.494335192816
    ## ContinentAsia                    0.901707468522
    ## ContinentEurope                  1.542321625092
    ## ContinentOceania                 0.431555807767
    ## log.HIV.AIDS                    -0.719934927419
    ## log.GDP                          0.182096178355
    ## log.percentage.expenditure       0.112513092145

``` r
# Metrics RMSE; R-squared; MAE
test_score = postResample(pred = ridge.pred, obs = ytest)
train_score = postResample(pred = ridge.pred_trn, obs = rtrain$Life.expectancy)


##### Visualize prediction vs actual
x_ridge = 1:dim(xtest)[1]
plot(x_fwd, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Ridge Regression)", ylab="Life expectancy")
lines(x_fwd, ridge.pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-30.png)<!-- -->

``` r
#### Scatter plot
plot(ridge.pred ~ ytest, main = "Original vs Predicted scatter plot (Ridge regression)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-31.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(coef(ridge.mod,s=bestlambda))-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = rmse_trn^2
n=dim(x)[1]
p = length(coef(ridge.mod,s=bestlambda))-1
adjrsqd_trn = 1 - (1 - rsqd_trn) * ((n - 1)/(n-p-1))

eval_test_df = rbind(eval_test_df, c('Ridge', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
eval_train_df = rbind(eval_train_df, c('Ridge', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))


#####################################################################################
#                            Elastic Net Regression                                 #
#####################################################################################

library(glmnetUtils)
```

    ## 
    ## Attaching package: 'glmnetUtils'

    ## The following objects are masked from 'package:glmnet':
    ## 
    ##     cv.glmnet, glmnet

``` r
cva.out = cva.glmnet(x,y)
plot(cva.out)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-32.png)<!-- -->

``` r
alpha = cva.out$alpha
mse = sapply(cva.out$modlist, function(mod) {min(mod$cvm)})
lambdaMin <- sapply(cva.out$modlist, `[[`, "lambda.min")
min_mse <- which.min(mse)
cva.min = data.frame(alpha = alpha[min_mse], lambdaMin = lambdaMin[min_mse], mse = mse[min_mse])
cva.min
```

    ##   alpha lambdaMin   mse
    ## 1     1   0.01455 6.316

``` r
elastic.mod = glmnet(x,y, alpha = cva.min$alpha, lambda = cva.min$lambdaMin)
elastic.pred_trn=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=x)
elastic.pred=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest)
elastic.pred_coef=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest, type = "coef")

testMSE_ELASTIC<-mean((ytest-elastic.pred)^2)
testMSE_ELASTIC
```

    ## [1] 6.109

``` r
# Metrics RMSE; R-squared; MAE
train_score = postResample(pred = elastic.pred_trn, obs = rtrain$Life.expectancy)
test_score = postResample(pred = elastic.pred, obs = ytest)

##### Visualize prediction vs actual
x_eNET = 1:dim(xtest)[1]
plot(x_eNET, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (ElasticNet Regression)", ylab="Life expectancy")
lines(x_fwd, elastic.pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-33.png)<!-- -->

``` r
#### Scatter plot
plot(elastic.pred ~ ytest, main = "Original vs Predicted scatter plot (ElasticNet regression)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-34.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(coef(elastic.mod))-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = rmse_trn^2
n=dim(x)[1]
p = length(coef(elastic.mod,s=bestlambda))-1
adjrsqd_trn = 1 - (1 - rsqd_trn) * ((n - 1)/(n-p-1))


eval_test_df = rbind(eval_test_df, c('ElasticNet', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('ElasticNet', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))

#####################################################################################
#                                      Manual MLR - Tamas                           #
#####################################################################################
# 5-fold cross validation
cv <- trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE
)

MLRT = train(
  Life.expectancy ~ Status + Continent + Income.composition.of.resources + Schooling + log.percentage.expenditure + Year + Adult.Mortality + infant.deaths,
  data = rtrain,
  method = "lm",
   preProcess = c("center", "scale"),
  trControl = cv)

### Visualize VIF
MLR_VIF = vif(MLRT$finalModel)
barplot(MLR_VIF, main = 'VIF Values (Custom MLR - Tamas)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-35.png)<!-- -->

``` r
### Hypothesis testing
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.955  -1.475  -0.267   1.135  10.815 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value
    ## (Intercept)                      69.1443     0.0515 1342.23
    ## StatusDeveloping                 -0.3360     0.0765   -4.39
    ## ContinentAmericas                 1.2624     0.0745   16.95
    ## ContinentAsia                     0.5754     0.0782    7.36
    ## ContinentEurope                   1.0985     0.0947   11.60
    ## ContinentOceania                  0.2431     0.0597    4.07
    ## Income.composition.of.resources   0.6185     0.0939    6.59
    ## Schooling                         1.6051     0.0987   16.26
    ## log.percentage.expenditure        0.2799     0.0574    4.87
    ## Year                              0.1342     0.0558    2.41
    ## Adult.Mortality                  -6.5059     0.0818  -79.54
    ## infant.deaths                    -0.3459     0.0534   -6.48
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                    0.00001167597011 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00000000000026 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                    0.00004774710542 ***
    ## Income.composition.of.resources     0.00000000005453 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.percentage.expenditure          0.00000116051310 ***
    ## Year                                           0.016 *  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                       0.00000000011261 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.57 on 2477 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.928 
    ## F-statistic: 2.94e+03 on 11 and 2477 DF,  p-value: <0.0000000000000002

``` r
# Predicting
train_pred = predict(MLRT, rtrain)
test_pred = predict(MLRT, rtest)

# Scoring the final model on Training and Test set
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.955  -1.475  -0.267   1.135  10.815 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value
    ## (Intercept)                      69.1443     0.0515 1342.23
    ## StatusDeveloping                 -0.3360     0.0765   -4.39
    ## ContinentAmericas                 1.2624     0.0745   16.95
    ## ContinentAsia                     0.5754     0.0782    7.36
    ## ContinentEurope                   1.0985     0.0947   11.60
    ## ContinentOceania                  0.2431     0.0597    4.07
    ## Income.composition.of.resources   0.6185     0.0939    6.59
    ## Schooling                         1.6051     0.0987   16.26
    ## log.percentage.expenditure        0.2799     0.0574    4.87
    ## Year                              0.1342     0.0558    2.41
    ## Adult.Mortality                  -6.5059     0.0818  -79.54
    ## infant.deaths                    -0.3459     0.0534   -6.48
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                    0.00001167597011 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00000000000026 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                    0.00004774710542 ***
    ## Income.composition.of.resources     0.00000000005453 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.percentage.expenditure          0.00000116051310 ***
    ## Year                                           0.016 *  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                       0.00000000011261 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.57 on 2477 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.928 
    ## F-statistic: 2.94e+03 on 11 and 2477 DF,  p-value: <0.0000000000000002

``` r
sm=summary(MLRT$finalModel)
mse_trn = mean(sm$residuals^2)
residuals = resid(MLRT$finalModel)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)

### Checking Multiple Liner Regression model assumptions
fit = lm(Life.expectancy ~ Status + Continent + Income.composition.of.resources + Schooling + log.percentage.expenditure + Year + Adult.Mortality + infant.deaths, data = rtrain)
confint(fit)
```

    ##                                      2.5 %   97.5 %
    ## (Intercept)                     -34.972628 60.48978
    ## StatusDeveloping                 -1.284245 -0.49151
    ## ContinentAmericas                 2.900276  3.65908
    ## ContinentAsia                     0.967813  1.67143
    ## ContinentEurope                   2.233943  3.14275
    ## ContinentOceania                  0.562456  1.60657
    ## Income.composition.of.resources   2.076537  3.83686
    ## Schooling                         0.421909  0.53760
    ## log.percentage.expenditure        0.060767  0.14257
    ## Year                              0.005408  0.05304
    ## Adult.Mortality                  -0.057466 -0.05470
    ## infant.deaths                    -0.003699 -0.00198

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-36.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-37.png)<!-- -->

``` r
plot(fit, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-38.png)<!-- -->

``` r
plot(fit, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-39.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_TMLR = 1:dim(xtest)[1]
plot(x_TMLR, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Manual MLR)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-40.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Tamas)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-41.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared


eval_test_df = rbind(eval_test_df, c('MLR - Tamas', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR - Tamas', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

``` r
#####################################################################################
#                                      Objective 2                                  #
#                   Model with complexity (adding interaction terms)                #
#####################################################################################
#####################################################################################
#                        Tamas's  Manual MLR Interaction                            #
#####################################################################################
fit_interaction = lm(Life.expectancy ~ Continent + Status + Schooling + log.percentage.expenditure + Year + Adult.Mortality + infant.deaths + Income.composition.of.resources:Status + Schooling:Status + I(GDP^2) ,data = train)

interact_pred_train = predict(fit_interaction, train)
train_score = postResample(pred = interact_pred_train, obs = train$Life.expectancy)

interact_pred = predict(fit_interaction, test)
test_score = postResample(pred = interact_pred, obs = test$Life.expectancy)

summary(fit_interaction)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Continent + Status + Schooling + 
    ##     log.percentage.expenditure + Year + Adult.Mortality + infant.deaths + 
    ##     Income.composition.of.resources:Status + Schooling:Status + 
    ##     I(GDP^2), data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.921  -1.449  -0.264   1.123  10.858 
    ## 
    ## Coefficients:
    ##                                                          Estimate
    ## (Intercept)                                      10.7033082346203
    ## ContinentAmericas                                 3.2826482894549
    ## ContinentAsia                                     1.3433954944256
    ## ContinentEurope                                   2.8742205130719
    ## ContinentOceania                                  1.2158854223812
    ## StatusDeveloping                                  7.0484793649483
    ## Schooling                                         0.1659690005870
    ## log.percentage.expenditure                        0.1003444071425
    ## Year                                              0.0262331580264
    ## Adult.Mortality                                  -0.0557935517321
    ## infant.deaths                                    -0.0028297012772
    ## I(GDP^2)                                         -0.0000000000292
    ## StatusDeveloped:Income.composition.of.resources  17.9984512984908
    ## StatusDeveloping:Income.composition.of.resources  2.7437970019810
    ## StatusDeveloping:Schooling                        0.3277235556393
    ##                                                        Std. Error t value
    ## (Intercept)                                      24.3158065637234    0.44
    ## ContinentAmericas                                 0.1937051489887   16.95
    ## ContinentAsia                                     0.1799249993177    7.47
    ## ContinentEurope                                   0.2361877077023   12.17
    ## ContinentOceania                                  0.2684700565321    4.53
    ## StatusDeveloping                                  2.2570156972723    3.12
    ## Schooling                                         0.1026710494270    1.62
    ## log.percentage.expenditure                        0.0215399783688    4.66
    ## Year                                              0.0121336829563    2.16
    ## Adult.Mortality                                   0.0007051036737  -79.13
    ## infant.deaths                                     0.0004375361514   -6.47
    ## I(GDP^2)                                          0.0000000000772   -0.38
    ## StatusDeveloped:Income.composition.of.resources   3.4357348691848    5.24
    ## StatusDeveloping:Income.composition.of.resources  0.4530203089306    6.06
    ## StatusDeveloping:Schooling                        0.1063609578972    3.08
    ##                                                              Pr(>|t|)    
    ## (Intercept)                                                    0.6598    
    ## ContinentAmericas                                < 0.0000000000000002 ***
    ## ContinentAsia                                        0.00000000000011 ***
    ## ContinentEurope                                  < 0.0000000000000002 ***
    ## ContinentOceania                                     0.00000620910741 ***
    ## StatusDeveloping                                               0.0018 ** 
    ## Schooling                                                      0.1061    
    ## log.percentage.expenditure                           0.00000335360281 ***
    ## Year                                                           0.0307 *  
    ## Adult.Mortality                                  < 0.0000000000000002 ***
    ## infant.deaths                                        0.00000000011978 ***
    ## I(GDP^2)                                                       0.7057    
    ## StatusDeveloped:Income.composition.of.resources      0.00000017544447 ***
    ## StatusDeveloping:Income.composition.of.resources     0.00000000160155 ***
    ## StatusDeveloping:Schooling                                     0.0021 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.56 on 2474 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.929 
    ## F-statistic: 2.33e+03 on 14 and 2474 DF,  p-value: <0.0000000000000002

``` r
confint(fit_interaction)
```

    ##                                                              2.5 %
    ## (Intercept)                                      -36.9781240637955
    ## ContinentAmericas                                  2.9028073443004
    ## ContinentAsia                                      0.9905763662064
    ## ContinentEurope                                    2.4110745275578
    ## ContinentOceania                                   0.6894362260060
    ## StatusDeveloping                                   2.6226446354310
    ## Schooling                                         -0.0353610552116
    ## log.percentage.expenditure                         0.0581061611025
    ## Year                                               0.0024399360787
    ## Adult.Mortality                                   -0.0571762059736
    ## infant.deaths                                     -0.0036876761227
    ## I(GDP^2)                                          -0.0000000001805
    ## StatusDeveloped:Income.composition.of.resources   11.2612386491377
    ## StatusDeveloping:Income.composition.of.resources   1.8554589107285
    ## StatusDeveloping:Schooling                         0.1191578722481
    ##                                                            97.5 %
    ## (Intercept)                                      58.3847405330361
    ## ContinentAmericas                                 3.6624892346094
    ## ContinentAsia                                     1.6962146226448
    ## ContinentEurope                                   3.3373664985860
    ## ContinentOceania                                  1.7423346187564
    ## StatusDeveloping                                 11.4743140944657
    ## Schooling                                         0.3672990563856
    ## log.percentage.expenditure                        0.1425826531826
    ## Year                                              0.0500263799741
    ## Adult.Mortality                                  -0.0544108974906
    ## infant.deaths                                    -0.0019717264317
    ## I(GDP^2)                                          0.0000000001222
    ## StatusDeveloped:Income.composition.of.resources  24.7356639478440
    ## StatusDeveloping:Income.composition.of.resources  3.6321350932335
    ## StatusDeveloping:Schooling                        0.5362892390305

``` r
hist(residuals, main = "Histogram of Residuals (Interaction)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-1.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot (Interaction)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-2.png)<!-- -->

``` r
plot(fit_interaction, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-3.png)<!-- -->

``` r
plot(fit_interaction, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-4.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_TMLR = 1:dim(xtest)[1]
plot(x_TMLR, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Interaction)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-5.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Interaction)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-6.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit_interaction$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
sm = summary(fit_interaction)
mse_trn = mean(sm$residuals^2)
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('MLR Interact - Tamas', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR Interact - Tamas', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

``` r
#####################################################################################
#                        Reuven's  Manual MLR Interaction                           #
#####################################################################################

# 5-fold cross validation
cv <- trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE
)
MLRT = train(
  Life.expectancy ~ Income.composition.of.resources + Schooling*log.percentage.expenditure  +  log.HIV.AIDS + log.GDP + BMI + Year + Adult.Mortality,
  data = rtrain,
  method = "lm",
  trControl = cv)

### Visualize VIF
MLR_VIF = vif(MLRT$finalModel)
barplot(MLR_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
### Hypothesis testing
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.932  -1.648  -0.253   1.290  12.389 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value
    ## (Intercept)                            112.71600   25.31631    4.45
    ## Income.composition.of.resources          3.00378    0.47323    6.35
    ## Schooling                                0.63430    0.03923   16.17
    ## log.percentage.expenditure               0.20986    0.08758    2.40
    ## log.HIV.AIDS                            -0.05812    0.07013   -0.83
    ## log.GDP                                  0.23494    0.04415    5.32
    ## BMI                                      0.02178    0.00335    6.51
    ## Year                                    -0.02215    0.01263   -1.75
    ## Adult.Mortality                         -0.05829    0.00107  -54.72
    ## `Schooling:log.percentage.expenditure`  -0.01233    0.00639   -1.93
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                  0.000008871388 ***
    ## Income.composition.of.resources              0.000000000260 ***
    ## Schooling                              < 0.0000000000000002 ***
    ## log.percentage.expenditure                            0.017 *  
    ## log.HIV.AIDS                                          0.407    
    ## log.GDP                                      0.000000112469 ***
    ## BMI                                          0.000000000092 ***
    ## Year                                                  0.080 .  
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`                0.054 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.73 on 2479 degrees of freedom
    ## Multiple R-squared:  0.92,   Adjusted R-squared:  0.919 
    ## F-statistic: 3.15e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
# Predicting
train_pred = predict(MLRT, rtrain)
test_pred = predict(MLRT, rtest)

# Scoring the final model on Training and Validation set
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.932  -1.648  -0.253   1.290  12.389 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value
    ## (Intercept)                            112.71600   25.31631    4.45
    ## Income.composition.of.resources          3.00378    0.47323    6.35
    ## Schooling                                0.63430    0.03923   16.17
    ## log.percentage.expenditure               0.20986    0.08758    2.40
    ## log.HIV.AIDS                            -0.05812    0.07013   -0.83
    ## log.GDP                                  0.23494    0.04415    5.32
    ## BMI                                      0.02178    0.00335    6.51
    ## Year                                    -0.02215    0.01263   -1.75
    ## Adult.Mortality                         -0.05829    0.00107  -54.72
    ## `Schooling:log.percentage.expenditure`  -0.01233    0.00639   -1.93
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                  0.000008871388 ***
    ## Income.composition.of.resources              0.000000000260 ***
    ## Schooling                              < 0.0000000000000002 ***
    ## log.percentage.expenditure                            0.017 *  
    ## log.HIV.AIDS                                          0.407    
    ## log.GDP                                      0.000000112469 ***
    ## BMI                                          0.000000000092 ***
    ## Year                                                  0.080 .  
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`                0.054 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.73 on 2479 degrees of freedom
    ## Multiple R-squared:  0.92,   Adjusted R-squared:  0.919 
    ## F-statistic: 3.15e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
residuals = resid(MLRT$finalModel)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)

### Checking Multiple Liner Regression model assumptions
fit = lm(Life.expectancy ~ Income.composition.of.resources + Schooling:log.percentage.expenditure  +  log.HIV.AIDS + log.GDP + BMI + Year + Adult.Mortality, rtrain)
confint(fit)
```

    ##                                          2.5 %    97.5 %
    ## (Intercept)                          19.762190 125.20912
    ## Income.composition.of.resources       7.282289   8.89128
    ## log.HIV.AIDS                         -0.339067  -0.04755
    ## log.GDP                               0.207767   0.39064
    ## BMI                                   0.023102   0.03697
    ## Year                                 -0.026855   0.02573
    ## Adult.Mortality                      -0.060861  -0.05641
    ## Schooling:log.percentage.expenditure  0.008641   0.01587

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
plot(fit, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

``` r
plot(fit, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-5.png)<!-- -->

``` r
anova(fit)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Life.expectancy
    ##                                        Df Sum Sq Mean Sq  F value
    ## Income.composition.of.resources         1 122886  122886 14513.33
    ## log.HIV.AIDS                            1  57001   57001  6732.05
    ## log.GDP                                 1   3469    3469   409.66
    ## BMI                                     1    774     774    91.37
    ## Year                                    1     49      49     5.81
    ## Adult.Mortality                         1  24126   24126  2849.36
    ## Schooling:log.percentage.expenditure    1    374     374    44.21
    ## Residuals                            2481  21007       8         
    ##                                                    Pr(>F)    
    ## Income.composition.of.resources      < 0.0000000000000002 ***
    ## log.HIV.AIDS                         < 0.0000000000000002 ***
    ## log.GDP                              < 0.0000000000000002 ***
    ## BMI                                  < 0.0000000000000002 ***
    ## Year                                                0.016 *  
    ## Adult.Mortality                      < 0.0000000000000002 ***
    ## Schooling:log.percentage.expenditure       0.000000000036 ***
    ## Residuals                                                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
x_TMLR = 1:dim(xtest)[1]
plot(x_TMLR, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Manual MLR)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-6.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Reuven)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-7.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))


## Train scores
sm = summary(MLRT$finalModel)
mse_trn = mean(sm$residuals^2)
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('MLR Interact - Reuven', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR Interact - Reuven', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

## Observations

-   The interaction of Schooling:log.percentage.expenditure is not
    statistically significant (p-value=0.48) at the .05 alpha level,
    this indicates that the effect of schooling does not depend on the
    linear log of percentage expenditure as it relates to Life
    expectancy holding all other variables constant

-   The interaction of Status:Continent is statistically significant
    (p-value \< 0.0001) at the .05 alpha level, this indicates that the
    effect of whether a continent is developed or not does directly
    correspond to a higher Life expectancy holding all other variables
    constant.

-   All other variables are statistically signficant for the model with
    an R-Squared on our prediction set of 0.8566.

``` r
#######################################################################################
#                               Manual MLR Miguel                                     #
#######################################################################################
#resplit here for now
index<-sample(1:dim(LifeExp)[1],round(dim(LifeExp)[1]*0.85),replace=F)
train = LifeExp[index,]
test = LifeExp[-index,]
#duplicate dataframe
LifeExp2 <- LifeExp
#create cat variables for HIV and Meas as binary No/Yes
LifeExp2 <- LifeExp2 %>% dplyr::mutate(HIV_cat = if_else(LifeExp2$log.HIV.AIDS>log(.1),1,0),
                                       Meas_cat= if_else(LifeExp2$Measles>0,1,0),
                                       log.Adult.Mortality = log(Adult.Mortality),
                                       log.infant.deaths = log(infant.deaths))
train_custom <- LifeExp2[index,]
test_custom <- LifeExp2[-index,]
```

``` r
#retrain/fit/test/predict and measure accuracy
fit.custom <- lm(Life.expectancy ~log.Adult.Mortality + infant.deaths + log.GDP + Measles + HIV_cat + Country + Country:infant.deaths +Country:log.Adult.Mortality + log.Adult.Mortality:infant.deaths:Country,data = train_custom)
#model has interactions between HIV and Measles variables and their respective
#binary categories to have a sort of conditional
#interaction between Country:infant.deaths
#check model performance on the test set and test set prediction
train_custom_pred <- predict(fit.custom,train_custom)
```

    ## Warning in predict.lm(fit.custom, train_custom): prediction from a rank-
    ## deficient fit may be misleading

``` r
test_custom_pred <- predict(fit.custom,test_custom) #rank deficient warning comes from the self-interactions (ex. HIV_cat:HIV)
```

    ## Warning in predict.lm(fit.custom, test_custom): prediction from a rank-deficient
    ## fit may be misleading

``` r
test_score = caret::postResample(pred = test_custom_pred,obs = test_custom$Life.expectancy)
train_score = caret::postResample(pred = train_custom_pred,obs = train_custom$Life.expectancy)
forecast::accuracy(test_custom_pred,test_custom$Life.expectancy)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ##               ME  RMSE    MAE     MPE MAPE
    ## Test set -0.1458 1.673 0.9427 -0.2657 1.38

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Miguel)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-1.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.custom$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
sm=summary(fit.custom)
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trn = rmse_trn^2
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('MLR Interact - Miguel', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR Interact - Miguel', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

``` r
#####################################################################################
#                                      Objective 2                                  #
#                                     KNN - regression                              #
#####################################################################################
# Check for zero variance
caret::nearZeroVar(LifeExp %>% dplyr::select(where(is.numeric)), saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
```

    ## [1] rowname       freqRatio     percentUnique zeroVar       nzv          
    ## <0 rows> (or 0-length row.names)

``` r
cat("No variable with zero variance in the selected list of variables.") 
```

    ## No variable with zero variance in the selected list of variables.

``` r
# Search for optimal k
k_grid = expand.grid(k = seq(3, 25, by = 2))

# Model Training
KNNRegressor = train(
  Life.expectancy~.,
  data = ktrain,
  method = "knn",
  preProcess = c("center", "scale"), 
  tuneGrid = k_grid,
  metric     = "RMSE",
  trControl = cv
  )


prediction_train = predict(KNNRegressor, ktrain)
train_score = postResample(pred = prediction_train, obs = ktrain$Life.expectancy)

prediction_test = predict(KNNRegressor, ktest)
test_score = postResample(pred = prediction_test, obs = ktest$Life.expectancy)

plot(KNNRegressor)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20KNN-1.png)<!-- -->

``` r
varImp(KNNRegressor)
```

    ## loess r-squared variable importance
    ## 
    ##   only 20 most important variables shown (out of 21)
    ## 
    ##                                  Overall
    ## Adult.Mortality                 100.0000
    ## Income.composition.of.resources  85.1323
    ## Schooling                        63.9699
    ## BMI                              45.6457
    ## GDP                              43.0152
    ## HIV.AIDS                         34.3617
    ## percentage.expenditure           28.6895
    ## Diphtheria                       26.5645
    ## Status                           26.0152
    ## thinness..1.19.years             25.0774
    ## thinness.5.9.years               24.2793
    ## Polio                            24.1359
    ## Alcohol                          20.9686
    ## Total.expenditure                11.7708
    ## Hepatitis.B                       9.5150
    ## under.five.deaths                 5.4203
    ## Measles                           4.9490
    ## infant.deaths                     4.2359
    ## Year                              3.6293
    ## Country                           0.0583

``` r
x_knn = 1:length(ktest$Life.expectancy)
plot(x_knn, ktest$Life.expectancy, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction")
lines(x_knn, prediction_test, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20KNN-2.png)<!-- -->

``` r
#### Scatter plot
plot(prediction_test ~ ytest, main = "Original vs Predicted scatter plot (KNN)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20KNN-3.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(KNNRegressor$coefnames)
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trn = rmse^2
mse_trndf = mse_trn
n=dim(ktrain)[1]
p = length(KNNRegressor$coefnames)
adjrsqd_trn = 1 - (1 - rsqd_trn) * ((n - 1)/(n-p-1))

eval_test_df = rbind(eval_test_df, c('KNN', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
eval_train_df = rbind(eval_train_df, c('KNN', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

### Visualize ‘k’ and the most important features

``` r
# Visualize 'k' and the most important features
ggplot(KNNRegressor) + ggtitle("Optimal k value for the lowest RMSE") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-1.png" angle=90 style="display: block; margin: auto;" />

``` r
KNNvarImp = varImp(KNNRegressor)
plot(KNNvarImp, top = 5, main='Top 5 Variable predicting life expectancy (KNN)')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-2.png" angle=90 style="display: block; margin: auto;" />

## KNN without cross validation

``` r
#####################################################################################
#                              KNN without cross validation                         #
#####################################################################################

# Scale the numerical variables as KNN is sensitive to that
drop_for_knn = c("log.HIV.AIDS", "log.GDP", "log.percentage.expenditure", "Region" ,"Continent")
KNN_scale = LifeExp[,!colnames(LifeExp) %in% drop_for_knn] %>% mutate_if(is.numeric, scale)
index<-sample(1:dim(KNN_scale)[1],round(dim(KNN_scale)[1]*0.85),replace=F)
trainKNN = KNN_scale[index,]
testKNN = KNN_scale[-index,]

x_knn=model.matrix(Life.expectancy~.,trainKNN)[,-4]
y_knn=trainKNN$Life.expectancy
xtest_knn = model.matrix(Life.expectancy~.,testKNN)[,-4]
ytest_knn = testKNN$Life.expectancy

knn_eval = data.frame(RMSE = numeric(30), k = numeric(30))

for(i in 1:30)
{
  regressions = knnmodel = knnreg(x_knn, y_knn, ytest_knn, k=i)
  pred_xy = predict(regressions, data.frame(x_knn))
  train_score = postResample(pred = pred_xy, obs = y_knn)
  rmse = train_score[1]
  knn_eval$RMSE[i] = rmse
  knn_eval$k[i] = i
}

plot(knn_eval$k,knn_eval$RMSE, type = "l", xlab = "k", main = "Optimal k value for the lowest RMSE" )
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/KNN%20without%20cross%20validation-1.png)<!-- -->

``` r
index = min(knn_eval[2:30,]$RMSE)
k = knn_eval[index==knn_eval$RMSE,]$k

knnmodel = knnreg(x_knn, y_knn, ytest_knn, k=k)

pred_xy = predict(knnmodel, data.frame(x_knn))
train_score = postResample(pred = pred_xy, obs = y_knn)
pred_y = predict(knnmodel, data.frame(xtest_knn))
test_score = postResample(pred = pred_y, obs = ytest_knn)

print(paste("Training score: ", train_score))
```

    ## [1] "Training score:  0.131413831767923"  "Training score:  0.982959804439541" 
    ## [3] "Training score:  0.0726769381461389"

``` r
print(paste("Test score: ", test_score))
```

    ## [1] "Test score:  0.233137358400609" "Test score:  0.941347902140814"
    ## [3] "Test score:  0.134047589623642"

``` r
plot(pred_y ~ ytest_knn, main = "Original vs Predicted scatter plot (KNN)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/KNN%20without%20cross%20validation-2.png)<!-- -->

``` r
x_knn_plot = 1:length(ktest$Life.expectancy)
plot(x_knn_plot, ytest_knn, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction")
lines(x_knn_plot, pred_y, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/KNN%20without%20cross%20validation-3.png)<!-- -->

``` r
mse = mean((ytest_knn - pred_y)^2)
print(paste("Test MSE: ", mse))
```

    ## [1] "Test MSE:  0.0543530278820138"

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest_knn)[1]
p = dim(knnmodel$learn$X)[2]-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trn = rmse^2
mse_trndf = mse_trn
n=dim(x_knn)[1]
p = dim(knnmodel$learn$X)[2]-1
adjrsqd_trn = 1 - (1 - rsqd_trn) * ((n - 1)/(n-p-1))

eval_test_df = rbind(eval_test_df, c('KNN w/o CV', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
eval_train_df = rbind(eval_train_df, c('KNN w/o CV', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

## KNN regression Observations:

KNN is a non-parametric regression or classification algorithm. In our
case we used the regression version of KNN with the objective of
predicting Life Expectancy. In KNN regression there is no regression
function (Least Squares). If we want to predict the ‘life expectancy’
then we take the ‘k’ number of closest life expectancy values to our new
data point and average them. The average of those ‘k’ number of neighbor
values will be our predicted value. The accuracy of the prediction
depends on the number of neighbors (k). The optimal k value is the
number of neighbors we should use to calculate the average to predict
any new observation. The optimal ‘k’ value is found where the prediction
error is the smallest. The distance for the closest neighbors is
calculated by different distance measures (e.g Euclidean distance).
Since KNN regression is non-parametric regression therefore it does not
assume any functional form like linearity in case of a Linear regression
therefore it can be a more flexible algorithm. The downside is that it
can easily overfit if a small ‘k’ value is selected like in our example.
The KNN regression model’s lowest error is at k=2 which means that the
model is taking the average of the two closest neighbors and provides it
as a prediction. This introduces low bias but high variance since
changing one data point has a high effect on the predicted value. The
model which overfits almost perfectly predicts the data points and will
have low mean square error as we can observe in our KNN model (3.7857 vs
6.093 MLR).

# Training set scores

``` r
knitr::kable(eval_train_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
model_name
</th>
<th style="text-align:left;">
MSE
</th>
<th style="text-align:left;">
R_Squared
</th>
<th style="text-align:left;">
AdjR_Squared
</th>
<th style="text-align:left;">
RMSE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Rsquared
</td>
<td style="text-align:left;">
LASSO
</td>
<td style="text-align:left;">
6.1668
</td>
<td style="text-align:left;">
0.9332
</td>
<td style="text-align:left;">
0.9326
</td>
<td style="text-align:left;">
2.4833
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FWD Selection
</td>
<td style="text-align:left;">
6.2796
</td>
<td style="text-align:left;">
0.9320
</td>
<td style="text-align:left;">
0.9316
</td>
<td style="text-align:left;">
2.5059
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Backward Elim.
</td>
<td style="text-align:left;">
6.2796
</td>
<td style="text-align:left;">
0.9320
</td>
<td style="text-align:left;">
0.9316
</td>
<td style="text-align:left;">
2.5059
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Ridge
</td>
<td style="text-align:left;">
6.8023
</td>
<td style="text-align:left;">
0.9272
</td>
<td style="text-align:left;">
0.9265
</td>
<td style="text-align:left;">
2.6081
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
ElasticNet
</td>
<td style="text-align:left;">
6.1723
</td>
<td style="text-align:left;">
0.9331
</td>
<td style="text-align:left;">
0.9325
</td>
<td style="text-align:left;">
2.4844
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
MLR - Tamas
</td>
<td style="text-align:left;">
6.5733
</td>
<td style="text-align:left;">
0.9288
</td>
<td style="text-align:left;">
0.9285
</td>
<td style="text-align:left;">
2.5639
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
MLR Interact - Tamas
</td>
<td style="text-align:left;">
6.5135
</td>
<td style="text-align:left;">
0.9294
</td>
<td style="text-align:left;">
0.9290
</td>
<td style="text-align:left;">
2.5522
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
MLR Interact - Reuven
</td>
<td style="text-align:left;">
7.4175
</td>
<td style="text-align:left;">
0.9196
</td>
<td style="text-align:left;">
0.9193
</td>
<td style="text-align:left;">
2.7235
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
MLR Interact - Miguel
</td>
<td style="text-align:left;">
1.4579
</td>
<td style="text-align:left;">
0.9838
</td>
<td style="text-align:left;">
0.9787
</td>
<td style="text-align:left;">
1.2074
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
KNN
</td>
<td style="text-align:left;">
3.7857
</td>
<td style="text-align:left;">
0.9829
</td>
<td style="text-align:left;">
0.9814
</td>
<td style="text-align:left;">
1.2574
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
KNN w/o CV
</td>
<td style="text-align:left;">
0.0544
</td>
<td style="text-align:left;">
0.9830
</td>
<td style="text-align:left;">
0.9815
</td>
<td style="text-align:left;">
0.1314
</td>
</tr>
</tbody>
</table>

# Test set scores

``` r
knitr::kable(eval_test_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
model_name
</th>
<th style="text-align:left;">
MSE
</th>
<th style="text-align:left;">
R_Squared
</th>
<th style="text-align:left;">
AdjR_Squared
</th>
<th style="text-align:left;">
RMSE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
RMSE
</td>
<td style="text-align:left;">
LASSO
</td>
<td style="text-align:left;">
6.1035
</td>
<td style="text-align:left;">
0.9253
</td>
<td style="text-align:left;">
0.9216
</td>
<td style="text-align:left;">
2.4705
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FWD Selection
</td>
<td style="text-align:left;">
6.0930
</td>
<td style="text-align:left;">
0.9255
</td>
<td style="text-align:left;">
0.9217
</td>
<td style="text-align:left;">
2.4684
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Backward Elim.
</td>
<td style="text-align:left;">
6.0930
</td>
<td style="text-align:left;">
0.9255
</td>
<td style="text-align:left;">
0.9232
</td>
<td style="text-align:left;">
2.4684
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Ridge
</td>
<td style="text-align:left;">
6.3077
</td>
<td style="text-align:left;">
0.9225
</td>
<td style="text-align:left;">
0.9180
</td>
<td style="text-align:left;">
2.5115
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
ElasticNet
</td>
<td style="text-align:left;">
6.1092
</td>
<td style="text-align:left;">
0.9252
</td>
<td style="text-align:left;">
0.9209
</td>
<td style="text-align:left;">
2.4717
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
MLR - Tamas
</td>
<td style="text-align:left;">
6.4192
</td>
<td style="text-align:left;">
0.9214
</td>
<td style="text-align:left;">
0.9193
</td>
<td style="text-align:left;">
2.5336
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
MLR Interact - Tamas
</td>
<td style="text-align:left;">
6.3827
</td>
<td style="text-align:left;">
0.9218
</td>
<td style="text-align:left;">
0.9192
</td>
<td style="text-align:left;">
2.5264
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
MLR Interact - Reuven
</td>
<td style="text-align:left;">
7.2968
</td>
<td style="text-align:left;">
0.9110
</td>
<td style="text-align:left;">
0.9096
</td>
<td style="text-align:left;">
2.7013
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
MLR Interact - Miguel
</td>
<td style="text-align:left;">
2.7993
</td>
<td style="text-align:left;">
0.9705
</td>
<td style="text-align:left;">
1.0436
</td>
<td style="text-align:left;">
1.6731
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
KNN
</td>
<td style="text-align:left;">
3.7857
</td>
<td style="text-align:left;">
0.9553
</td>
<td style="text-align:left;">
0.9171
</td>
<td style="text-align:left;">
1.9457
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
KNN w/o CV
</td>
<td style="text-align:left;">
0.0544
</td>
<td style="text-align:left;">
0.9413
</td>
<td style="text-align:left;">
0.8916
</td>
<td style="text-align:left;">
0.2331
</td>
</tr>
</tbody>
</table>
