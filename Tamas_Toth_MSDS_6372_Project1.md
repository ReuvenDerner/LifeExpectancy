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
LifeExp = read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Data%20Files/Life_Expectancy_Data.csv', header = TRUE, sep = ",")
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
Lao People’s Democratic Republic
</td>
<td style="text-align:right;">
2014
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
65.3
</td>
<td style="text-align:right;">
199
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
88
</td>
<td style="text-align:right;">
339
</td>
<td style="text-align:right;">
2.9
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
88
</td>
<td style="text-align:right;">
1.87
</td>
<td style="text-align:right;">
88
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.9
</td>
<td style="text-align:right;">
9.0
</td>
<td style="text-align:right;">
0.573
</td>
<td style="text-align:right;">
10.6
</td>
</tr>
<tr>
<td style="text-align:left;">
Finland
</td>
<td style="text-align:right;">
2014
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
89.0
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8.80
</td>
<td style="text-align:right;">
6164.455
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
61.7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
9.68
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
49914.6
</td>
<td style="text-align:right;">
5461512
</td>
<td style="text-align:right;">
0.9
</td>
<td style="text-align:right;">
0.8
</td>
<td style="text-align:right;">
0.890
</td>
<td style="text-align:right;">
17.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Papua New Guinea
</td>
<td style="text-align:right;">
2013
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
62.4
</td>
<td style="text-align:right;">
281
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
25.709
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
47.2
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
4.78
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
0.8
</td>
<td style="text-align:right;">
230.0
</td>
<td style="text-align:right;">
7592865
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
0.506
</td>
<td style="text-align:right;">
10.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ukraine
</td>
<td style="text-align:right;">
2013
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
71.0
</td>
<td style="text-align:right;">
198
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8.44
</td>
<td style="text-align:right;">
52.425
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6.1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
7.67
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
429.7
</td>
<td style="text-align:right;">
454896
</td>
<td style="text-align:right;">
2.3
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
0.744
</td>
<td style="text-align:right;">
15.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Niger
</td>
<td style="text-align:right;">
2001
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
56.0
</td>
<td style="text-align:right;">
283
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
1.818
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
61208
</td>
<td style="text-align:right;">
13.9
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
7.10
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
1.6
</td>
<td style="text-align:right;">
165.3
</td>
<td style="text-align:right;">
11771976
</td>
<td style="text-align:right;">
12.7
</td>
<td style="text-align:right;">
12.7
</td>
<td style="text-align:right;">
0.255
</td>
<td style="text-align:right;">
2.9
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

### Scatterplots

``` r
#####################################################################################
#                        Scatter plots for checking linearity                       #
#####################################################################################
LifeExp$Status = as.factor(LifeExp$Status)

################### Linear - Linear ###################
pairs(Life.expectancy~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-1.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-2.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-3.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~HIV.AIDS+GDP+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-4.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Linear Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-5.png" angle=90 style="display: block; margin: auto;" />

``` r
################### Linear - Log transformation ###################
pairs(Life.expectancy~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-6.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-7.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-8.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~log(HIV.AIDS)+log(GDP)+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-9.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(Life.expectancy~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Linear-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-10.png" angle=90 style="display: block; margin: auto;" />

``` r
################### Log - Log transformation ###################
pairs(log(Life.expectancy)~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-11.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(log(Life.expectancy)~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-12.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(log(Life.expectancy)~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-13.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(log(Life.expectancy)~log(HIV.AIDS)+log(GDP)+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-14.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(log(Life.expectancy)~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"), main = "Log-Log Scatter Plot")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear regression scatter plots-15.png" angle=90 style="display: block; margin: auto;" />

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

### Adding region column to do regional imputation

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

#LifeExp <-  join(LifeExp, regions, by = "Country", type = 'left')
LifeExp <-  merge(x=LifeExp, y=regions, by = "Country", all.x=TRUE)
LifeExp$Country <- replace(LifeExp$Country, LifeExp$Country == "Micronesia", "Micronesia (Federated States of)")
LifeExp$Region <- as.factor(LifeExp$Region)
```

### Fixing the missing values by replacing with median

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
US_GDP <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Data%20Files/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4150786.csv',header = FALSE)
colnames(US_GDP) <- US_GDP[3,]
US_GDP <- rename(US_GDP,c("Country Name"="Country"))
US_GDP <- US_GDP[US_GDP$Country == "United States",c(1,45:60)]
US_GDP <- US_GDP %>% pivot_longer(!Country,names_to = "Year",values_to = "GDP2")
US_GDP$Country <- replace(US_GDP$Country, US_GDP$Country == "United States", "United States of America")
US_GDP$Year <- as.integer(US_GDP$Year)

LifeExp <- left_join(LifeExp,US_GDP, by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate(GDP = ifelse(LifeExp$Country == "United States of America", LifeExp$GDP2, LifeExp$GDP))

#US Schooling
US_Scho <- read.csv(file = "https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Data%20Files/Expected%20years%20of%20schooling%20(years).csv",skip = 6,header = FALSE)
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
US_Inc <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Data%20Files/Income%20index.csv',skip = 5, header = FALSE)
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
pop_all <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Data%20Files/WPP2019_TotalPopulationBySex.csv')
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
adlt_mort <- read.csv(file = 'https://raw.githubusercontent.com/ttoth76/LifeExpectancy/main/Data%20Files/Adult_mort.csv',header = TRUE)
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
```

### Full Correlation Matrix for Linear Regression (Life.expectancy)

``` r
#####################################################################################
#      Full Correlation Matrix for Linear Regression (Life.expectancy)              #
#####################################################################################
plot_correlate(LifeExp)
```

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

## Overall life expectancy over time

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
    ##  2 Life.expectancy          2489     0  6.92e+1 9.58e+0 1.92e-1 1.26e+1 -0.619  
    ##  3 Adult.Mortality          2489     0  1.96e+2 1.16e+2 2.32e+0 1.46e+2  1.31   
    ##  4 infant.deaths            2489     0  3.08e+1 1.16e+2 2.33e+0 2.2 e+1  9.32   
    ##  5 Alcohol                  2489     0  4.59e+0 4.05e+0 8.11e-2 6.76e+0  0.601  
    ##  6 Hepatitis.B              2489     0  8.02e+1 2.46e+1 4.92e-1 2.1 e+1 -1.82   
    ##  7 Measles                  2489     0  2.42e+3 1.12e+4 2.25e+2 3.94e+2  9.69   
    ##  8 BMI                      2489     0  3.82e+1 2.00e+1 4.01e-1 3.67e+1 -0.242  
    ##  9 under.five.deaths        2489     0  4.28e+1 1.59e+2 3.18e+0 2.9 e+1  9.02   
    ## 10 Polio                    2489     0  8.23e+1 2.36e+1 4.73e-1 2   e+1 -2.06   
    ## 11 Total.expenditure        2489     0  5.92e+0 2.46e+0 4.93e-2 3.22e+0  0.593  
    ## 12 Diphtheria               2489     0  8.23e+1 2.36e+1 4.72e-1 1.9 e+1 -2.06   
    ## 13 Population               2489     0  3.68e+7 1.37e+8 2.74e+6 2.29e+7  8.29   
    ## 14 thinness..1.19.years     2489     0  4.89e+0 4.44e+0 8.89e-2 5.6 e+0  1.68   
    ## 15 thinness.5.9.years       2489     0  4.91e+0 4.52e+0 9.05e-2 5.7 e+0  1.76   
    ## 16 Income.composition.of.…  2489     0  6.30e-1 2.07e-1 4.15e-3 2.87e-1 -1.08   
    ## 17 Schooling                2489     0  1.20e+1 3.33e+0 6.67e-2 4.3 e+0 -0.577  
    ## 18 log.HIV.AIDS             2489     0 -1.21e+0 1.62e+0 3.26e-2 2.08e+0  1.28   
    ## 19 log.GDP                  2489     0  7.48e+0 1.80e+0 3.62e-2 2.30e+0 -0.196  
    ## 20 log.percentage.expendi…  2489     0  3.94e+0 2.76e+0 5.53e-2 4.56e+0 -0.0774 
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
    ##  2 Life.expectancy                     0.957 1.81e-26   2489
    ##  3 Adult.Mortality                     0.892 1.07e-38   2489
    ##  4 infant.deaths                       0.243 2.76e-72   2489
    ##  5 Alcohol                             0.909 2.86e-36   2489
    ##  6 Hepatitis.B                         0.723 1.45e-53   2489
    ##  7 Measles                             0.211 4.03e-73   2489
    ##  8 BMI                                 0.925 1.75e-33   2489
    ##  9 under.five.deaths                   0.252 4.68e-72   2489
    ## 10 Polio                               0.692 1.89e-55   2489
    ## 11 Total.expenditure                   0.978 4.87e-19   2489
    ## 12 Diphtheria                          0.691 1.66e-55   2489
    ## 13 Population                          0.225 9.25e-73   2489
    ## 14 thinness..1.19.years                0.842 2.11e-44   2489
    ## 15 thinness.5.9.years                  0.837 7.89e-45   2489
    ## 16 Income.composition.of.resources     0.917 6.49e-35   2489
    ## 17 Schooling                           0.980 3.39e-18   2489
    ## 18 log.HIV.AIDS                        0.711 2.55e-54   2489
    ## 19 log.GDP                             0.992 8.05e-11   2489
    ## 20 log.percentage.expenditure          0.940 1.59e-30   2489

``` r
#Runs a Shapario-Wilk Tests, if the p-value is >= .05 then the data is normally distrusted, if <0.05 the data is not normally distrusted.

#Find Features that are not normally distributed 

rtrain %>%
  normality() %>%
  filter(p_value < 0.05) %>%
  arrange(abs(p_value))
```

    ## # A tibble: 20 × 4
    ##    vars                            statistic  p_value sample
    ##    <chr>                               <dbl>    <dbl>  <dbl>
    ##  1 Measles                             0.211 4.03e-73   2489
    ##  2 Population                          0.225 9.25e-73   2489
    ##  3 infant.deaths                       0.243 2.76e-72   2489
    ##  4 under.five.deaths                   0.252 4.68e-72   2489
    ##  5 Diphtheria                          0.691 1.66e-55   2489
    ##  6 Polio                               0.692 1.89e-55   2489
    ##  7 log.HIV.AIDS                        0.711 2.55e-54   2489
    ##  8 Hepatitis.B                         0.723 1.45e-53   2489
    ##  9 thinness.5.9.years                  0.837 7.89e-45   2489
    ## 10 thinness..1.19.years                0.842 2.11e-44   2489
    ## 11 Adult.Mortality                     0.892 1.07e-38   2489
    ## 12 Alcohol                             0.909 2.86e-36   2489
    ## 13 Income.composition.of.resources     0.917 6.49e-35   2489
    ## 14 BMI                                 0.925 1.75e-33   2489
    ## 15 log.percentage.expenditure          0.940 1.59e-30   2489
    ## 16 Year                                0.948 7.20e-29   2489
    ## 17 Life.expectancy                     0.957 1.81e-26   2489
    ## 18 Total.expenditure                   0.978 4.87e-19   2489
    ## 19 Schooling                           0.980 3.39e-18   2489
    ## 20 log.GDP                             0.992 8.05e-11   2489

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
eval_test_df = data.frame(model_name = character(11), MSE=numeric(11), R_Squared = numeric(11), AdjR_Squared = numeric(11), RMSE = numeric(11))

#####################################################################################
#                                       Lasso                                       #
#####################################################################################
library(glmnet)

grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(x,y,alpha=1, lambda =grid)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-1.png" angle=90 style="display: block; margin: auto;" />

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
lasso.pred=predict(lasso.mod ,s=bestlambda ,newx=xtest)

testMSE_LASSO<-mean((ytest-lasso.pred)^2)
testMSE_LASSO
```

    ## [1] 6.68

``` r
coef(lasso.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                              s1
    ## (Intercept)                     37.261062321342
    ## Year                             0.015949109764
    ## StatusDeveloping                -0.670397326604
    ## Adult.Mortality                 -0.054949012355
    ## infant.deaths                    .             
    ## Alcohol                          0.073163995686
    ## Hepatitis.B                     -0.005758902711
    ## Measles                         -0.000014565707
    ## BMI                              0.009890125719
    ## under.five.deaths               -0.002378366715
    ## Polio                            0.012975090615
    ## Total.expenditure                .             
    ## Diphtheria                       0.014661586860
    ## Population                       0.000000001933
    ## thinness..1.19.years            -0.002619521775
    ## thinness.5.9.years              -0.009559653783
    ## Income.composition.of.resources  2.127560186511
    ## Schooling                        0.417976210784
    ## ContinentAmericas                2.648455619736
    ## ContinentAsia                    1.006232828999
    ## ContinentEurope                  1.893928071481
    ## ContinentOceania                 0.831806308702
    ## log.HIV.AIDS                     .             
    ## log.GDP                          0.138476354964
    ## log.percentage.expenditure       0.063224098671

``` r
lasso_residuals = (ytest - lasso.pred)
hist(lasso_residuals, main = "Histogram of Residuals (LASSO)")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-2.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(lasso_residuals, main = "Residuals plot (LASSO)") 
abline(h=0, col="blue")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-3.png" angle=90 style="display: block; margin: auto;" />

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = lasso.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   2.5846   0.9212   1.8637

``` r
##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
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
    ## -13.386  -1.586  -0.324   1.292  10.974 
    ## 
    ## Coefficients:
    ##                                        Estimate      Std. Error t value
    ## (Intercept)                     72.131112901399 24.175503365165    2.98
    ## Year                            -0.001112675322  0.012083783382   -0.09
    ## Adult.Mortality                 -0.058480129652  0.000650487200  -89.90
    ## Alcohol                          0.174434833911  0.016868005079   10.34
    ## Hepatitis.B                     -0.007758950737  0.002650700908   -2.93
    ## Measles                         -0.000022805970  0.000005606880   -4.07
    ## BMI                              0.015431317106  0.003374834593    4.57
    ## under.five.deaths               -0.002425340326  0.000525660184   -4.61
    ## Polio                            0.011892394521  0.003095207880    3.84
    ## Diphtheria                       0.017411269053  0.003344499084    5.21
    ## Population                       0.000000002577  0.000000000565    4.56
    ## thinness..1.19.years            -0.022306983687  0.035320342714   -0.63
    ## thinness.5.9.years              -0.021979225967  0.034630391699   -0.63
    ## Income.composition.of.resources  2.164357278336  0.465229847777    4.65
    ## Schooling                        0.431267428274  0.031193991953   13.83
    ## log.GDP                          0.148926266995  0.042438455188    3.51
    ## log.percentage.expenditure       0.050738866053  0.023141231758    2.19
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.00288 ** 
    ## Year                                         0.92664    
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## Hepatitis.B                                  0.00345 ** 
    ## Measles                                   0.00004901 ***
    ## BMI                                       0.00000506 ***
    ## under.five.deaths                         0.00000415 ***
    ## Polio                                        0.00013 ***
    ## Diphtheria                                0.00000021 ***
    ## Population                                0.00000540 ***
    ## thinness..1.19.years                         0.52773    
    ## thinness.5.9.years                           0.52570    
    ## Income.composition.of.resources           0.00000346 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                      0.00046 ***
    ## log.percentage.expenditure                   0.02843 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.57 on 2472 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.928 
    ## F-statistic: 2.01e+03 on 16 and 2472 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.lasso.lm_VIF = vif(fit.lasso.lm)
barplot(fit.lasso.lm_VIF, main = 'VIF Values (LASSO)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-4.png" angle=90 style="display: block; margin: auto;" />

``` r
### Fit linear model with factors
### Categorical variables included
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
    ##     Min      1Q  Median      3Q     Max 
    ## -12.381  -1.470  -0.299   1.075  10.200 
    ## 
    ## Coefficients:
    ##                                       Estimate     Std. Error t value
    ## (Intercept)                     24.64513643938 23.87627158845    1.03
    ## StatusDeveloping                -0.71861971269  0.20424804366   -3.52
    ## ContinentAmericas                2.83799966293  0.20595546548   13.78
    ## ContinentAsia                    1.17678625861  0.17817028514    6.60
    ## ContinentEurope                  2.08464295732  0.24805058278    8.40
    ## ContinentOceania                 1.04103816463  0.27368640029    3.80
    ## Year                             0.02222280845  0.01193169772    1.86
    ## Adult.Mortality                 -0.05455745538  0.00073165268  -74.57
    ## Alcohol                          0.07319349325  0.02069621592    3.54
    ## Hepatitis.B                     -0.00723541133  0.00256772818   -2.82
    ## Measles                         -0.00001565069  0.00000542905   -2.88
    ## BMI                              0.00975474075  0.00337196701    2.89
    ## under.five.deaths               -0.00268596432  0.00050992314   -5.27
    ## Polio                            0.01342937427  0.00299116288    4.49
    ## Diphtheria                       0.01526104166  0.00323640211    4.72
    ## Population                       0.00000000226  0.00000000055    4.12
    ## thinness..1.19.years             0.00149378683  0.03416731043    0.04
    ## thinness.5.9.years              -0.01102722130  0.03382356046   -0.33
    ## Income.composition.of.resources  2.08873362540  0.45654854152    4.58
    ## Schooling                        0.41121638601  0.03047890833   13.49
    ## log.GDP                          0.13299142289  0.04132481773    3.22
    ## log.percentage.expenditure       0.07040458010  0.02246904992    3.13
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.30208    
    ## StatusDeveloping                             0.00044 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                         0.000000000049 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                             0.00015 ***
    ## Year                                         0.06265 .  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                      0.00041 ***
    ## Hepatitis.B                                  0.00487 ** 
    ## Measles                                      0.00398 ** 
    ## BMI                                          0.00385 ** 
    ## under.five.deaths                     0.000000150344 ***
    ## Polio                                 0.000007460842 ***
    ## Diphtheria                            0.000002546332 ***
    ## Population                            0.000039794451 ***
    ## thinness..1.19.years                         0.96513    
    ## thinness.5.9.years                           0.74444    
    ## Income.composition.of.resources       0.000004996598 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                      0.00131 ** 
    ## log.percentage.expenditure                   0.00175 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.47 on 2467 degrees of freedom
    ## Multiple R-squared:  0.934,  Adjusted R-squared:  0.933 
    ## F-statistic: 1.66e+03 on 21 and 2467 DF,  p-value: <0.0000000000000002

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
    ## (Intercept)                     -22.174466497830 71.464739376594
    ## StatusDeveloping                 -1.119135022187 -0.318104403199
    ## ContinentAmericas                 2.434136225509  3.241863100342
    ## ContinentAsia                     0.827407505135  1.526165012094
    ## ContinentEurope                   1.598234107798  2.571051806841
    ## ContinentOceania                  0.504359372747  1.577716956506
    ## Year                             -0.001174368427  0.045619985320
    ## Adult.Mortality                  -0.055992172185 -0.053122738571
    ## Alcohol                           0.032609744317  0.113777242181
    ## Hepatitis.B                      -0.012270536407 -0.002200286244
    ## Measles                          -0.000026296646 -0.000005004732
    ## BMI                               0.003142562805  0.016366918695
    ## under.five.deaths                -0.003685885882 -0.001686042760
    ## Polio                             0.007563925067  0.019294823477
    ## Diphtheria                        0.008914696458  0.021607386858
    ## Population                        0.000000001186  0.000000003344
    ## thinness..1.19.years             -0.065505782243  0.068493355912
    ## thinness.5.9.years               -0.077352722114  0.055298279521
    ## Income.composition.of.resources   1.193475697197  2.983991553595
    ## Schooling                         0.351449500700  0.470983271331
    ## log.GDP                           0.051956511289  0.214026334497
    ## log.percentage.expenditure        0.026344434787  0.114464725420

``` r
hist(residuals, main = "Histogram of Residuals (Lasso MLR fit)")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-5.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(residuals, main = "Residuals plot (Lasso MLR fit)") 
abline(h=0, col="blue")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-6.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.lasso.lm3, which = 2)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-7.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.lasso.lm3, which = 4)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-8.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.lasso.lm3, which = 5)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-9.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-10.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (LASSO MLR fit)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-11.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-12.png" angle=90 style="display: block; margin: auto;" />

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
    ##   2.5910   0.9208   1.8751

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
    ## -13.761  -1.545  -0.286   1.233  10.262 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.488930   0.456752  152.14
    ## Adult.Mortality                 -0.059052   0.000642  -92.01
    ## infant.deaths                    0.034745   0.005592    6.21
    ## Alcohol                          0.206105   0.016264   12.67
    ## under.five.deaths               -0.027757   0.004128   -6.72
    ## Polio                            0.010118   0.003078    3.29
    ## Diphtheria                       0.012837   0.003134    4.10
    ## Income.composition.of.resources  2.395097   0.463421    5.17
    ## Schooling                        0.445759   0.031008   14.38
    ## log.GDP                          0.226553   0.037984    5.96
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                         0.000000000605 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                     0.000000000022 ***
    ## Polio                                          0.001 ** 
    ## Diphtheria                            0.000043379017 ***
    ## Income.composition.of.resources       0.000000255070 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000002805 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.59 on 2479 degrees of freedom
    ## Multiple R-squared:  0.927,  Adjusted R-squared:  0.927 
    ## F-statistic: 3.51e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.fwd.lm_VIF = vif(fit.fwd.lm)
barplot(fit.fwd.lm_VIF, main = 'VIF Values (FWD selection)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-13.png" angle=90 style="display: block; margin: auto;" />

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
    ## -13.971  -1.571  -0.267   1.241  10.364 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.340444   0.459572  150.88
    ## Adult.Mortality                 -0.059679   0.000639  -93.46
    ## Alcohol                          0.189414   0.016162   11.72
    ## under.five.deaths               -0.002193   0.000342   -6.41
    ## Polio                            0.010918   0.003099    3.52
    ## Diphtheria                       0.014713   0.003143    4.68
    ## Income.composition.of.resources  2.577797   0.465981    5.53
    ## Schooling                        0.448999   0.031238   14.37
    ## log.GDP                          0.220018   0.038256    5.75
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                      0.00000000017 ***
    ## Polio                                        0.00043 ***
    ## Diphtheria                             0.00000300570 ***
    ## Income.composition.of.resources        0.00000003498 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                0.00000000995 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.61 on 2480 degrees of freedom
    ## Multiple R-squared:  0.926,  Adjusted R-squared:  0.926 
    ## F-statistic: 3.89e+03 on 8 and 2480 DF,  p-value: <0.0000000000000002

``` r
### re-run Visualize VIF
fit.fwd.lm2_VIF = vif(fit.fwd.lm2)
barplot(fit.fwd.lm2_VIF, main = 'Re-test of VIF Values (FWD selection)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-14.png" angle=90 style="display: block; margin: auto;" />

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
    ## -12.525  -1.489  -0.231   1.108  10.082 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     68.514380   0.510756  134.14
    ## StatusDeveloping                -0.635025   0.203534   -3.12
    ## ContinentAmericas                2.977236   0.197889   15.04
    ## ContinentAsia                    1.127696   0.171787    6.56
    ## ContinentEurope                  2.278490   0.240149    9.49
    ## ContinentOceania                 1.326810   0.258619    5.13
    ## Adult.Mortality                 -0.055088   0.000726  -75.90
    ## Alcohol                          0.062233   0.020523    3.03
    ## under.five.deaths               -0.001833   0.000333   -5.51
    ## Polio                            0.012593   0.002974    4.23
    ## Diphtheria                       0.013478   0.003027    4.45
    ## Income.composition.of.resources  2.571985   0.450552    5.71
    ## Schooling                        0.418113   0.030410   13.75
    ## log.GDP                          0.215794   0.036798    5.86
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                              0.0018 ** 
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                         0.000000000063 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                      0.000000311614 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                       0.0025 ** 
    ## under.five.deaths                     0.000000039235 ***
    ## Polio                                 0.000023730933 ***
    ## Diphtheria                            0.000008888029 ***
    ## Income.composition.of.resources       0.000000012757 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000005111 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.49 on 2475 degrees of freedom
    ## Multiple R-squared:  0.933,  Adjusted R-squared:  0.932 
    ## F-statistic: 2.63e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

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
    ## (Intercept)                     67.512826 69.515934
    ## StatusDeveloping                -1.034140 -0.235910
    ## ContinentAmericas                2.589191  3.365281
    ## ContinentAsia                    0.790834  1.464558
    ## ContinentEurope                  1.807576  2.749404
    ## ContinentOceania                 0.819677  1.833942
    ## Adult.Mortality                 -0.056511 -0.053664
    ## Alcohol                          0.021989  0.102478
    ## under.five.deaths               -0.002485 -0.001181
    ## Polio                            0.006762  0.018424
    ## Diphtheria                       0.007541  0.019415
    ## Income.composition.of.resources  1.688488  3.455483
    ## Schooling                        0.358481  0.477745
    ## log.GDP                          0.143636  0.287952

``` r
hist(residuals, main = "Histogram of Residuals")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-15.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-16.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.fwd.lm3, which = 2)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-17.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.fwd.lm3, which = 4)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-18.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.fwd.lm3, which = 5)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-19.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-20.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (FWD Selection fit)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-21.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-22.png" angle=90 style="display: block; margin: auto;" />

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
    ##   2.5910   0.9208   1.8751

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
    ## -13.761  -1.545  -0.286   1.233  10.262 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.488930   0.456752  152.14
    ## Adult.Mortality                 -0.059052   0.000642  -92.01
    ## infant.deaths                    0.034745   0.005592    6.21
    ## Alcohol                          0.206105   0.016264   12.67
    ## under.five.deaths               -0.027757   0.004128   -6.72
    ## Polio                            0.010118   0.003078    3.29
    ## Diphtheria                       0.012837   0.003134    4.10
    ## Income.composition.of.resources  2.395097   0.463421    5.17
    ## Schooling                        0.445759   0.031008   14.38
    ## log.GDP                          0.226553   0.037984    5.96
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                         0.000000000605 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                     0.000000000022 ***
    ## Polio                                          0.001 ** 
    ## Diphtheria                            0.000043379017 ***
    ## Income.composition.of.resources       0.000000255070 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000002805 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.59 on 2479 degrees of freedom
    ## Multiple R-squared:  0.927,  Adjusted R-squared:  0.927 
    ## F-statistic: 3.51e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.bck.lm_VIF = vif(fit.bck.lm)
barplot(fit.bck.lm_VIF, main = 'VIF Values (Backward elimination)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-23.png" angle=90 style="display: block; margin: auto;" />

``` r
#two are correlated and giving a high VIF it's under.five and infant.deaths
fit.bck.lm2 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                   Diphtheria+Income.composition.of.resources + Schooling + log.GDP,data = rtrain)


fit.bck.lm2_VIF = vif(fit.bck.lm2)
barplot(fit.bck.lm2_VIF, main = 'VIF Values (Backward elimination)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-24.png" angle=90 style="display: block; margin: auto;" />

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
    ## -12.525  -1.489  -0.231   1.108  10.082 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     68.514380   0.510756  134.14
    ## StatusDeveloping                -0.635025   0.203534   -3.12
    ## ContinentAmericas                2.977236   0.197889   15.04
    ## ContinentAsia                    1.127696   0.171787    6.56
    ## ContinentEurope                  2.278490   0.240149    9.49
    ## ContinentOceania                 1.326810   0.258619    5.13
    ## Adult.Mortality                 -0.055088   0.000726  -75.90
    ## Alcohol                          0.062233   0.020523    3.03
    ## under.five.deaths               -0.001833   0.000333   -5.51
    ## Polio                            0.012593   0.002974    4.23
    ## Diphtheria                       0.013478   0.003027    4.45
    ## Income.composition.of.resources  2.571985   0.450552    5.71
    ## Schooling                        0.418113   0.030410   13.75
    ## log.GDP                          0.215794   0.036798    5.86
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                              0.0018 ** 
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                         0.000000000063 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                      0.000000311614 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                       0.0025 ** 
    ## under.five.deaths                     0.000000039235 ***
    ## Polio                                 0.000023730933 ***
    ## Diphtheria                            0.000008888029 ***
    ## Income.composition.of.resources       0.000000012757 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000005111 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.49 on 2475 degrees of freedom
    ## Multiple R-squared:  0.933,  Adjusted R-squared:  0.932 
    ## F-statistic: 2.63e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

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
    ## (Intercept)                     67.512826 69.515934
    ## StatusDeveloping                -1.034140 -0.235910
    ## ContinentAmericas                2.589191  3.365281
    ## ContinentAsia                    0.790834  1.464558
    ## ContinentEurope                  1.807576  2.749404
    ## ContinentOceania                 0.819677  1.833942
    ## Adult.Mortality                 -0.056511 -0.053664
    ## Alcohol                          0.021989  0.102478
    ## under.five.deaths               -0.002485 -0.001181
    ## Polio                            0.006762  0.018424
    ## Diphtheria                       0.007541  0.019415
    ## Income.composition.of.resources  1.688488  3.455483
    ## Schooling                        0.358481  0.477745
    ## log.GDP                          0.143636  0.287952

``` r
hist(residuals, main = "Histogram of Residuals (Backward elimination)")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-25.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(residuals, main = "Residuals plot (Backward elimination)") 
abline(h=0, col="blue")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-26.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.fwd.lm3, which = 2)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-27.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.fwd.lm3, which = 4)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-28.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit.fwd.lm3, which = 5)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-29.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-30.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Backward elimination)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-31.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-32.png" angle=90 style="display: block; margin: auto;" />

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
ridge.pred_trn=predict(ridge.mod ,s=bestlambda ,newx=x)
ridge.pred=predict(ridge.mod ,s=bestlambda ,newx=xtest)

testMSE_RIDGE<-mean((ytest-ridge.pred)^2)
testMSE_RIDGE
```

    ## [1] 7.019

``` r
coef(ridge.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                              s1
    ## (Intercept)                     -9.720405670525
    ## Year                             0.037107042524
    ## StatusDeveloping                -1.166796463456
    ## Adult.Mortality                 -0.039923240136
    ## infant.deaths                   -0.000939624593
    ## Alcohol                          0.056152046411
    ## Hepatitis.B                     -0.005094813290
    ## Measles                         -0.000014664374
    ## BMI                              0.013426639626
    ## under.five.deaths               -0.001683167640
    ## Polio                            0.015200489569
    ## Total.expenditure                0.002196875234
    ## Diphtheria                       0.016874286048
    ## Population                       0.000000002381
    ## thinness..1.19.years            -0.017871069064
    ## thinness.5.9.years              -0.028025844315
    ## Income.composition.of.resources  3.606247627611
    ## Schooling                        0.371211508487
    ## ContinentAmericas                2.582571404724
    ## ContinentAsia                    0.936449182745
    ## ContinentEurope                  1.571120413542
    ## ContinentOceania                 0.470300273633
    ## log.HIV.AIDS                    -0.702556953309
    ## log.GDP                          0.182784591244
    ## log.percentage.expenditure       0.112649221409

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-33.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(ridge.pred ~ ytest, main = "Original vs Predicted scatter plot (Ridge regression)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-34.png" angle=90 style="display: block; margin: auto;" />

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
cva.out = cva.glmnet(x,y)
plot(cva.out)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-35.png" angle=90 style="display: block; margin: auto;" />

``` r
alpha = cva.out$alpha
mse = sapply(cva.out$modlist, function(mod) {min(mod$cvm)})
lambdaMin <- sapply(cva.out$modlist, `[[`, "lambda.min")
min_mse <- which.min(mse)
cva.min = data.frame(alpha = alpha[min_mse], lambdaMin = lambdaMin[min_mse], mse = mse[min_mse])
cva.min
```

    ##   alpha lambdaMin   mse
    ## 1 0.064   0.03867 6.181

``` r
elastic.mod = glmnet(x,y, alpha = cva.min$alpha, lambda = cva.min$lambdaMin)
elastic.pred_trn=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=x)
elastic.pred=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest)
elastic.pred_coef=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest, type = "coef")

testMSE_ELASTIC<-mean((ytest-elastic.pred)^2)
testMSE_ELASTIC
```

    ## [1] 6.699

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-36.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(elastic.pred ~ ytest, main = "Original vs Predicted scatter plot (ElasticNet regression)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-37.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-38.png" angle=90 style="display: block; margin: auto;" />

``` r
### Hypothesis testing
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -11.91  -1.48  -0.28   1.12  10.90 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value
    ## (Intercept)                      69.1944     0.0511 1353.82
    ## StatusDeveloping                 -0.3379     0.0757   -4.47
    ## ContinentAmericas                 1.3056     0.0745   17.51
    ## ContinentAsia                     0.5871     0.0781    7.52
    ## ContinentEurope                   1.1256     0.0945   11.92
    ## ContinentOceania                  0.2564     0.0595    4.31
    ## Income.composition.of.resources   0.5974     0.0957    6.24
    ## Schooling                         1.6397     0.0997   16.45
    ## log.percentage.expenditure        0.2926     0.0572    5.12
    ## Year                              0.1329     0.0553    2.40
    ## Adult.Mortality                  -6.4296     0.0819  -78.52
    ## infant.deaths                    -0.3364     0.0530   -6.34
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                   0.000008287171584 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                      0.000000000000076 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                   0.000016792364347 ***
    ## Income.composition.of.resources    0.000000000512798 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.percentage.expenditure         0.000000331908506 ***
    ## Year                                           0.016 *  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                      0.000000000263899 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.55 on 2477 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.929 
    ## F-statistic: 2.97e+03 on 11 and 2477 DF,  p-value: <0.0000000000000002

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
    ##    Min     1Q Median     3Q    Max 
    ## -11.91  -1.48  -0.28   1.12  10.90 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value
    ## (Intercept)                      69.1944     0.0511 1353.82
    ## StatusDeveloping                 -0.3379     0.0757   -4.47
    ## ContinentAmericas                 1.3056     0.0745   17.51
    ## ContinentAsia                     0.5871     0.0781    7.52
    ## ContinentEurope                   1.1256     0.0945   11.92
    ## ContinentOceania                  0.2564     0.0595    4.31
    ## Income.composition.of.resources   0.5974     0.0957    6.24
    ## Schooling                         1.6397     0.0997   16.45
    ## log.percentage.expenditure        0.2926     0.0572    5.12
    ## Year                              0.1329     0.0553    2.40
    ## Adult.Mortality                  -6.4296     0.0819  -78.52
    ## infant.deaths                    -0.3364     0.0530   -6.34
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                   0.000008287171584 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                      0.000000000000076 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                   0.000016792364347 ***
    ## Income.composition.of.resources    0.000000000512798 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.percentage.expenditure         0.000000331908506 ***
    ## Year                                           0.016 *  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                      0.000000000263899 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.55 on 2477 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.929 
    ## F-statistic: 2.97e+03 on 11 and 2477 DF,  p-value: <0.0000000000000002

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

    ##                                      2.5 %    97.5 %
    ## (Intercept)                     -34.338511 60.408841
    ## StatusDeveloping                 -1.277932 -0.498236
    ## ContinentAmericas                 3.006963  3.765183
    ## ContinentAsia                     0.994795  1.696674
    ## ContinentEurope                   2.290216  3.192496
    ## ContinentOceania                  0.621606  1.658399
    ## Income.composition.of.resources   1.978023  3.790878
    ## Schooling                         0.433811  0.551247
    ## log.percentage.expenditure        0.065446  0.146737
    ## Year                              0.005317  0.052587
    ## Adult.Mortality                  -0.057011 -0.054233
    ## infant.deaths                    -0.003785 -0.001998

``` r
hist(residuals, main = "Histogram of Residuals")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-39.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-40.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 2)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-41.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 4)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-42.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 5)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-43.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-44.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Tamas)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model building-45.png" angle=90 style="display: block; margin: auto;" />

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
    ## -11.874  -1.443  -0.284   1.133  10.945 
    ## 
    ## Coefficients:
    ##                                                          Estimate
    ## (Intercept)                                      11.3653142046687
    ## ContinentAmericas                                 3.3911992378426
    ## ContinentAsia                                     1.3673080187136
    ## ContinentEurope                                   2.9246966950438
    ## ContinentOceania                                  1.2785446390456
    ## StatusDeveloping                                  6.6391112121189
    ## Schooling                                         0.1699537478581
    ## log.percentage.expenditure                        0.1041169971460
    ## Year                                              0.0259677262553
    ## Adult.Mortality                                  -0.0553132351683
    ## infant.deaths                                    -0.0028744826609
    ## I(GDP^2)                                         -0.0000000000112
    ## StatusDeveloped:Income.composition.of.resources  17.6148673203680
    ## StatusDeveloping:Income.composition.of.resources  2.6395530535259
    ## StatusDeveloping:Schooling                        0.3390982582799
    ##                                                        Std. Error t value
    ## (Intercept)                                      24.1336243386802    0.47
    ## ContinentAmericas                                 0.1935092926417   17.52
    ## ContinentAsia                                     0.1795016990312    7.62
    ## ContinentEurope                                   0.2345824880245   12.47
    ## ContinentOceania                                  0.2664040090226    4.80
    ## StatusDeveloping                                  2.2204928546854    2.99
    ## Schooling                                         0.1007440790950    1.69
    ## log.percentage.expenditure                        0.0214264001802    4.86
    ## Year                                              0.0120422254531    2.16
    ## Adult.Mortality                                   0.0007084660860  -78.07
    ## infant.deaths                                     0.0004548249869   -6.32
    ## I(GDP^2)                                          0.0000000000761   -0.15
    ## StatusDeveloped:Income.composition.of.resources   3.3979285482048    5.18
    ## StatusDeveloping:Income.composition.of.resources  0.4672929722246    5.65
    ## StatusDeveloping:Schooling                        0.1045775270480    3.24
    ##                                                              Pr(>|t|)    
    ## (Intercept)                                                    0.6377    
    ## ContinentAmericas                                < 0.0000000000000002 ***
    ## ContinentAsia                                       0.000000000000037 ***
    ## ContinentEurope                                  < 0.0000000000000002 ***
    ## ContinentOceania                                    0.000001687273468 ***
    ## StatusDeveloping                                               0.0028 ** 
    ## Schooling                                                      0.0917 .  
    ## log.percentage.expenditure                          0.000001251776824 ***
    ## Year                                                           0.0311 *  
    ## Adult.Mortality                                  < 0.0000000000000002 ***
    ## infant.deaths                                       0.000000000309284 ***
    ## I(GDP^2)                                                       0.8834    
    ## StatusDeveloped:Income.composition.of.resources     0.000000234741042 ***
    ## StatusDeveloping:Income.composition.of.resources    0.000000018026212 ***
    ## StatusDeveloping:Schooling                                     0.0012 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.54 on 2474 degrees of freedom
    ## Multiple R-squared:  0.93,   Adjusted R-squared:  0.93 
    ## F-statistic: 2.35e+03 on 14 and 2474 DF,  p-value: <0.0000000000000002

``` r
confint(fit_interaction)
```

    ##                                                              2.5 %
    ## (Intercept)                                      -35.9588727189491
    ## ContinentAmericas                                  3.0117423519677
    ## ContinentAsia                                      1.0153189499005
    ## ContinentEurope                                    2.4646984222405
    ## ContinentOceania                                   0.7561468034258
    ## StatusDeveloping                                   2.2848949765878
    ## Schooling                                         -0.0275976668666
    ## log.percentage.expenditure                         0.0621014692254
    ## Year                                               0.0023538454589
    ## Adult.Mortality                                   -0.0567024828425
    ## infant.deaths                                     -0.0037663595873
    ## I(GDP^2)                                          -0.0000000001604
    ## StatusDeveloped:Income.composition.of.resources   10.9517899677213
    ## StatusDeveloping:Income.composition.of.resources   1.7232273638860
    ## StatusDeveloping:Schooling                         0.1340297460426
    ##                                                            97.5 %
    ## (Intercept)                                      58.6895011282865
    ## ContinentAmericas                                 3.7706561237176
    ## ContinentAsia                                     1.7192970875267
    ## ContinentEurope                                   3.3846949678472
    ## ContinentOceania                                  1.8009424746653
    ## StatusDeveloping                                 10.9933274476500
    ## Schooling                                         0.3675051625828
    ## log.percentage.expenditure                        0.1461325250666
    ## Year                                              0.0495816070517
    ## Adult.Mortality                                  -0.0539239874941
    ## infant.deaths                                    -0.0019826057345
    ## I(GDP^2)                                          0.0000000001381
    ## StatusDeveloped:Income.composition.of.resources  24.2779446730147
    ## StatusDeveloping:Income.composition.of.resources  3.5558787431657
    ## StatusDeveloping:Schooling                        0.5441667705172

``` r
residuals = resid(fit_interaction)
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
plot(fit_interaction, which = 5)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-5.png)<!-- -->

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

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-6.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Interaction)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-7.png)<!-- -->

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
  Life.expectancy ~ Income.composition.of.resources + Schooling:log.percentage.expenditure  +  log.HIV.AIDS + log.GDP + BMI + Year + Adult.Mortality,
  data = rtrain,
  method = "lm",
  trControl = cv)

### Visualize VIF
MLR_VIF = vif(MLRT$finalModel)
barplot(MLR_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-1.png" angle=90 style="display: block; margin: auto;" />

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
    ## -13.070  -1.765  -0.249   1.428  16.552 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value
    ## (Intercept)                            79.84731   26.81081    2.98
    ## Income.composition.of.resources         8.38414    0.41813   20.05
    ## log.HIV.AIDS                           -0.16722    0.07412   -2.26
    ## log.GDP                                 0.30315    0.04636    6.54
    ## BMI                                     0.03034    0.00353    8.58
    ## Year                                   -0.00434    0.01337   -0.32
    ## Adult.Mortality                        -0.05849    0.00114  -51.22
    ## `Schooling:log.percentage.expenditure`  0.01181    0.00184    6.42
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                          0.0029 ** 
    ## Income.composition.of.resources        < 0.0000000000000002 ***
    ## log.HIV.AIDS                                         0.0242 *  
    ## log.GDP                                      0.000000000075 ***
    ## BMI                                    < 0.0000000000000002 ***
    ## Year                                                 0.7457    
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`       0.000000000168 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.9 on 2481 degrees of freedom
    ## Multiple R-squared:  0.909,  Adjusted R-squared:  0.909 
    ## F-statistic: 3.53e+03 on 7 and 2481 DF,  p-value: <0.0000000000000002

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
    ## -13.070  -1.765  -0.249   1.428  16.552 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value
    ## (Intercept)                            79.84731   26.81081    2.98
    ## Income.composition.of.resources         8.38414    0.41813   20.05
    ## log.HIV.AIDS                           -0.16722    0.07412   -2.26
    ## log.GDP                                 0.30315    0.04636    6.54
    ## BMI                                     0.03034    0.00353    8.58
    ## Year                                   -0.00434    0.01337   -0.32
    ## Adult.Mortality                        -0.05849    0.00114  -51.22
    ## `Schooling:log.percentage.expenditure`  0.01181    0.00184    6.42
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                          0.0029 ** 
    ## Income.composition.of.resources        < 0.0000000000000002 ***
    ## log.HIV.AIDS                                         0.0242 *  
    ## log.GDP                                      0.000000000075 ***
    ## BMI                                    < 0.0000000000000002 ***
    ## Year                                                 0.7457    
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`       0.000000000168 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.9 on 2481 degrees of freedom
    ## Multiple R-squared:  0.909,  Adjusted R-squared:  0.909 
    ## F-statistic: 3.53e+03 on 7 and 2481 DF,  p-value: <0.0000000000000002

``` r
residuals = resid(MLRT$finalModel)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)

### Checking Multiple Liner Regression model assumptions
fit = lm(Life.expectancy ~ Income.composition.of.resources + Schooling:log.percentage.expenditure  +  log.HIV.AIDS + log.GDP + BMI + Year + Adult.Mortality, rtrain)
confint(fit)
```

    ##                                          2.5 %    97.5 %
    ## (Intercept)                          27.273451 132.42118
    ## Income.composition.of.resources       7.564223   9.20406
    ## log.HIV.AIDS                         -0.312554  -0.02188
    ## log.GDP                               0.212235   0.39407
    ## BMI                                   0.023412   0.03727
    ## Year                                 -0.030553   0.02188
    ## Adult.Mortality                      -0.060730  -0.05625
    ## Schooling:log.percentage.expenditure  0.008202   0.01542

``` r
hist(residuals, main = "Histogram of Residuals")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-2.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-3.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 2)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-4.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 4)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-5.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 5)
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-6.png" angle=90 style="display: block; margin: auto;" />

``` r
anova(fit)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Life.expectancy
    ##                                        Df Sum Sq Mean Sq  F value
    ## Income.composition.of.resources         1 124915  124915 14882.44
    ## log.HIV.AIDS                            1  54559   54559  6500.18
    ## log.GDP                                 1   3307    3307   394.03
    ## BMI                                     1    846     846   100.80
    ## Year                                    1     43      43     5.08
    ## Adult.Mortality                         1  23486   23486  2798.09
    ## Schooling:log.percentage.expenditure    1    345     345    41.16
    ## Residuals                            2481  20824       8         
    ##                                                    Pr(>F)    
    ## Income.composition.of.resources      < 0.0000000000000002 ***
    ## log.HIV.AIDS                         < 0.0000000000000002 ***
    ## log.GDP                              < 0.0000000000000002 ***
    ## BMI                                  < 0.0000000000000002 ***
    ## Year                                                0.024 *  
    ## Adult.Mortality                      < 0.0000000000000002 ***
    ## Schooling:log.percentage.expenditure        0.00000000017 ***
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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-7.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Reuven)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-8.png" angle=90 style="display: block; margin: auto;" />

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

    ##                ME  RMSE    MAE      MPE  MAPE
    ## Test set -0.02062 1.655 0.9072 -0.07508 1.334

``` r
##fits
residuals = resid(fit.custom)
summary(fit.custom)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ log.Adult.Mortality + infant.deaths + 
    ##     log.GDP + Measles + HIV_cat + Country + Country:infant.deaths + 
    ##     Country:log.Adult.Mortality + log.Adult.Mortality:infant.deaths:Country, 
    ##     data = train_custom)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.776 -0.368 -0.028  0.180  7.354 
    ## 
    ## Coefficients: (137 not defined because of singularities)
    ##                                                                                                     Estimate
    ## (Intercept)                                                                                     632.85215095
    ## log.Adult.Mortality                                                                            -103.81796233
    ## infant.deaths                                                                                    -5.36861402
    ## log.GDP                                                                                           0.00354140
    ## Measles                                                                                          -0.00000387
    ## HIV_cat                                                                                          -0.47231807
    ## CountryAlbania                                                                                 -522.65528722
    ## CountryAlgeria                                                                                 -496.66353411
    ## CountryAngola                                                                                  -286.65707398
    ## CountryAntigua and Barbuda                                                                     -502.59352814
    ## CountryArgentina                                                                               -476.39137524
    ## CountryArmenia                                                                                 -500.38494302
    ## CountryAustralia                                                                               -515.57947267
    ## CountryAustria                                                                                 -465.49895297
    ## CountryAzerbaijan                                                                              -552.09692685
    ## CountryBahamas                                                                                 -508.72325107
    ## CountryBahrain                                                                                 -540.21139113
    ## CountryBangladesh                                                                              -720.72581597
    ## CountryBarbados                                                                                -528.37295899
    ## CountryBelarus                                                                                 -553.40195570
    ## CountryBelgium                                                                                 -408.84644659
    ## CountryBelize                                                                                  -475.86702914
    ## CountryBenin                                                                                   -288.69983378
    ## CountryBhutan                                                                                  -439.20704561
    ## CountryBolivia                                                                                 -177.01181449
    ## CountryBosnia and Herzegovina                                                                  -514.42330938
    ## CountryBotswana                                                                                -430.88701197
    ## CountryBrazil                                                                                  -277.79245522
    ## CountryBrunei Darussalam                                                                       -496.47698534
    ## CountryBulgaria                                                                                -472.79733640
    ## CountryBurkina Faso                                                                           -1429.93209217
    ## CountryBurundi                                                                                  474.19196508
    ## CountryCabo Verde                                                                              -551.23783551
    ## CountryCambodia                                                                                -553.13805312
    ## CountryCameroon                                                                                -400.00577024
    ## CountryCanada                                                                                  -505.24993590
    ## CountryCentral African Republic                                                                -456.86438946
    ## CountryChad                                                                                   -2530.04234188
    ## CountryChile                                                                                   -386.48805675
    ## CountryChina                                                                                   -569.93237044
    ## CountryColombia                                                                                -471.92125706
    ## CountryComoros                                                                                 -398.39147714
    ## CountryCongo                                                                                   -280.74025195
    ## CountryCosta Rica                                                                              -492.81983516
    ## CountryCroatia                                                                                 -507.84140885
    ## CountryCuba                                                                                    -511.51101951
    ## CountryCyprus                                                                                  -500.93578520
    ## CountryCzechia                                                                                 -508.49903015
    ## CountryDemocratic People's Republic of Korea                                                   -424.21287967
    ## CountryDemocratic Republic of the Congo                                                          10.94896267
    ## CountryDenmark                                                                                 -473.98362990
    ## CountryDjibouti                                                                                -783.06515125
    ## CountryDominican Republic                                                                     -1699.34422140
    ## CountryEcuador                                                                                 -505.17278711
    ## CountryEgypt                                                                                   1377.57018248
    ## CountryEl Salvador                                                                             -635.74952934
    ## CountryEquatorial Guinea                                                                       -397.81784805
    ## CountryEritrea                                                                                 -979.55234218
    ## CountryEstonia                                                                                 -534.04535270
    ## CountryEthiopia                                                                                -631.86838534
    ## CountryFiji                                                                                    -498.90222031
    ## CountryFinland                                                                                 -409.16472979
    ## CountryFrance                                                                                  -514.85944459
    ## CountryGabon                                                                                   -466.42616892
    ## CountryGambia                                                                                  -179.81495216
    ## CountryGeorgia                                                                                 -636.23215198
    ## CountryGermany                                                                                   11.39171633
    ## CountryGhana                                                                                  -2784.94799105
    ## CountryGreece                                                                                  -376.82053367
    ## CountryGrenada                                                                                 -581.78867662
    ## CountryGuatemala                                                                              -1403.06690395
    ## CountryGuinea                                                                                  -598.55880567
    ## CountryGuinea-Bissau                                                                           1282.05711003
    ## CountryGuyana                                                                                  -550.19122748
    ## CountryHaiti                                                                                   -853.38788023
    ## CountryHonduras                                                                                -465.08099858
    ## CountryHungary                                                                                 -516.75040787
    ## CountryIceland                                                                                 -540.17129124
    ## CountryIndia                                                                                   -388.75332598
    ## CountryIndonesia                                                                               -854.57918009
    ## CountryIran                                                                                    -516.24549660
    ## CountryIraq                                                                                   -1590.46003678
    ## CountryIreland                                                                                 -482.03743255
    ## CountryIsrael                                                                                  -517.18704871
    ## CountryItaly                                                                                   -566.60648373
    ## CountryIvory Coast                                                                              972.63110414
    ## CountryJamaica                                                                                 -498.30371511
    ## CountryJapan                                                                                   -549.06007775
    ## CountryJordan                                                                                  -473.87352674
    ## CountryKazakhstan                                                                              -524.86183843
    ## CountryKenya                                                                                   -604.75145333
    ## CountryKiribati                                                                                -461.76114994
    ## CountryKuwait                                                                                  -512.06079495
    ## CountryKyrgyzstan                                                                              -944.31207777
    ## CountryLao People's Democratic Republic                                                        -538.59749851
    ## CountryLatvia                                                                                  -539.29680497
    ## CountryLebanon                                                                                 -507.05736242
    ## CountryLesotho                                                                                 -221.20203243
    ## CountryLiberia                                                                                 -468.97510111
    ## CountryLibya                                                                                   1081.88454933
    ## CountryLithuania                                                                               -571.78653289
    ## CountryLuxembourg                                                                              -508.65974111
    ## CountryMadagascar                                                                             -1068.21431556
    ## CountryMalawi                                                                                  -554.01693144
    ## CountryMalaysia                                                                                -496.33458303
    ## CountryMaldives                                                                                -534.70627922
    ## CountryMali                                                                                     889.66167809
    ## CountryMalta                                                                                   -471.68506847
    ## CountryMauritania                                                                             10954.39016871
    ## CountryMauritius                                                                               -462.22066403
    ## CountryMexico                                                                                  -391.41454756
    ## CountryMicronesia (Federated States of)                                                        -468.50744962
    ## CountryMongolia                                                                                -583.11415217
    ## CountryMontenegro                                                                              -511.03977518
    ## CountryMorocco                                                                                 -544.08516133
    ## CountryMozambique                                                                               504.76866363
    ## CountryMyanmar                                                                                 -585.57299978
    ## CountryNamibia                                                                                 -735.07755866
    ## CountryNepal                                                                                   -543.85763648
    ## CountryNetherlands                                                                             -482.42678141
    ## CountryNew Zealand                                                                             -460.07511465
    ## CountryNicaragua                                                                               -323.81105866
    ## CountryNiger                                                                                   6850.53054547
    ## CountryNigeria                                                                                -2466.37639246
    ## CountryNorway                                                                                  -504.29844208
    ## CountryOman                                                                                    -498.02512657
    ## CountryPakistan                                                                               -1163.14767489
    ## CountryPanama                                                                                 -1180.62519083
    ## CountryPapua New Guinea                                                                        -816.01113542
    ## CountryParaguay                                                                                -204.26853970
    ## CountryPeru                                                                                    -580.11376288
    ## CountryPhilippines                                                                             -592.43967282
    ## CountryPoland                                                                                  -519.52212174
    ## CountryPortugal                                                                                -424.83895046
    ## CountryQatar                                                                                   -508.28355815
    ## CountryRepublic of Korea                                                                       -492.80570884
    ## CountryRepublic of Moldova                                                                     -414.44625222
    ## CountryRomania                                                                                 -151.11173782
    ## CountryRussian Federation                                                                      -222.51072463
    ## CountryRwanda                                                                                  -516.29942451
    ## CountrySaint Lucia                                                                             -493.14436601
    ## CountrySaint Vincent and the Grenadines                                                        -704.21500193
    ## CountrySamoa                                                                                   -577.12521884
    ## CountrySao Tome and Principe                                                                   -434.99105284
    ## CountrySaudi Arabia                                                                            -472.74877283
    ## CountrySenegal                                                                                 -575.91945406
    ## CountrySerbia                                                                                  -495.89919558
    ## CountrySeychelles                                                                              -508.77302744
    ## CountrySierra Leone                                                                             495.78706493
    ## CountrySingapore                                                                               -505.98010032
    ## CountrySlovakia                                                                                -510.22445357
    ## CountrySlovenia                                                                                -468.42774612
    ## CountrySolomon Islands                                                                         -527.53102271
    ## CountrySomalia                                                                                -4015.97249269
    ## CountrySouth Africa                                                                             -88.09812098
    ## CountrySouth Sudan                                                                             1160.56861064
    ## CountrySpain                                                                                   -566.86981573
    ## CountrySri Lanka                                                                               -574.36783574
    ## CountrySudan                                                                                  -1217.44196332
    ## CountrySuriname                                                                                -428.64303467
    ## CountrySwaziland                                                                               -542.96667651
    ## CountrySweden                                                                                  -534.32710139
    ## CountrySwitzerland                                                                             -536.96282337
    ## CountrySyrian Arab Republic                                                                    -516.59515260
    ## CountryTajikistan                                                                              -178.37267341
    ## CountryThailand                                                                                -524.39187461
    ## CountryThe former Yugoslav republic of Macedonia                                               -513.72256823
    ## CountryTimor-Leste                                                                             -475.65181857
    ## CountryTogo                                                                                    -246.94880660
    ## CountryTonga                                                                                   -504.29303475
    ## CountryTrinidad and Tobago                                                                     -402.04639840
    ## CountryTunisia                                                                                 -693.37481182
    ## CountryTurkey                                                                                  -270.60042574
    ## CountryTurkmenistan                                                                            -563.14878494
    ## CountryUganda                                                                                  -361.36100544
    ## CountryUkraine                                                                                 -848.70754972
    ## CountryUnited Arab Emirates                                                                    -517.14414202
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                    -664.63259926
    ## CountryUnited Republic of Tanzania                                                             1071.73696430
    ## CountryUnited States of America                                                               -1151.15101561
    ## CountryUruguay                                                                                 -507.44250227
    ## CountryUzbekistan                                                                              -554.88632291
    ## CountryVanuatu                                                                                 -501.74888643
    ## CountryVenezuela                                                                               -743.41156595
    ## CountryViet Nam                                                                                -656.85098470
    ## CountryYemen                                                                                   1228.91477913
    ## CountryZambia                                                                                  -649.84568828
    ## CountryZimbabwe                                                                                -217.33601264
    ## infant.deaths:CountryAlbania                                                                     68.39202038
    ## infant.deaths:CountryAlgeria                                                                      4.92610096
    ## infant.deaths:CountryAngola                                                                       3.30554624
    ## infant.deaths:CountryAntigua and Barbuda                                                                  NA
    ## infant.deaths:CountryArgentina                                                                    2.56770250
    ## infant.deaths:CountryArmenia                                                                              NA
    ## infant.deaths:CountryAustralia                                                                            NA
    ## infant.deaths:CountryAustria                                                                              NA
    ## infant.deaths:CountryAzerbaijan                                                                  14.04537299
    ## infant.deaths:CountryBahamas                                                                              NA
    ## infant.deaths:CountryBahrain                                                                              NA
    ## infant.deaths:CountryBangladesh                                                                   5.22287320
    ## infant.deaths:CountryBarbados                                                                             NA
    ## infant.deaths:CountryBelarus                                                                    101.76821064
    ## infant.deaths:CountryBelgium                                                                    -93.29662562
    ## infant.deaths:CountryBelize                                                                               NA
    ## infant.deaths:CountryBenin                                                                                NA
    ## infant.deaths:CountryBhutan                                                                      10.12433527
    ## infant.deaths:CountryBolivia                                                                     15.02477114
    ## infant.deaths:CountryBosnia and Herzegovina                                                               NA
    ## infant.deaths:CountryBotswana                                                                             NA
    ## infant.deaths:CountryBrazil                                                                       3.04139611
    ## infant.deaths:CountryBrunei Darussalam                                                                    NA
    ## infant.deaths:CountryBulgaria                                                                     5.27117534
    ## infant.deaths:CountryBurkina Faso                                                                23.14925855
    ## infant.deaths:CountryBurundi                                                                    -29.34443679
    ## infant.deaths:CountryCabo Verde                                                                           NA
    ## infant.deaths:CountryCambodia                                                                     6.94525719
    ## infant.deaths:CountryCameroon                                                                     3.57108632
    ## infant.deaths:CountryCanada                                                                               NA
    ## infant.deaths:CountryCentral African Republic                                                     5.05067522
    ## infant.deaths:CountryChad                                                                        51.69901682
    ## infant.deaths:CountryChile                                                                                NA
    ## infant.deaths:CountryChina                                                                        5.39669079
    ## infant.deaths:CountryColombia                                                                     2.12813418
    ## infant.deaths:CountryComoros                                                                    -30.06054729
    ## infant.deaths:CountryCongo                                                                      -28.25184019
    ## infant.deaths:CountryCosta Rica                                                                           NA
    ## infant.deaths:CountryCroatia                                                                              NA
    ## infant.deaths:CountryCuba                                                                                 NA
    ## infant.deaths:CountryCyprus                                                                               NA
    ## infant.deaths:CountryCzechia                                                                              NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                       -0.12037051
    ## infant.deaths:CountryDemocratic Republic of the Congo                                             3.53108916
    ## infant.deaths:CountryDenmark                                                                              NA
    ## infant.deaths:CountryDjibouti                                                                   176.17318575
    ## infant.deaths:CountryDominican Republic                                                         194.96658081
    ## infant.deaths:CountryEcuador                                                                      3.03658652
    ## infant.deaths:CountryEgypt                                                                      -19.50242978
    ## infant.deaths:CountryEl Salvador                                                                 78.27292297
    ## infant.deaths:CountryEquatorial Guinea                                                                    NA
    ## infant.deaths:CountryEritrea                                                                     82.59843213
    ## infant.deaths:CountryEstonia                                                                              NA
    ## infant.deaths:CountryEthiopia                                                                     6.49532187
    ## infant.deaths:CountryFiji                                                                                 NA
    ## infant.deaths:CountryFinland                                                                              NA
    ## infant.deaths:CountryFrance                                                                       7.07204197
    ## infant.deaths:CountryGabon                                                                                NA
    ## infant.deaths:CountryGambia                                                                               NA
    ## infant.deaths:CountryGeorgia                                                                     59.60059299
    ## infant.deaths:CountryGermany                                                                   -165.14907214
    ## infant.deaths:CountryGhana                                                                       65.76999554
    ## infant.deaths:CountryGreece                                                                       5.19293775
    ## infant.deaths:CountryGrenada                                                                              NA
    ## infant.deaths:CountryGuatemala                                                                   84.51356897
    ## infant.deaths:CountryGuinea                                                                       8.46028764
    ## infant.deaths:CountryGuinea-Bissau                                                             -419.06079691
    ## infant.deaths:CountryGuyana                                                                      24.31656447
    ## infant.deaths:CountryHaiti                                                                       23.20489122
    ## infant.deaths:CountryHonduras                                                                     7.08383673
    ## infant.deaths:CountryHungary                                                                     44.27899494
    ## infant.deaths:CountryIceland                                                                              NA
    ## infant.deaths:CountryIndia                                                                        5.38751041
    ## infant.deaths:CountryIndonesia                                                                    7.39505775
    ## infant.deaths:CountryIran                                                                         4.80955933
    ## infant.deaths:CountryIraq                                                                        41.70441356
    ## infant.deaths:CountryIreland                                                                              NA
    ## infant.deaths:CountryIsrael                                                                       5.36022470
    ## infant.deaths:CountryItaly                                                                       23.06692064
    ## infant.deaths:CountryIvory Coast                                                                -18.25746176
    ## infant.deaths:CountryJamaica                                                                              NA
    ## infant.deaths:CountryJapan                                                                       17.23681671
    ## infant.deaths:CountryJordan                                                                               NA
    ## infant.deaths:CountryKazakhstan                                                                  10.83381606
    ## infant.deaths:CountryKenya                                                                        7.13271272
    ## infant.deaths:CountryKiribati                                                                             NA
    ## infant.deaths:CountryKuwait                                                                      25.87283733
    ## infant.deaths:CountryKyrgyzstan                                                                 123.73599630
    ## infant.deaths:CountryLao People's Democratic Republic                                            14.39751575
    ## infant.deaths:CountryLatvia                                                                               NA
    ## infant.deaths:CountryLebanon                                                                              NA
    ## infant.deaths:CountryLesotho                                                                    -43.68469217
    ## infant.deaths:CountryLiberia                                                                      6.64591424
    ## infant.deaths:CountryLibya                                                                     -813.08602567
    ## infant.deaths:CountryLithuania                                                                            NA
    ## infant.deaths:CountryLuxembourg                                                                           NA
    ## infant.deaths:CountryMadagascar                                                                  24.60241860
    ## infant.deaths:CountryMalawi                                                                       7.42588120
    ## infant.deaths:CountryMalaysia                                                                    11.04250162
    ## infant.deaths:CountryMaldives                                                                             NA
    ## infant.deaths:CountryMali                                                                       -17.46423074
    ## infant.deaths:CountryMalta                                                                                NA
    ## infant.deaths:CountryMauritania                                                               -1450.44955709
    ## infant.deaths:CountryMauritius                                                                            NA
    ## infant.deaths:CountryMexico                                                                       2.24359588
    ## infant.deaths:CountryMicronesia (Federated States of)                                                     NA
    ## infant.deaths:CountryMongolia                                                                   120.17472339
    ## infant.deaths:CountryMontenegro                                                                           NA
    ## infant.deaths:CountryMorocco                                                                      4.31962902
    ## infant.deaths:CountryMozambique                                                                 -11.83802844
    ## infant.deaths:CountryMyanmar                                                                      7.26801435
    ## infant.deaths:CountryNamibia                                                                     98.09296794
    ## infant.deaths:CountryNepal                                                                        6.70561327
    ## infant.deaths:CountryNetherlands                                                                          NA
    ## infant.deaths:CountryNew Zealand                                                                          NA
    ## infant.deaths:CountryNicaragua                                                                  -53.14443765
    ## infant.deaths:CountryNiger                                                                     -148.18320680
    ## infant.deaths:CountryNigeria                                                                      9.66076805
    ## infant.deaths:CountryNorway                                                                               NA
    ## infant.deaths:CountryOman                                                                                 NA
    ## infant.deaths:CountryPakistan                                                                     7.28979197
    ## infant.deaths:CountryPanama                                                                     707.42947970
    ## infant.deaths:CountryPapua New Guinea                                                            43.17361042
    ## infant.deaths:CountryParaguay                                                                   -98.43167179
    ## infant.deaths:CountryPeru                                                                         0.55859879
    ## infant.deaths:CountryPhilippines                                                                  6.29568548
    ## infant.deaths:CountryPoland                                                                      12.45163674
    ## infant.deaths:CountryPortugal                                                                   -35.23399596
    ## infant.deaths:CountryQatar                                                                                NA
    ## infant.deaths:CountryRepublic of Korea                                                           16.39531457
    ## infant.deaths:CountryRepublic of Moldova                                                                  NA
    ## infant.deaths:CountryRomania                                                                   -175.04532367
    ## infant.deaths:CountryRussian Federation                                                         -13.76283113
    ## infant.deaths:CountryRwanda                                                                       9.14026168
    ## infant.deaths:CountrySaint Lucia                                                                          NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                                     NA
    ## infant.deaths:CountrySamoa                                                                                NA
    ## infant.deaths:CountrySao Tome and Principe                                                                NA
    ## infant.deaths:CountrySaudi Arabia                                                                 0.50783776
    ## infant.deaths:CountrySenegal                                                                      9.55798475
    ## infant.deaths:CountrySerbia                                                                               NA
    ## infant.deaths:CountrySeychelles                                                                           NA
    ## infant.deaths:CountrySierra Leone                                                               -27.55922792
    ## infant.deaths:CountrySingapore                                                                            NA
    ## infant.deaths:CountrySlovakia                                                                             NA
    ## infant.deaths:CountrySlovenia                                                                             NA
    ## infant.deaths:CountrySolomon Islands                                                                      NA
    ## infant.deaths:CountrySomalia                                                                     76.56612264
    ## infant.deaths:CountrySouth Africa                                                                -2.73747218
    ## infant.deaths:CountrySouth Sudan                                                                -57.49316896
    ## infant.deaths:CountrySpain                                                                       49.13921464
    ## infant.deaths:CountrySri Lanka                                                                   22.04710064
    ## infant.deaths:CountrySudan                                                                       15.94093317
    ## infant.deaths:CountrySuriname                                                                             NA
    ## infant.deaths:CountrySwaziland                                                                   58.21399028
    ## infant.deaths:CountrySweden                                                                               NA
    ## infant.deaths:CountrySwitzerland                                                                          NA
    ## infant.deaths:CountrySyrian Arab Republic                                                         6.16572182
    ## infant.deaths:CountryTajikistan                                                                 -23.09229076
    ## infant.deaths:CountryThailand                                                                     8.60061107
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                            NA
    ## infant.deaths:CountryTimor-Leste                                                                  5.57534949
    ## infant.deaths:CountryTogo                                                                        -8.71406093
    ## infant.deaths:CountryTonga                                                                                NA
    ## infant.deaths:CountryTrinidad and Tobago                                                                  NA
    ## infant.deaths:CountryTunisia                                                                     61.54396274
    ## infant.deaths:CountryTurkey                                                                       2.39857960
    ## infant.deaths:CountryTurkmenistan                                                                17.54743387
    ## infant.deaths:CountryUganda                                                                       6.02181157
    ## infant.deaths:CountryUkraine                                                                     89.10185441
    ## infant.deaths:CountryUnited Arab Emirates                                                                 NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        52.85129755
    ## infant.deaths:CountryUnited Republic of Tanzania                                                -12.63931787
    ## infant.deaths:CountryUnited States of America                                                    29.55347933
    ## infant.deaths:CountryUruguay                                                                      4.57565720
    ## infant.deaths:CountryUzbekistan                                                                   5.36818705
    ## infant.deaths:CountryVanuatu                                                                              NA
    ## infant.deaths:CountryVenezuela                                                                   29.34436979
    ## infant.deaths:CountryViet Nam                                                                    11.10622184
    ## infant.deaths:CountryYemen                                                                      -38.33651393
    ## infant.deaths:CountryZambia                                                                       9.83192403
    ## infant.deaths:CountryZimbabwe                                                                    -3.12669812
    ## log.Adult.Mortality:CountryAlbania                                                               96.65099226
    ## log.Adult.Mortality:CountryAlgeria                                                               89.85286765
    ## log.Adult.Mortality:CountryAngola                                                                50.58774784
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   92.67879392
    ## log.Adult.Mortality:CountryArgentina                                                             86.96585934
    ## log.Adult.Mortality:CountryArmenia                                                               92.87773471
    ## log.Adult.Mortality:CountryAustralia                                                             96.64602802
    ## log.Adult.Mortality:CountryAustria                                                               84.12822237
    ## log.Adult.Mortality:CountryAzerbaijan                                                           102.59141828
    ## log.Adult.Mortality:CountryBahamas                                                               94.20029847
    ## log.Adult.Mortality:CountryBahrain                                                               99.92597522
    ## log.Adult.Mortality:CountryBangladesh                                                           138.56706144
    ## log.Adult.Mortality:CountryBarbados                                                              97.47122330
    ## log.Adult.Mortality:CountryBelarus                                                              102.37299794
    ## log.Adult.Mortality:CountryBelgium                                                               71.46985707
    ## log.Adult.Mortality:CountryBelize                                                                87.14968615
    ## log.Adult.Mortality:CountryBenin                                                                 76.66109649
    ## log.Adult.Mortality:CountryBhutan                                                                80.76906677
    ## log.Adult.Mortality:CountryBolivia                                                               27.83523076
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                94.72590077
    ## log.Adult.Mortality:CountryBotswana                                                              81.50271018
    ## log.Adult.Mortality:CountryBrazil                                                                47.82752494
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     90.62718698
    ## log.Adult.Mortality:CountryBulgaria                                                              86.40160109
    ## log.Adult.Mortality:CountryBurkina Faso                                                         262.36116177
    ## log.Adult.Mortality:CountryBurundi                                                              -81.33184678
    ## log.Adult.Mortality:CountryCabo Verde                                                           102.10062928
    ## log.Adult.Mortality:CountryCambodia                                                             102.33108018
    ## log.Adult.Mortality:CountryCameroon                                                              75.84976135
    ## log.Adult.Mortality:CountryCanada                                                                95.64326426
    ## log.Adult.Mortality:CountryCentral African Republic                                              84.06886081
    ## log.Adult.Mortality:CountryChad                                                                 425.47119654
    ## log.Adult.Mortality:CountryChile                                                                 69.48681096
    ## log.Adult.Mortality:CountryChina                                                                107.19010803
    ## log.Adult.Mortality:CountryColombia                                                              86.84963065
    ## log.Adult.Mortality:CountryComoros                                                               72.39550374
    ## log.Adult.Mortality:CountryCongo                                                                 60.13184650
    ## log.Adult.Mortality:CountryCosta Rica                                                            91.60179755
    ## log.Adult.Mortality:CountryCroatia                                                               93.42728156
    ## log.Adult.Mortality:CountryCuba                                                                  95.61976376
    ## log.Adult.Mortality:CountryCyprus                                                                91.37102022
    ## log.Adult.Mortality:CountryCzechia                                                               93.56966362
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 76.57073177
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                      -1.48466554
    ## log.Adult.Mortality:CountryDenmark                                                               85.93531188
    ## log.Adult.Mortality:CountryDjibouti                                                             142.30978207
    ## log.Adult.Mortality:CountryDominican Republic                                                   318.54417112
    ## log.Adult.Mortality:CountryEcuador                                                               92.95615202
    ## log.Adult.Mortality:CountryEgypt                                                               -266.72353244
    ## log.Adult.Mortality:CountryEl Salvador                                                          117.70001433
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     75.88240570
    ## log.Adult.Mortality:CountryEritrea                                                              177.40927265
    ## log.Adult.Mortality:CountryEstonia                                                               99.15010033
    ## log.Adult.Mortality:CountryEthiopia                                                             113.41379166
    ## log.Adult.Mortality:CountryFiji                                                                  91.57148514
    ## log.Adult.Mortality:CountryFinland                                                               72.13119489
    ## log.Adult.Mortality:CountryFrance                                                                94.72192091
    ## log.Adult.Mortality:CountryGabon                                                                 87.40505700
    ## log.Adult.Mortality:CountryGambia                                                                37.07502038
    ## log.Adult.Mortality:CountryGeorgia                                                              119.61677006
    ## log.Adult.Mortality:CountryGermany                                                              -25.04683455
    ## log.Adult.Mortality:CountryGhana                                                                493.20584642
    ## log.Adult.Mortality:CountryGreece                                                                63.33768037
    ## log.Adult.Mortality:CountryGrenada                                                              108.18281494
    ## log.Adult.Mortality:CountryGuatemala                                                            262.52361356
    ## log.Adult.Mortality:CountryGuinea                                                               109.79945334
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                       -219.64038972
    ## log.Adult.Mortality:CountryGuyana                                                               100.89351177
    ## log.Adult.Mortality:CountryHaiti                                                                156.22806907
    ## log.Adult.Mortality:CountryHonduras                                                              85.17853626
    ## log.Adult.Mortality:CountryHungary                                                               95.54072605
    ## log.Adult.Mortality:CountryIceland                                                              101.30803840
    ## log.Adult.Mortality:CountryIndia                                                                 69.57999985
    ## log.Adult.Mortality:CountryIndonesia                                                            160.62625043
    ## log.Adult.Mortality:CountryIran                                                                  94.55627326
    ## log.Adult.Mortality:CountryIraq                                                                 300.22783259
    ## log.Adult.Mortality:CountryIreland                                                               87.33088567
    ## log.Adult.Mortality:CountryIsrael                                                                95.62918493
    ## log.Adult.Mortality:CountryItaly                                                                107.96599717
    ## log.Adult.Mortality:CountryIvory Coast                                                         -147.64114878
    ## log.Adult.Mortality:CountryJamaica                                                               92.97376089
    ## log.Adult.Mortality:CountryJapan                                                                103.50702430
    ## log.Adult.Mortality:CountryJordan                                                                90.35245809
    ## log.Adult.Mortality:CountryKazakhstan                                                            96.92388358
    ## log.Adult.Mortality:CountryKenya                                                                109.82171668
    ## log.Adult.Mortality:CountryKiribati                                                              84.00370467
    ## log.Adult.Mortality:CountryKuwait                                                                93.29345097
    ## log.Adult.Mortality:CountryKyrgyzstan                                                           178.86783273
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      97.68660742
    ## log.Adult.Mortality:CountryLatvia                                                                99.95709028
    ## log.Adult.Mortality:CountryLebanon                                                               93.83516587
    ## log.Adult.Mortality:CountryLesotho                                                               46.70849799
    ## log.Adult.Mortality:CountryLiberia                                                               86.68235662
    ## log.Adult.Mortality:CountryLibya                                                               -225.86406435
    ## log.Adult.Mortality:CountryLithuania                                                            106.06653511
    ## log.Adult.Mortality:CountryLuxembourg                                                            93.58583225
    ## log.Adult.Mortality:CountryMadagascar                                                           193.60608909
    ## log.Adult.Mortality:CountryMalawi                                                               100.12872020
    ## log.Adult.Mortality:CountryMalaysia                                                              91.00360624
    ## log.Adult.Mortality:CountryMaldives                                                              98.78098976
    ## log.Adult.Mortality:CountryMali                                                                -152.20077978
    ## log.Adult.Mortality:CountryMalta                                                                 84.20813789
    ## log.Adult.Mortality:CountryMauritania                                                         -2023.76332743
    ## log.Adult.Mortality:CountryMauritius                                                             84.52305358
    ## log.Adult.Mortality:CountryMexico                                                                70.37584366
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      85.22091741
    ## log.Adult.Mortality:CountryMongolia                                                             106.62787305
    ## log.Adult.Mortality:CountryMontenegro                                                            93.92441284
    ## log.Adult.Mortality:CountryMorocco                                                              103.43353044
    ## log.Adult.Mortality:CountryMozambique                                                           -71.89472823
    ## log.Adult.Mortality:CountryMyanmar                                                              107.39669699
    ## log.Adult.Mortality:CountryNamibia                                                              133.62848091
    ## log.Adult.Mortality:CountryNepal                                                                100.05259943
    ## log.Adult.Mortality:CountryNetherlands                                                           88.76719071
    ## log.Adult.Mortality:CountryNew Zealand                                                           82.60282465
    ## log.Adult.Mortality:CountryNicaragua                                                             57.06327414
    ## log.Adult.Mortality:CountryNiger                                                              -1191.66885836
    ## log.Adult.Mortality:CountryNigeria                                                              417.35685563
    ## log.Adult.Mortality:CountryNorway                                                                92.80084538
    ## log.Adult.Mortality:CountryOman                                                                  92.27930603
    ## log.Adult.Mortality:CountryPakistan                                                             221.16612781
    ## log.Adult.Mortality:CountryPanama                                                               234.28706498
    ## log.Adult.Mortality:CountryPapua New Guinea                                                     144.55350672
    ## log.Adult.Mortality:CountryParaguay                                                              33.58909836
    ## log.Adult.Mortality:CountryPeru                                                                 109.72202761
    ## log.Adult.Mortality:CountryPhilippines                                                          109.64701490
    ## log.Adult.Mortality:CountryPoland                                                                96.46762711
    ## log.Adult.Mortality:CountryPortugal                                                              75.37491980
    ## log.Adult.Mortality:CountryQatar                                                                 92.50512519
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     89.48520191
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   77.04379188
    ## log.Adult.Mortality:CountryRomania                                                               24.02594766
    ## log.Adult.Mortality:CountryRussian Federation                                                    44.39035902
    ## log.Adult.Mortality:CountryRwanda                                                                93.54450499
    ## log.Adult.Mortality:CountrySaint Lucia                                                           90.78888373
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                     131.81071752
    ## log.Adult.Mortality:CountrySamoa                                                                107.44015090
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 79.13783600
    ## log.Adult.Mortality:CountrySaudi Arabia                                                          84.89524417
    ## log.Adult.Mortality:CountrySenegal                                                              107.35670716
    ## log.Adult.Mortality:CountrySerbia                                                                91.78119047
    ## log.Adult.Mortality:CountrySeychelles                                                            93.86310058
    ## log.Adult.Mortality:CountrySierra Leone                                                         -72.13983490
    ## log.Adult.Mortality:CountrySingapore                                                             92.85919573
    ## log.Adult.Mortality:CountrySlovakia                                                              93.96630970
    ## log.Adult.Mortality:CountrySlovenia                                                              85.27210658
    ## log.Adult.Mortality:CountrySolomon Islands                                                       96.60821782
    ## log.Adult.Mortality:CountrySomalia                                                              697.50027738
    ## log.Adult.Mortality:CountrySouth Africa                                                          25.29547124
    ## log.Adult.Mortality:CountrySouth Sudan                                                         -184.86165349
    ## log.Adult.Mortality:CountrySpain                                                                106.77867860
    ## log.Adult.Mortality:CountrySri Lanka                                                            106.92566317
    ## log.Adult.Mortality:CountrySudan                                                                223.95967717
    ## log.Adult.Mortality:CountrySuriname                                                              78.64044186
    ## log.Adult.Mortality:CountrySwaziland                                                             98.00660576
    ## log.Adult.Mortality:CountrySweden                                                                99.85579137
    ## log.Adult.Mortality:CountrySwitzerland                                                          100.49224326
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  95.24215650
    ## log.Adult.Mortality:CountryTajikistan                                                            27.31895311
    ## log.Adult.Mortality:CountryThailand                                                              96.72739484
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             94.28188283
    ## log.Adult.Mortality:CountryTimor-Leste                                                           85.45985984
    ## log.Adult.Mortality:CountryTogo                                                                  46.01639550
    ## log.Adult.Mortality:CountryTonga                                                                 92.57411427
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   73.39254795
    ## log.Adult.Mortality:CountryTunisia                                                              132.95260661
    ## log.Adult.Mortality:CountryTurkey                                                                41.60217799
    ## log.Adult.Mortality:CountryTurkmenistan                                                         104.02004338
    ## log.Adult.Mortality:CountryUganda                                                                65.61336513
    ## log.Adult.Mortality:CountryUkraine                                                              157.29996272
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  96.06604931
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                 132.39825191
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                         -161.52255586
    ## log.Adult.Mortality:CountryUnited States of America                                             232.47997383
    ## log.Adult.Mortality:CountryUruguay                                                               93.58018455
    ## log.Adult.Mortality:CountryUzbekistan                                                           102.35035638
    ## log.Adult.Mortality:CountryVanuatu                                                               91.98899742
    ## log.Adult.Mortality:CountryVenezuela                                                            140.23581185
    ## log.Adult.Mortality:CountryViet Nam                                                             125.66713786
    ## log.Adult.Mortality:CountryYemen                                                               -223.27007918
    ## log.Adult.Mortality:CountryZambia                                                               118.56660308
    ## log.Adult.Mortality:CountryZimbabwe                                                              46.72472843
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                              0.97157136
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                -14.19373937
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                  0.13031571
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                   0.37468124
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                                0.57826339
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                              -1.89798468
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                               0.00667430
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                -18.05742960
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 21.98642754
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                  -0.95714148
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                 -1.46396018
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                   0.46365643
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                            -3.31274394
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                  6.15667456
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                -0.36411913
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                 0.26455067
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                    -7.65938630
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                   -0.00892008
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                 0.60590011
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                  6.52818315
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                    4.92512351
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                    1.04778221
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                         0.33207390
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                               -30.75727154
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                     -35.70918679
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                  0.49665519
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                    4.72234025
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                            -13.62359042
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                -13.81287049
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                                -0.19218830
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                -11.22594576
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                 38.94085426
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                  -10.62676531
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                              -14.89642432
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                  -0.60741717
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                           73.84546619
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                  -3.46571385
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                   -3.29805690
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                -0.32731054
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                 -7.53841799
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                   -0.00304111
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                               -0.39671522
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                     0.12367513
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                    -6.95357519
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                   -4.46368099
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                              3.81529813
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                   -2.84739133
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                              -1.06281839
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                   -0.31388732
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                  -4.65044231
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                             -23.15288591
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                        -1.61863029
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                  7.66494813
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                 -0.37701257
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                  164.30690616
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                              -3.45908379
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                  -0.36506150
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                -1.15175646
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                     3.97862221
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                             268.81202942
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                   0.62717978
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                               -20.71446638
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                  0.07293496
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                               2.79549907
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                 -0.35580196
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                -16.39744536
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                   -0.26708500
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                               11.65455615
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                   26.82142195
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                 -0.71452736
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                -0.37765220
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                -146.77353857
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                        -6.50049606
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                20.48951655
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                     0.83783612
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                             -0.18576844
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                  -1.58827899
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                 9.12297398
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                       -2.14873746
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                 35.20229043
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                       3.30291946
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                  -0.63953692
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                                 NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                             1.06692732
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                 -0.88285037
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                             5.34412955
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                -12.29890560
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                             1.29546261
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             10.43212312
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                   -9.64377689
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                               -3.35467444
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                   -1.95905300
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                               -8.46961231
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                    -0.20848990
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                               5.57860084
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                -0.60019443
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                        NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                              0.28165698
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                     2.49044645
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                -12.12584541
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                   0.69726327
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                            -2.45695924
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                  -0.08149841
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                -15.49051872
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland   -11.63317741
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                              2.89886402
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                -5.21193659
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                  0.07646687
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                              -0.01962012
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                               -4.73469359
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                -1.22768500
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                    7.94564777
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                  -0.81698317
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                 1.31805414
    ##                                                                                                   Std. Error
    ## (Intercept)                                                                                     488.44908560
    ## log.Adult.Mortality                                                                              90.43852273
    ## infant.deaths                                                                                     4.99478071
    ## log.GDP                                                                                           0.02820913
    ## Measles                                                                                           0.00000441
    ## HIV_cat                                                                                           0.27949196
    ## CountryAlbania                                                                                  499.85967070
    ## CountryAlgeria                                                                                  768.23294236
    ## CountryAngola                                                                                   660.07510644
    ## CountryAntigua and Barbuda                                                                      488.93634168
    ## CountryArgentina                                                                                570.41572365
    ## CountryArmenia                                                                                  484.35049664
    ## CountryAustralia                                                                                484.28765276
    ## CountryAustria                                                                                  488.68285135
    ## CountryAzerbaijan                                                                               506.94107557
    ## CountryBahamas                                                                                  488.83476629
    ## CountryBahrain                                                                                  488.51873153
    ## CountryBangladesh                                                                               627.84293814
    ## CountryBarbados                                                                                 488.94107099
    ## CountryBelarus                                                                                  488.93336174
    ## CountryBelgium                                                                                  489.04737817
    ## CountryBelize                                                                                   490.76442986
    ## CountryBenin                                                                                    367.25019777
    ## CountryBhutan                                                                                   510.01421203
    ## CountryBolivia                                                                                  506.41820062
    ## CountryBosnia and Herzegovina                                                                   488.83762003
    ## CountryBotswana                                                                                 478.62797689
    ## CountryBrazil                                                                                   536.00239445
    ## CountryBrunei Darussalam                                                                        490.10047306
    ## CountryBulgaria                                                                                 489.76419204
    ## CountryBurkina Faso                                                                             889.29156497
    ## CountryBurundi                                                                                  833.20122011
    ## CountryCabo Verde                                                                               488.80019365
    ## CountryCambodia                                                                                 492.09244333
    ## CountryCameroon                                                                                1164.48201513
    ## CountryCanada                                                                                   479.15710523
    ## CountryCentral African Republic                                                                 490.18949536
    ## CountryChad                                                                                   12454.29623808
    ## CountryChile                                                                                    479.81947556
    ## CountryChina                                                                                    511.92388952
    ## CountryColombia                                                                                 592.51252780
    ## CountryComoros                                                                                  499.79849608
    ## CountryCongo                                                                                    561.85199577
    ## CountryCosta Rica                                                                               485.81241001
    ## CountryCroatia                                                                                  488.74407604
    ## CountryCuba                                                                                     483.97967156
    ## CountryCyprus                                                                                   488.66594875
    ## CountryCzechia                                                                                  488.70536388
    ## CountryDemocratic People's Republic of Korea                                                    490.96079060
    ## CountryDemocratic Republic of the Congo                                                        2428.23298618
    ## CountryDenmark                                                                                  488.59398350
    ## CountryDjibouti                                                                                 523.16341391
    ## CountryDominican Republic                                                                      1358.38567416
    ## CountryEcuador                                                                                  561.01350305
    ## CountryEgypt                                                                                   1505.80513663
    ## CountryEl Salvador                                                                              546.04333373
    ## CountryEquatorial Guinea                                                                        475.59767727
    ## CountryEritrea                                                                                  533.78950636
    ## CountryEstonia                                                                                  488.51806151
    ## CountryEthiopia                                                                                 493.18335203
    ## CountryFiji                                                                                     489.54275694
    ## CountryFinland                                                                                  488.70486405
    ## CountryFrance                                                                                   488.77163728
    ## CountryGabon                                                                                    478.93778557
    ## CountryGambia                                                                                   476.05891629
    ## CountryGeorgia                                                                                  500.50498352
    ## CountryGermany                                                                                  527.34343143
    ## CountryGhana                                                                                    909.79765398
    ## CountryGreece                                                                                   489.51296476
    ## CountryGrenada                                                                                  488.85062858
    ## CountryGuatemala                                                                                514.90707973
    ## CountryGuinea                                                                                   612.07623652
    ## CountryGuinea-Bissau                                                                            970.44417100
    ## CountryGuyana                                                                                   506.16127404
    ## CountryHaiti                                                                                    520.16806185
    ## CountryHonduras                                                                                 532.38634462
    ## CountryHungary                                                                                  489.99503929
    ## CountryIceland                                                                                  488.60991313
    ## CountryIndia                                                                                    589.57414856
    ## CountryIndonesia                                                                                946.53098232
    ## CountryIran                                                                                     495.21341029
    ## CountryIraq                                                                                     657.98269280
    ## CountryIreland                                                                                  488.62025091
    ## CountryIsrael                                                                                   488.60974159
    ## CountryItaly                                                                                    492.04930577
    ## CountryIvory Coast                                                                             2017.99775281
    ## CountryJamaica                                                                                  483.93063307
    ## CountryJapan                                                                                    497.02192498
    ## CountryJordan                                                                                   469.71150259
    ## CountryKazakhstan                                                                               503.27039987
    ## CountryKenya                                                                                    501.81956975
    ## CountryKiribati                                                                                 493.44498618
    ## CountryKuwait                                                                                   490.14477639
    ## CountryKyrgyzstan                                                                               599.48302662
    ## CountryLao People's Democratic Republic                                                         525.28606530
    ## CountryLatvia                                                                                   488.63965290
    ## CountryLebanon                                                                                  484.39210214
    ## CountryLesotho                                                                                  562.46076226
    ## CountryLiberia                                                                                  500.78719828
    ## CountryLibya                                                                                    789.77900377
    ## CountryLithuania                                                                                488.72834295
    ## CountryLuxembourg                                                                               488.56277732
    ## CountryMadagascar                                                                               610.48815376
    ## CountryMalawi                                                                                   491.86622509
    ## CountryMalaysia                                                                                 581.75478317
    ## CountryMaldives                                                                                 488.47879215
    ## CountryMali                                                                                    1875.41982024
    ## CountryMalta                                                                                    488.85856329
    ## CountryMauritania                                                                             10556.32246131
    ## CountryMauritius                                                                                489.79538595
    ## CountryMexico                                                                                   623.99107546
    ## CountryMicronesia (Federated States of)                                                         490.15368216
    ## CountryMongolia                                                                                 512.47999058
    ## CountryMontenegro                                                                               488.73522716
    ## CountryMorocco                                                                                  491.89910256
    ## CountryMozambique                                                                               644.43562691
    ## CountryMyanmar                                                                                  518.62807986
    ## CountryNamibia                                                                                 1098.91920937
    ## CountryNepal                                                                                    495.17466236
    ## CountryNetherlands                                                                              483.70353110
    ## CountryNew Zealand                                                                              488.90294617
    ## CountryNicaragua                                                                                503.98604156
    ## CountryNiger                                                                                   1934.83916685
    ## CountryNigeria                                                                                 1187.69162033
    ## CountryNorway                                                                                   488.65604663
    ## CountryOman                                                                                     483.80680957
    ## CountryPakistan                                                                                2402.44250277
    ## CountryPanama                                                                                  2316.83584146
    ## CountryPapua New Guinea                                                                        2324.06370228
    ## CountryParaguay                                                                                 529.00750851
    ## CountryPeru                                                                                     515.88338308
    ## CountryPhilippines                                                                             1017.65550503
    ## CountryPoland                                                                                   542.22360626
    ## CountryPortugal                                                                                 488.77313810
    ## CountryQatar                                                                                    489.57482495
    ## CountryRepublic of Korea                                                                        490.22122037
    ## CountryRepublic of Moldova                                                                      483.89908807
    ## CountryRomania                                                                                  503.87849529
    ## CountryRussian Federation                                                                       517.51557375
    ## CountryRwanda                                                                                   489.99610754
    ## CountrySaint Lucia                                                                              489.13340581
    ## CountrySaint Vincent and the Grenadines                                                         489.40702125
    ## CountrySamoa                                                                                    488.57664001
    ## CountrySao Tome and Principe                                                                    489.38862182
    ## CountrySaudi Arabia                                                                             550.92703884
    ## CountrySenegal                                                                                  633.66078258
    ## CountrySerbia                                                                                   484.14294749
    ## CountrySeychelles                                                                               491.46204574
    ## CountrySierra Leone                                                                             642.27850992
    ## CountrySingapore                                                                                488.59801111
    ## CountrySlovakia                                                                                 488.69773699
    ## CountrySlovenia                                                                                 488.52584121
    ## CountrySolomon Islands                                                                          488.63414962
    ## CountrySomalia                                                                                 2549.48905682
    ## CountrySouth Africa                                                                             532.60955126
    ## CountrySouth Sudan                                                                             2032.49605963
    ## CountrySpain                                                                                    517.51400507
    ## CountrySri Lanka                                                                                514.57797221
    ## CountrySudan                                                                                    722.26680562
    ## CountrySuriname                                                                                 489.22097182
    ## CountrySwaziland                                                                                514.52768129
    ## CountrySweden                                                                                   488.70245766
    ## CountrySwitzerland                                                                              488.59779304
    ## CountrySyrian Arab Republic                                                                     492.08529401
    ## CountryTajikistan                                                                               637.99100379
    ## CountryThailand                                                                                 499.24745214
    ## CountryThe former Yugoslav republic of Macedonia                                                488.81546634
    ## CountryTimor-Leste                                                                              514.10656232
    ## CountryTogo                                                                                     791.86085813
    ## CountryTonga                                                                                    490.31508193
    ## CountryTrinidad and Tobago                                                                      489.63441542
    ## CountryTunisia                                                                                  821.73314301
    ## CountryTurkey                                                                                   558.99425117
    ## CountryTurkmenistan                                                                             628.87496008
    ## CountryUganda                                                                                   499.19591548
    ## CountryUkraine                                                                                  514.86334529
    ## CountryUnited Arab Emirates                                                                     483.77803033
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                     607.43906091
    ## CountryUnited Republic of Tanzania                                                              760.82413431
    ## CountryUnited States of America                                                                1302.36862824
    ## CountryUruguay                                                                                  519.15234901
    ## CountryUzbekistan                                                                               500.50996614
    ## CountryVanuatu                                                                                  488.76911455
    ## CountryVenezuela                                                                               5956.15886217
    ## CountryViet Nam                                                                                2411.75124144
    ## CountryYemen                                                                                   1424.20412823
    ## CountryZambia                                                                                   579.17705556
    ## CountryZimbabwe                                                                                 502.24618196
    ## infant.deaths:CountryAlbania                                                                    146.82898193
    ## infant.deaths:CountryAlgeria                                                                     29.17823861
    ## infant.deaths:CountryAngola                                                                       6.14300642
    ## infant.deaths:CountryAntigua and Barbuda                                                                  NA
    ## infant.deaths:CountryArgentina                                                                   25.14635380
    ## infant.deaths:CountryArmenia                                                                              NA
    ## infant.deaths:CountryAustralia                                                                            NA
    ## infant.deaths:CountryAustria                                                                              NA
    ## infant.deaths:CountryAzerbaijan                                                                  26.91187088
    ## infant.deaths:CountryBahamas                                                                              NA
    ## infant.deaths:CountryBahrain                                                                              NA
    ## infant.deaths:CountryBangladesh                                                                   5.02861099
    ## infant.deaths:CountryBarbados                                                                             NA
    ## infant.deaths:CountryBelarus                                                                     60.29205926
    ## infant.deaths:CountryBelgium                                                                     80.99621940
    ## infant.deaths:CountryBelize                                                                               NA
    ## infant.deaths:CountryBenin                                                                                NA
    ## infant.deaths:CountryBhutan                                                                     148.97864138
    ## infant.deaths:CountryBolivia                                                                     23.29261709
    ## infant.deaths:CountryBosnia and Herzegovina                                                               NA
    ## infant.deaths:CountryBotswana                                                                             NA
    ## infant.deaths:CountryBrazil                                                                       5.22390370
    ## infant.deaths:CountryBrunei Darussalam                                                                    NA
    ## infant.deaths:CountryBulgaria                                                                     5.24511481
    ## infant.deaths:CountryBurkina Faso                                                                15.41719505
    ## infant.deaths:CountryBurundi                                                                     29.68806590
    ## infant.deaths:CountryCabo Verde                                                                           NA
    ## infant.deaths:CountryCambodia                                                                     6.44539242
    ## infant.deaths:CountryCameroon                                                                    17.97012693
    ## infant.deaths:CountryCanada                                                                               NA
    ## infant.deaths:CountryCentral African Republic                                                     5.40185006
    ## infant.deaths:CountryChad                                                                       270.95036727
    ## infant.deaths:CountryChile                                                                                NA
    ## infant.deaths:CountryChina                                                                        4.99865933
    ## infant.deaths:CountryColombia                                                                    19.34603248
    ## infant.deaths:CountryComoros                                                                     77.73643958
    ## infant.deaths:CountryCongo                                                                       41.59681747
    ## infant.deaths:CountryCosta Rica                                                                           NA
    ## infant.deaths:CountryCroatia                                                                              NA
    ## infant.deaths:CountryCuba                                                                                 NA
    ## infant.deaths:CountryCyprus                                                                               NA
    ## infant.deaths:CountryCzechia                                                                              NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                        8.24974041
    ## infant.deaths:CountryDemocratic Republic of the Congo                                            11.22890292
    ## infant.deaths:CountryDenmark                                                                              NA
    ## infant.deaths:CountryDjibouti                                                                   160.86075576
    ## infant.deaths:CountryDominican Republic                                                         206.20723590
    ## infant.deaths:CountryEcuador                                                                     40.78391443
    ## infant.deaths:CountryEgypt                                                                       25.96788549
    ## infant.deaths:CountryEl Salvador                                                                124.49475936
    ## infant.deaths:CountryEquatorial Guinea                                                                    NA
    ## infant.deaths:CountryEritrea                                                                     31.07027898
    ## infant.deaths:CountryEstonia                                                                              NA
    ## infant.deaths:CountryEthiopia                                                                     5.04191193
    ## infant.deaths:CountryFiji                                                                                 NA
    ## infant.deaths:CountryFinland                                                                              NA
    ## infant.deaths:CountryFrance                                                                       5.25445340
    ## infant.deaths:CountryGabon                                                                                NA
    ## infant.deaths:CountryGambia                                                                               NA
    ## infant.deaths:CountryGeorgia                                                                     69.39873915
    ## infant.deaths:CountryGermany                                                                     74.10314086
    ## infant.deaths:CountryGhana                                                                       18.93270955
    ## infant.deaths:CountryGreece                                                                       5.23016047
    ## infant.deaths:CountryGrenada                                                                              NA
    ## infant.deaths:CountryGuatemala                                                                   15.20682810
    ## infant.deaths:CountryGuinea                                                                      13.49030780
    ## infant.deaths:CountryGuinea-Bissau                                                              188.76514962
    ## infant.deaths:CountryGuyana                                                                     148.56377954
    ## infant.deaths:CountryHaiti                                                                        8.61169234
    ## infant.deaths:CountryHonduras                                                                    36.99305525
    ## infant.deaths:CountryHungary                                                                     69.65744813
    ## infant.deaths:CountryIceland                                                                              NA
    ## infant.deaths:CountryIndia                                                                        4.99648198
    ## infant.deaths:CountryIndonesia                                                                    6.88080538
    ## infant.deaths:CountryIran                                                                         6.76258951
    ## infant.deaths:CountryIraq                                                                        15.13766895
    ## infant.deaths:CountryIreland                                                                              NA
    ## infant.deaths:CountryIsrael                                                                       5.21806402
    ## infant.deaths:CountryItaly                                                                       28.24378608
    ## infant.deaths:CountryIvory Coast                                                                 32.13508035
    ## infant.deaths:CountryJamaica                                                                              NA
    ## infant.deaths:CountryJapan                                                                       41.35282083
    ## infant.deaths:CountryJordan                                                                               NA
    ## infant.deaths:CountryKazakhstan                                                                  14.44750822
    ## infant.deaths:CountryKenya                                                                        5.42486455
    ## infant.deaths:CountryKiribati                                                                             NA
    ## infant.deaths:CountryKuwait                                                                     120.75056045
    ## infant.deaths:CountryKyrgyzstan                                                                  92.02713495
    ## infant.deaths:CountryLao People's Democratic Republic                                            16.62394725
    ## infant.deaths:CountryLatvia                                                                               NA
    ## infant.deaths:CountryLebanon                                                                              NA
    ## infant.deaths:CountryLesotho                                                                     61.29938829
    ## infant.deaths:CountryLiberia                                                                     11.93076971
    ## infant.deaths:CountryLibya                                                                      311.21953218
    ## infant.deaths:CountryLithuania                                                                            NA
    ## infant.deaths:CountryLuxembourg                                                                           NA
    ## infant.deaths:CountryMadagascar                                                                  17.30487520
    ## infant.deaths:CountryMalawi                                                                       5.40050919
    ## infant.deaths:CountryMalaysia                                                                    85.92330609
    ## infant.deaths:CountryMaldives                                                                             NA
    ## infant.deaths:CountryMali                                                                        29.52752669
    ## infant.deaths:CountryMalta                                                                                NA
    ## infant.deaths:CountryMauritania                                                                1318.46811506
    ## infant.deaths:CountryMauritius                                                                            NA
    ## infant.deaths:CountryMexico                                                                      11.70884153
    ## infant.deaths:CountryMicronesia (Federated States of)                                                     NA
    ## infant.deaths:CountryMongolia                                                                   102.94274774
    ## infant.deaths:CountryMontenegro                                                                           NA
    ## infant.deaths:CountryMorocco                                                                      5.75108023
    ## infant.deaths:CountryMozambique                                                                   8.19495459
    ## infant.deaths:CountryMyanmar                                                                      5.89003221
    ## infant.deaths:CountryNamibia                                                                    328.31232701
    ## infant.deaths:CountryNepal                                                                        5.61797246
    ## infant.deaths:CountryNetherlands                                                                          NA
    ## infant.deaths:CountryNew Zealand                                                                          NA
    ## infant.deaths:CountryNicaragua                                                                   33.60199498
    ## infant.deaths:CountryNiger                                                                       41.09607487
    ## infant.deaths:CountryNigeria                                                                      5.35514990
    ## infant.deaths:CountryNorway                                                                               NA
    ## infant.deaths:CountryOman                                                                                 NA
    ## infant.deaths:CountryPakistan                                                                     8.14702602
    ## infant.deaths:CountryPanama                                                                    2260.71279657
    ## infant.deaths:CountryPapua New Guinea                                                           207.77459682
    ## infant.deaths:CountryParaguay                                                                    62.46088767
    ## infant.deaths:CountryPeru                                                                        17.52978179
    ## infant.deaths:CountryPhilippines                                                                 16.17424052
    ## infant.deaths:CountryPoland                                                                     114.02832397
    ## infant.deaths:CountryPortugal                                                                   318.85666441
    ## infant.deaths:CountryQatar                                                                                NA
    ## infant.deaths:CountryRepublic of Korea                                                           14.71002014
    ## infant.deaths:CountryRepublic of Moldova                                                                  NA
    ## infant.deaths:CountryRomania                                                                     52.46919793
    ## infant.deaths:CountryRussian Federation                                                          13.98022252
    ## infant.deaths:CountryRwanda                                                                       5.32101347
    ## infant.deaths:CountrySaint Lucia                                                                          NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                                     NA
    ## infant.deaths:CountrySamoa                                                                                NA
    ## infant.deaths:CountrySao Tome and Principe                                                                NA
    ## infant.deaths:CountrySaudi Arabia                                                                29.10113846
    ## infant.deaths:CountrySenegal                                                                     22.47490810
    ## infant.deaths:CountrySerbia                                                                               NA
    ## infant.deaths:CountrySeychelles                                                                           NA
    ## infant.deaths:CountrySierra Leone                                                                15.55522504
    ## infant.deaths:CountrySingapore                                                                            NA
    ## infant.deaths:CountrySlovakia                                                                             NA
    ## infant.deaths:CountrySlovenia                                                                             NA
    ## infant.deaths:CountrySolomon Islands                                                                      NA
    ## infant.deaths:CountrySomalia                                                                     50.22089541
    ## infant.deaths:CountrySouth Africa                                                                 6.74549962
    ## infant.deaths:CountrySouth Sudan                                                                 75.18871103
    ## infant.deaths:CountrySpain                                                                       90.40072997
    ## infant.deaths:CountrySri Lanka                                                                   39.55890020
    ## infant.deaths:CountrySudan                                                                        9.41064390
    ## infant.deaths:CountrySuriname                                                                             NA
    ## infant.deaths:CountrySwaziland                                                                   62.49706183
    ## infant.deaths:CountrySweden                                                                               NA
    ## infant.deaths:CountrySwitzerland                                                                          NA
    ## infant.deaths:CountrySyrian Arab Republic                                                         9.90348098
    ## infant.deaths:CountryTajikistan                                                                  41.44385903
    ## infant.deaths:CountryThailand                                                                    10.52074404
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                            NA
    ## infant.deaths:CountryTimor-Leste                                                                 66.52292609
    ## infant.deaths:CountryTogo                                                                        45.69370820
    ## infant.deaths:CountryTonga                                                                                NA
    ## infant.deaths:CountryTrinidad and Tobago                                                                  NA
    ## infant.deaths:CountryTunisia                                                                    214.72846814
    ## infant.deaths:CountryTurkey                                                                      11.22397859
    ## infant.deaths:CountryTurkmenistan                                                                60.89330018
    ## infant.deaths:CountryUganda                                                                       5.29955448
    ## infant.deaths:CountryUkraine                                                                     34.72956281
    ## infant.deaths:CountryUnited Arab Emirates                                                                 NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        94.22326680
    ## infant.deaths:CountryUnited Republic of Tanzania                                                  8.57357237
    ## infant.deaths:CountryUnited States of America                                                    46.53950894
    ## infant.deaths:CountryUruguay                                                                    186.05348202
    ## infant.deaths:CountryUzbekistan                                                                   7.36316964
    ## infant.deaths:CountryVanuatu                                                                              NA
    ## infant.deaths:CountryVenezuela                                                                  656.14393672
    ## infant.deaths:CountryViet Nam                                                                    83.87568357
    ## infant.deaths:CountryYemen                                                                       37.57247260
    ## infant.deaths:CountryZambia                                                                      12.40193460
    ## infant.deaths:CountryZimbabwe                                                                     6.85523113
    ## log.Adult.Mortality:CountryAlbania                                                               93.38056314
    ## log.Adult.Mortality:CountryAlgeria                                                              153.73457428
    ## log.Adult.Mortality:CountryAngola                                                               123.20576115
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   90.54554159
    ## log.Adult.Mortality:CountryArgentina                                                            109.84892901
    ## log.Adult.Mortality:CountryArmenia                                                               90.62544680
    ## log.Adult.Mortality:CountryAustralia                                                             90.67559607
    ## log.Adult.Mortality:CountryAustria                                                               90.50511912
    ## log.Adult.Mortality:CountryAzerbaijan                                                            94.36970001
    ## log.Adult.Mortality:CountryBahamas                                                               90.51617896
    ## log.Adult.Mortality:CountryBahrain                                                               90.45837006
    ## log.Adult.Mortality:CountryBangladesh                                                           122.66093571
    ## log.Adult.Mortality:CountryBarbados                                                              90.55785873
    ## log.Adult.Mortality:CountryBelarus                                                               90.53215304
    ## log.Adult.Mortality:CountryBelgium                                                               90.60507316
    ## log.Adult.Mortality:CountryBelize                                                                90.89338444
    ## log.Adult.Mortality:CountryBenin                                                                 90.66792878
    ## log.Adult.Mortality:CountryBhutan                                                                94.45312661
    ## log.Adult.Mortality:CountryBolivia                                                               93.88640336
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                90.53476102
    ## log.Adult.Mortality:CountryBotswana                                                              90.44610897
    ## log.Adult.Mortality:CountryBrazil                                                               101.20010111
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     90.86846714
    ## log.Adult.Mortality:CountryBulgaria                                                              90.73199536
    ## log.Adult.Mortality:CountryBurkina Faso                                                         161.98753869
    ## log.Adult.Mortality:CountryBurundi                                                              147.53478683
    ## log.Adult.Mortality:CountryCabo Verde                                                            90.51517411
    ## log.Adult.Mortality:CountryCambodia                                                              91.17358075
    ## log.Adult.Mortality:CountryCameroon                                                             201.70966921
    ## log.Adult.Mortality:CountryCanada                                                                90.60675747
    ## log.Adult.Mortality:CountryCentral African Republic                                              90.90589564
    ## log.Adult.Mortality:CountryChad                                                                2069.55038392
    ## log.Adult.Mortality:CountryChile                                                                 90.75756356
    ## log.Adult.Mortality:CountryChina                                                                 97.19297550
    ## log.Adult.Mortality:CountryColombia                                                             113.19682008
    ## log.Adult.Mortality:CountryComoros                                                               92.44815926
    ## log.Adult.Mortality:CountryCongo                                                                101.32932769
    ## log.Adult.Mortality:CountryCosta Rica                                                            91.02179233
    ## log.Adult.Mortality:CountryCroatia                                                               90.51054999
    ## log.Adult.Mortality:CountryCuba                                                                  90.55430536
    ## log.Adult.Mortality:CountryCyprus                                                                90.50479895
    ## log.Adult.Mortality:CountryCzechia                                                               90.50255632
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 90.98262095
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                     420.33806421
    ## log.Adult.Mortality:CountryDenmark                                                               90.47794925
    ## log.Adult.Mortality:CountryDjibouti                                                              96.22465848
    ## log.Adult.Mortality:CountryDominican Republic                                                   254.65825852
    ## log.Adult.Mortality:CountryEcuador                                                              106.54471283
    ## log.Adult.Mortality:CountryEgypt                                                                292.04388639
    ## log.Adult.Mortality:CountryEl Salvador                                                          101.17249618
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     90.73888933
    ## log.Adult.Mortality:CountryEritrea                                                               98.27402766
    ## log.Adult.Mortality:CountryEstonia                                                               90.45287006
    ## log.Adult.Mortality:CountryEthiopia                                                              91.16155458
    ## log.Adult.Mortality:CountryFiji                                                                  90.64640001
    ## log.Adult.Mortality:CountryFinland                                                               90.50627434
    ## log.Adult.Mortality:CountryFrance                                                                90.54499972
    ## log.Adult.Mortality:CountryGabon                                                                 90.49803811
    ## log.Adult.Mortality:CountryGambia                                                                90.83639686
    ## log.Adult.Mortality:CountryGeorgia                                                               93.04074341
    ## log.Adult.Mortality:CountryGermany                                                              101.43969310
    ## log.Adult.Mortality:CountryGhana                                                                166.16224070
    ## log.Adult.Mortality:CountryGreece                                                                90.75006981
    ## log.Adult.Mortality:CountryGrenada                                                               90.52232636
    ## log.Adult.Mortality:CountryGuatemala                                                             95.56646992
    ## log.Adult.Mortality:CountryGuinea                                                               110.91323421
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                        173.09904055
    ## log.Adult.Mortality:CountryGuyana                                                                93.52641977
    ## log.Adult.Mortality:CountryHaiti                                                                 96.32830965
    ## log.Adult.Mortality:CountryHonduras                                                              99.91760300
    ## log.Adult.Mortality:CountryHungary                                                               90.77739717
    ## log.Adult.Mortality:CountryIceland                                                               90.49035826
    ## log.Adult.Mortality:CountryIndia                                                                111.93492207
    ## log.Adult.Mortality:CountryIndonesia                                                            179.80558685
    ## log.Adult.Mortality:CountryIran                                                                  91.82880014
    ## log.Adult.Mortality:CountryIraq                                                                 125.26691022
    ## log.Adult.Mortality:CountryIreland                                                               90.48878668
    ## log.Adult.Mortality:CountryIsrael                                                                90.49059995
    ## log.Adult.Mortality:CountryItaly                                                                 91.58268664
    ## log.Adult.Mortality:CountryIvory Coast                                                          333.63050509
    ## log.Adult.Mortality:CountryJamaica                                                               90.52823552
    ## log.Adult.Mortality:CountryJapan                                                                 93.12776667
    ## log.Adult.Mortality:CountryJordan                                                                90.66482720
    ## log.Adult.Mortality:CountryKazakhstan                                                            93.35293404
    ## log.Adult.Mortality:CountryKenya                                                                 92.33323268
    ## log.Adult.Mortality:CountryKiribati                                                              91.38260718
    ## log.Adult.Mortality:CountryKuwait                                                                90.89461740
    ## log.Adult.Mortality:CountryKyrgyzstan                                                           112.31128429
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      97.67312874
    ## log.Adult.Mortality:CountryLatvia                                                                90.47582861
    ## log.Adult.Mortality:CountryLebanon                                                               90.65695038
    ## log.Adult.Mortality:CountryLesotho                                                              100.82497048
    ## log.Adult.Mortality:CountryLiberia                                                               92.50550168
    ## log.Adult.Mortality:CountryLibya                                                                153.89878680
    ## log.Adult.Mortality:CountryLithuania                                                             90.49209887
    ## log.Adult.Mortality:CountryLuxembourg                                                            90.47186102
    ## log.Adult.Mortality:CountryMadagascar                                                           110.27622390
    ## log.Adult.Mortality:CountryMalawi                                                                90.91539265
    ## log.Adult.Mortality:CountryMalaysia                                                             111.06237209
    ## log.Adult.Mortality:CountryMaldives                                                              90.44676867
    ## log.Adult.Mortality:CountryMali                                                                 336.07821722
    ## log.Adult.Mortality:CountryMalta                                                                 90.56852912
    ## log.Adult.Mortality:CountryMauritania                                                          1944.36581970
    ## log.Adult.Mortality:CountryMauritius                                                             90.71965962
    ## log.Adult.Mortality:CountryMexico                                                               120.25854836
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      90.78321825
    ## log.Adult.Mortality:CountryMongolia                                                              94.81083031
    ## log.Adult.Mortality:CountryMontenegro                                                            90.50610480
    ## log.Adult.Mortality:CountryMorocco                                                               91.43383000
    ## log.Adult.Mortality:CountryMozambique                                                           113.93766895
    ## log.Adult.Mortality:CountryMyanmar                                                               96.06580268
    ## log.Adult.Mortality:CountryNamibia                                                              195.67507119
    ## log.Adult.Mortality:CountryNepal                                                                 91.89176619
    ## log.Adult.Mortality:CountryNetherlands                                                           90.49589302
    ## log.Adult.Mortality:CountryNew Zealand                                                           90.57008385
    ## log.Adult.Mortality:CountryNicaragua                                                             93.76401980
    ## log.Adult.Mortality:CountryNiger                                                                339.71301667
    ## log.Adult.Mortality:CountryNigeria                                                              205.32648923
    ## log.Adult.Mortality:CountryNorway                                                                90.50102089
    ## log.Adult.Mortality:CountryOman                                                                  90.50934174
    ## log.Adult.Mortality:CountryPakistan                                                             464.88048628
    ## log.Adult.Mortality:CountryPanama                                                               481.75704398
    ## log.Adult.Mortality:CountryPapua New Guinea                                                     426.68864496
    ## log.Adult.Mortality:CountryParaguay                                                              98.98502339
    ## log.Adult.Mortality:CountryPeru                                                                  97.12650638
    ## log.Adult.Mortality:CountryPhilippines                                                          190.18943904
    ## log.Adult.Mortality:CountryPoland                                                               102.09098942
    ## log.Adult.Mortality:CountryPortugal                                                              90.52602517
    ## log.Adult.Mortality:CountryQatar                                                                 90.78310824
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     91.01017554
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   90.51117213
    ## log.Adult.Mortality:CountryRomania                                                               93.64625074
    ## log.Adult.Mortality:CountryRussian Federation                                                    95.17942522
    ## log.Adult.Mortality:CountryRwanda                                                                90.77733482
    ## log.Adult.Mortality:CountrySaint Lucia                                                           90.58232418
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                      90.63126671
    ## log.Adult.Mortality:CountrySamoa                                                                 90.46639543
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 90.61543625
    ## log.Adult.Mortality:CountrySaudi Arabia                                                         106.38175886
    ## log.Adult.Mortality:CountrySenegal                                                              115.74655210
    ## log.Adult.Mortality:CountrySerbia                                                                90.58685340
    ## log.Adult.Mortality:CountrySeychelles                                                            91.03932857
    ## log.Adult.Mortality:CountrySierra Leone                                                         113.59032793
    ## log.Adult.Mortality:CountrySingapore                                                             90.48422629
    ## log.Adult.Mortality:CountrySlovakia                                                              90.49528076
    ## log.Adult.Mortality:CountrySlovenia                                                              90.45835315
    ## log.Adult.Mortality:CountrySolomon Islands                                                       90.47527130
    ## log.Adult.Mortality:CountrySomalia                                                              436.74973687
    ## log.Adult.Mortality:CountrySouth Africa                                                          97.12525180
    ## log.Adult.Mortality:CountrySouth Sudan                                                          344.29530078
    ## log.Adult.Mortality:CountrySpain                                                                 99.59377456
    ## log.Adult.Mortality:CountrySri Lanka                                                             96.22041965
    ## log.Adult.Mortality:CountrySudan                                                                133.65337934
    ## log.Adult.Mortality:CountrySuriname                                                              90.58622560
    ## log.Adult.Mortality:CountrySwaziland                                                             94.17767394
    ## log.Adult.Mortality:CountrySweden                                                                90.51818609
    ## log.Adult.Mortality:CountrySwitzerland                                                           90.48580626
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  91.24516414
    ## log.Adult.Mortality:CountryTajikistan                                                           120.88379932
    ## log.Adult.Mortality:CountryThailand                                                              92.79342486
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             90.52808807
    ## log.Adult.Mortality:CountryTimor-Leste                                                           95.40252220
    ## log.Adult.Mortality:CountryTogo                                                                 141.19647965
    ## log.Adult.Mortality:CountryTonga                                                                 90.84344482
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   90.67200800
    ## log.Adult.Mortality:CountryTunisia                                                              167.62889647
    ## log.Adult.Mortality:CountryTurkey                                                               110.09342860
    ## log.Adult.Mortality:CountryTurkmenistan                                                         116.88258807
    ## log.Adult.Mortality:CountryUganda                                                                92.08877416
    ## log.Adult.Mortality:CountryUkraine                                                               95.22196816
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  90.51033708
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                 123.89553230
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                          130.33799755
    ## log.Adult.Mortality:CountryUnited States of America                                             272.60290362
    ## log.Adult.Mortality:CountryUruguay                                                               97.69703646
    ## log.Adult.Mortality:CountryUzbekistan                                                            93.00079542
    ## log.Adult.Mortality:CountryVanuatu                                                               90.50621257
    ## log.Adult.Mortality:CountryVenezuela                                                           1155.47049592
    ## log.Adult.Mortality:CountryViet Nam                                                             487.81481329
    ## log.Adult.Mortality:CountryYemen                                                                258.52075642
    ## log.Adult.Mortality:CountryZambia                                                               102.34253644
    ## log.Adult.Mortality:CountryZimbabwe                                                              92.36834612
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                              0.92791605
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                 31.91922964
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                  6.03063313
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                   0.68975501
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                                5.19342452
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                               5.23789348
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                               0.11734068
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                 10.96439658
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 17.77724590
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                  27.62236810
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                  4.01275886
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                   0.28767877
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                             2.64055992
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                  5.02991428
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                 0.71882728
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                 2.94350879
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                    45.00886790
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                    0.04398872
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                 3.79464672
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                 14.00810234
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                    6.76670906
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                    1.24624370
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                         1.73563809
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                                27.97958419
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                      38.65397465
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                  8.19094641
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                    4.96836154
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                             23.04944960
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                  5.47679594
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                                 0.10682968
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                 13.92917393
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                 16.98122968
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                    3.30857990
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                                2.66940356
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                   2.16850342
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                           33.18459575
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                  26.63840540
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                    1.32324276
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                 7.25561509
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                 13.57116239
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                    0.02310127
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                                0.90719607
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                     0.89365568
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                     2.81009628
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                    6.68844700
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                              5.20254863
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                    9.73791871
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                               2.55273345
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                    0.33513116
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                  27.16119259
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                              17.55118537
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                         2.88655529
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                  9.74091600
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                  1.89240651
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                   62.44172825
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                               2.88699035
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                   0.32573462
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                17.49530352
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                     5.20372186
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                             242.84511861
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                   2.15753943
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                                18.70114907
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                  0.57253239
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                               1.06861046
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                  0.57760141
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                 57.86261223
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                    0.46374646
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                                6.51422552
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                    7.13608471
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                  0.32430041
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                 1.24761999
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                 472.33517039
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                        38.11273261
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                12.26804309
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                     3.22752464
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                              2.87477318
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                  22.89592567
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                68.04112712
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                        2.90918665
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                 10.17440311
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                       2.26083994
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                   0.27689203
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                                 NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                             6.21828967
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                  3.92310087
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                             2.42290647
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                  8.53475652
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                             0.75393352
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             12.63086679
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                   21.91336122
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                                7.91246067
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                    1.43493759
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                               10.02623224
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                     1.74748208
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                               8.03708212
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                 1.71554405
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                        NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                             12.35330719
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                     7.88707059
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                 45.81005102
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                   1.84608640
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                            11.35587920
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                   0.26496048
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                  6.27262218
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland    22.00498836
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                              1.12452670
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                 9.84660921
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                 39.01669526
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                               1.05228446
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                              127.31481686
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                16.99320778
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                    6.74012082
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                   1.75680852
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                 0.75227638
    ##                                                                                               t value
    ## (Intercept)                                                                                      1.30
    ## log.Adult.Mortality                                                                             -1.15
    ## infant.deaths                                                                                   -1.07
    ## log.GDP                                                                                          0.13
    ## Measles                                                                                         -0.88
    ## HIV_cat                                                                                         -1.69
    ## CountryAlbania                                                                                  -1.05
    ## CountryAlgeria                                                                                  -0.65
    ## CountryAngola                                                                                   -0.43
    ## CountryAntigua and Barbuda                                                                      -1.03
    ## CountryArgentina                                                                                -0.84
    ## CountryArmenia                                                                                  -1.03
    ## CountryAustralia                                                                                -1.06
    ## CountryAustria                                                                                  -0.95
    ## CountryAzerbaijan                                                                               -1.09
    ## CountryBahamas                                                                                  -1.04
    ## CountryBahrain                                                                                  -1.11
    ## CountryBangladesh                                                                               -1.15
    ## CountryBarbados                                                                                 -1.08
    ## CountryBelarus                                                                                  -1.13
    ## CountryBelgium                                                                                  -0.84
    ## CountryBelize                                                                                   -0.97
    ## CountryBenin                                                                                    -0.79
    ## CountryBhutan                                                                                   -0.86
    ## CountryBolivia                                                                                  -0.35
    ## CountryBosnia and Herzegovina                                                                   -1.05
    ## CountryBotswana                                                                                 -0.90
    ## CountryBrazil                                                                                   -0.52
    ## CountryBrunei Darussalam                                                                        -1.01
    ## CountryBulgaria                                                                                 -0.97
    ## CountryBurkina Faso                                                                             -1.61
    ## CountryBurundi                                                                                   0.57
    ## CountryCabo Verde                                                                               -1.13
    ## CountryCambodia                                                                                 -1.12
    ## CountryCameroon                                                                                 -0.34
    ## CountryCanada                                                                                   -1.05
    ## CountryCentral African Republic                                                                 -0.93
    ## CountryChad                                                                                     -0.20
    ## CountryChile                                                                                    -0.81
    ## CountryChina                                                                                    -1.11
    ## CountryColombia                                                                                 -0.80
    ## CountryComoros                                                                                  -0.80
    ## CountryCongo                                                                                    -0.50
    ## CountryCosta Rica                                                                               -1.01
    ## CountryCroatia                                                                                  -1.04
    ## CountryCuba                                                                                     -1.06
    ## CountryCyprus                                                                                   -1.03
    ## CountryCzechia                                                                                  -1.04
    ## CountryDemocratic People's Republic of Korea                                                    -0.86
    ## CountryDemocratic Republic of the Congo                                                          0.00
    ## CountryDenmark                                                                                  -0.97
    ## CountryDjibouti                                                                                 -1.50
    ## CountryDominican Republic                                                                       -1.25
    ## CountryEcuador                                                                                  -0.90
    ## CountryEgypt                                                                                     0.91
    ## CountryEl Salvador                                                                              -1.16
    ## CountryEquatorial Guinea                                                                        -0.84
    ## CountryEritrea                                                                                  -1.84
    ## CountryEstonia                                                                                  -1.09
    ## CountryEthiopia                                                                                 -1.28
    ## CountryFiji                                                                                     -1.02
    ## CountryFinland                                                                                  -0.84
    ## CountryFrance                                                                                   -1.05
    ## CountryGabon                                                                                    -0.97
    ## CountryGambia                                                                                   -0.38
    ## CountryGeorgia                                                                                  -1.27
    ## CountryGermany                                                                                   0.02
    ## CountryGhana                                                                                    -3.06
    ## CountryGreece                                                                                   -0.77
    ## CountryGrenada                                                                                  -1.19
    ## CountryGuatemala                                                                                -2.72
    ## CountryGuinea                                                                                   -0.98
    ## CountryGuinea-Bissau                                                                             1.32
    ## CountryGuyana                                                                                   -1.09
    ## CountryHaiti                                                                                    -1.64
    ## CountryHonduras                                                                                 -0.87
    ## CountryHungary                                                                                  -1.05
    ## CountryIceland                                                                                  -1.11
    ## CountryIndia                                                                                    -0.66
    ## CountryIndonesia                                                                                -0.90
    ## CountryIran                                                                                     -1.04
    ## CountryIraq                                                                                     -2.42
    ## CountryIreland                                                                                  -0.99
    ## CountryIsrael                                                                                   -1.06
    ## CountryItaly                                                                                    -1.15
    ## CountryIvory Coast                                                                               0.48
    ## CountryJamaica                                                                                  -1.03
    ## CountryJapan                                                                                    -1.10
    ## CountryJordan                                                                                   -1.01
    ## CountryKazakhstan                                                                               -1.04
    ## CountryKenya                                                                                    -1.21
    ## CountryKiribati                                                                                 -0.94
    ## CountryKuwait                                                                                   -1.04
    ## CountryKyrgyzstan                                                                               -1.58
    ## CountryLao People's Democratic Republic                                                         -1.03
    ## CountryLatvia                                                                                   -1.10
    ## CountryLebanon                                                                                  -1.05
    ## CountryLesotho                                                                                  -0.39
    ## CountryLiberia                                                                                  -0.94
    ## CountryLibya                                                                                     1.37
    ## CountryLithuania                                                                                -1.17
    ## CountryLuxembourg                                                                               -1.04
    ## CountryMadagascar                                                                               -1.75
    ## CountryMalawi                                                                                   -1.13
    ## CountryMalaysia                                                                                 -0.85
    ## CountryMaldives                                                                                 -1.09
    ## CountryMali                                                                                      0.47
    ## CountryMalta                                                                                    -0.96
    ## CountryMauritania                                                                                1.04
    ## CountryMauritius                                                                                -0.94
    ## CountryMexico                                                                                   -0.63
    ## CountryMicronesia (Federated States of)                                                         -0.96
    ## CountryMongolia                                                                                 -1.14
    ## CountryMontenegro                                                                               -1.05
    ## CountryMorocco                                                                                  -1.11
    ## CountryMozambique                                                                                0.78
    ## CountryMyanmar                                                                                  -1.13
    ## CountryNamibia                                                                                  -0.67
    ## CountryNepal                                                                                    -1.10
    ## CountryNetherlands                                                                              -1.00
    ## CountryNew Zealand                                                                              -0.94
    ## CountryNicaragua                                                                                -0.64
    ## CountryNiger                                                                                     3.54
    ## CountryNigeria                                                                                  -2.08
    ## CountryNorway                                                                                   -1.03
    ## CountryOman                                                                                     -1.03
    ## CountryPakistan                                                                                 -0.48
    ## CountryPanama                                                                                   -0.51
    ## CountryPapua New Guinea                                                                         -0.35
    ## CountryParaguay                                                                                 -0.39
    ## CountryPeru                                                                                     -1.12
    ## CountryPhilippines                                                                              -0.58
    ## CountryPoland                                                                                   -0.96
    ## CountryPortugal                                                                                 -0.87
    ## CountryQatar                                                                                    -1.04
    ## CountryRepublic of Korea                                                                        -1.01
    ## CountryRepublic of Moldova                                                                      -0.86
    ## CountryRomania                                                                                  -0.30
    ## CountryRussian Federation                                                                       -0.43
    ## CountryRwanda                                                                                   -1.05
    ## CountrySaint Lucia                                                                              -1.01
    ## CountrySaint Vincent and the Grenadines                                                         -1.44
    ## CountrySamoa                                                                                    -1.18
    ## CountrySao Tome and Principe                                                                    -0.89
    ## CountrySaudi Arabia                                                                             -0.86
    ## CountrySenegal                                                                                  -0.91
    ## CountrySerbia                                                                                   -1.02
    ## CountrySeychelles                                                                               -1.04
    ## CountrySierra Leone                                                                              0.77
    ## CountrySingapore                                                                                -1.04
    ## CountrySlovakia                                                                                 -1.04
    ## CountrySlovenia                                                                                 -0.96
    ## CountrySolomon Islands                                                                          -1.08
    ## CountrySomalia                                                                                  -1.58
    ## CountrySouth Africa                                                                             -0.17
    ## CountrySouth Sudan                                                                               0.57
    ## CountrySpain                                                                                    -1.10
    ## CountrySri Lanka                                                                                -1.12
    ## CountrySudan                                                                                    -1.69
    ## CountrySuriname                                                                                 -0.88
    ## CountrySwaziland                                                                                -1.06
    ## CountrySweden                                                                                   -1.09
    ## CountrySwitzerland                                                                              -1.10
    ## CountrySyrian Arab Republic                                                                     -1.05
    ## CountryTajikistan                                                                               -0.28
    ## CountryThailand                                                                                 -1.05
    ## CountryThe former Yugoslav republic of Macedonia                                                -1.05
    ## CountryTimor-Leste                                                                              -0.93
    ## CountryTogo                                                                                     -0.31
    ## CountryTonga                                                                                    -1.03
    ## CountryTrinidad and Tobago                                                                      -0.82
    ## CountryTunisia                                                                                  -0.84
    ## CountryTurkey                                                                                   -0.48
    ## CountryTurkmenistan                                                                             -0.90
    ## CountryUganda                                                                                   -0.72
    ## CountryUkraine                                                                                  -1.65
    ## CountryUnited Arab Emirates                                                                     -1.07
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                     -1.09
    ## CountryUnited Republic of Tanzania                                                               1.41
    ## CountryUnited States of America                                                                 -0.88
    ## CountryUruguay                                                                                  -0.98
    ## CountryUzbekistan                                                                               -1.11
    ## CountryVanuatu                                                                                  -1.03
    ## CountryVenezuela                                                                                -0.12
    ## CountryViet Nam                                                                                 -0.27
    ## CountryYemen                                                                                     0.86
    ## CountryZambia                                                                                   -1.12
    ## CountryZimbabwe                                                                                 -0.43
    ## infant.deaths:CountryAlbania                                                                     0.47
    ## infant.deaths:CountryAlgeria                                                                     0.17
    ## infant.deaths:CountryAngola                                                                      0.54
    ## infant.deaths:CountryAntigua and Barbuda                                                           NA
    ## infant.deaths:CountryArgentina                                                                   0.10
    ## infant.deaths:CountryArmenia                                                                       NA
    ## infant.deaths:CountryAustralia                                                                     NA
    ## infant.deaths:CountryAustria                                                                       NA
    ## infant.deaths:CountryAzerbaijan                                                                  0.52
    ## infant.deaths:CountryBahamas                                                                       NA
    ## infant.deaths:CountryBahrain                                                                       NA
    ## infant.deaths:CountryBangladesh                                                                  1.04
    ## infant.deaths:CountryBarbados                                                                      NA
    ## infant.deaths:CountryBelarus                                                                     1.69
    ## infant.deaths:CountryBelgium                                                                    -1.15
    ## infant.deaths:CountryBelize                                                                        NA
    ## infant.deaths:CountryBenin                                                                         NA
    ## infant.deaths:CountryBhutan                                                                      0.07
    ## infant.deaths:CountryBolivia                                                                     0.65
    ## infant.deaths:CountryBosnia and Herzegovina                                                        NA
    ## infant.deaths:CountryBotswana                                                                      NA
    ## infant.deaths:CountryBrazil                                                                      0.58
    ## infant.deaths:CountryBrunei Darussalam                                                             NA
    ## infant.deaths:CountryBulgaria                                                                    1.00
    ## infant.deaths:CountryBurkina Faso                                                                1.50
    ## infant.deaths:CountryBurundi                                                                    -0.99
    ## infant.deaths:CountryCabo Verde                                                                    NA
    ## infant.deaths:CountryCambodia                                                                    1.08
    ## infant.deaths:CountryCameroon                                                                    0.20
    ## infant.deaths:CountryCanada                                                                        NA
    ## infant.deaths:CountryCentral African Republic                                                    0.93
    ## infant.deaths:CountryChad                                                                        0.19
    ## infant.deaths:CountryChile                                                                         NA
    ## infant.deaths:CountryChina                                                                       1.08
    ## infant.deaths:CountryColombia                                                                    0.11
    ## infant.deaths:CountryComoros                                                                    -0.39
    ## infant.deaths:CountryCongo                                                                      -0.68
    ## infant.deaths:CountryCosta Rica                                                                    NA
    ## infant.deaths:CountryCroatia                                                                       NA
    ## infant.deaths:CountryCuba                                                                          NA
    ## infant.deaths:CountryCyprus                                                                        NA
    ## infant.deaths:CountryCzechia                                                                       NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                      -0.01
    ## infant.deaths:CountryDemocratic Republic of the Congo                                            0.31
    ## infant.deaths:CountryDenmark                                                                       NA
    ## infant.deaths:CountryDjibouti                                                                    1.10
    ## infant.deaths:CountryDominican Republic                                                          0.95
    ## infant.deaths:CountryEcuador                                                                     0.07
    ## infant.deaths:CountryEgypt                                                                      -0.75
    ## infant.deaths:CountryEl Salvador                                                                 0.63
    ## infant.deaths:CountryEquatorial Guinea                                                             NA
    ## infant.deaths:CountryEritrea                                                                     2.66
    ## infant.deaths:CountryEstonia                                                                       NA
    ## infant.deaths:CountryEthiopia                                                                    1.29
    ## infant.deaths:CountryFiji                                                                          NA
    ## infant.deaths:CountryFinland                                                                       NA
    ## infant.deaths:CountryFrance                                                                      1.35
    ## infant.deaths:CountryGabon                                                                         NA
    ## infant.deaths:CountryGambia                                                                        NA
    ## infant.deaths:CountryGeorgia                                                                     0.86
    ## infant.deaths:CountryGermany                                                                    -2.23
    ## infant.deaths:CountryGhana                                                                       3.47
    ## infant.deaths:CountryGreece                                                                      0.99
    ## infant.deaths:CountryGrenada                                                                       NA
    ## infant.deaths:CountryGuatemala                                                                   5.56
    ## infant.deaths:CountryGuinea                                                                      0.63
    ## infant.deaths:CountryGuinea-Bissau                                                              -2.22
    ## infant.deaths:CountryGuyana                                                                      0.16
    ## infant.deaths:CountryHaiti                                                                       2.69
    ## infant.deaths:CountryHonduras                                                                    0.19
    ## infant.deaths:CountryHungary                                                                     0.64
    ## infant.deaths:CountryIceland                                                                       NA
    ## infant.deaths:CountryIndia                                                                       1.08
    ## infant.deaths:CountryIndonesia                                                                   1.07
    ## infant.deaths:CountryIran                                                                        0.71
    ## infant.deaths:CountryIraq                                                                        2.76
    ## infant.deaths:CountryIreland                                                                       NA
    ## infant.deaths:CountryIsrael                                                                      1.03
    ## infant.deaths:CountryItaly                                                                       0.82
    ## infant.deaths:CountryIvory Coast                                                                -0.57
    ## infant.deaths:CountryJamaica                                                                       NA
    ## infant.deaths:CountryJapan                                                                       0.42
    ## infant.deaths:CountryJordan                                                                        NA
    ## infant.deaths:CountryKazakhstan                                                                  0.75
    ## infant.deaths:CountryKenya                                                                       1.31
    ## infant.deaths:CountryKiribati                                                                      NA
    ## infant.deaths:CountryKuwait                                                                      0.21
    ## infant.deaths:CountryKyrgyzstan                                                                  1.34
    ## infant.deaths:CountryLao People's Democratic Republic                                            0.87
    ## infant.deaths:CountryLatvia                                                                        NA
    ## infant.deaths:CountryLebanon                                                                       NA
    ## infant.deaths:CountryLesotho                                                                    -0.71
    ## infant.deaths:CountryLiberia                                                                     0.56
    ## infant.deaths:CountryLibya                                                                      -2.61
    ## infant.deaths:CountryLithuania                                                                     NA
    ## infant.deaths:CountryLuxembourg                                                                    NA
    ## infant.deaths:CountryMadagascar                                                                  1.42
    ## infant.deaths:CountryMalawi                                                                      1.38
    ## infant.deaths:CountryMalaysia                                                                    0.13
    ## infant.deaths:CountryMaldives                                                                      NA
    ## infant.deaths:CountryMali                                                                       -0.59
    ## infant.deaths:CountryMalta                                                                         NA
    ## infant.deaths:CountryMauritania                                                                 -1.10
    ## infant.deaths:CountryMauritius                                                                     NA
    ## infant.deaths:CountryMexico                                                                      0.19
    ## infant.deaths:CountryMicronesia (Federated States of)                                              NA
    ## infant.deaths:CountryMongolia                                                                    1.17
    ## infant.deaths:CountryMontenegro                                                                    NA
    ## infant.deaths:CountryMorocco                                                                     0.75
    ## infant.deaths:CountryMozambique                                                                 -1.44
    ## infant.deaths:CountryMyanmar                                                                     1.23
    ## infant.deaths:CountryNamibia                                                                     0.30
    ## infant.deaths:CountryNepal                                                                       1.19
    ## infant.deaths:CountryNetherlands                                                                   NA
    ## infant.deaths:CountryNew Zealand                                                                   NA
    ## infant.deaths:CountryNicaragua                                                                  -1.58
    ## infant.deaths:CountryNiger                                                                      -3.61
    ## infant.deaths:CountryNigeria                                                                     1.80
    ## infant.deaths:CountryNorway                                                                        NA
    ## infant.deaths:CountryOman                                                                          NA
    ## infant.deaths:CountryPakistan                                                                    0.89
    ## infant.deaths:CountryPanama                                                                      0.31
    ## infant.deaths:CountryPapua New Guinea                                                            0.21
    ## infant.deaths:CountryParaguay                                                                   -1.58
    ## infant.deaths:CountryPeru                                                                        0.03
    ## infant.deaths:CountryPhilippines                                                                 0.39
    ## infant.deaths:CountryPoland                                                                      0.11
    ## infant.deaths:CountryPortugal                                                                   -0.11
    ## infant.deaths:CountryQatar                                                                         NA
    ## infant.deaths:CountryRepublic of Korea                                                           1.11
    ## infant.deaths:CountryRepublic of Moldova                                                           NA
    ## infant.deaths:CountryRomania                                                                    -3.34
    ## infant.deaths:CountryRussian Federation                                                         -0.98
    ## infant.deaths:CountryRwanda                                                                      1.72
    ## infant.deaths:CountrySaint Lucia                                                                   NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                              NA
    ## infant.deaths:CountrySamoa                                                                         NA
    ## infant.deaths:CountrySao Tome and Principe                                                         NA
    ## infant.deaths:CountrySaudi Arabia                                                                0.02
    ## infant.deaths:CountrySenegal                                                                     0.43
    ## infant.deaths:CountrySerbia                                                                        NA
    ## infant.deaths:CountrySeychelles                                                                    NA
    ## infant.deaths:CountrySierra Leone                                                               -1.77
    ## infant.deaths:CountrySingapore                                                                     NA
    ## infant.deaths:CountrySlovakia                                                                      NA
    ## infant.deaths:CountrySlovenia                                                                      NA
    ## infant.deaths:CountrySolomon Islands                                                               NA
    ## infant.deaths:CountrySomalia                                                                     1.52
    ## infant.deaths:CountrySouth Africa                                                               -0.41
    ## infant.deaths:CountrySouth Sudan                                                                -0.76
    ## infant.deaths:CountrySpain                                                                       0.54
    ## infant.deaths:CountrySri Lanka                                                                   0.56
    ## infant.deaths:CountrySudan                                                                       1.69
    ## infant.deaths:CountrySuriname                                                                      NA
    ## infant.deaths:CountrySwaziland                                                                   0.93
    ## infant.deaths:CountrySweden                                                                        NA
    ## infant.deaths:CountrySwitzerland                                                                   NA
    ## infant.deaths:CountrySyrian Arab Republic                                                        0.62
    ## infant.deaths:CountryTajikistan                                                                 -0.56
    ## infant.deaths:CountryThailand                                                                    0.82
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                     NA
    ## infant.deaths:CountryTimor-Leste                                                                 0.08
    ## infant.deaths:CountryTogo                                                                       -0.19
    ## infant.deaths:CountryTonga                                                                         NA
    ## infant.deaths:CountryTrinidad and Tobago                                                           NA
    ## infant.deaths:CountryTunisia                                                                     0.29
    ## infant.deaths:CountryTurkey                                                                      0.21
    ## infant.deaths:CountryTurkmenistan                                                                0.29
    ## infant.deaths:CountryUganda                                                                      1.14
    ## infant.deaths:CountryUkraine                                                                     2.57
    ## infant.deaths:CountryUnited Arab Emirates                                                          NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        0.56
    ## infant.deaths:CountryUnited Republic of Tanzania                                                -1.47
    ## infant.deaths:CountryUnited States of America                                                    0.64
    ## infant.deaths:CountryUruguay                                                                     0.02
    ## infant.deaths:CountryUzbekistan                                                                  0.73
    ## infant.deaths:CountryVanuatu                                                                       NA
    ## infant.deaths:CountryVenezuela                                                                   0.04
    ## infant.deaths:CountryViet Nam                                                                    0.13
    ## infant.deaths:CountryYemen                                                                      -1.02
    ## infant.deaths:CountryZambia                                                                      0.79
    ## infant.deaths:CountryZimbabwe                                                                   -0.46
    ## log.Adult.Mortality:CountryAlbania                                                               1.04
    ## log.Adult.Mortality:CountryAlgeria                                                               0.58
    ## log.Adult.Mortality:CountryAngola                                                                0.41
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   1.02
    ## log.Adult.Mortality:CountryArgentina                                                             0.79
    ## log.Adult.Mortality:CountryArmenia                                                               1.02
    ## log.Adult.Mortality:CountryAustralia                                                             1.07
    ## log.Adult.Mortality:CountryAustria                                                               0.93
    ## log.Adult.Mortality:CountryAzerbaijan                                                            1.09
    ## log.Adult.Mortality:CountryBahamas                                                               1.04
    ## log.Adult.Mortality:CountryBahrain                                                               1.10
    ## log.Adult.Mortality:CountryBangladesh                                                            1.13
    ## log.Adult.Mortality:CountryBarbados                                                              1.08
    ## log.Adult.Mortality:CountryBelarus                                                               1.13
    ## log.Adult.Mortality:CountryBelgium                                                               0.79
    ## log.Adult.Mortality:CountryBelize                                                                0.96
    ## log.Adult.Mortality:CountryBenin                                                                 0.85
    ## log.Adult.Mortality:CountryBhutan                                                                0.86
    ## log.Adult.Mortality:CountryBolivia                                                               0.30
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                1.05
    ## log.Adult.Mortality:CountryBotswana                                                              0.90
    ## log.Adult.Mortality:CountryBrazil                                                                0.47
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     1.00
    ## log.Adult.Mortality:CountryBulgaria                                                              0.95
    ## log.Adult.Mortality:CountryBurkina Faso                                                          1.62
    ## log.Adult.Mortality:CountryBurundi                                                              -0.55
    ## log.Adult.Mortality:CountryCabo Verde                                                            1.13
    ## log.Adult.Mortality:CountryCambodia                                                              1.12
    ## log.Adult.Mortality:CountryCameroon                                                              0.38
    ## log.Adult.Mortality:CountryCanada                                                                1.06
    ## log.Adult.Mortality:CountryCentral African Republic                                              0.92
    ## log.Adult.Mortality:CountryChad                                                                  0.21
    ## log.Adult.Mortality:CountryChile                                                                 0.77
    ## log.Adult.Mortality:CountryChina                                                                 1.10
    ## log.Adult.Mortality:CountryColombia                                                              0.77
    ## log.Adult.Mortality:CountryComoros                                                               0.78
    ## log.Adult.Mortality:CountryCongo                                                                 0.59
    ## log.Adult.Mortality:CountryCosta Rica                                                            1.01
    ## log.Adult.Mortality:CountryCroatia                                                               1.03
    ## log.Adult.Mortality:CountryCuba                                                                  1.06
    ## log.Adult.Mortality:CountryCyprus                                                                1.01
    ## log.Adult.Mortality:CountryCzechia                                                               1.03
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 0.84
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                      0.00
    ## log.Adult.Mortality:CountryDenmark                                                               0.95
    ## log.Adult.Mortality:CountryDjibouti                                                              1.48
    ## log.Adult.Mortality:CountryDominican Republic                                                    1.25
    ## log.Adult.Mortality:CountryEcuador                                                               0.87
    ## log.Adult.Mortality:CountryEgypt                                                                -0.91
    ## log.Adult.Mortality:CountryEl Salvador                                                           1.16
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     0.84
    ## log.Adult.Mortality:CountryEritrea                                                               1.81
    ## log.Adult.Mortality:CountryEstonia                                                               1.10
    ## log.Adult.Mortality:CountryEthiopia                                                              1.24
    ## log.Adult.Mortality:CountryFiji                                                                  1.01
    ## log.Adult.Mortality:CountryFinland                                                               0.80
    ## log.Adult.Mortality:CountryFrance                                                                1.05
    ## log.Adult.Mortality:CountryGabon                                                                 0.97
    ## log.Adult.Mortality:CountryGambia                                                                0.41
    ## log.Adult.Mortality:CountryGeorgia                                                               1.29
    ## log.Adult.Mortality:CountryGermany                                                              -0.25
    ## log.Adult.Mortality:CountryGhana                                                                 2.97
    ## log.Adult.Mortality:CountryGreece                                                                0.70
    ## log.Adult.Mortality:CountryGrenada                                                               1.20
    ## log.Adult.Mortality:CountryGuatemala                                                             2.75
    ## log.Adult.Mortality:CountryGuinea                                                                0.99
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                        -1.27
    ## log.Adult.Mortality:CountryGuyana                                                                1.08
    ## log.Adult.Mortality:CountryHaiti                                                                 1.62
    ## log.Adult.Mortality:CountryHonduras                                                              0.85
    ## log.Adult.Mortality:CountryHungary                                                               1.05
    ## log.Adult.Mortality:CountryIceland                                                               1.12
    ## log.Adult.Mortality:CountryIndia                                                                 0.62
    ## log.Adult.Mortality:CountryIndonesia                                                             0.89
    ## log.Adult.Mortality:CountryIran                                                                  1.03
    ## log.Adult.Mortality:CountryIraq                                                                  2.40
    ## log.Adult.Mortality:CountryIreland                                                               0.97
    ## log.Adult.Mortality:CountryIsrael                                                                1.06
    ## log.Adult.Mortality:CountryItaly                                                                 1.18
    ## log.Adult.Mortality:CountryIvory Coast                                                          -0.44
    ## log.Adult.Mortality:CountryJamaica                                                               1.03
    ## log.Adult.Mortality:CountryJapan                                                                 1.11
    ## log.Adult.Mortality:CountryJordan                                                                1.00
    ## log.Adult.Mortality:CountryKazakhstan                                                            1.04
    ## log.Adult.Mortality:CountryKenya                                                                 1.19
    ## log.Adult.Mortality:CountryKiribati                                                              0.92
    ## log.Adult.Mortality:CountryKuwait                                                                1.03
    ## log.Adult.Mortality:CountryKyrgyzstan                                                            1.59
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      1.00
    ## log.Adult.Mortality:CountryLatvia                                                                1.10
    ## log.Adult.Mortality:CountryLebanon                                                               1.04
    ## log.Adult.Mortality:CountryLesotho                                                               0.46
    ## log.Adult.Mortality:CountryLiberia                                                               0.94
    ## log.Adult.Mortality:CountryLibya                                                                -1.47
    ## log.Adult.Mortality:CountryLithuania                                                             1.17
    ## log.Adult.Mortality:CountryLuxembourg                                                            1.03
    ## log.Adult.Mortality:CountryMadagascar                                                            1.76
    ## log.Adult.Mortality:CountryMalawi                                                                1.10
    ## log.Adult.Mortality:CountryMalaysia                                                              0.82
    ## log.Adult.Mortality:CountryMaldives                                                              1.09
    ## log.Adult.Mortality:CountryMali                                                                 -0.45
    ## log.Adult.Mortality:CountryMalta                                                                 0.93
    ## log.Adult.Mortality:CountryMauritania                                                           -1.04
    ## log.Adult.Mortality:CountryMauritius                                                             0.93
    ## log.Adult.Mortality:CountryMexico                                                                0.59
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      0.94
    ## log.Adult.Mortality:CountryMongolia                                                              1.12
    ## log.Adult.Mortality:CountryMontenegro                                                            1.04
    ## log.Adult.Mortality:CountryMorocco                                                               1.13
    ## log.Adult.Mortality:CountryMozambique                                                           -0.63
    ## log.Adult.Mortality:CountryMyanmar                                                               1.12
    ## log.Adult.Mortality:CountryNamibia                                                               0.68
    ## log.Adult.Mortality:CountryNepal                                                                 1.09
    ## log.Adult.Mortality:CountryNetherlands                                                           0.98
    ## log.Adult.Mortality:CountryNew Zealand                                                           0.91
    ## log.Adult.Mortality:CountryNicaragua                                                             0.61
    ## log.Adult.Mortality:CountryNiger                                                                -3.51
    ## log.Adult.Mortality:CountryNigeria                                                               2.03
    ## log.Adult.Mortality:CountryNorway                                                                1.03
    ## log.Adult.Mortality:CountryOman                                                                  1.02
    ## log.Adult.Mortality:CountryPakistan                                                              0.48
    ## log.Adult.Mortality:CountryPanama                                                                0.49
    ## log.Adult.Mortality:CountryPapua New Guinea                                                      0.34
    ## log.Adult.Mortality:CountryParaguay                                                              0.34
    ## log.Adult.Mortality:CountryPeru                                                                  1.13
    ## log.Adult.Mortality:CountryPhilippines                                                           0.58
    ## log.Adult.Mortality:CountryPoland                                                                0.94
    ## log.Adult.Mortality:CountryPortugal                                                              0.83
    ## log.Adult.Mortality:CountryQatar                                                                 1.02
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     0.98
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   0.85
    ## log.Adult.Mortality:CountryRomania                                                               0.26
    ## log.Adult.Mortality:CountryRussian Federation                                                    0.47
    ## log.Adult.Mortality:CountryRwanda                                                                1.03
    ## log.Adult.Mortality:CountrySaint Lucia                                                           1.00
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                      1.45
    ## log.Adult.Mortality:CountrySamoa                                                                 1.19
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 0.87
    ## log.Adult.Mortality:CountrySaudi Arabia                                                          0.80
    ## log.Adult.Mortality:CountrySenegal                                                               0.93
    ## log.Adult.Mortality:CountrySerbia                                                                1.01
    ## log.Adult.Mortality:CountrySeychelles                                                            1.03
    ## log.Adult.Mortality:CountrySierra Leone                                                         -0.64
    ## log.Adult.Mortality:CountrySingapore                                                             1.03
    ## log.Adult.Mortality:CountrySlovakia                                                              1.04
    ## log.Adult.Mortality:CountrySlovenia                                                              0.94
    ## log.Adult.Mortality:CountrySolomon Islands                                                       1.07
    ## log.Adult.Mortality:CountrySomalia                                                               1.60
    ## log.Adult.Mortality:CountrySouth Africa                                                          0.26
    ## log.Adult.Mortality:CountrySouth Sudan                                                          -0.54
    ## log.Adult.Mortality:CountrySpain                                                                 1.07
    ## log.Adult.Mortality:CountrySri Lanka                                                             1.11
    ## log.Adult.Mortality:CountrySudan                                                                 1.68
    ## log.Adult.Mortality:CountrySuriname                                                              0.87
    ## log.Adult.Mortality:CountrySwaziland                                                             1.04
    ## log.Adult.Mortality:CountrySweden                                                                1.10
    ## log.Adult.Mortality:CountrySwitzerland                                                           1.11
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  1.04
    ## log.Adult.Mortality:CountryTajikistan                                                            0.23
    ## log.Adult.Mortality:CountryThailand                                                              1.04
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             1.04
    ## log.Adult.Mortality:CountryTimor-Leste                                                           0.90
    ## log.Adult.Mortality:CountryTogo                                                                  0.33
    ## log.Adult.Mortality:CountryTonga                                                                 1.02
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   0.81
    ## log.Adult.Mortality:CountryTunisia                                                               0.79
    ## log.Adult.Mortality:CountryTurkey                                                                0.38
    ## log.Adult.Mortality:CountryTurkmenistan                                                          0.89
    ## log.Adult.Mortality:CountryUganda                                                                0.71
    ## log.Adult.Mortality:CountryUkraine                                                               1.65
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  1.06
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                  1.07
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                          -1.24
    ## log.Adult.Mortality:CountryUnited States of America                                              0.85
    ## log.Adult.Mortality:CountryUruguay                                                               0.96
    ## log.Adult.Mortality:CountryUzbekistan                                                            1.10
    ## log.Adult.Mortality:CountryVanuatu                                                               1.02
    ## log.Adult.Mortality:CountryVenezuela                                                             0.12
    ## log.Adult.Mortality:CountryViet Nam                                                              0.26
    ## log.Adult.Mortality:CountryYemen                                                                -0.86
    ## log.Adult.Mortality:CountryZambia                                                                1.16
    ## log.Adult.Mortality:CountryZimbabwe                                                              0.51
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                             1.05
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                -0.44
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                 0.02
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                  0.54
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                               0.11
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                             -0.36
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                              0.06
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                -1.65
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 1.24
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                 -0.03
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                -0.36
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                  1.61
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                           -1.25
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                 1.22
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                               -0.51
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                0.09
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                   -0.17
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                  -0.20
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                0.16
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                 0.47
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                   0.73
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                   0.84
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                        0.19
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                               -1.10
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                     -0.92
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                 0.06
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                   0.95
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                            -0.59
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                -2.52
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                               -1.80
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                -0.81
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                 2.29
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                  -3.21
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                              -5.58
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                 -0.28
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                           2.23
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                 -0.13
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                  -2.49
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                               -0.05
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                -0.56
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                  -0.13
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                              -0.44
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                    0.14
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                   -2.47
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                  -0.67
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                             0.73
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                  -0.29
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                             -0.42
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                  -0.94
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                 -0.17
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                             -1.32
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                       -0.56
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                 0.79
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                -0.20
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                   2.63
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                             -1.20
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                 -1.12
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                               -0.07
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                    0.76
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                              1.11
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                  0.29
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                          NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                               -1.11
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                 0.13
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                              2.62
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                -0.62
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                -0.28
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                  -0.58
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                               NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                               NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                               1.79
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                   3.76
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                -2.20
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                               -0.30
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                 -0.31
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                       -0.17
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                1.67
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                    0.26
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                            -0.06
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                 -0.07
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                0.13
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                      -0.74
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                 3.46
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                      1.46
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                 -2.31
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                               NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                          NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                     NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                            0.17
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                -0.23
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                            2.21
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                -1.44
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                            1.72
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             0.83
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                  -0.44
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                              -0.42
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                  -1.37
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                              -0.84
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                               NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                   -0.12
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                              0.69
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                               -0.35
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                 NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                             0.02
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                    0.32
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                -0.26
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                  0.38
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                           -0.22
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                 -0.31
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                -2.47
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland   -0.53
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                             2.58
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                               -0.53
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                 0.00
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                             -0.02
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                              -0.04
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                               -0.07
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                   1.18
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                 -0.47
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                1.75
    ##                                                                                                  Pr(>|t|)
    ## (Intercept)                                                                                       0.19526
    ## log.Adult.Mortality                                                                               0.25114
    ## infant.deaths                                                                                     0.28258
    ## log.GDP                                                                                           0.90011
    ## Measles                                                                                           0.38002
    ## HIV_cat                                                                                           0.09121
    ## CountryAlbania                                                                                    0.29588
    ## CountryAlgeria                                                                                    0.51803
    ## CountryAngola                                                                                     0.66414
    ## CountryAntigua and Barbuda                                                                        0.30411
    ## CountryArgentina                                                                                  0.40373
    ## CountryArmenia                                                                                    0.30169
    ## CountryAustralia                                                                                  0.28719
    ## CountryAustria                                                                                    0.34094
    ## CountryAzerbaijan                                                                                 0.27626
    ## CountryBahamas                                                                                    0.29815
    ## CountryBahrain                                                                                    0.26895
    ## CountryBangladesh                                                                                 0.25114
    ## CountryBarbados                                                                                   0.27999
    ## CountryBelarus                                                                                    0.25784
    ## CountryBelgium                                                                                    0.40326
    ## CountryBelize                                                                                     0.33235
    ## CountryBenin                                                                                      0.43190
    ## CountryBhutan                                                                                     0.38926
    ## CountryBolivia                                                                                    0.72673
    ## CountryBosnia and Herzegovina                                                                     0.29278
    ## CountryBotswana                                                                                   0.36810
    ## CountryBrazil                                                                                     0.60433
    ## CountryBrunei Darussalam                                                                          0.31118
    ## CountryBulgaria                                                                                   0.33449
    ## CountryBurkina Faso                                                                               0.10801
    ## CountryBurundi                                                                                    0.56934
    ## CountryCabo Verde                                                                                 0.25957
    ## CountryCambodia                                                                                   0.26113
    ## CountryCameroon                                                                                   0.73126
    ## CountryCanada                                                                                     0.29181
    ## CountryCentral African Republic                                                                   0.35145
    ## CountryChad                                                                                       0.83904
    ## CountryChile                                                                                      0.42064
    ## CountryChina                                                                                      0.26571
    ## CountryColombia                                                                                   0.42586
    ## CountryComoros                                                                                    0.42549
    ## CountryCongo                                                                                      0.61737
    ## CountryCosta Rica                                                                                 0.31051
    ## CountryCroatia                                                                                    0.29890
    ## CountryCuba                                                                                       0.29070
    ## CountryCyprus                                                                                     0.30544
    ## CountryCzechia                                                                                    0.29824
    ## CountryDemocratic People's Republic of Korea                                                      0.38767
    ## CountryDemocratic Republic of the Congo                                                           0.99640
    ## CountryDenmark                                                                                    0.33212
    ## CountryDjibouti                                                                                   0.13462
    ## CountryDominican Republic                                                                         0.21109
    ## CountryEcuador                                                                                    0.36799
    ## CountryEgypt                                                                                      0.36039
    ## CountryEl Salvador                                                                                0.24446
    ## CountryEquatorial Guinea                                                                          0.40300
    ## CountryEritrea                                                                                    0.06665
    ## CountryEstonia                                                                                    0.27445
    ## CountryEthiopia                                                                                   0.20028
    ## CountryFiji                                                                                       0.30828
    ## CountryFinland                                                                                    0.40256
    ## CountryFrance                                                                                     0.29230
    ## CountryGabon                                                                                      0.33024
    ## CountryGambia                                                                                     0.70568
    ## CountryGeorgia                                                                                    0.20382
    ## CountryGermany                                                                                    0.98277
    ## CountryGhana                                                                                      0.00224
    ## CountryGreece                                                                                     0.44152
    ## CountryGrenada                                                                                    0.23415
    ## CountryGuatemala                                                                                  0.00649
    ## CountryGuinea                                                                                     0.32824
    ## CountryGuinea-Bissau                                                                              0.18663
    ## CountryGuyana                                                                                     0.27718
    ## CountryHaiti                                                                                      0.10105
    ## CountryHonduras                                                                                   0.38246
    ## CountryHungary                                                                                    0.29174
    ## CountryIceland                                                                                    0.26907
    ## CountryIndia                                                                                      0.50973
    ## CountryIndonesia                                                                                  0.36672
    ## CountryIran                                                                                       0.29733
    ## CountryIraq                                                                                       0.01574
    ## CountryIreland                                                                                    0.32400
    ## CountryIsrael                                                                                     0.28997
    ## CountryItaly                                                                                      0.24966
    ## CountryIvory Coast                                                                                0.62988
    ## CountryJamaica                                                                                    0.30328
    ## CountryJapan                                                                                      0.26943
    ## CountryJordan                                                                                     0.31317
    ## CountryKazakhstan                                                                                 0.29713
    ## CountryKenya                                                                                      0.22831
    ## CountryKiribati                                                                                   0.34950
    ## CountryKuwait                                                                                     0.29629
    ## CountryKyrgyzstan                                                                                 0.11538
    ## CountryLao People's Democratic Republic                                                           0.30533
    ## CountryLatvia                                                                                     0.26988
    ## CountryLebanon                                                                                    0.29533
    ## CountryLesotho                                                                                    0.69416
    ## CountryLiberia                                                                                    0.34915
    ## CountryLibya                                                                                      0.17089
    ## CountryLithuania                                                                                  0.24217
    ## CountryLuxembourg                                                                                 0.29795
    ## CountryMadagascar                                                                                 0.08032
    ## CountryMalawi                                                                                     0.26016
    ## CountryMalaysia                                                                                   0.39367
    ## CountryMaldives                                                                                   0.27382
    ## CountryMali                                                                                       0.63528
    ## CountryMalta                                                                                      0.33473
    ## CountryMauritania                                                                                 0.29954
    ## CountryMauritius                                                                                  0.34544
    ## CountryMexico                                                                                     0.53055
    ## CountryMicronesia (Federated States of)                                                           0.33928
    ## CountryMongolia                                                                                   0.25534
    ## CountryMontenegro                                                                                 0.29586
    ## CountryMorocco                                                                                    0.26883
    ## CountryMozambique                                                                                 0.43357
    ## CountryMyanmar                                                                                    0.25901
    ## CountryNamibia                                                                                    0.50363
    ## CountryNepal                                                                                      0.27221
    ## CountryNetherlands                                                                                0.31872
    ## CountryNew Zealand                                                                                0.34681
    ## CountryNicaragua                                                                                  0.52063
    ## CountryNiger                                                                                      0.00041
    ## CountryNigeria                                                                                    0.03797
    ## CountryNorway                                                                                     0.30220
    ## CountryOman                                                                                       0.30343
    ## CountryPakistan                                                                                   0.62833
    ## CountryPanama                                                                                     0.61040
    ## CountryPapua New Guinea                                                                           0.72554
    ## CountryParaguay                                                                                   0.69944
    ## CountryPeru                                                                                       0.26094
    ## CountryPhilippines                                                                                0.56053
    ## CountryPoland                                                                                     0.33812
    ## CountryPortugal                                                                                   0.38485
    ## CountryQatar                                                                                      0.29930
    ## CountryRepublic of Korea                                                                          0.31489
    ## CountryRepublic of Moldova                                                                        0.39185
    ## CountryRomania                                                                                    0.76429
    ## CountryRussian Federation                                                                         0.66727
    ## CountryRwanda                                                                                     0.29216
    ## CountrySaint Lucia                                                                                0.31349
    ## CountrySaint Vincent and the Grenadines                                                           0.15034
    ## CountrySamoa                                                                                      0.23766
    ## CountrySao Tome and Principe                                                                      0.37420
    ## CountrySaudi Arabia                                                                               0.39095
    ## CountrySenegal                                                                                    0.36353
    ## CountrySerbia                                                                                     0.30583
    ## CountrySeychelles                                                                                 0.30070
    ## CountrySierra Leone                                                                               0.44026
    ## CountrySingapore                                                                                  0.30053
    ## CountrySlovakia                                                                                   0.29660
    ## CountrySlovenia                                                                                   0.33775
    ## CountrySolomon Islands                                                                            0.28046
    ## CountrySomalia                                                                                    0.11538
    ## CountrySouth Africa                                                                               0.86864
    ## CountrySouth Sudan                                                                                0.56806
    ## CountrySpain                                                                                      0.27349
    ## CountrySri Lanka                                                                                  0.26448
    ## CountrySudan                                                                                      0.09204
    ## CountrySuriname                                                                                   0.38105
    ## CountrySwaziland                                                                                  0.29144
    ## CountrySweden                                                                                     0.27438
    ## CountrySwitzerland                                                                                0.27191
    ## CountrySyrian Arab Republic                                                                       0.29394
    ## CountryTajikistan                                                                                 0.77983
    ## CountryThailand                                                                                   0.29368
    ## CountryThe former Yugoslav republic of Macedonia                                                  0.29341
    ## CountryTimor-Leste                                                                                0.35498
    ## CountryTogo                                                                                       0.75518
    ## CountryTonga                                                                                      0.30384
    ## CountryTrinidad and Tobago                                                                        0.41168
    ## CountryTunisia                                                                                    0.39889
    ## CountryTurkey                                                                                     0.62838
    ## CountryTurkmenistan                                                                               0.37064
    ## CountryUganda                                                                                     0.46923
    ## CountryUkraine                                                                                    0.09943
    ## CountryUnited Arab Emirates                                                                       0.28522
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                       0.27403
    ## CountryUnited Republic of Tanzania                                                                0.15910
    ## CountryUnited States of America                                                                   0.37687
    ## CountryUruguay                                                                                    0.32847
    ## CountryUzbekistan                                                                                 0.26773
    ## CountryVanuatu                                                                                    0.30476
    ## CountryVenezuela                                                                                  0.90068
    ## CountryViet Nam                                                                                   0.78538
    ## CountryYemen                                                                                      0.38831
    ## CountryZambia                                                                                     0.26200
    ## CountryZimbabwe                                                                                   0.66526
    ## infant.deaths:CountryAlbania                                                                      0.64142
    ## infant.deaths:CountryAlgeria                                                                      0.86595
    ## infant.deaths:CountryAngola                                                                       0.59057
    ## infant.deaths:CountryAntigua and Barbuda                                                               NA
    ## infant.deaths:CountryArgentina                                                                    0.91868
    ## infant.deaths:CountryArmenia                                                                           NA
    ## infant.deaths:CountryAustralia                                                                         NA
    ## infant.deaths:CountryAustria                                                                           NA
    ## infant.deaths:CountryAzerbaijan                                                                   0.60180
    ## infant.deaths:CountryBahamas                                                                           NA
    ## infant.deaths:CountryBahrain                                                                           NA
    ## infant.deaths:CountryBangladesh                                                                   0.29911
    ## infant.deaths:CountryBarbados                                                                          NA
    ## infant.deaths:CountryBelarus                                                                      0.09159
    ## infant.deaths:CountryBelgium                                                                      0.24952
    ## infant.deaths:CountryBelize                                                                            NA
    ## infant.deaths:CountryBenin                                                                             NA
    ## infant.deaths:CountryBhutan                                                                       0.94583
    ## infant.deaths:CountryBolivia                                                                      0.51898
    ## infant.deaths:CountryBosnia and Herzegovina                                                            NA
    ## infant.deaths:CountryBotswana                                                                          NA
    ## infant.deaths:CountryBrazil                                                                       0.56050
    ## infant.deaths:CountryBrunei Darussalam                                                                 NA
    ## infant.deaths:CountryBulgaria                                                                     0.31504
    ## infant.deaths:CountryBurkina Faso                                                                 0.13339
    ## infant.deaths:CountryBurundi                                                                      0.32307
    ## infant.deaths:CountryCabo Verde                                                                        NA
    ## infant.deaths:CountryCambodia                                                                     0.28137
    ## infant.deaths:CountryCameroon                                                                     0.84250
    ## infant.deaths:CountryCanada                                                                            NA
    ## infant.deaths:CountryCentral African Republic                                                     0.34991
    ## infant.deaths:CountryChad                                                                         0.84870
    ## infant.deaths:CountryChile                                                                             NA
    ## infant.deaths:CountryChina                                                                        0.28045
    ## infant.deaths:CountryColombia                                                                     0.91242
    ## infant.deaths:CountryComoros                                                                      0.69902
    ## infant.deaths:CountryCongo                                                                        0.49711
    ## infant.deaths:CountryCosta Rica                                                                        NA
    ## infant.deaths:CountryCroatia                                                                           NA
    ## infant.deaths:CountryCuba                                                                              NA
    ## infant.deaths:CountryCyprus                                                                            NA
    ## infant.deaths:CountryCzechia                                                                           NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                        0.98836
    ## infant.deaths:CountryDemocratic Republic of the Congo                                             0.75320
    ## infant.deaths:CountryDenmark                                                                           NA
    ## infant.deaths:CountryDjibouti                                                                     0.27357
    ## infant.deaths:CountryDominican Republic                                                           0.34453
    ## infant.deaths:CountryEcuador                                                                      0.94066
    ## infant.deaths:CountryEgypt                                                                        0.45273
    ## infant.deaths:CountryEl Salvador                                                                  0.52961
    ## infant.deaths:CountryEquatorial Guinea                                                                 NA
    ## infant.deaths:CountryEritrea                                                                      0.00792
    ## infant.deaths:CountryEstonia                                                                           NA
    ## infant.deaths:CountryEthiopia                                                                     0.19781
    ## infant.deaths:CountryFiji                                                                              NA
    ## infant.deaths:CountryFinland                                                                           NA
    ## infant.deaths:CountryFrance                                                                       0.17849
    ## infant.deaths:CountryGabon                                                                             NA
    ## infant.deaths:CountryGambia                                                                            NA
    ## infant.deaths:CountryGeorgia                                                                      0.39055
    ## infant.deaths:CountryGermany                                                                      0.02596
    ## infant.deaths:CountryGhana                                                                        0.00052
    ## infant.deaths:CountryGreece                                                                       0.32089
    ## infant.deaths:CountryGrenada                                                                           NA
    ## infant.deaths:CountryGuatemala                                                                0.000000031
    ## infant.deaths:CountryGuinea                                                                       0.53064
    ## infant.deaths:CountryGuinea-Bissau                                                                0.02654
    ## infant.deaths:CountryGuyana                                                                       0.87000
    ## infant.deaths:CountryHaiti                                                                        0.00711
    ## infant.deaths:CountryHonduras                                                                     0.84816
    ## infant.deaths:CountryHungary                                                                      0.52507
    ## infant.deaths:CountryIceland                                                                           NA
    ## infant.deaths:CountryIndia                                                                        0.28105
    ## infant.deaths:CountryIndonesia                                                                    0.28263
    ## infant.deaths:CountryIran                                                                         0.47705
    ## infant.deaths:CountryIraq                                                                         0.00593
    ## infant.deaths:CountryIreland                                                                           NA
    ## infant.deaths:CountryIsrael                                                                       0.30444
    ## infant.deaths:CountryItaly                                                                        0.41420
    ## infant.deaths:CountryIvory Coast                                                                  0.57000
    ## infant.deaths:CountryJamaica                                                                           NA
    ## infant.deaths:CountryJapan                                                                        0.67686
    ## infant.deaths:CountryJordan                                                                            NA
    ## infant.deaths:CountryKazakhstan                                                                   0.45342
    ## infant.deaths:CountryKenya                                                                        0.18873
    ## infant.deaths:CountryKiribati                                                                          NA
    ## infant.deaths:CountryKuwait                                                                       0.83036
    ## infant.deaths:CountryKyrgyzstan                                                                   0.17893
    ## infant.deaths:CountryLao People's Democratic Republic                                             0.38656
    ## infant.deaths:CountryLatvia                                                                            NA
    ## infant.deaths:CountryLebanon                                                                           NA
    ## infant.deaths:CountryLesotho                                                                      0.47615
    ## infant.deaths:CountryLiberia                                                                      0.57757
    ## infant.deaths:CountryLibya                                                                        0.00906
    ## infant.deaths:CountryLithuania                                                                         NA
    ## infant.deaths:CountryLuxembourg                                                                        NA
    ## infant.deaths:CountryMadagascar                                                                   0.15528
    ## infant.deaths:CountryMalawi                                                                       0.16928
    ## infant.deaths:CountryMalaysia                                                                     0.89775
    ## infant.deaths:CountryMaldives                                                                          NA
    ## infant.deaths:CountryMali                                                                         0.55429
    ## infant.deaths:CountryMalta                                                                             NA
    ## infant.deaths:CountryMauritania                                                                   0.27143
    ## infant.deaths:CountryMauritius                                                                         NA
    ## infant.deaths:CountryMexico                                                                       0.84806
    ## infant.deaths:CountryMicronesia (Federated States of)                                                  NA
    ## infant.deaths:CountryMongolia                                                                     0.24320
    ## infant.deaths:CountryMontenegro                                                                        NA
    ## infant.deaths:CountryMorocco                                                                      0.45269
    ## infant.deaths:CountryMozambique                                                                   0.14875
    ## infant.deaths:CountryMyanmar                                                                      0.21737
    ## infant.deaths:CountryNamibia                                                                      0.76514
    ## infant.deaths:CountryNepal                                                                        0.23278
    ## infant.deaths:CountryNetherlands                                                                       NA
    ## infant.deaths:CountryNew Zealand                                                                       NA
    ## infant.deaths:CountryNicaragua                                                                    0.11391
    ## infant.deaths:CountryNiger                                                                        0.00032
    ## infant.deaths:CountryNigeria                                                                      0.07139
    ## infant.deaths:CountryNorway                                                                            NA
    ## infant.deaths:CountryOman                                                                              NA
    ## infant.deaths:CountryPakistan                                                                     0.37102
    ## infant.deaths:CountryPanama                                                                       0.75437
    ## infant.deaths:CountryPapua New Guinea                                                             0.83541
    ## infant.deaths:CountryParaguay                                                                     0.11522
    ## infant.deaths:CountryPeru                                                                         0.97458
    ## infant.deaths:CountryPhilippines                                                                  0.69714
    ## infant.deaths:CountryPoland                                                                       0.91306
    ## infant.deaths:CountryPortugal                                                                     0.91202
    ## infant.deaths:CountryQatar                                                                             NA
    ## infant.deaths:CountryRepublic of Korea                                                            0.26518
    ## infant.deaths:CountryRepublic of Moldova                                                               NA
    ## infant.deaths:CountryRomania                                                                      0.00087
    ## infant.deaths:CountryRussian Federation                                                           0.32502
    ## infant.deaths:CountryRwanda                                                                       0.08600
    ## infant.deaths:CountrySaint Lucia                                                                       NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                                  NA
    ## infant.deaths:CountrySamoa                                                                             NA
    ## infant.deaths:CountrySao Tome and Principe                                                             NA
    ## infant.deaths:CountrySaudi Arabia                                                                 0.98608
    ## infant.deaths:CountrySenegal                                                                      0.67069
    ## infant.deaths:CountrySerbia                                                                            NA
    ## infant.deaths:CountrySeychelles                                                                        NA
    ## infant.deaths:CountrySierra Leone                                                                 0.07661
    ## infant.deaths:CountrySingapore                                                                         NA
    ## infant.deaths:CountrySlovakia                                                                          NA
    ## infant.deaths:CountrySlovenia                                                                          NA
    ## infant.deaths:CountrySolomon Islands                                                                   NA
    ## infant.deaths:CountrySomalia                                                                      0.12753
    ## infant.deaths:CountrySouth Africa                                                                 0.68492
    ## infant.deaths:CountrySouth Sudan                                                                  0.44457
    ## infant.deaths:CountrySpain                                                                        0.58680
    ## infant.deaths:CountrySri Lanka                                                                    0.57737
    ## infant.deaths:CountrySudan                                                                        0.09044
    ## infant.deaths:CountrySuriname                                                                          NA
    ## infant.deaths:CountrySwaziland                                                                    0.35173
    ## infant.deaths:CountrySweden                                                                            NA
    ## infant.deaths:CountrySwitzerland                                                                       NA
    ## infant.deaths:CountrySyrian Arab Republic                                                         0.53363
    ## infant.deaths:CountryTajikistan                                                                   0.57746
    ## infant.deaths:CountryThailand                                                                     0.41375
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                         NA
    ## infant.deaths:CountryTimor-Leste                                                                  0.93322
    ## infant.deaths:CountryTogo                                                                         0.84878
    ## infant.deaths:CountryTonga                                                                             NA
    ## infant.deaths:CountryTrinidad and Tobago                                                               NA
    ## infant.deaths:CountryTunisia                                                                      0.77444
    ## infant.deaths:CountryTurkey                                                                       0.83080
    ## infant.deaths:CountryTurkmenistan                                                                 0.77325
    ## infant.deaths:CountryUganda                                                                       0.25598
    ## infant.deaths:CountryUkraine                                                                      0.01038
    ## infant.deaths:CountryUnited Arab Emirates                                                              NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                         0.57492
    ## infant.deaths:CountryUnited Republic of Tanzania                                                  0.14059
    ## infant.deaths:CountryUnited States of America                                                     0.52549
    ## infant.deaths:CountryUruguay                                                                      0.98038
    ## infant.deaths:CountryUzbekistan                                                                   0.46606
    ## infant.deaths:CountryVanuatu                                                                           NA
    ## infant.deaths:CountryVenezuela                                                                    0.96433
    ## infant.deaths:CountryViet Nam                                                                     0.89467
    ## infant.deaths:CountryYemen                                                                        0.30770
    ## infant.deaths:CountryZambia                                                                       0.42801
    ## infant.deaths:CountryZimbabwe                                                                     0.64837
    ## log.Adult.Mortality:CountryAlbania                                                                0.30079
    ## log.Adult.Mortality:CountryAlgeria                                                                0.55898
    ## log.Adult.Mortality:CountryAngola                                                                 0.68142
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                    0.30617
    ## log.Adult.Mortality:CountryArgentina                                                              0.42864
    ## log.Adult.Mortality:CountryArmenia                                                                0.30556
    ## log.Adult.Mortality:CountryAustralia                                                              0.28663
    ## log.Adult.Mortality:CountryAustria                                                                0.35273
    ## log.Adult.Mortality:CountryAzerbaijan                                                             0.27712
    ## log.Adult.Mortality:CountryBahamas                                                                0.29815
    ## log.Adult.Mortality:CountryBahrain                                                                0.26945
    ## log.Adult.Mortality:CountryBangladesh                                                             0.25876
    ## log.Adult.Mortality:CountryBarbados                                                               0.28191
    ## log.Adult.Mortality:CountryBelarus                                                                0.25829
    ## log.Adult.Mortality:CountryBelgium                                                                0.43032
    ## log.Adult.Mortality:CountryBelize                                                                 0.33778
    ## log.Adult.Mortality:CountryBenin                                                                  0.39793
    ## log.Adult.Mortality:CountryBhutan                                                                 0.39259
    ## log.Adult.Mortality:CountryBolivia                                                                0.76690
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                 0.29556
    ## log.Adult.Mortality:CountryBotswana                                                               0.36764
    ## log.Adult.Mortality:CountryBrazil                                                                 0.63655
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                      0.31872
    ## log.Adult.Mortality:CountryBulgaria                                                               0.34108
    ## log.Adult.Mortality:CountryBurkina Faso                                                           0.10548
    ## log.Adult.Mortality:CountryBurundi                                                                0.58151
    ## log.Adult.Mortality:CountryCabo Verde                                                             0.25947
    ## log.Adult.Mortality:CountryCambodia                                                               0.26184
    ## log.Adult.Mortality:CountryCameroon                                                               0.70693
    ## log.Adult.Mortality:CountryCanada                                                                 0.29129
    ## log.Adult.Mortality:CountryCentral African Republic                                               0.35519
    ## log.Adult.Mortality:CountryChad                                                                   0.83714
    ## log.Adult.Mortality:CountryChile                                                                  0.44399
    ## log.Adult.Mortality:CountryChina                                                                  0.27023
    ## log.Adult.Mortality:CountryColombia                                                               0.44303
    ## log.Adult.Mortality:CountryComoros                                                                0.43367
    ## log.Adult.Mortality:CountryCongo                                                                  0.55296
    ## log.Adult.Mortality:CountryCosta Rica                                                             0.31437
    ## log.Adult.Mortality:CountryCroatia                                                                0.30210
    ## log.Adult.Mortality:CountryCuba                                                                   0.29113
    ## log.Adult.Mortality:CountryCyprus                                                                 0.31283
    ## log.Adult.Mortality:CountryCzechia                                                                0.30132
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                  0.40012
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                       0.99718
    ## log.Adult.Mortality:CountryDenmark                                                                0.34234
    ## log.Adult.Mortality:CountryDjibouti                                                               0.13932
    ## log.Adult.Mortality:CountryDominican Republic                                                     0.21114
    ## log.Adult.Mortality:CountryEcuador                                                                0.38307
    ## log.Adult.Mortality:CountryEgypt                                                                  0.36120
    ## log.Adult.Mortality:CountryEl Salvador                                                            0.24483
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                      0.40311
    ## log.Adult.Mortality:CountryEritrea                                                                0.07119
    ## log.Adult.Mortality:CountryEstonia                                                                0.27315
    ## log.Adult.Mortality:CountryEthiopia                                                               0.21362
    ## log.Adult.Mortality:CountryFiji                                                                   0.31253
    ## log.Adult.Mortality:CountryFinland                                                                0.42557
    ## log.Adult.Mortality:CountryFrance                                                                 0.29563
    ## log.Adult.Mortality:CountryGabon                                                                  0.33426
    ## log.Adult.Mortality:CountryGambia                                                                 0.68321
    ## log.Adult.Mortality:CountryGeorgia                                                                0.19873
    ## log.Adult.Mortality:CountryGermany                                                                0.80500
    ## log.Adult.Mortality:CountryGhana                                                                  0.00303
    ## log.Adult.Mortality:CountryGreece                                                                 0.48530
    ## log.Adult.Mortality:CountryGrenada                                                                0.23220
    ## log.Adult.Mortality:CountryGuatemala                                                              0.00607
    ## log.Adult.Mortality:CountryGuinea                                                                 0.32232
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                          0.20464
    ## log.Adult.Mortality:CountryGuyana                                                                 0.28083
    ## log.Adult.Mortality:CountryHaiti                                                                  0.10501
    ## log.Adult.Mortality:CountryHonduras                                                               0.39405
    ## log.Adult.Mortality:CountryHungary                                                                0.29272
    ## log.Adult.Mortality:CountryIceland                                                                0.26305
    ## log.Adult.Mortality:CountryIndia                                                                  0.53427
    ## log.Adult.Mortality:CountryIndonesia                                                              0.37179
    ## log.Adult.Mortality:CountryIran                                                                   0.30328
    ## log.Adult.Mortality:CountryIraq                                                                   0.01664
    ## log.Adult.Mortality:CountryIreland                                                                0.33462
    ## log.Adult.Mortality:CountryIsrael                                                                 0.29074
    ## log.Adult.Mortality:CountryItaly                                                                  0.23859
    ## log.Adult.Mortality:CountryIvory Coast                                                            0.65816
    ## log.Adult.Mortality:CountryJamaica                                                                0.30455
    ## log.Adult.Mortality:CountryJapan                                                                  0.26652
    ## log.Adult.Mortality:CountryJordan                                                                 0.31911
    ## log.Adult.Mortality:CountryKazakhstan                                                             0.29929
    ## log.Adult.Mortality:CountryKenya                                                                  0.23443
    ## log.Adult.Mortality:CountryKiribati                                                               0.35808
    ## log.Adult.Mortality:CountryKuwait                                                                 0.30484
    ## log.Adult.Mortality:CountryKyrgyzstan                                                             0.11142
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                       0.31737
    ## log.Adult.Mortality:CountryLatvia                                                                 0.26939
    ## log.Adult.Mortality:CountryLebanon                                                                0.30077
    ## log.Adult.Mortality:CountryLesotho                                                                0.64323
    ## log.Adult.Mortality:CountryLiberia                                                                0.34885
    ## log.Adult.Mortality:CountryLibya                                                                  0.14238
    ## log.Adult.Mortality:CountryLithuania                                                              0.24130
    ## log.Adult.Mortality:CountryLuxembourg                                                             0.30107
    ## log.Adult.Mortality:CountryMadagascar                                                             0.07931
    ## log.Adult.Mortality:CountryMalawi                                                                 0.27089
    ## log.Adult.Mortality:CountryMalaysia                                                               0.41267
    ## log.Adult.Mortality:CountryMaldives                                                               0.27491
    ## log.Adult.Mortality:CountryMali                                                                   0.65069
    ## log.Adult.Mortality:CountryMalta                                                                  0.35261
    ## log.Adult.Mortality:CountryMauritania                                                             0.29809
    ## log.Adult.Mortality:CountryMauritius                                                              0.35161
    ## log.Adult.Mortality:CountryMexico                                                                 0.55848
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                       0.34799
    ## log.Adult.Mortality:CountryMongolia                                                               0.26089
    ## log.Adult.Mortality:CountryMontenegro                                                             0.29951
    ## log.Adult.Mortality:CountryMorocco                                                                0.25810
    ## log.Adult.Mortality:CountryMozambique                                                             0.52812
    ## log.Adult.Mortality:CountryMyanmar                                                                0.26373
    ## log.Adult.Mortality:CountryNamibia                                                                0.49475
    ## log.Adult.Mortality:CountryNepal                                                                  0.27638
    ## log.Adult.Mortality:CountryNetherlands                                                            0.32677
    ## log.Adult.Mortality:CountryNew Zealand                                                            0.36187
    ## log.Adult.Mortality:CountryNicaragua                                                              0.54287
    ## log.Adult.Mortality:CountryNiger                                                                  0.00046
    ## log.Adult.Mortality:CountryNigeria                                                                0.04223
    ## log.Adult.Mortality:CountryNorway                                                                 0.30530
    ## log.Adult.Mortality:CountryOman                                                                   0.30807
    ## log.Adult.Mortality:CountryPakistan                                                               0.63431
    ## log.Adult.Mortality:CountryPanama                                                                 0.62680
    ## log.Adult.Mortality:CountryPapua New Guinea                                                       0.73481
    ## log.Adult.Mortality:CountryParaguay                                                               0.73439
    ## log.Adult.Mortality:CountryPeru                                                                   0.25875
    ## log.Adult.Mortality:CountryPhilippines                                                            0.56434
    ## log.Adult.Mortality:CountryPoland                                                                 0.34482
    ## log.Adult.Mortality:CountryPortugal                                                               0.40516
    ## log.Adult.Mortality:CountryQatar                                                                  0.30835
    ## log.Adult.Mortality:CountryRepublic of Korea                                                      0.32561
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                    0.39476
    ## log.Adult.Mortality:CountryRomania                                                                0.79755
    ## log.Adult.Mortality:CountryRussian Federation                                                     0.64099
    ## log.Adult.Mortality:CountryRwanda                                                                 0.30292
    ## log.Adult.Mortality:CountrySaint Lucia                                                            0.31634
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                       0.14601
    ## log.Adult.Mortality:CountrySamoa                                                                  0.23513
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                  0.38259
    ## log.Adult.Mortality:CountrySaudi Arabia                                                           0.42496
    ## log.Adult.Mortality:CountrySenegal                                                                0.35378
    ## log.Adult.Mortality:CountrySerbia                                                                 0.31110
    ## log.Adult.Mortality:CountrySeychelles                                                             0.30266
    ## log.Adult.Mortality:CountrySierra Leone                                                           0.52545
    ## log.Adult.Mortality:CountrySingapore                                                              0.30491
    ## log.Adult.Mortality:CountrySlovakia                                                               0.29924
    ## log.Adult.Mortality:CountrySlovenia                                                               0.34597
    ## log.Adult.Mortality:CountrySolomon Islands                                                        0.28575
    ## log.Adult.Mortality:CountrySomalia                                                                0.11043
    ## log.Adult.Mortality:CountrySouth Africa                                                           0.79455
    ## log.Adult.Mortality:CountrySouth Sudan                                                            0.59138
    ## log.Adult.Mortality:CountrySpain                                                                  0.28379
    ## log.Adult.Mortality:CountrySri Lanka                                                              0.26660
    ## log.Adult.Mortality:CountrySudan                                                                  0.09397
    ## log.Adult.Mortality:CountrySuriname                                                               0.38543
    ## log.Adult.Mortality:CountrySwaziland                                                              0.29817
    ## log.Adult.Mortality:CountrySweden                                                                 0.27010
    ## log.Adult.Mortality:CountrySwitzerland                                                            0.26689
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                   0.29671
    ## log.Adult.Mortality:CountryTajikistan                                                             0.82123
    ## log.Adult.Mortality:CountryThailand                                                               0.29736
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                              0.29779
    ## log.Adult.Mortality:CountryTimor-Leste                                                            0.37048
    ## log.Adult.Mortality:CountryTogo                                                                   0.74453
    ## log.Adult.Mortality:CountryTonga                                                                  0.30831
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                    0.41837
    ## log.Adult.Mortality:CountryTunisia                                                                0.42780
    ## log.Adult.Mortality:CountryTurkey                                                                 0.70556
    ## log.Adult.Mortality:CountryTurkmenistan                                                           0.37360
    ## log.Adult.Mortality:CountryUganda                                                                 0.47624
    ## log.Adult.Mortality:CountryUkraine                                                                0.09871
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                   0.28865
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                   0.28537
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                            0.21540
    ## log.Adult.Mortality:CountryUnited States of America                                               0.39387
    ## log.Adult.Mortality:CountryUruguay                                                                0.33826
    ## log.Adult.Mortality:CountryUzbekistan                                                             0.27124
    ## log.Adult.Mortality:CountryVanuatu                                                                0.30958
    ## log.Adult.Mortality:CountryVenezuela                                                              0.90341
    ## log.Adult.Mortality:CountryViet Nam                                                               0.79673
    ## log.Adult.Mortality:CountryYemen                                                                  0.38789
    ## log.Adult.Mortality:CountryZambia                                                                 0.24680
    ## log.Adult.Mortality:CountryZimbabwe                                                               0.61302
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                              0.29521
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                  0.65660
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                  0.98276
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                   0.58705
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                                0.91135
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                               0.71713
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                               0.95465
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                  0.09974
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                  0.21632
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                   0.97236
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                  0.71528
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                   0.10719
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                             0.20979
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                  0.22110
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                 0.61253
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                 0.92840
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                     0.86489
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                    0.83933
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                 0.87316
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                  0.64125
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                    0.46680
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                    0.40059
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                         0.84829
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                                 0.27179
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                       0.35570
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                  0.95166
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                    0.34199
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                              0.55455
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                  0.01175
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                                 0.07218
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                  0.42038
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                  0.02195
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                    0.00134
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                            0.000000027
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                   0.77942
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                            0.02618
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                   0.89650
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                    0.01277
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                 0.96402
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                  0.57864
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                    0.89528
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                                0.66195
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                     0.88995
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                     0.01343
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                    0.50462
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                              0.46343
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                    0.77001
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                               0.67720
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                    0.34908
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                   0.86407
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                               0.18727
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                         0.57504
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                  0.43145
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                  0.84211
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                    0.00857
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                               0.23100
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                   0.26254
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                 0.94752
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                     0.44462
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                               0.26847
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                   0.77132
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                              NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                                 0.26815
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                  0.89865
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                               0.00897
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                  0.53797
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                  0.77691
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                    0.56473
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                                0.07376
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                    0.00018
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                  0.02769
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                 0.76215
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                   0.75603
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                         0.86459
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                 0.09505
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                     0.79521
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                              0.94848
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                   0.94470
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                 0.89335
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                        0.46024
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                  0.00055
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                       0.14420
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                   0.02101
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                              NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                             0.86379
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                  0.82197
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                             0.02753
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                               NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                  0.14974
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                             0.08591
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                              0.40895
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                    0.65992
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                                0.67163
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                    0.17234
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                                0.39836
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                     0.90504
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                               0.48770
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                 0.72649
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                     NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                              0.98181
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                     0.75222
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                  0.79127
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                   0.70570
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                             0.82873
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                   0.75843
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                  0.01362
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland     0.59710
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                              0.01002
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                 0.59665
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                  0.99844
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                               0.98513
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                                0.97034
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                 0.94241
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                    0.23860
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                   0.64196
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                 0.07992
    ##                                                                                                  
    ## (Intercept)                                                                                      
    ## log.Adult.Mortality                                                                              
    ## infant.deaths                                                                                    
    ## log.GDP                                                                                          
    ## Measles                                                                                          
    ## HIV_cat                                                                                       .  
    ## CountryAlbania                                                                                   
    ## CountryAlgeria                                                                                   
    ## CountryAngola                                                                                    
    ## CountryAntigua and Barbuda                                                                       
    ## CountryArgentina                                                                                 
    ## CountryArmenia                                                                                   
    ## CountryAustralia                                                                                 
    ## CountryAustria                                                                                   
    ## CountryAzerbaijan                                                                                
    ## CountryBahamas                                                                                   
    ## CountryBahrain                                                                                   
    ## CountryBangladesh                                                                                
    ## CountryBarbados                                                                                  
    ## CountryBelarus                                                                                   
    ## CountryBelgium                                                                                   
    ## CountryBelize                                                                                    
    ## CountryBenin                                                                                     
    ## CountryBhutan                                                                                    
    ## CountryBolivia                                                                                   
    ## CountryBosnia and Herzegovina                                                                    
    ## CountryBotswana                                                                                  
    ## CountryBrazil                                                                                    
    ## CountryBrunei Darussalam                                                                         
    ## CountryBulgaria                                                                                  
    ## CountryBurkina Faso                                                                              
    ## CountryBurundi                                                                                   
    ## CountryCabo Verde                                                                                
    ## CountryCambodia                                                                                  
    ## CountryCameroon                                                                                  
    ## CountryCanada                                                                                    
    ## CountryCentral African Republic                                                                  
    ## CountryChad                                                                                      
    ## CountryChile                                                                                     
    ## CountryChina                                                                                     
    ## CountryColombia                                                                                  
    ## CountryComoros                                                                                   
    ## CountryCongo                                                                                     
    ## CountryCosta Rica                                                                                
    ## CountryCroatia                                                                                   
    ## CountryCuba                                                                                      
    ## CountryCyprus                                                                                    
    ## CountryCzechia                                                                                   
    ## CountryDemocratic People's Republic of Korea                                                     
    ## CountryDemocratic Republic of the Congo                                                          
    ## CountryDenmark                                                                                   
    ## CountryDjibouti                                                                                  
    ## CountryDominican Republic                                                                        
    ## CountryEcuador                                                                                   
    ## CountryEgypt                                                                                     
    ## CountryEl Salvador                                                                               
    ## CountryEquatorial Guinea                                                                         
    ## CountryEritrea                                                                                .  
    ## CountryEstonia                                                                                   
    ## CountryEthiopia                                                                                  
    ## CountryFiji                                                                                      
    ## CountryFinland                                                                                   
    ## CountryFrance                                                                                    
    ## CountryGabon                                                                                     
    ## CountryGambia                                                                                    
    ## CountryGeorgia                                                                                   
    ## CountryGermany                                                                                   
    ## CountryGhana                                                                                  ** 
    ## CountryGreece                                                                                    
    ## CountryGrenada                                                                                   
    ## CountryGuatemala                                                                              ** 
    ## CountryGuinea                                                                                    
    ## CountryGuinea-Bissau                                                                             
    ## CountryGuyana                                                                                    
    ## CountryHaiti                                                                                     
    ## CountryHonduras                                                                                  
    ## CountryHungary                                                                                   
    ## CountryIceland                                                                                   
    ## CountryIndia                                                                                     
    ## CountryIndonesia                                                                                 
    ## CountryIran                                                                                      
    ## CountryIraq                                                                                   *  
    ## CountryIreland                                                                                   
    ## CountryIsrael                                                                                    
    ## CountryItaly                                                                                     
    ## CountryIvory Coast                                                                               
    ## CountryJamaica                                                                                   
    ## CountryJapan                                                                                     
    ## CountryJordan                                                                                    
    ## CountryKazakhstan                                                                                
    ## CountryKenya                                                                                     
    ## CountryKiribati                                                                                  
    ## CountryKuwait                                                                                    
    ## CountryKyrgyzstan                                                                                
    ## CountryLao People's Democratic Republic                                                          
    ## CountryLatvia                                                                                    
    ## CountryLebanon                                                                                   
    ## CountryLesotho                                                                                   
    ## CountryLiberia                                                                                   
    ## CountryLibya                                                                                     
    ## CountryLithuania                                                                                 
    ## CountryLuxembourg                                                                                
    ## CountryMadagascar                                                                             .  
    ## CountryMalawi                                                                                    
    ## CountryMalaysia                                                                                  
    ## CountryMaldives                                                                                  
    ## CountryMali                                                                                      
    ## CountryMalta                                                                                     
    ## CountryMauritania                                                                                
    ## CountryMauritius                                                                                 
    ## CountryMexico                                                                                    
    ## CountryMicronesia (Federated States of)                                                          
    ## CountryMongolia                                                                                  
    ## CountryMontenegro                                                                                
    ## CountryMorocco                                                                                   
    ## CountryMozambique                                                                                
    ## CountryMyanmar                                                                                   
    ## CountryNamibia                                                                                   
    ## CountryNepal                                                                                     
    ## CountryNetherlands                                                                               
    ## CountryNew Zealand                                                                               
    ## CountryNicaragua                                                                                 
    ## CountryNiger                                                                                  ***
    ## CountryNigeria                                                                                *  
    ## CountryNorway                                                                                    
    ## CountryOman                                                                                      
    ## CountryPakistan                                                                                  
    ## CountryPanama                                                                                    
    ## CountryPapua New Guinea                                                                          
    ## CountryParaguay                                                                                  
    ## CountryPeru                                                                                      
    ## CountryPhilippines                                                                               
    ## CountryPoland                                                                                    
    ## CountryPortugal                                                                                  
    ## CountryQatar                                                                                     
    ## CountryRepublic of Korea                                                                         
    ## CountryRepublic of Moldova                                                                       
    ## CountryRomania                                                                                   
    ## CountryRussian Federation                                                                        
    ## CountryRwanda                                                                                    
    ## CountrySaint Lucia                                                                               
    ## CountrySaint Vincent and the Grenadines                                                          
    ## CountrySamoa                                                                                     
    ## CountrySao Tome and Principe                                                                     
    ## CountrySaudi Arabia                                                                              
    ## CountrySenegal                                                                                   
    ## CountrySerbia                                                                                    
    ## CountrySeychelles                                                                                
    ## CountrySierra Leone                                                                              
    ## CountrySingapore                                                                                 
    ## CountrySlovakia                                                                                  
    ## CountrySlovenia                                                                                  
    ## CountrySolomon Islands                                                                           
    ## CountrySomalia                                                                                   
    ## CountrySouth Africa                                                                              
    ## CountrySouth Sudan                                                                               
    ## CountrySpain                                                                                     
    ## CountrySri Lanka                                                                                 
    ## CountrySudan                                                                                  .  
    ## CountrySuriname                                                                                  
    ## CountrySwaziland                                                                                 
    ## CountrySweden                                                                                    
    ## CountrySwitzerland                                                                               
    ## CountrySyrian Arab Republic                                                                      
    ## CountryTajikistan                                                                                
    ## CountryThailand                                                                                  
    ## CountryThe former Yugoslav republic of Macedonia                                                 
    ## CountryTimor-Leste                                                                               
    ## CountryTogo                                                                                      
    ## CountryTonga                                                                                     
    ## CountryTrinidad and Tobago                                                                       
    ## CountryTunisia                                                                                   
    ## CountryTurkey                                                                                    
    ## CountryTurkmenistan                                                                              
    ## CountryUganda                                                                                    
    ## CountryUkraine                                                                                .  
    ## CountryUnited Arab Emirates                                                                      
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                      
    ## CountryUnited Republic of Tanzania                                                               
    ## CountryUnited States of America                                                                  
    ## CountryUruguay                                                                                   
    ## CountryUzbekistan                                                                                
    ## CountryVanuatu                                                                                   
    ## CountryVenezuela                                                                                 
    ## CountryViet Nam                                                                                  
    ## CountryYemen                                                                                     
    ## CountryZambia                                                                                    
    ## CountryZimbabwe                                                                                  
    ## infant.deaths:CountryAlbania                                                                     
    ## infant.deaths:CountryAlgeria                                                                     
    ## infant.deaths:CountryAngola                                                                      
    ## infant.deaths:CountryAntigua and Barbuda                                                         
    ## infant.deaths:CountryArgentina                                                                   
    ## infant.deaths:CountryArmenia                                                                     
    ## infant.deaths:CountryAustralia                                                                   
    ## infant.deaths:CountryAustria                                                                     
    ## infant.deaths:CountryAzerbaijan                                                                  
    ## infant.deaths:CountryBahamas                                                                     
    ## infant.deaths:CountryBahrain                                                                     
    ## infant.deaths:CountryBangladesh                                                                  
    ## infant.deaths:CountryBarbados                                                                    
    ## infant.deaths:CountryBelarus                                                                  .  
    ## infant.deaths:CountryBelgium                                                                     
    ## infant.deaths:CountryBelize                                                                      
    ## infant.deaths:CountryBenin                                                                       
    ## infant.deaths:CountryBhutan                                                                      
    ## infant.deaths:CountryBolivia                                                                     
    ## infant.deaths:CountryBosnia and Herzegovina                                                      
    ## infant.deaths:CountryBotswana                                                                    
    ## infant.deaths:CountryBrazil                                                                      
    ## infant.deaths:CountryBrunei Darussalam                                                           
    ## infant.deaths:CountryBulgaria                                                                    
    ## infant.deaths:CountryBurkina Faso                                                                
    ## infant.deaths:CountryBurundi                                                                     
    ## infant.deaths:CountryCabo Verde                                                                  
    ## infant.deaths:CountryCambodia                                                                    
    ## infant.deaths:CountryCameroon                                                                    
    ## infant.deaths:CountryCanada                                                                      
    ## infant.deaths:CountryCentral African Republic                                                    
    ## infant.deaths:CountryChad                                                                        
    ## infant.deaths:CountryChile                                                                       
    ## infant.deaths:CountryChina                                                                       
    ## infant.deaths:CountryColombia                                                                    
    ## infant.deaths:CountryComoros                                                                     
    ## infant.deaths:CountryCongo                                                                       
    ## infant.deaths:CountryCosta Rica                                                                  
    ## infant.deaths:CountryCroatia                                                                     
    ## infant.deaths:CountryCuba                                                                        
    ## infant.deaths:CountryCyprus                                                                      
    ## infant.deaths:CountryCzechia                                                                     
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                       
    ## infant.deaths:CountryDemocratic Republic of the Congo                                            
    ## infant.deaths:CountryDenmark                                                                     
    ## infant.deaths:CountryDjibouti                                                                    
    ## infant.deaths:CountryDominican Republic                                                          
    ## infant.deaths:CountryEcuador                                                                     
    ## infant.deaths:CountryEgypt                                                                       
    ## infant.deaths:CountryEl Salvador                                                                 
    ## infant.deaths:CountryEquatorial Guinea                                                           
    ## infant.deaths:CountryEritrea                                                                  ** 
    ## infant.deaths:CountryEstonia                                                                     
    ## infant.deaths:CountryEthiopia                                                                    
    ## infant.deaths:CountryFiji                                                                        
    ## infant.deaths:CountryFinland                                                                     
    ## infant.deaths:CountryFrance                                                                      
    ## infant.deaths:CountryGabon                                                                       
    ## infant.deaths:CountryGambia                                                                      
    ## infant.deaths:CountryGeorgia                                                                     
    ## infant.deaths:CountryGermany                                                                  *  
    ## infant.deaths:CountryGhana                                                                    ***
    ## infant.deaths:CountryGreece                                                                      
    ## infant.deaths:CountryGrenada                                                                     
    ## infant.deaths:CountryGuatemala                                                                ***
    ## infant.deaths:CountryGuinea                                                                      
    ## infant.deaths:CountryGuinea-Bissau                                                            *  
    ## infant.deaths:CountryGuyana                                                                      
    ## infant.deaths:CountryHaiti                                                                    ** 
    ## infant.deaths:CountryHonduras                                                                    
    ## infant.deaths:CountryHungary                                                                     
    ## infant.deaths:CountryIceland                                                                     
    ## infant.deaths:CountryIndia                                                                       
    ## infant.deaths:CountryIndonesia                                                                   
    ## infant.deaths:CountryIran                                                                        
    ## infant.deaths:CountryIraq                                                                     ** 
    ## infant.deaths:CountryIreland                                                                     
    ## infant.deaths:CountryIsrael                                                                      
    ## infant.deaths:CountryItaly                                                                       
    ## infant.deaths:CountryIvory Coast                                                                 
    ## infant.deaths:CountryJamaica                                                                     
    ## infant.deaths:CountryJapan                                                                       
    ## infant.deaths:CountryJordan                                                                      
    ## infant.deaths:CountryKazakhstan                                                                  
    ## infant.deaths:CountryKenya                                                                       
    ## infant.deaths:CountryKiribati                                                                    
    ## infant.deaths:CountryKuwait                                                                      
    ## infant.deaths:CountryKyrgyzstan                                                                  
    ## infant.deaths:CountryLao People's Democratic Republic                                            
    ## infant.deaths:CountryLatvia                                                                      
    ## infant.deaths:CountryLebanon                                                                     
    ## infant.deaths:CountryLesotho                                                                     
    ## infant.deaths:CountryLiberia                                                                     
    ## infant.deaths:CountryLibya                                                                    ** 
    ## infant.deaths:CountryLithuania                                                                   
    ## infant.deaths:CountryLuxembourg                                                                  
    ## infant.deaths:CountryMadagascar                                                                  
    ## infant.deaths:CountryMalawi                                                                      
    ## infant.deaths:CountryMalaysia                                                                    
    ## infant.deaths:CountryMaldives                                                                    
    ## infant.deaths:CountryMali                                                                        
    ## infant.deaths:CountryMalta                                                                       
    ## infant.deaths:CountryMauritania                                                                  
    ## infant.deaths:CountryMauritius                                                                   
    ## infant.deaths:CountryMexico                                                                      
    ## infant.deaths:CountryMicronesia (Federated States of)                                            
    ## infant.deaths:CountryMongolia                                                                    
    ## infant.deaths:CountryMontenegro                                                                  
    ## infant.deaths:CountryMorocco                                                                     
    ## infant.deaths:CountryMozambique                                                                  
    ## infant.deaths:CountryMyanmar                                                                     
    ## infant.deaths:CountryNamibia                                                                     
    ## infant.deaths:CountryNepal                                                                       
    ## infant.deaths:CountryNetherlands                                                                 
    ## infant.deaths:CountryNew Zealand                                                                 
    ## infant.deaths:CountryNicaragua                                                                   
    ## infant.deaths:CountryNiger                                                                    ***
    ## infant.deaths:CountryNigeria                                                                  .  
    ## infant.deaths:CountryNorway                                                                      
    ## infant.deaths:CountryOman                                                                        
    ## infant.deaths:CountryPakistan                                                                    
    ## infant.deaths:CountryPanama                                                                      
    ## infant.deaths:CountryPapua New Guinea                                                            
    ## infant.deaths:CountryParaguay                                                                    
    ## infant.deaths:CountryPeru                                                                        
    ## infant.deaths:CountryPhilippines                                                                 
    ## infant.deaths:CountryPoland                                                                      
    ## infant.deaths:CountryPortugal                                                                    
    ## infant.deaths:CountryQatar                                                                       
    ## infant.deaths:CountryRepublic of Korea                                                           
    ## infant.deaths:CountryRepublic of Moldova                                                         
    ## infant.deaths:CountryRomania                                                                  ***
    ## infant.deaths:CountryRussian Federation                                                          
    ## infant.deaths:CountryRwanda                                                                   .  
    ## infant.deaths:CountrySaint Lucia                                                                 
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                            
    ## infant.deaths:CountrySamoa                                                                       
    ## infant.deaths:CountrySao Tome and Principe                                                       
    ## infant.deaths:CountrySaudi Arabia                                                                
    ## infant.deaths:CountrySenegal                                                                     
    ## infant.deaths:CountrySerbia                                                                      
    ## infant.deaths:CountrySeychelles                                                                  
    ## infant.deaths:CountrySierra Leone                                                             .  
    ## infant.deaths:CountrySingapore                                                                   
    ## infant.deaths:CountrySlovakia                                                                    
    ## infant.deaths:CountrySlovenia                                                                    
    ## infant.deaths:CountrySolomon Islands                                                             
    ## infant.deaths:CountrySomalia                                                                     
    ## infant.deaths:CountrySouth Africa                                                                
    ## infant.deaths:CountrySouth Sudan                                                                 
    ## infant.deaths:CountrySpain                                                                       
    ## infant.deaths:CountrySri Lanka                                                                   
    ## infant.deaths:CountrySudan                                                                    .  
    ## infant.deaths:CountrySuriname                                                                    
    ## infant.deaths:CountrySwaziland                                                                   
    ## infant.deaths:CountrySweden                                                                      
    ## infant.deaths:CountrySwitzerland                                                                 
    ## infant.deaths:CountrySyrian Arab Republic                                                        
    ## infant.deaths:CountryTajikistan                                                                  
    ## infant.deaths:CountryThailand                                                                    
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                   
    ## infant.deaths:CountryTimor-Leste                                                                 
    ## infant.deaths:CountryTogo                                                                        
    ## infant.deaths:CountryTonga                                                                       
    ## infant.deaths:CountryTrinidad and Tobago                                                         
    ## infant.deaths:CountryTunisia                                                                     
    ## infant.deaths:CountryTurkey                                                                      
    ## infant.deaths:CountryTurkmenistan                                                                
    ## infant.deaths:CountryUganda                                                                      
    ## infant.deaths:CountryUkraine                                                                  *  
    ## infant.deaths:CountryUnited Arab Emirates                                                        
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        
    ## infant.deaths:CountryUnited Republic of Tanzania                                                 
    ## infant.deaths:CountryUnited States of America                                                    
    ## infant.deaths:CountryUruguay                                                                     
    ## infant.deaths:CountryUzbekistan                                                                  
    ## infant.deaths:CountryVanuatu                                                                     
    ## infant.deaths:CountryVenezuela                                                                   
    ## infant.deaths:CountryViet Nam                                                                    
    ## infant.deaths:CountryYemen                                                                       
    ## infant.deaths:CountryZambia                                                                      
    ## infant.deaths:CountryZimbabwe                                                                    
    ## log.Adult.Mortality:CountryAlbania                                                               
    ## log.Adult.Mortality:CountryAlgeria                                                               
    ## log.Adult.Mortality:CountryAngola                                                                
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   
    ## log.Adult.Mortality:CountryArgentina                                                             
    ## log.Adult.Mortality:CountryArmenia                                                               
    ## log.Adult.Mortality:CountryAustralia                                                             
    ## log.Adult.Mortality:CountryAustria                                                               
    ## log.Adult.Mortality:CountryAzerbaijan                                                            
    ## log.Adult.Mortality:CountryBahamas                                                               
    ## log.Adult.Mortality:CountryBahrain                                                               
    ## log.Adult.Mortality:CountryBangladesh                                                            
    ## log.Adult.Mortality:CountryBarbados                                                              
    ## log.Adult.Mortality:CountryBelarus                                                               
    ## log.Adult.Mortality:CountryBelgium                                                               
    ## log.Adult.Mortality:CountryBelize                                                                
    ## log.Adult.Mortality:CountryBenin                                                                 
    ## log.Adult.Mortality:CountryBhutan                                                                
    ## log.Adult.Mortality:CountryBolivia                                                               
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                
    ## log.Adult.Mortality:CountryBotswana                                                              
    ## log.Adult.Mortality:CountryBrazil                                                                
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     
    ## log.Adult.Mortality:CountryBulgaria                                                              
    ## log.Adult.Mortality:CountryBurkina Faso                                                          
    ## log.Adult.Mortality:CountryBurundi                                                               
    ## log.Adult.Mortality:CountryCabo Verde                                                            
    ## log.Adult.Mortality:CountryCambodia                                                              
    ## log.Adult.Mortality:CountryCameroon                                                              
    ## log.Adult.Mortality:CountryCanada                                                                
    ## log.Adult.Mortality:CountryCentral African Republic                                              
    ## log.Adult.Mortality:CountryChad                                                                  
    ## log.Adult.Mortality:CountryChile                                                                 
    ## log.Adult.Mortality:CountryChina                                                                 
    ## log.Adult.Mortality:CountryColombia                                                              
    ## log.Adult.Mortality:CountryComoros                                                               
    ## log.Adult.Mortality:CountryCongo                                                                 
    ## log.Adult.Mortality:CountryCosta Rica                                                            
    ## log.Adult.Mortality:CountryCroatia                                                               
    ## log.Adult.Mortality:CountryCuba                                                                  
    ## log.Adult.Mortality:CountryCyprus                                                                
    ## log.Adult.Mortality:CountryCzechia                                                               
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                      
    ## log.Adult.Mortality:CountryDenmark                                                               
    ## log.Adult.Mortality:CountryDjibouti                                                              
    ## log.Adult.Mortality:CountryDominican Republic                                                    
    ## log.Adult.Mortality:CountryEcuador                                                               
    ## log.Adult.Mortality:CountryEgypt                                                                 
    ## log.Adult.Mortality:CountryEl Salvador                                                           
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     
    ## log.Adult.Mortality:CountryEritrea                                                            .  
    ## log.Adult.Mortality:CountryEstonia                                                               
    ## log.Adult.Mortality:CountryEthiopia                                                              
    ## log.Adult.Mortality:CountryFiji                                                                  
    ## log.Adult.Mortality:CountryFinland                                                               
    ## log.Adult.Mortality:CountryFrance                                                                
    ## log.Adult.Mortality:CountryGabon                                                                 
    ## log.Adult.Mortality:CountryGambia                                                                
    ## log.Adult.Mortality:CountryGeorgia                                                               
    ## log.Adult.Mortality:CountryGermany                                                               
    ## log.Adult.Mortality:CountryGhana                                                              ** 
    ## log.Adult.Mortality:CountryGreece                                                                
    ## log.Adult.Mortality:CountryGrenada                                                               
    ## log.Adult.Mortality:CountryGuatemala                                                          ** 
    ## log.Adult.Mortality:CountryGuinea                                                                
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                         
    ## log.Adult.Mortality:CountryGuyana                                                                
    ## log.Adult.Mortality:CountryHaiti                                                                 
    ## log.Adult.Mortality:CountryHonduras                                                              
    ## log.Adult.Mortality:CountryHungary                                                               
    ## log.Adult.Mortality:CountryIceland                                                               
    ## log.Adult.Mortality:CountryIndia                                                                 
    ## log.Adult.Mortality:CountryIndonesia                                                             
    ## log.Adult.Mortality:CountryIran                                                                  
    ## log.Adult.Mortality:CountryIraq                                                               *  
    ## log.Adult.Mortality:CountryIreland                                                               
    ## log.Adult.Mortality:CountryIsrael                                                                
    ## log.Adult.Mortality:CountryItaly                                                                 
    ## log.Adult.Mortality:CountryIvory Coast                                                           
    ## log.Adult.Mortality:CountryJamaica                                                               
    ## log.Adult.Mortality:CountryJapan                                                                 
    ## log.Adult.Mortality:CountryJordan                                                                
    ## log.Adult.Mortality:CountryKazakhstan                                                            
    ## log.Adult.Mortality:CountryKenya                                                                 
    ## log.Adult.Mortality:CountryKiribati                                                              
    ## log.Adult.Mortality:CountryKuwait                                                                
    ## log.Adult.Mortality:CountryKyrgyzstan                                                            
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      
    ## log.Adult.Mortality:CountryLatvia                                                                
    ## log.Adult.Mortality:CountryLebanon                                                               
    ## log.Adult.Mortality:CountryLesotho                                                               
    ## log.Adult.Mortality:CountryLiberia                                                               
    ## log.Adult.Mortality:CountryLibya                                                                 
    ## log.Adult.Mortality:CountryLithuania                                                             
    ## log.Adult.Mortality:CountryLuxembourg                                                            
    ## log.Adult.Mortality:CountryMadagascar                                                         .  
    ## log.Adult.Mortality:CountryMalawi                                                                
    ## log.Adult.Mortality:CountryMalaysia                                                              
    ## log.Adult.Mortality:CountryMaldives                                                              
    ## log.Adult.Mortality:CountryMali                                                                  
    ## log.Adult.Mortality:CountryMalta                                                                 
    ## log.Adult.Mortality:CountryMauritania                                                            
    ## log.Adult.Mortality:CountryMauritius                                                             
    ## log.Adult.Mortality:CountryMexico                                                                
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      
    ## log.Adult.Mortality:CountryMongolia                                                              
    ## log.Adult.Mortality:CountryMontenegro                                                            
    ## log.Adult.Mortality:CountryMorocco                                                               
    ## log.Adult.Mortality:CountryMozambique                                                            
    ## log.Adult.Mortality:CountryMyanmar                                                               
    ## log.Adult.Mortality:CountryNamibia                                                               
    ## log.Adult.Mortality:CountryNepal                                                                 
    ## log.Adult.Mortality:CountryNetherlands                                                           
    ## log.Adult.Mortality:CountryNew Zealand                                                           
    ## log.Adult.Mortality:CountryNicaragua                                                             
    ## log.Adult.Mortality:CountryNiger                                                              ***
    ## log.Adult.Mortality:CountryNigeria                                                            *  
    ## log.Adult.Mortality:CountryNorway                                                                
    ## log.Adult.Mortality:CountryOman                                                                  
    ## log.Adult.Mortality:CountryPakistan                                                              
    ## log.Adult.Mortality:CountryPanama                                                                
    ## log.Adult.Mortality:CountryPapua New Guinea                                                      
    ## log.Adult.Mortality:CountryParaguay                                                              
    ## log.Adult.Mortality:CountryPeru                                                                  
    ## log.Adult.Mortality:CountryPhilippines                                                           
    ## log.Adult.Mortality:CountryPoland                                                                
    ## log.Adult.Mortality:CountryPortugal                                                              
    ## log.Adult.Mortality:CountryQatar                                                                 
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   
    ## log.Adult.Mortality:CountryRomania                                                               
    ## log.Adult.Mortality:CountryRussian Federation                                                    
    ## log.Adult.Mortality:CountryRwanda                                                                
    ## log.Adult.Mortality:CountrySaint Lucia                                                           
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                      
    ## log.Adult.Mortality:CountrySamoa                                                                 
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 
    ## log.Adult.Mortality:CountrySaudi Arabia                                                          
    ## log.Adult.Mortality:CountrySenegal                                                               
    ## log.Adult.Mortality:CountrySerbia                                                                
    ## log.Adult.Mortality:CountrySeychelles                                                            
    ## log.Adult.Mortality:CountrySierra Leone                                                          
    ## log.Adult.Mortality:CountrySingapore                                                             
    ## log.Adult.Mortality:CountrySlovakia                                                              
    ## log.Adult.Mortality:CountrySlovenia                                                              
    ## log.Adult.Mortality:CountrySolomon Islands                                                       
    ## log.Adult.Mortality:CountrySomalia                                                               
    ## log.Adult.Mortality:CountrySouth Africa                                                          
    ## log.Adult.Mortality:CountrySouth Sudan                                                           
    ## log.Adult.Mortality:CountrySpain                                                                 
    ## log.Adult.Mortality:CountrySri Lanka                                                             
    ## log.Adult.Mortality:CountrySudan                                                              .  
    ## log.Adult.Mortality:CountrySuriname                                                              
    ## log.Adult.Mortality:CountrySwaziland                                                             
    ## log.Adult.Mortality:CountrySweden                                                                
    ## log.Adult.Mortality:CountrySwitzerland                                                           
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  
    ## log.Adult.Mortality:CountryTajikistan                                                            
    ## log.Adult.Mortality:CountryThailand                                                              
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             
    ## log.Adult.Mortality:CountryTimor-Leste                                                           
    ## log.Adult.Mortality:CountryTogo                                                                  
    ## log.Adult.Mortality:CountryTonga                                                                 
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   
    ## log.Adult.Mortality:CountryTunisia                                                               
    ## log.Adult.Mortality:CountryTurkey                                                                
    ## log.Adult.Mortality:CountryTurkmenistan                                                          
    ## log.Adult.Mortality:CountryUganda                                                                
    ## log.Adult.Mortality:CountryUkraine                                                            .  
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                  
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                           
    ## log.Adult.Mortality:CountryUnited States of America                                              
    ## log.Adult.Mortality:CountryUruguay                                                               
    ## log.Adult.Mortality:CountryUzbekistan                                                            
    ## log.Adult.Mortality:CountryVanuatu                                                               
    ## log.Adult.Mortality:CountryVenezuela                                                             
    ## log.Adult.Mortality:CountryViet Nam                                                              
    ## log.Adult.Mortality:CountryYemen                                                                 
    ## log.Adult.Mortality:CountryZambia                                                                
    ## log.Adult.Mortality:CountryZimbabwe                                                              
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                             
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                  
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                     
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                               
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                               
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                              
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                              
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                              .  
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                  
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                   
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                  
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                  
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                  
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                       
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                            
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                 
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                              
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                  
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                    
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                   
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                   
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                 
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                   
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                              
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                    
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                  
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                   
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                        
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                 
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                                
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                      
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                 
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                   
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                             
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                       
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                              *  
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                             .  
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                    
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                 
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                   
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                              *  
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                ** 
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                 
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                            ***
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                        *  
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                  
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                *  
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                 
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                 
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                   
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                               
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                    
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                 *  
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                 
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                  
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                   
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                             
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                 
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                   
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                  
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                              
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                   
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                  
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                              
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                        
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                  
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                 
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                 
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                ** 
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                               
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                              
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                              
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                  
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                    
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                   
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                              
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                               
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                  
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                        
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                                
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                              
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                 
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                           ** 
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                 
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                   
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                             
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                             
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                            .  
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                ***
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                              *  
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                  
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                    
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                  
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                        
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                             .  
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                    
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                             
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                  
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                   
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                       
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                     
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                              ***
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                      
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                               *  
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                             
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                        
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                   
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                   
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                            
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                 
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                  
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                              
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                         *  
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                               
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                         
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                 
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                         .  
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                   
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                               
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                   
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                               
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                  
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                             
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                    
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                              
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia               
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                             
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                    
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                   
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                     
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                  
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                            
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                  
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                              *  
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                    
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland    
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                          *  
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                 
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                              
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                 
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                               
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                   
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                  
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                             .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.38 on 1891 degrees of freedom
    ## Multiple R-squared:  0.984,  Adjusted R-squared:  0.979 
    ## F-statistic:  194 on 597 and 1891 DF,  p-value: <0.0000000000000002

``` r
hist(residuals, main ="Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-1.png)<!-- -->

``` r
plot(residuals, main="Residuals Plot")
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-2.png)<!-- -->

``` r
plot(fit.custom,which=2)
```

    ## Warning: not plotting observations with leverage one:
    ##   106, 363, 421, 712, 785, 911, 919, 1019, 1236, 1244, 1300, 1398, 1979, 1996, 2072

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-3.png)<!-- -->

``` r
plot(fit.custom, which =4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-4.png)<!-- -->

``` r
plot(fit.custom, which =5)
```

    ## Warning: not plotting observations with leverage one:
    ##   106, 363, 421, 712, 785, 911, 919, 1019, 1236, 1244, 1300, 1398, 1979, 1996, 2072

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-5.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Miguel)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-6.png)<!-- -->

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective 2 KNN-1.png" angle=90 style="display: block; margin: auto;" />

``` r
varImp(KNNRegressor)
```

    ## loess r-squared variable importance
    ## 
    ##   only 20 most important variables shown (out of 21)
    ## 
    ##                                  Overall
    ## Adult.Mortality                 100.0000
    ## Income.composition.of.resources  85.6586
    ## Schooling                        64.5583
    ## BMI                              46.2276
    ## GDP                              43.4106
    ## HIV.AIDS                         34.4627
    ## percentage.expenditure           29.2876
    ## Diphtheria                       27.1122
    ## Status                           26.2316
    ## thinness..1.19.years             25.7893
    ## thinness.5.9.years               25.0253
    ## Polio                            24.7984
    ## Alcohol                          21.5448
    ## Total.expenditure                12.6037
    ## Hepatitis.B                       9.5862
    ## under.five.deaths                 5.7798
    ## infant.deaths                     4.5111
    ## Measles                           3.6363
    ## Year                              3.6149
    ## Country                           0.0933

``` r
x_knn = 1:length(ktest$Life.expectancy)
plot(x_knn, ktest$Life.expectancy, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction")
lines(x_knn, prediction_test, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective 2 KNN-2.png" angle=90 style="display: block; margin: auto;" />

``` r
#### Scatter plot
plot(prediction_test ~ ytest, main = "Original vs Predicted scatter plot (KNN)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective 2 KNN-3.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/angle=90-1.png" angle=90 style="display: block; margin: auto;" />

``` r
KNNvarImp = varImp(KNNRegressor)
plot(KNNvarImp, top = 5, main='Top 5 Variable predicting life expectancy (KNN)')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/angle=90-2.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/KNN without cross validation-1.png" angle=90 style="display: block; margin: auto;" />

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

    ## [1] "Training score:  0.129758638977709"  "Training score:  0.983440598831773" 
    ## [3] "Training score:  0.0725166337782753"

``` r
print(paste("Test score: ", test_score))
```

    ## [1] "Test score:  0.221299022859664" "Test score:  0.945857337805419"
    ## [3] "Test score:  0.120785141868033"

``` r
plot(pred_y ~ ytest_knn, main = "Original vs Predicted scatter plot (KNN)", xlab = 'Original observations', ylab='Predicted values')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/KNN without cross validation-2.png" angle=90 style="display: block; margin: auto;" />

``` r
x_knn_plot = 1:length(ktest$Life.expectancy)
plot(x_knn_plot, ytest_knn, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction")
lines(x_knn_plot, pred_y, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/KNN without cross validation-3.png" angle=90 style="display: block; margin: auto;" />

``` r
mse = mean((ytest_knn - pred_y)^2)
print(paste("Test MSE: ", mse))
```

    ## [1] "Test MSE:  0.0489732575186423"

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
6.0670
</td>
<td style="text-align:left;">
0.9339
</td>
<td style="text-align:left;">
0.9333
</td>
<td style="text-align:left;">
2.4631
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
6.1872
</td>
<td style="text-align:left;">
0.9326
</td>
<td style="text-align:left;">
0.9322
</td>
<td style="text-align:left;">
2.4874
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
6.1872
</td>
<td style="text-align:left;">
0.9326
</td>
<td style="text-align:left;">
0.9322
</td>
<td style="text-align:left;">
2.4874
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
6.7017
</td>
<td style="text-align:left;">
0.9278
</td>
<td style="text-align:left;">
0.9271
</td>
<td style="text-align:left;">
2.5888
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
6.0677
</td>
<td style="text-align:left;">
0.9339
</td>
<td style="text-align:left;">
0.9332
</td>
<td style="text-align:left;">
2.4633
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
6.4706
</td>
<td style="text-align:left;">
0.9295
</td>
<td style="text-align:left;">
0.9291
</td>
<td style="text-align:left;">
2.5437
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
6.4099
</td>
<td style="text-align:left;">
0.9301
</td>
<td style="text-align:left;">
0.9297
</td>
<td style="text-align:left;">
2.5318
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
8.3664
</td>
<td style="text-align:left;">
0.9088
</td>
<td style="text-align:left;">
0.9085
</td>
<td style="text-align:left;">
2.8925
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
1.4417
</td>
<td style="text-align:left;">
0.9839
</td>
<td style="text-align:left;">
0.9789
</td>
<td style="text-align:left;">
1.2007
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
4.9071
</td>
<td style="text-align:left;">
0.9840
</td>
<td style="text-align:left;">
0.9825
</td>
<td style="text-align:left;">
1.2146
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
0.0490
</td>
<td style="text-align:left;">
0.9834
</td>
<td style="text-align:left;">
0.9820
</td>
<td style="text-align:left;">
0.1298
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
6.7042
</td>
<td style="text-align:left;">
0.9209
</td>
<td style="text-align:left;">
0.9169
</td>
<td style="text-align:left;">
2.5892
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
6.6404
</td>
<td style="text-align:left;">
0.9217
</td>
<td style="text-align:left;">
0.9178
</td>
<td style="text-align:left;">
2.5769
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
6.6404
</td>
<td style="text-align:left;">
0.9217
</td>
<td style="text-align:left;">
0.9193
</td>
<td style="text-align:left;">
2.5769
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
7.0194
</td>
<td style="text-align:left;">
0.9174
</td>
<td style="text-align:left;">
0.9126
</td>
<td style="text-align:left;">
2.6494
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
6.6989
</td>
<td style="text-align:left;">
0.9209
</td>
<td style="text-align:left;">
0.9164
</td>
<td style="text-align:left;">
2.5882
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
7.0187
</td>
<td style="text-align:left;">
0.9173
</td>
<td style="text-align:left;">
0.9152
</td>
<td style="text-align:left;">
2.6493
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
6.9850
</td>
<td style="text-align:left;">
0.9177
</td>
<td style="text-align:left;">
0.9150
</td>
<td style="text-align:left;">
2.6429
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
8.8189
</td>
<td style="text-align:left;">
0.8969
</td>
<td style="text-align:left;">
0.8953
</td>
<td style="text-align:left;">
2.9697
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
2.7378
</td>
<td style="text-align:left;">
0.9716
</td>
<td style="text-align:left;">
1.0421
</td>
<td style="text-align:left;">
1.6546
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
4.9071
</td>
<td style="text-align:left;">
0.9433
</td>
<td style="text-align:left;">
0.8947
</td>
<td style="text-align:left;">
2.2152
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
0.0490
</td>
<td style="text-align:left;">
0.9459
</td>
<td style="text-align:left;">
0.8999
</td>
<td style="text-align:left;">
0.2213
</td>
</tr>
</tbody>
</table>

## Summary

-   There is an increasing trend in average life expectancy over time.

-   Mortality rates are significant for predicting life expectancy.
    Adult Mortality and Infant Mortality are two of the variables which
    if they increase the life expectancy decrease since there is
    negative correlation with the response variable.

-   We have observed that Europe as a continent has the highest life
    expectancy which is in-line with the observation that developed
    countries have higher life expectancies than developing ones.

-   Year, Percentage Expenditure, Schooling, GDPGDP, and the
    immunizations (Diphtheria & Polio) variables have positive
    coefficients therefore if this increase life expectancy will also
    increase.

-   Adding complexity to the models increases the prediction accuracy
    but reduces explainability. In this scenario explainability would be
    important so a more parsimonious model is more useful.

-   This is an observational study without random selection or random
    assignments to groups therefore no casual inferencing can be made.
    Inferencing from this study conclusion can be applied to similar
    scenarios and the countries included intoin this research. The
    models would perform poorly with data that requires extrapolation.

-   We observed extreme values in the data setset, however we have not
    removed them without subject matter expert opinion as those values
    could be meaningful for the overall study.

-   Overall, we found the missing data points to be the primary source
    of concern with the dataset provided, adding in the supplemental
    material and resources that we did made performing our analysis
    easier rather than if we had only relied on the data provided. Given
    more time on this, we would have explored more interactions on
    relationships between the variables themselves, but not necessarily
    collect any additional data points.
