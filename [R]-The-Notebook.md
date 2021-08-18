\[R\] The Notebook
================
*Veneta Baeva*

## ![](/Users/venetabaeva/git/repository4/Picture1.jpg)

## I.

1.\[file\] read

``` r
dfAlc <- read.csv(
                      file = ("/Users/venetabaeva/git/repository4/gapminder.csv"),
                      header = TRUE,
                      sep = ";",
                      dec = ".")
```

###### <sup>1</sup> click: [Alcohol Consumption](https://www.kaggle.com/sansuthi/alcohol-consumption)

1.\[file\] view

``` r
View(dfAlc)
```

1.\[file\] check structure

``` r
str(dfAlc)
```

    ## 'data.frame':    213 obs. of  7 variables:
    ##  $ country      : chr  "Afghanistan" "Albania" "Algeria" "Andorra" ...
    ##  $ abbrv        : chr  "AF" "Al" "DZ" "AD" ...
    ##  $ aconsum      : int  0 7 1 10 6 8 9 14 NA 10 ...
    ##  $ incomeper1   : int  NA 1915 2232 21943 1381 11894 10749 1327 NA 25250 ...
    ##  $ suicideper100: int  7 8 5 5 15 2 8 4 NA 8 ...
    ##  $ employrt     : int  56 51 51 NA 76 NA 58 40 NA 62 ...
    ##  $ urbanrt      : int  24 47 65 89 57 30 92 64 47 89 ...

1.\[table\] create frequency table for factor

``` r
tableAlcCountry <- c(dfAlc$abbrv)
table(tableAlcCountry)
```

    ## tableAlcCountry
    ## AD AE AF AG Al AM AN AO AR AS AT AU AW AZ BA BB BE BF BG BH BI BJ BM BN BO BR 
    ##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
    ## BS BT BU BW BY BZ CA CD CF CG CH CI CK CL CM CO CR CU CV CY CZ DE DJ DK DM DO 
    ##  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
    ## DZ EC EE EG EH ER ES ET FI FJ FM FO FR FX GA GB GD GE GH GI GL GM GN GP GQ GR 
    ##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
    ## GT GW GY HK HN HR HT HU ID IE IL IN IQ IR IS IT JM JO JP KE KG KH KI KM KN KP 
    ##  2  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
    ## KR KW KY KZ LA LB LC LI LK LR LS LT LU LV LY MA MC MD ME MG MH MK ML MM MN MO 
    ##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  1  1 
    ## MQ MR MT MU MV MW MX MY MZ NC NE NG NI NL NO NP NR NU NZ OM PA PE PG PH PK PL 
    ##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
    ## PR PT PW PY QA RE RO RS RU RW SA SB SC SD SE SG SI SK SL SM SN SO SR ST SV SY 
    ##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
    ## SZ TD TG TH TJ TM TN TO TP TR TT TV TW TZ UA UG US UY UZ VC VE VN VU YE ZA ZM 
    ##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
    ## ZW 
    ##  1

1.\[vector\] create

``` r
country<- c("Afghanistan","Albania","Algeria","Andorra","Angola", "Antigua and Barbuda")
abbrv <- c("AF", "AL", "DZ", "AD", "AO", "AG")
aconsum <- c(0, 7, 1, 10, 6, 8)
```

1.\[vector\] sort elements’ values in increasing order

``` r
x <- c(60,50 ,40, 30, 20)
sort(x) 
```

    ## [1] 20 30 40 50 60

1.  \[vector\] return/keep index of the elements’ values in increasing
    order

``` r
index <- order(x)
index
```

    ## [1] 5 4 3 2 1

1.\[vector\] sort vector’s elements in order, through indexing

``` r
x[index]
```

    ## [1] 20 30 40 50 60

1.\[vector\] check order

``` r
order(x)
```

    ## [1] 5 4 3 2 1

1.\[vector\] rank; returns the order of each element in an ascending
list

``` r
rank(x)
```

    ## [1] 5 4 3 2 1

1.{vector} associate

``` r
abbrvCountry <- c("AF"="Afghanistan", "AL" = "Albania", "DZ" ="Algeria", "AD" = "Andorra", "AO" = "Angola", "AG"= "Antigua and Barbuda")
```

1.\[vector\] assign name

``` r
names(abbrvCountry)<-abbrv 
```

1.\[vector\] access vector’s element’s value descreately, through
indexing

``` r
abbrvCountry[2]
```

    ##        AL 
    ## "Albania"

1.\[vector\] access vector’s elements’ values descreately, through
indexing

``` r
abbrvCountry[c(1,3)]
```

    ##            AF            DZ 
    ## "Afghanistan"     "Algeria"

1.\[vector\] access vector’s elements’ values interval-ly, through
indexing

``` r
abbrvCountry[1:2]
```

    ##            AF            AL 
    ## "Afghanistan"     "Albania"

1.\[vector\] access vector’s elements’ values interval-ly, through
naming the element

``` r
abbrvCountry["AF"]
```

    ##            AF 
    ## "Afghanistan"

``` r
abbrvCountry[c("AL","DZ")]
```

    ##        AL        DZ 
    ## "Albania" "Algeria"

1.\[dataFrame\] create data frame

``` r
dfAbbrvAconsum <- data.frame(country = abbrv, alchoholconsumption = aconsum)
```

1.\[file\] check class

``` r
class(dfAlc)
```

    ## [1] "data.frame"

``` r
class(dfAlc$aconsum)
```

    ## [1] "integer"

1.\[element\] change type of element

``` r
class(1)
```

    ## [1] "numeric"

``` r
class(1L)
```

    ## [1] "integer"

1.\[sequence\] show

``` r
seq(1,10)
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10

``` r
1:10
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10

1.\[sequence\] generate increasing sequence of numbers with pretermed
length

``` r
seq(1, 10, length.out = 100)
```

    ##   [1]  1.000000  1.090909  1.181818  1.272727  1.363636  1.454545  1.545455
    ##   [8]  1.636364  1.727273  1.818182  1.909091  2.000000  2.090909  2.181818
    ##  [15]  2.272727  2.363636  2.454545  2.545455  2.636364  2.727273  2.818182
    ##  [22]  2.909091  3.000000  3.090909  3.181818  3.272727  3.363636  3.454545
    ##  [29]  3.545455  3.636364  3.727273  3.818182  3.909091  4.000000  4.090909
    ##  [36]  4.181818  4.272727  4.363636  4.454545  4.545455  4.636364  4.727273
    ##  [43]  4.818182  4.909091  5.000000  5.090909  5.181818  5.272727  5.363636
    ##  [50]  5.454545  5.545455  5.636364  5.727273  5.818182  5.909091  6.000000
    ##  [57]  6.090909  6.181818  6.272727  6.363636  6.454545  6.545455  6.636364
    ##  [64]  6.727273  6.818182  6.909091  7.000000  7.090909  7.181818  7.272727
    ##  [71]  7.363636  7.454545  7.545455  7.636364  7.727273  7.818182  7.909091
    ##  [78]  8.000000  8.090909  8.181818  8.272727  8.363636  8.454545  8.545455
    ##  [85]  8.636364  8.727273  8.818182  8.909091  9.000000  9.090909  9.181818
    ##  [92]  9.272727  9.363636  9.454545  9.545455  9.636364  9.727273  9.818182
    ##  [99]  9.909091 10.000000

1.\[sequence\] check length

``` r
length(seq(1,10))
```

    ## [1] 10

1.\[dataFrame\] check levels of a factor

``` r
levels(dfAlc$aconsum)
```

    ## NULL

1.\[dataFrame\] check number of levels of a factor

``` r
nlevels(dfAlc$aconsum)
```

    ## [1] 0

1.\[file\] check headers

``` r
head(dfAlc)
```

    ##               country abbrv aconsum incomeper1 suicideper100 employrt urbanrt
    ## 1         Afghanistan    AF       0         NA             7       56      24
    ## 2             Albania    Al       7       1915             8       51      47
    ## 3             Algeria    DZ       1       2232             5       51      65
    ## 4             Andorra    AD      10      21943             5       NA      89
    ## 5              Angola    AO       6       1381            15       76      57
    ## 6 Antigua and Barbuda    AG       8      11894             2       NA      30

1.\[file\] check headers’ names

``` r
names(dfAlc)
```

    ## [1] "country"       "abbrv"         "aconsum"       "incomeper1"   
    ## [5] "suicideper100" "employrt"      "urbanrt"

1.\[column\] access

``` r
dfAlc["aconsum"]
```

    ##     aconsum
    ## 1         0
    ## 2         7
    ## 3         1
    ## 4        10
    ## 5         6
    ## 6         8
    ## 7         9
    ## 8        14
    ## 9        NA
    ## 10       10
    ## 11       12
    ## 12       13
    ## 13        9
    ## 14        4
    ## 15        0
    ## 16        6
    ## 17       19
    ## 18       10
    ## 19        6
    ## 20        2
    ## 21       NA
    ## 22        1
    ## 23        6
    ## 24       10
    ## 25        7
    ## 26       10
    ## 27        2
    ## 28       11
    ## 29        7
    ## 30       10
    ## 31        5
    ## 32        8
    ## 33       10
    ## 34        5
    ## 35       NA
    ## 36        3
    ## 37        4
    ## 38        9
    ## 39        6
    ## 40        7
    ## 41        0
    ## 42        3
    ## 43        4
    ## 44        3
    ## 45        6
    ## 46        6
    ## 47       15
    ## 48        5
    ## 49        9
    ## 50       16
    ## 51       12
    ## 52        2
    ## 53        9
    ## 54        6
    ## 55        9
    ## 56        0
    ## 57        4
    ## 58        6
    ## 59        2
    ## 60       17
    ## 61        4
    ## 62       NA
    ## 63        3
    ## 64       13
    ## 65       12
    ## 66       NA
    ## 67        9
    ## 68        4
    ## 69        7
    ## 70       12
    ## 71        3
    ## 72       NA
    ## 73       11
    ## 74       NA
    ## 75       11
    ## 76       NA
    ## 77       NA
    ## 78        7
    ## 79        1
    ## 80        4
    ## 81        9
    ## 82        6
    ## 83        4
    ## 84       NA
    ## 85       16
    ## 86        7
    ## 87        3
    ## 88        1
    ## 89        1
    ## 90        0
    ## 91       15
    ## 92        3
    ## 93       10
    ## 94        5
    ## 95        8
    ## 96        1
    ## 97       11
    ## 98        4
    ## 99        3
    ## 100      NA
    ## 101      19
    ## 102       0
    ## 103       5
    ## 104       7
    ## 105      13
    ## 106       2
    ## 107       6
    ## 108       5
    ## 109       0
    ## 110      NA
    ## 111      16
    ## 112      13
    ## 113      NA
    ## 114       9
    ## 115       1
    ## 116       1
    ## 117       1
    ## 118      NA
    ## 119       1
    ## 120       4
    ## 121      NA
    ## 122      NA
    ## 123       0
    ## 124       4
    ## 125       9
    ## 126       5
    ## 127      23
    ## 128      NA
    ## 129       3
    ## 130      NA
    ## 131       1
    ## 132       2
    ## 133       1
    ## 134      12
    ## 135       5
    ## 136       2
    ## 137      10
    ## 138      NA
    ## 139      NA
    ## 140      10
    ## 141       5
    ## 142       0
    ## 143      13
    ## 144       9
    ## 145       8
    ## 146       1
    ## 147       0
    ## 148      10
    ## 149       7
    ## 150       4
    ## 151       8
    ## 152       7
    ## 153       6
    ## 154      14
    ## 155      14
    ## 156      NA
    ## 157       1
    ## 158      NA
    ## 159      16
    ## 160      16
    ## 161      10
    ## 162      11
    ## 163      12
    ## 164       5
    ## 165       5
    ## 166      NA
    ## 167       8
    ## 168       0
    ## 169       1
    ## 170      12
    ## 171      NA
    ## 172      12
    ## 173       9
    ## 174       2
    ## 175      13
    ## 176      15
    ## 177       1
    ## 178       1
    ## 179      10
    ## 180      12
    ## 181       1
    ## 182       3
    ## 183       7
    ## 184       5
    ## 185      10
    ## 186      11
    ## 187       1
    ## 188      NA
    ## 189       3
    ## 190       8
    ## 191       7
    ## 192       1
    ## 193       2
    ## 194       4
    ## 195       6
    ## 196       1
    ## 197       3
    ## 198       5
    ## 199       2
    ## 200      16
    ## 201      17
    ## 202       1
    ## 203      13
    ## 204      10
    ## 205       9
    ## 206       4
    ## 207       1
    ## 208       8
    ## 209       4
    ## 210      NA
    ## 211       0
    ## 212       4
    ## 213       5

1.\[vector\]sort in increasing order

``` r
sort(dfAlc$aconsum)
```

    ##   [1]  0  0  0  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  1
    ##  [26]  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3
    ##  [51]  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  5  5
    ##  [76]  5  5  5  5  5  5  5  5  5  5  5  5  6  6  6  6  6  6  6  6  6  6  6  6  6
    ## [101]  7  7  7  7  7  7  7  7  7  7  7  7  8  8  8  8  8  8  8  8  9  9  9  9  9
    ## [126]  9  9  9  9  9  9  9  9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 11 11
    ## [151] 11 11 11 11 12 12 12 12 12 12 12 12 12 13 13 13 13 13 13 13 14 14 14 15 15
    ## [176] 15 16 16 16 16 16 16 17 17 19 19 23

1.\[column\] access through vector

``` r
dfAlc[["aconsum"]]
```

    ##   [1]  0  7  1 10  6  8  9 14 NA 10 12 13  9  4  0  6 19 10  6  2 NA  1  6 10  7
    ##  [26] 10  2 11  7 10  5  8 10  5 NA  3  4  9  6  7  0  3  4  3  6  6 15  5  9 16
    ##  [51] 12  2  9  6  9  0  4  6  2 17  4 NA  3 13 12 NA  9  4  7 12  3 NA 11 NA 11
    ##  [76] NA NA  7  1  4  9  6  4 NA 16  7  3  1  1  0 15  3 10  5  8  1 11  4  3 NA
    ## [101] 19  0  5  7 13  2  6  5  0 NA 16 13 NA  9  1  1  1 NA  1  4 NA NA  0  4  9
    ## [126]  5 23 NA  3 NA  1  2  1 12  5  2 10 NA NA 10  5  0 13  9  8  1  0 10  7  4
    ## [151]  8  7  6 14 14 NA  1 NA 16 16 10 11 12  5  5 NA  8  0  1 12 NA 12  9  2 13
    ## [176] 15  1  1 10 12  1  3  7  5 10 11  1 NA  3  8  7  1  2  4  6  1  3  5  2 16
    ## [201] 17  1 13 10  9  4  1  8  4 NA  0  4  5

1.\[column\] access through vector

``` r
dfAlc$aconsum
```

    ##   [1]  0  7  1 10  6  8  9 14 NA 10 12 13  9  4  0  6 19 10  6  2 NA  1  6 10  7
    ##  [26] 10  2 11  7 10  5  8 10  5 NA  3  4  9  6  7  0  3  4  3  6  6 15  5  9 16
    ##  [51] 12  2  9  6  9  0  4  6  2 17  4 NA  3 13 12 NA  9  4  7 12  3 NA 11 NA 11
    ##  [76] NA NA  7  1  4  9  6  4 NA 16  7  3  1  1  0 15  3 10  5  8  1 11  4  3 NA
    ## [101] 19  0  5  7 13  2  6  5  0 NA 16 13 NA  9  1  1  1 NA  1  4 NA NA  0  4  9
    ## [126]  5 23 NA  3 NA  1  2  1 12  5  2 10 NA NA 10  5  0 13  9  8  1  0 10  7  4
    ## [151]  8  7  6 14 14 NA  1 NA 16 16 10 11 12  5  5 NA  8  0  1 12 NA 12  9  2 13
    ## [176] 15  1  1 10 12  1  3  7  5 10 11  1 NA  3  8  7  1  2  4  6  1  3  5  2 16
    ## [201] 17  1 13 10  9  4  1  8  4 NA  0  4  5

1.\[object\] define

``` r
aconsum <- dfAlc$aconsum
urbanrt <-dfAlc$urbanrt
employrt <- dfAlc$employrt
```

1.\[object\] define lenght

``` r
length(aconsum)
```

    ## [1] 213

1.\[object\] check identical

``` r
identical(urbanrt,employrt)
```

    ## [1] FALSE

1.\[vector\] return index of the elements’ values in increasing order
1.1.\[vector\] sort vector’s elements in order, through indexing

``` r
iOrdDfAconsum<-order(dfAlc$aconsum)
dfAlc$abbrv[iOrdDfAconsum]
```

    ##   [1] "AF" "BU" "KM" "EG" "IQ" "KW" "LY" "MR" "NE" "PK" "SA" "YE" "DZ" "BT" "GN"
    ##  [16] "ID" "IR" "JO" "MG" "MW" "MY" "ML" "MA" "MM" "OM" "QA" "SN" "SB" "SO" "LK"
    ##  [31] "SY" "TP" "TN" "AE" "VU" "BJ" "BN" "DJ" "ER" "LB" "MZ" "NP" "SG" "TG" "TV"
    ##  [46] "CF" "CD" "CK" "FJ" "GH" "IN" "IL" "KI" "MN" "SD" "TJ" "TR" "BH" "TD" "CG"
    ##  [61] "SV" "ET" "GM" "GW" "HN" "KE" "MT" "MU" "PG" "TO" "UZ" "VN" "ZM" "KH" "CV"
    ##  [76] "CU" "JM" "KG" "LR" "FM" "NR" "NI" "VC" "AS" "SZ" "TM" "ZW" "AO" "BB" "BZ"
    ##  [91] "BO" "CH" "CR" "CI" "DO" "GQ" "HT" "LS" "PH" "TT" "Al" "BW" "BF" "CO" "GE"
    ## [106] "GT" "IS" "LA" "PA" "PE" "SR" "TH" "AG" "CM" "JP" "NO" "PY" "ST" "TZ" "VE"
    ## [121] "AR" "BS" "CL" "CY" "DM" "EC" "GA" "GY" "MK" "MX" "NU" "SL" "UY" "AD" "AU"
    ## [136] "BE" "BA" "BR" "BI" "CA" "IT" "AN" "NZ" "PW" "RW" "ZA" "SE" "US" "BG" "GR"
    ## [151] "GD" "KZ" "KN" "CH" "AT" "DK" "FR" "DE" NA   "LC" "RS" "SC" "ES" "AZ" "FI"
    ## [166] "LV" "LU" "NG" "SK" "GB" "AM" "PL" "PT" "HR" "IE" "SI" "CZ" "HU" "LT" "RO"
    ## [181] "RU" "UG" "EE" "UA" "BY" "KR" "MD" "AW" "BM" "KY" "FO" "FX" "GI" "GL" "GP"
    ## [196] "GT" "HK" "KP" "LI" "MO" "MV" "MH" "MQ" "MC" "ME" "NL" "NC" "PR" "RE" "SM"
    ## [211] "ME" "TW" "EH"

1.\[vector\] find max value between elements, through indexing
1.1\[object\] return index 1.1.1\[vector\] show value of element,
through indexing the object

``` r
iMax<-which.max(dfAlc$aconsum)
iMax
```

    ## [1] 127

``` r
dfAlc$abbrv[iMax]
```

    ## [1] "MD"

1.\[vector\] find max value between elements, through indexing
1.1\[object\] return index 1.1.1\[vector\] show value of element,
through indexing the object

``` r
iMin<-which.max(dfAlc$aconsum)
iMin
```

    ## [1] 127

``` r
dfAlc$abbrv[iMin]
```

    ## [1] "MD"

> library load

``` r
library(dplyr)
library(dslabs)
library(tidyverse)
```

2.\[vector\] define 2.2.\[vector\] rank elements’ values, through
indexing 2.2.2.\[object\]keep index of the elements’ values in
increasing order 2.2.2.2.\[dataFrame\] create

``` r
abbCountry <- dfAlc$abbrv 
suicidesPer100 <- dfAlc$suicideper100
urbanRT <- dfAlc$urbanrt
ranksAConsumption <- rank(dfAlc$aconsum,na.last = NA)
i <- order(dfAlc$aconsum)
df<- data.frame(country = abbCountry[i], suicide = suicidesPer100[i], rank = ranksAConsumption[i],urbanrate = urbanRT[i])
df %>%
  ggplot(aes(urbanrate, suicide, label=country,color=rank)) +
  geom_label()
```

    ## Warning: Removed 28 rows containing missing values (geom_label).

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

2.  

``` r
dfAlc %>%
  ggplot(aes(urbanrt, employrt, label=abbrv, color=aconsum)) +
  geom_label()
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

## II.

## III.

## Libraries

``` r
library(dplyr)
```

``` r
library(dslabs)
```

``` r
library(tidyverse)
```

## References

## Contact

![](/Users/venetabaeva/git/repository4/OrgVenetaBaeva-images/0001.jpg)
![](/Users/venetabaeva/git/repository4/OrgVenetaBaeva-images/0002.jpg)
![](/Users/venetabaeva/git/repository4/OrgVenetaBaeva-images/0003.jpg)
