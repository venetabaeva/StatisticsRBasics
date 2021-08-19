\[R\] The Notebook
================
*Veneta Baeva*

## ![](/Users/venetabaeva/git/repository4/Picture1.jpg)

## Libraries

> library load

``` r
library(dplyr)
```

``` r
library(dslabs)
```

``` r
library(tidyverse)
```

## I.

1.\[dataFrame\] create

``` r
dfTest <- data.frame(names=c("A","B","C","D"),
                     num1 = c(1,2,3,4),
                     num2 = c(10,20,30,40),
                     stringsAsFactors = FALSE)
dfTest
```

    ##   names num1 num2
    ## 1     A    1   10
    ## 2     B    2   20
    ## 3     C    3   30
    ## 4     D    4   40

1.\[data\] read

``` r
data(heights)
```

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

    ## 'data.frame':    213 obs. of  8 variables:
    ##  $ country      : chr  "Afghanistan" "Albania" "Algeria" "Andorra" ...
    ##  $ abbrv        : chr  "AF" "Al" "DZ" "AD" ...
    ##  $ aconsum      : int  0 7 1 10 6 8 9 14 NA 10 ...
    ##  $ incomeper1   : int  NA 1915 2232 21943 1381 11894 10749 1327 NA 25250 ...
    ##  $ suicideper100: int  7 8 5 5 15 2 8 4 NA 8 ...
    ##  $ employrt     : int  56 51 51 NA 76 NA 58 40 NA 62 ...
    ##  $ urbanrt      : int  24 47 65 89 57 30 92 64 47 89 ...
    ##  $ region       : chr  "EMEA" "EMEA" "EMEA" "EMEA" ...

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
country
```

    ## [1] "Afghanistan"         "Albania"             "Algeria"            
    ## [4] "Andorra"             "Angola"              "Antigua and Barbuda"

``` r
abbrv <- c("AF", "AL", "DZ", "AD", "AO", "AG")
abbrv
```

    ## [1] "AF" "AL" "DZ" "AD" "AO" "AG"

``` r
aconsum <- c(0, 7, 1, 10, 6, 8)
aconsum
```

    ## [1]  0  7  1 10  6  8

``` r
x <- c(60,50 ,40, 30, 20)
x
```

    ## [1] 60 50 40 30 20

1.\[vector\] change elements’ type

``` r
y<- as.character(x) 
y
```

    ## [1] "60" "50" "40" "30" "20"

``` r
as.numeric(y)
```

    ## [1] 60 50 40 30 20

``` r
x<-y
```

1.\[vector\]check default type

``` r
x<-c(1,"test",3)
x
```

    ## [1] "1"    "test" "3"

``` r
class(x)
```

    ## [1] "character"

1.\[vector\]check default type

``` r
x<-c("1","test","3")
x
```

    ## [1] "1"    "test" "3"

``` r
class(x)
```

    ## [1] "character"

``` r
as.numeric(x)
```

    ## Warning: NAs introduced by coercion

    ## [1]  1 NA  3

###### <sup>1</sup> Note: coercion problem -&gt; NAs introduced by coercion

1.\[vector\] sort elements’ values in increasing order

``` r
sort(x) 
```

    ## [1] "1"    "3"    "test"

1.  \[vector\] return/keep index of the elements’ values in increasing
    order

``` r
index <- order(x)
index
```

    ## [1] 1 3 2

1.\[vector\] sort vector’s elements in order, through indexing

``` r
x[index]
```

    ## [1] "1"    "3"    "test"

1.\[vector\] check order

``` r
order(x)
```

    ## [1] 1 3 2

1.\[vector\] rank; returns the order of each element in an ascending
list

``` r
rank(x)
```

    ## [1] 1 3 2

1.{vector} associate

``` r
abbrvCountry <- c("AF"="Afghanistan", "AL" = "Albania", "DZ" ="Algeria", "AD" = "Andorra", "AO" = "Angola", "AG"= "Antigua and Barbuda")
abbrvCountry
```

    ##                    AF                    AL                    DZ 
    ##         "Afghanistan"             "Albania"             "Algeria" 
    ##                    AD                    AO                    AG 
    ##             "Andorra"              "Angola" "Antigua and Barbuda"

1.\[vector\] assign name

``` r
names(abbrvCountry)<-abbrv 
abbrv
```

    ## [1] "AF" "AL" "DZ" "AD" "AO" "AG"

``` r
abbrvCountry
```

    ##                    AF                    AL                    DZ 
    ##         "Afghanistan"             "Albania"             "Algeria" 
    ##                    AD                    AO                    AG 
    ##             "Andorra"              "Angola" "Antigua and Barbuda"

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
    ##   region
    ## 1   EMEA
    ## 2   EMEA
    ## 3   EMEA
    ## 4   EMEA
    ## 5   EMEA
    ## 6  LATAM

1.\[file\] check headers’ names

``` r
names(dfAlc)
```

    ## [1] "country"       "abbrv"         "aconsum"       "incomeper1"   
    ## [5] "suicideper100" "employrt"      "urbanrt"       "region"

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

1.\[vector\] count NAs

``` r
naS <- is.na(aconsum)
sum(naS)
```

    ## [1] 26

1.\[vector\] expell NAs

``` r
mean(aconsum[!naS])
```

    ## [1] 6.679144

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

1.1\[object\] return index

1.1.1\[vector\] show value of element, through indexing the object

``` r
iMax<-which.max(dfAlc$aconsum)
iMax
```

    ## [1] 127

``` r
dfAlc$abbrv[iMax]
```

    ## [1] "MD"

``` r
dfAlc$abbrv[which.max(dfAlc$aconsum)]
```

    ## [1] "MD"

1.\[vector\] find max value between elements, through indexing

1.1\[object\] return index

1.1.1\[vector\] show value of element, through indexing the object

``` r
iMin<-which.max(dfAlc$aconsum)
iMin
```

    ## [1] 127

``` r
dfAlc$abbrv[iMin]
```

    ## [1] "MD"

``` r
dfAlc$abbrv[which.min(dfAlc$aconsum)]
```

    ## [1] "AF"

1.\[vector\] order

``` r
dfAlc$abbrv[order(dfAlc$employrt,decreasing=TRUE)]
```

    ##   [1] "BI" "MG" "UG" "GN" "BF" "ET" "RW" "KH" "LA" "TZ" "MZ" "AO" "QA" "MM" "AE"
    ##  [16] "IS" "CH" "KE" "PY" "BJ" "GM" "MW" "TH" "CF" "VN" "BO" "PG" "TD" "BU" "KM"
    ##  [31] "PE" "BS" "BB" "TP" "ZW" "CD" "GW" "KW" "LR" "SN" "SO" "BR" "ER" "GH" "KP"
    ##  [46] "NZ" "NO" "SB" "BN" "CA" "CG" "KZ" "MO" "SL" "CH" "TG" "CO" "DK" "GT" "AU"
    ##  [61] "GQ" "ID" "NP" "SG" "TT" "US" "AZ" "MY" "AN" "PH" "SE" "ZM" "BH" "CI" "EC"
    ##  [76] "IE" "NE" "PA" "VE" "CM" "CR" "CY" "GA" "GY" "HK" "KR" "KG" "NI" "RU" "TM"
    ##  [91] "GB" "AR" "BT" "SV" "JM" "MX" "PT" "UY" "UZ" "AT" "BZ" "EE" "FI" "JP" "LV"
    ## [106] "MV" "AF" "CV" "CU" "CZ" "FJ" "GE" "HT" "HN" "LS" "SI" "IN" "MU" "LK" "TW"
    ## [121] "TJ" "DE" "LU" "NL" "UA" "BY" "DO" "LT" "SK" "ES" "MN" "Al" "DZ" "CL" "FR"
    ## [136] "IL" "NG" "OM" "PK" "SA" "SZ" "GR" "RO" "BE" "LY" "PL" "ME" "IR" "BG" "HR"
    ## [151] "HU" "MT" "MR" "SD" "BW" "IT" "LB" "ML" "MA" "SR" "SY" "MD" "RE" "GP" "MQ"
    ## [166] "TR" "EG" NA   "PR" "TN" "BA" "ZA" "AM" "JO" "YE" "IQ" "MK" "EH" "AD" "AG"
    ## [181] "AW" "BM" "KY" "CK" "DJ" "DM" "FO" "FX" "GI" "GL" "GD" "GT" "KI" "LI" "MH"
    ## [196] "FM" "MC" "ME" "NR" "NC" "NU" "PW" "KN" "LC" "VC" "AS" "SM" "ST" "RS" "SC"
    ## [211] "TO" "TV" "VU"

1.\[vector\] check range through indexing

1.1.\[vector\] count the number of elements in the range

``` r
i <- dfAlc$aconsum  < 5
i
```

    ##   [1]  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE    NA FALSE FALSE FALSE
    ##  [13] FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE    NA  TRUE FALSE FALSE
    ##  [25] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE    NA  TRUE
    ##  [37]  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ##  [49] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE
    ##  [61]  TRUE    NA  TRUE FALSE FALSE    NA FALSE  TRUE FALSE FALSE  TRUE    NA
    ##  [73] FALSE    NA FALSE    NA    NA FALSE  TRUE  TRUE FALSE FALSE  TRUE    NA
    ##  [85] FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE
    ##  [97] FALSE  TRUE  TRUE    NA FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## [109]  TRUE    NA FALSE FALSE    NA FALSE  TRUE  TRUE  TRUE    NA  TRUE  TRUE
    ## [121]    NA    NA  TRUE  TRUE FALSE FALSE FALSE    NA  TRUE    NA  TRUE  TRUE
    ## [133]  TRUE FALSE FALSE  TRUE FALSE    NA    NA FALSE FALSE  TRUE FALSE FALSE
    ## [145] FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE    NA
    ## [157]  TRUE    NA FALSE FALSE FALSE FALSE FALSE FALSE FALSE    NA FALSE  TRUE
    ## [169]  TRUE FALSE    NA FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE
    ## [181]  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE    NA  TRUE FALSE FALSE  TRUE
    ## [193]  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE
    ## [205] FALSE  TRUE  TRUE FALSE  TRUE    NA  TRUE  TRUE FALSE

``` r
dfAlc$abbrv[i]
```

    ##  [1] "AF" "DZ" NA   "BH" "BU" "BJ" NA   "BT" "BN" NA   "CF" "TD" "KM" "CD" "CG"
    ## [16] "CK" "DJ" "EG" "SV" "ER" "ET" NA   "FJ" NA   "GM" "GH" NA   NA   NA   NA  
    ## [31] "GN" "GW" "HN" NA   "IN" "ID" "IR" "IQ" "IL" "JO" "KE" "KI" NA   "KW" "LB"
    ## [46] "LY" NA   NA   "MG" "MW" "MY" NA   "ML" "MT" NA   NA   "MR" "MU" NA   "MN"
    ## [61] NA   "MA" "MZ" "MM" "NP" NA   NA   "NE" "OM" "PK" "PG" NA   "QA" NA   NA  
    ## [76] "SA" "SN" NA   "SG" "SB" "SO" "LK" "SD" "SY" NA   "TJ" "TP" "TG" "TO" "TN"
    ## [91] "TR" "TV" "AE" "UZ" "VU" "VN" NA   "YE" "ZM"

``` r
sum(i,na.rm =TRUE)
```

    ## [1] 73

1.1.\[vector\] filter data by sub-setting row

``` r
filter(dfAlc, region=="EMEA")
```

    ##                    country abbrv aconsum incomeper1 suicideper100 employrt
    ## 1              Afghanistan    AF       0         NA             7       56
    ## 2                  Albania    Al       7       1915             8       51
    ## 3                  Algeria    DZ       1       2232             5       51
    ## 4                  Andorra    AD      10      21943             5       NA
    ## 5                   Angola    AO       6       1381            15       76
    ## 6                  Armenia    AM      14       1327             4       40
    ## 7                  Austria    AT      12      26693            13       57
    ## 8               Azerbaijan    AZ      13       2345             1       61
    ## 9                  Bahrain    BH       4      12505             4       60
    ## 10                 Belarus    BY      19       2738            27       53
    ## 11                 Belgium    BE      10      24496            16       49
    ## 12                   Benin    BJ       2        377             6       72
    ## 13  Bosnia and Herzegovina    BA      10       2183            12       41
    ## 14                Botswana    BW       7       4189            11       46
    ## 15                Bulgaria    BG      11       2550             9       47
    ## 16            Burkina Faso    BF       7        276             8       81
    ## 17                 Burundi    BI      10        115            15       83
    ## 18                Cameroon    CM       8        714             7       59
    ## 19    Central African Rep,    CF       3        240            14       71
    ## 20                    Chad    TD       4        276             8       69
    ## 21                 Comoros    KM       0        336             5       68
    ## 22        Congo, Dem, Rep,    CD       3        104            15       66
    ## 23             Congo, Rep,    CG       4       1253            10       64
    ## 24            Cook Islands    CK       3         NA             4       NA
    ## 25           Cote d'Ivoire    CI       6        591            20       60
    ## 26                 Croatia    HR      15       6338            15       47
    ## 27                  Cyprus    CY       9      15314             2       59
    ## 28              Czech Rep,    CZ      16       7381            12       56
    ## 29                 Denmark    DK      12      30532             9       63
    ## 30                Djibouti    DJ       2        895             5       NA
    ## 31                   Egypt    EG       0       1976             2       42
    ## 32       Equatorial Guinea    GQ       6       8655            10       62
    ## 33                 Eritrea    ER       2        132             9       65
    ## 34                 Estonia    EE      17       6239            17       57
    ## 35                Ethiopia    ET       4        221            10       81
    ## 36                 Finland    FI      13      27111            16       57
    ## 37                  France    FR      12      22878            14       51
    ## 38        French Polynesia    FX      NA         NA            NA       NA
    ## 39                   Gabon    GA       9       4181             8       59
    ## 40                  Gambia    GM       4        355             6       72
    ## 41                 Georgia    GE       7       1259             2       56
    ## 42                 Germany    DE      12      25306             9       54
    ## 43                   Ghana    GH       3        359             6       65
    ## 44               Gibraltar    GI      NA         NA            NA       NA
    ## 45                  Greece    GR      11      13578             3       50
    ## 46               Greenland    GL      NA      20752            NA       NA
    ## 47                 Grenada    GD      11       5330             4       NA
    ## 48              Guadeloupe    GP      NA         NA            NA       43
    ## 49                  Guinea    GN       1        412             7       82
    ## 50           Guinea-Bissau    GW       4        161             8       66
    ## 51                 Hungary    HU      16       5634            20       47
    ## 52                 Iceland    IS       7      33945            11       74
    ## 53                    Iran    IR       1       2162             6       48
    ## 54                    Iraq    IQ       0        736            17       37
    ## 55                 Ireland    IE      15      27595            10       60
    ## 56                  Israel    IL       3      22276             6       51
    ## 57                   Italy    IT      10      18982             5       46
    ## 58                  Jordan    JO       1       2534             0       39
    ## 59              Kazakhstan    KZ      11       2482            25       64
    ## 60                   Kenya    KE       4        469            11       73
    ## 61                  Kuwait    KW       0         NA             1       66
    ## 62              Kyrgyzstan    KG       5        373            10       59
    ## 63                  Latvia    LV      13       5011            20       57
    ## 64                 Lebanon    LB       2       6747             6       46
    ## 65                 Lesotho    LS       6        496             8       56
    ## 66                 Liberia    LR       5        155             7       66
    ## 67                   Libya    LY       0       7885             5       49
    ## 68           Liechtenstein    LI      NA      81647            NA       NA
    ## 69               Lithuania    LT      16       5332            33       53
    ## 70              Luxembourg    LU      13      52302            12       54
    ## 71          Macedonia, FYR    MK       9       2221             8       35
    ## 72              Madagascar    MG       1        243             6       83
    ## 73                  Malawi    MW       1        184            10       72
    ## 74                    Mali    ML       1        270             8       46
    ## 75                   Malta    MT       4      11067             5       47
    ## 76              Martinique    MQ      NA         NA            NA       43
    ## 77              Mauritania    MR       0        609             7       47
    ## 78               Mauritius    MU       4       5182             8       55
    ## 79                 Moldova    MD      23        596            16       44
    ## 80                  Monaco    MC      NA     105147            11       NA
    ## 81              Montenegro    ME      NA       2222            NA       NA
    ## 82                 Morocco    MA       1       1844             2       46
    ## 83              Mozambique    MZ       2        390            11       77
    ## 84                 Namibia  <NA>      12       2667             8       42
    ## 85             Netherlands    AN      10      26552             8       61
    ## 86    Netherlands Antilles    NL      NA         NA            NA       54
    ## 87           New Caledonia    NC      NA         NA            NA       NA
    ## 88                   Niger    NE       0        180             9       60
    ## 89                 Nigeria    NG      13        545             8       51
    ## 90                  Norway    NO       8      39972            11       65
    ## 91                    Oman    OM       1      11192             4       51
    ## 92                  Poland    PL      14       6576            14       49
    ## 93                Portugal    PT      14      11745             8       58
    ## 94                   Qatar    QA       1      33932             3       76
    ## 95                 Reunion    RE      NA         NA            NA       44
    ## 96                 Romania    RO      16       2637            10       50
    ## 97                  Russia    RU      16       2923            28       59
    ## 98                  Rwanda    RW      10        338            13       80
    ## 99              San Marino    SM      NA      31993             6       NA
    ## 100  Sao Tome and Principe    ST       8         NA            10       NA
    ## 101           Saudi Arabia    SA       0       9425             6       51
    ## 102                Senegal    SN       1        562             7       66
    ## 103                 Serbia    RS      12       1195            14       NA
    ## 104  Serbia and Montenegro    ME      NA         NA            14       49
    ## 105             Seychelles    SC      12       8614            10       NA
    ## 106           Sierra Leone    SL       9        268            12       64
    ## 107        Slovak Republic    SK      13       8446            11       53
    ## 108               Slovenia    SI      15      12729            19       56
    ## 109                Somalia    SO       1         NA            30       66
    ## 110           South Africa    ZA      10       3746            16       41
    ## 111                  Spain    ES      12      15462             6       53
    ## 112                  Sudan    SD       3        524            10       47
    ## 113              Swaziland    SZ       5       1810            13       51
    ## 114                 Sweden    SE      10      32292            11       61
    ## 115            Switzerland    CH      11      37663            13       64
    ## 116                  Syria    SY       1       1526             1       45
    ## 117             Tajikistan    TJ       3        279             3       55
    ## 118               Tanzania    TZ       8        456            12       78
    ## 119                   Togo    TG       2        285             6       64
    ## 120                Tunisia    TN       1       3165             3       42
    ## 121                 Turkey    TR       3       5349             4       43
    ## 122           Turkmenistan    TM       5       2062            12       59
    ## 123                 Uganda    UG      16        377            12       83
    ## 124                Ukraine    UA      17       1037            19       54
    ## 125   United Arab Emirates    AE       1      21087             1       75
    ## 126         United Kingdom    GB      13      28033             6       59
    ## 127             Uzbekistan    UZ       4        953             5       58
    ## 128     West Bank and Gaza    EH      NA         NA            NA       32
    ## 129            Yemen, Rep,    YE       0        610             6       39
    ## 130                 Zambia    ZM       4        432            12       61
    ## 131               Zimbabwe    ZW       5        321            14       67
    ##     urbanrt region
    ## 1        24   EMEA
    ## 2        47   EMEA
    ## 3        65   EMEA
    ## 4        89   EMEA
    ## 5        57   EMEA
    ## 6        64   EMEA
    ## 7        67   EMEA
    ## 8        52   EMEA
    ## 9        89   EMEA
    ## 10       73   EMEA
    ## 11       97   EMEA
    ## 12       41   EMEA
    ## 13       47   EMEA
    ## 14       60   EMEA
    ## 15       71   EMEA
    ## 16       20   EMEA
    ## 17       10   EMEA
    ## 18       57   EMEA
    ## 19       39   EMEA
    ## 20       27   EMEA
    ## 21       28   EMEA
    ## 22       34   EMEA
    ## 23       61   EMEA
    ## 24       NA   EMEA
    ## 25       49   EMEA
    ## 26       57   EMEA
    ## 27       70   EMEA
    ## 28       74   EMEA
    ## 29       87   EMEA
    ## 30       87   EMEA
    ## 31       43   EMEA
    ## 32       39   EMEA
    ## 33       21   EMEA
    ## 34       69   EMEA
    ## 35       17   EMEA
    ## 36       63   EMEA
    ## 37       77   EMEA
    ## 38       52   EMEA
    ## 39       85   EMEA
    ## 40       56   EMEA
    ## 41       53   EMEA
    ## 42       74   EMEA
    ## 43       50   EMEA
    ## 44       NA   EMEA
    ## 45       61   EMEA
    ## 46       84   EMEA
    ## 47       31   EMEA
    ## 48       NA   EMEA
    ## 49       34   EMEA
    ## 50       30   EMEA
    ## 51       68   EMEA
    ## 52       92   EMEA
    ## 53       68   EMEA
    ## 54       67   EMEA
    ## 55       61   EMEA
    ## 56       92   EMEA
    ## 57       68   EMEA
    ## 58       78   EMEA
    ## 59       58   EMEA
    ## 60       22   EMEA
    ## 61       98   EMEA
    ## 62       36   EMEA
    ## 63       68   EMEA
    ## 64       87   EMEA
    ## 65       25   EMEA
    ## 66       60   EMEA
    ## 67       78   EMEA
    ## 68       14   EMEA
    ## 69       67   EMEA
    ## 70       82   EMEA
    ## 71       67   EMEA
    ## 72       30   EMEA
    ## 73       19   EMEA
    ## 74       32   EMEA
    ## 75       94   EMEA
    ## 76       NA   EMEA
    ## 77       41   EMEA
    ## 78       42   EMEA
    ## 79       42   EMEA
    ## 80      100   EMEA
    ## 81       60   EMEA
    ## 82       56   EMEA
    ## 83       37   EMEA
    ## 84       37   EMEA
    ## 85       82   EMEA
    ## 86       93   EMEA
    ## 87       65   EMEA
    ## 88       17   EMEA
    ## 89       48   EMEA
    ## 90       77   EMEA
    ## 91       72   EMEA
    ## 92       61   EMEA
    ## 93       59   EMEA
    ## 94       96   EMEA
    ## 95       NA   EMEA
    ## 96       54   EMEA
    ## 97       73   EMEA
    ## 98       18   EMEA
    ## 99       94   EMEA
    ## 100      61   EMEA
    ## 101      82   EMEA
    ## 102      42   EMEA
    ## 103      52   EMEA
    ## 104      NA   EMEA
    ## 105      54   EMEA
    ## 106      38   EMEA
    ## 107      57   EMEA
    ## 108      49   EMEA
    ## 109      37   EMEA
    ## 110      61   EMEA
    ## 111      77   EMEA
    ## 112      43   EMEA
    ## 113      25   EMEA
    ## 114      85   EMEA
    ## 115      73   EMEA
    ## 116      54   EMEA
    ## 117      26   EMEA
    ## 118      26   EMEA
    ## 119      42   EMEA
    ## 120      67   EMEA
    ## 121      69   EMEA
    ## 122      49   EMEA
    ## 123      13   EMEA
    ## 124      68   EMEA
    ## 125      78   EMEA
    ## 126      90   EMEA
    ## 127      37   EMEA
    ## 128      72   EMEA
    ## 129      31   EMEA
    ## 130      35   EMEA
    ## 131      37   EMEA

1.\[vector\] filter

``` r
emea<- dfAlc$region == "EMEA" 
aconsum <- dfAlc$aconsum <= 5 
i <- aconsum&emea
dfAlc$abbrv[i]
```

    ##  [1] "AF" "DZ" "BH" "BJ" "CF" "TD" "KM" "CD" "CG" "CK" "DJ" "EG" "ER" "ET" NA  
    ## [16] "GM" "GH" NA   NA   NA   "GN" "GW" "IR" "IQ" "IL" "JO" "KE" "KW" "KG" "LB"
    ## [31] "LR" "LY" NA   "MG" "MW" "ML" "MT" NA   "MR" "MU" NA   NA   "MA" "MZ" NA  
    ## [46] NA   "NE" "OM" "QA" NA   NA   "SA" "SN" NA   "SO" "SD" "SZ" "SY" "TJ" "TG"
    ## [61] "TN" "TR" "TM" "AE" "UZ" NA   "YE" "ZM" "ZW"

1.1.\[vector\] which indexes of elements has value TRUE

``` r
which(i)
```

    ##  [1]   1   3  14  20  36  37  41  42  43  44  52  56  59  61  68  71  79  80  89
    ## [20]  90  92  96  98 102 103 106 108 109 115 116 119 120 123 124 131 132 142 146
    ## [39] 157 168 169 178 182 184 187 189 193 196 197 198 202 206 211 212 213

1.1.\[vector\] which value TRUE or FALSE of element

``` r
i<-which(dfAlc$abbrv == "BG")
i
```

    ## [1] 28

``` r
aconsum[i]
```

    ## [1] FALSE

1.1.\[vector\] match values of elements and return TRUE or FALSE

``` r
i <- match(c("BG","IT","ES"),dfAlc$abbrv)
i
```

    ## [1]  28  93 180

``` r
dfAlc$abbrv[i]
```

    ## [1] "BG" "IT" "ES"

``` r
aconsum[i]
```

    ## [1] FALSE FALSE FALSE

1.1.\[vector\] check whether values of vector in values of vector and
return TRUE or FALSE

``` r
emea<- dfAlc$region == "EMEA" 
aconsum5 <- dfAlc$aconsum <= 5 
aconsum10 <- dfAlc$aconsum <= 5 
aconsum5&emea %in% aconsum10&emea
```

    ##   [1]  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [13] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ##  [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    ##  [37]  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ##  [49] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE
    ##  [61]  TRUE FALSE FALSE FALSE FALSE    NA FALSE  TRUE FALSE FALSE  TRUE    NA
    ##  [73] FALSE    NA FALSE    NA FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ##  [85] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE
    ##  [97] FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE
    ## [109]  TRUE    NA FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
    ## [121] FALSE    NA  TRUE  TRUE FALSE FALSE FALSE    NA FALSE    NA  TRUE  TRUE
    ## [133] FALSE FALSE FALSE FALSE FALSE    NA    NA FALSE FALSE  TRUE FALSE FALSE
    ## [145] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [157]  TRUE    NA FALSE FALSE FALSE FALSE FALSE FALSE FALSE    NA FALSE  TRUE
    ## [169]  TRUE FALSE    NA FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## [181] FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE
    ## [193]  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## [205] FALSE  TRUE FALSE FALSE FALSE    NA  TRUE  TRUE  TRUE

1.1.\[vector\] check whether each value of vector in values of the same
vector and return TRUE or FALSE

``` r
checkAbbrv<- c("GB","BG","MZ") %in% dfAlc$abbrv
```

1.1.\[vector\] check whether each value of vector in values of the same
vector, through indexing and return TRUE or FALSE

``` r
checkAbbrv<- c("GB","BG","MZ") %in% dfAlc$abbrv
i <- which(!checkAbbrv%in%dfAlc$abbrv)
i
```

    ## [1] 1 2 3

``` r
checkAbbrv[i] 
```

    ## [1] TRUE TRUE TRUE

1.1.\[dataFrame\] mutate data frame

``` r
dfAlc <- mutate(dfAlc,rank=rank(-dfAlc$aconsum))
str(dfAlc)
```

    ## 'data.frame':    213 obs. of  9 variables:
    ##  $ country      : chr  "Afghanistan" "Albania" "Algeria" "Andorra" ...
    ##  $ abbrv        : chr  "AF" "Al" "DZ" "AD" ...
    ##  $ aconsum      : int  0 7 1 10 6 8 9 14 NA 10 ...
    ##  $ incomeper1   : int  NA 1915 2232 21943 1381 11894 10749 1327 NA 25250 ...
    ##  $ suicideper100: int  7 8 5 5 15 2 8 4 NA 8 ...
    ##  $ employrt     : int  56 51 51 NA 76 NA 58 40 NA 62 ...
    ##  $ urbanrt      : int  24 47 65 89 57 30 92 64 47 89 ...
    ##  $ region       : chr  "EMEA" "EMEA" "EMEA" "EMEA" ...
    ##  $ rank         : num  181.5 81.5 164 47 94 ...

###### <sup>2</sup> Note: include a column named rank with the ranks of rate from highest to lowest

1.1.\[dataFrame\] excluding filter value and create data frame

``` r
dfNoEMEA <- data.frame(filter(dfAlc,region!="EMEA"))
nrow(dfNoEMEA)
```

    ## [1] 82

1.1.\[dataFrame\] including filter value and create data frame

``` r
dfEMEAAPAC <- data.frame(filter(dfAlc,region %in% c("EMEA","APAC")))
nrow(dfEMEAAPAC)
```

    ## [1] 173

1.1.\[dataFrame\] filter and select only

``` r
EMEAAPACAconsum10 <- filter(dfAlc,region %in% c("EMEA","APAC") & aconsum < 10)
select(EMEAAPACAconsum10,country,aconsum,rank)
```

    ##                   country aconsum  rank
    ## 1             Afghanistan       0 181.5
    ## 2                 Albania       7  81.5
    ## 3                 Algeria       1 164.0
    ## 4                  Angola       6  94.0
    ## 5                 Bahrain       4 122.5
    ## 6              Bangladesh       0 181.5
    ## 7                   Benin       2 147.5
    ## 8                  Bhutan       1 164.0
    ## 9                Botswana       7  81.5
    ## 10                 Brunei       2 147.5
    ## 11           Burkina Faso       7  81.5
    ## 12               Cambodia       5 107.5
    ## 13               Cameroon       8  71.5
    ## 14   Central African Rep,       3 136.5
    ## 15                   Chad       4 122.5
    ## 16                  China       6  94.0
    ## 17                Comoros       0 181.5
    ## 18       Congo, Dem, Rep,       3 136.5
    ## 19            Congo, Rep,       4 122.5
    ## 20           Cook Islands       3 136.5
    ## 21          Cote d'Ivoire       6  94.0
    ## 22                 Cyprus       9  61.0
    ## 23               Djibouti       2 147.5
    ## 24                  Egypt       0 181.5
    ## 25      Equatorial Guinea       6  94.0
    ## 26                Eritrea       2 147.5
    ## 27               Ethiopia       4 122.5
    ## 28                   Fiji       3 136.5
    ## 29                  Gabon       9  61.0
    ## 30                 Gambia       4 122.5
    ## 31                Georgia       7  81.5
    ## 32                  Ghana       3 136.5
    ## 33                 Guinea       1 164.0
    ## 34          Guinea-Bissau       4 122.5
    ## 35                Iceland       7  81.5
    ## 36                  India       3 136.5
    ## 37              Indonesia       1 164.0
    ## 38                   Iran       1 164.0
    ## 39                   Iraq       0 181.5
    ## 40                 Israel       3 136.5
    ## 41                  Japan       8  71.5
    ## 42                 Jordan       1 164.0
    ## 43                  Kenya       4 122.5
    ## 44               Kiribati       3 136.5
    ## 45                 Kuwait       0 181.5
    ## 46             Kyrgyzstan       5 107.5
    ## 47                   Laos       7  81.5
    ## 48                Lebanon       2 147.5
    ## 49                Lesotho       6  94.0
    ## 50                Liberia       5 107.5
    ## 51                  Libya       0 181.5
    ## 52         Macedonia, FYR       9  61.0
    ## 53             Madagascar       1 164.0
    ## 54                 Malawi       1 164.0
    ## 55               Malaysia       1 164.0
    ## 56                   Mali       1 164.0
    ## 57                  Malta       4 122.5
    ## 58             Mauritania       0 181.5
    ## 59              Mauritius       4 122.5
    ## 60  Micronesia, Fed, Sts,       5 107.5
    ## 61               Mongolia       3 136.5
    ## 62                Morocco       1 164.0
    ## 63             Mozambique       2 147.5
    ## 64                Myanmar       1 164.0
    ## 65                  Nauru       5 107.5
    ## 66                  Nepal       2 147.5
    ## 67                  Niger       0 181.5
    ## 68                   Niue       9  61.0
    ## 69                 Norway       8  71.5
    ## 70                   Oman       1 164.0
    ## 71               Pakistan       0 181.5
    ## 72       Papua New Guinea       4 122.5
    ## 73            Philippines       6  94.0
    ## 74                  Qatar       1 164.0
    ## 75                  Samoa       5 107.5
    ## 76  Sao Tome and Principe       8  71.5
    ## 77           Saudi Arabia       0 181.5
    ## 78                Senegal       1 164.0
    ## 79           Sierra Leone       9  61.0
    ## 80              Singapore       2 147.5
    ## 81        Solomon Islands       1 164.0
    ## 82                Somalia       1 164.0
    ## 83              Sri Lanka       1 164.0
    ## 84                  Sudan       3 136.5
    ## 85              Swaziland       5 107.5
    ## 86                  Syria       1 164.0
    ## 87             Tajikistan       3 136.5
    ## 88               Tanzania       8  71.5
    ## 89               Thailand       7  81.5
    ## 90            Timor-Leste       1 164.0
    ## 91                   Togo       2 147.5
    ## 92                  Tonga       4 122.5
    ## 93                Tunisia       1 164.0
    ## 94                 Turkey       3 136.5
    ## 95           Turkmenistan       5 107.5
    ## 96                 Tuvalu       2 147.5
    ## 97   United Arab Emirates       1 164.0
    ## 98             Uzbekistan       4 122.5
    ## 99                Vanuatu       1 164.0
    ## 100               Vietnam       4 122.5
    ## 101           Yemen, Rep,       0 181.5
    ## 102                Zambia       4 122.5
    ## 103              Zimbabwe       5 107.5

1.1.\[dataFrame\] select subsetting and filter

``` r
newTable <- select(dfAlc,country,region,aconsum) 
filter(newTable,aconsum <= 10)
```

    ##                              country region aconsum
    ## 1                        Afghanistan   EMEA       0
    ## 2                            Albania   EMEA       7
    ## 3                            Algeria   EMEA       1
    ## 4                            Andorra   EMEA      10
    ## 5                             Angola   EMEA       6
    ## 6                Antigua and Barbuda  LATAM       8
    ## 7                          Argentina  LATAM       9
    ## 8                          Australia   APAC      10
    ## 9                            Bahamas  LATAM       9
    ## 10                           Bahrain   EMEA       4
    ## 11                        Bangladesh   APAC       0
    ## 12                          Barbados  LATAM       6
    ## 13                           Belgium   EMEA      10
    ## 14                            Belize  LATAM       6
    ## 15                             Benin   EMEA       2
    ## 16                            Bhutan   APAC       1
    ## 17                           Bolivia  LATAM       6
    ## 18            Bosnia and Herzegovina   EMEA      10
    ## 19                          Botswana   EMEA       7
    ## 20                            Brazil  LATAM      10
    ## 21                            Brunei   APAC       2
    ## 22                      Burkina Faso   EMEA       7
    ## 23                           Burundi   EMEA      10
    ## 24                          Cambodia   APAC       5
    ## 25                          Cameroon   EMEA       8
    ## 26                            Canada             10
    ## 27                        Cape Verde              5
    ## 28              Central African Rep,   EMEA       3
    ## 29                              Chad   EMEA       4
    ## 30                             Chile  LATAM       9
    ## 31                             China   APAC       6
    ## 32                          Colombia  LATAM       7
    ## 33                           Comoros   EMEA       0
    ## 34                  Congo, Dem, Rep,   EMEA       3
    ## 35                       Congo, Rep,   EMEA       4
    ## 36                      Cook Islands   EMEA       3
    ## 37                        Costa Rica  LATAM       6
    ## 38                     Cote d'Ivoire   EMEA       6
    ## 39                              Cuba  LATAM       5
    ## 40                            Cyprus   EMEA       9
    ## 41                          Djibouti   EMEA       2
    ## 42                          Dominica  LATAM       9
    ## 43                    Dominican Rep,  LATAM       6
    ## 44                           Ecuador  LATAM       9
    ## 45                             Egypt   EMEA       0
    ## 46                       El Salvador  LATAM       4
    ## 47                 Equatorial Guinea   EMEA       6
    ## 48                           Eritrea   EMEA       2
    ## 49                          Ethiopia   EMEA       4
    ## 50                              Fiji   APAC       3
    ## 51                             Gabon   EMEA       9
    ## 52                            Gambia   EMEA       4
    ## 53                           Georgia   EMEA       7
    ## 54                             Ghana   EMEA       3
    ## 55                         Guatemala  LATAM       7
    ## 56                            Guinea   EMEA       1
    ## 57                     Guinea-Bissau   EMEA       4
    ## 58                            Guyana  LATAM       9
    ## 59                             Haiti  LATAM       6
    ## 60                          Honduras  LATAM       4
    ## 61                           Iceland   EMEA       7
    ## 62                             India   APAC       3
    ## 63                         Indonesia   APAC       1
    ## 64                              Iran   EMEA       1
    ## 65                              Iraq   EMEA       0
    ## 66                            Israel   EMEA       3
    ## 67                             Italy   EMEA      10
    ## 68                           Jamaica  LATAM       5
    ## 69                             Japan   APAC       8
    ## 70                            Jordan   EMEA       1
    ## 71                             Kenya   EMEA       4
    ## 72                          Kiribati   APAC       3
    ## 73                            Kuwait   EMEA       0
    ## 74                        Kyrgyzstan   EMEA       5
    ## 75                              Laos   APAC       7
    ## 76                           Lebanon   EMEA       2
    ## 77                           Lesotho   EMEA       6
    ## 78                           Liberia   EMEA       5
    ## 79                             Libya   EMEA       0
    ## 80                    Macedonia, FYR   EMEA       9
    ## 81                        Madagascar   EMEA       1
    ## 82                            Malawi   EMEA       1
    ## 83                          Malaysia   APAC       1
    ## 84                              Mali   EMEA       1
    ## 85                             Malta   EMEA       4
    ## 86                        Mauritania   EMEA       0
    ## 87                         Mauritius   EMEA       4
    ## 88                            Mexico  LATAM       9
    ## 89             Micronesia, Fed, Sts,   APAC       5
    ## 90                          Mongolia   APAC       3
    ## 91                           Morocco   EMEA       1
    ## 92                        Mozambique   EMEA       2
    ## 93                           Myanmar   APAC       1
    ## 94                             Nauru   APAC       5
    ## 95                             Nepal   APAC       2
    ## 96                       Netherlands   EMEA      10
    ## 97                       New Zealand   APAC      10
    ## 98                         Nicaragua  LATAM       5
    ## 99                             Niger   EMEA       0
    ## 100                             Niue   APAC       9
    ## 101                           Norway   EMEA       8
    ## 102                             Oman   EMEA       1
    ## 103                         Pakistan   APAC       0
    ## 104                            Palau   APAC      10
    ## 105                           Panama  LATAM       7
    ## 106                 Papua New Guinea   APAC       4
    ## 107                         Paraguay  LATAM       8
    ## 108                             Peru  LATAM       7
    ## 109                      Philippines   APAC       6
    ## 110                            Qatar   EMEA       1
    ## 111                           Rwanda   EMEA      10
    ## 112 Saint Vincent and the Grenadines  LATAM       5
    ## 113                            Samoa   APAC       5
    ## 114            Sao Tome and Principe   EMEA       8
    ## 115                     Saudi Arabia   EMEA       0
    ## 116                          Senegal   EMEA       1
    ## 117                     Sierra Leone   EMEA       9
    ## 118                        Singapore   APAC       2
    ## 119                  Solomon Islands   APAC       1
    ## 120                          Somalia   EMEA       1
    ## 121                     South Africa   EMEA      10
    ## 122                        Sri Lanka   APAC       1
    ## 123                            Sudan   EMEA       3
    ## 124                         Suriname  LATAM       7
    ## 125                        Swaziland   EMEA       5
    ## 126                           Sweden   EMEA      10
    ## 127                            Syria   EMEA       1
    ## 128                       Tajikistan   EMEA       3
    ## 129                         Tanzania   EMEA       8
    ## 130                         Thailand   APAC       7
    ## 131                      Timor-Leste   APAC       1
    ## 132                             Togo   EMEA       2
    ## 133                            Tonga   APAC       4
    ## 134              Trinidad and Tobago  LATAM       6
    ## 135                          Tunisia   EMEA       1
    ## 136                           Turkey   EMEA       3
    ## 137                     Turkmenistan   EMEA       5
    ## 138                           Tuvalu   APAC       2
    ## 139             United Arab Emirates   EMEA       1
    ## 140                    United States             10
    ## 141                          Uruguay  LATAM       9
    ## 142                       Uzbekistan   EMEA       4
    ## 143                          Vanuatu   APAC       1
    ## 144                        Venezuela  LATAM       8
    ## 145                          Vietnam   APAC       4
    ## 146                      Yemen, Rep,   EMEA       0
    ## 147                           Zambia   EMEA       4
    ## 148                         Zimbabwe   EMEA       5

``` r
str(newTable)
```

    ## 'data.frame':    213 obs. of  3 variables:
    ##  $ country: chr  "Afghanistan" "Albania" "Algeria" "Andorra" ...
    ##  $ region : chr  "EMEA" "EMEA" "EMEA" "EMEA" ...
    ##  $ aconsum: int  0 7 1 10 6 8 9 14 NA 10 ...

1.\[dataFrame\] pipe select and pipe filter

``` r
dfAlc %>% select(country,region,aconsum) %>% filter(aconsum <= 10)
```

    ##                              country region aconsum
    ## 1                        Afghanistan   EMEA       0
    ## 2                            Albania   EMEA       7
    ## 3                            Algeria   EMEA       1
    ## 4                            Andorra   EMEA      10
    ## 5                             Angola   EMEA       6
    ## 6                Antigua and Barbuda  LATAM       8
    ## 7                          Argentina  LATAM       9
    ## 8                          Australia   APAC      10
    ## 9                            Bahamas  LATAM       9
    ## 10                           Bahrain   EMEA       4
    ## 11                        Bangladesh   APAC       0
    ## 12                          Barbados  LATAM       6
    ## 13                           Belgium   EMEA      10
    ## 14                            Belize  LATAM       6
    ## 15                             Benin   EMEA       2
    ## 16                            Bhutan   APAC       1
    ## 17                           Bolivia  LATAM       6
    ## 18            Bosnia and Herzegovina   EMEA      10
    ## 19                          Botswana   EMEA       7
    ## 20                            Brazil  LATAM      10
    ## 21                            Brunei   APAC       2
    ## 22                      Burkina Faso   EMEA       7
    ## 23                           Burundi   EMEA      10
    ## 24                          Cambodia   APAC       5
    ## 25                          Cameroon   EMEA       8
    ## 26                            Canada             10
    ## 27                        Cape Verde              5
    ## 28              Central African Rep,   EMEA       3
    ## 29                              Chad   EMEA       4
    ## 30                             Chile  LATAM       9
    ## 31                             China   APAC       6
    ## 32                          Colombia  LATAM       7
    ## 33                           Comoros   EMEA       0
    ## 34                  Congo, Dem, Rep,   EMEA       3
    ## 35                       Congo, Rep,   EMEA       4
    ## 36                      Cook Islands   EMEA       3
    ## 37                        Costa Rica  LATAM       6
    ## 38                     Cote d'Ivoire   EMEA       6
    ## 39                              Cuba  LATAM       5
    ## 40                            Cyprus   EMEA       9
    ## 41                          Djibouti   EMEA       2
    ## 42                          Dominica  LATAM       9
    ## 43                    Dominican Rep,  LATAM       6
    ## 44                           Ecuador  LATAM       9
    ## 45                             Egypt   EMEA       0
    ## 46                       El Salvador  LATAM       4
    ## 47                 Equatorial Guinea   EMEA       6
    ## 48                           Eritrea   EMEA       2
    ## 49                          Ethiopia   EMEA       4
    ## 50                              Fiji   APAC       3
    ## 51                             Gabon   EMEA       9
    ## 52                            Gambia   EMEA       4
    ## 53                           Georgia   EMEA       7
    ## 54                             Ghana   EMEA       3
    ## 55                         Guatemala  LATAM       7
    ## 56                            Guinea   EMEA       1
    ## 57                     Guinea-Bissau   EMEA       4
    ## 58                            Guyana  LATAM       9
    ## 59                             Haiti  LATAM       6
    ## 60                          Honduras  LATAM       4
    ## 61                           Iceland   EMEA       7
    ## 62                             India   APAC       3
    ## 63                         Indonesia   APAC       1
    ## 64                              Iran   EMEA       1
    ## 65                              Iraq   EMEA       0
    ## 66                            Israel   EMEA       3
    ## 67                             Italy   EMEA      10
    ## 68                           Jamaica  LATAM       5
    ## 69                             Japan   APAC       8
    ## 70                            Jordan   EMEA       1
    ## 71                             Kenya   EMEA       4
    ## 72                          Kiribati   APAC       3
    ## 73                            Kuwait   EMEA       0
    ## 74                        Kyrgyzstan   EMEA       5
    ## 75                              Laos   APAC       7
    ## 76                           Lebanon   EMEA       2
    ## 77                           Lesotho   EMEA       6
    ## 78                           Liberia   EMEA       5
    ## 79                             Libya   EMEA       0
    ## 80                    Macedonia, FYR   EMEA       9
    ## 81                        Madagascar   EMEA       1
    ## 82                            Malawi   EMEA       1
    ## 83                          Malaysia   APAC       1
    ## 84                              Mali   EMEA       1
    ## 85                             Malta   EMEA       4
    ## 86                        Mauritania   EMEA       0
    ## 87                         Mauritius   EMEA       4
    ## 88                            Mexico  LATAM       9
    ## 89             Micronesia, Fed, Sts,   APAC       5
    ## 90                          Mongolia   APAC       3
    ## 91                           Morocco   EMEA       1
    ## 92                        Mozambique   EMEA       2
    ## 93                           Myanmar   APAC       1
    ## 94                             Nauru   APAC       5
    ## 95                             Nepal   APAC       2
    ## 96                       Netherlands   EMEA      10
    ## 97                       New Zealand   APAC      10
    ## 98                         Nicaragua  LATAM       5
    ## 99                             Niger   EMEA       0
    ## 100                             Niue   APAC       9
    ## 101                           Norway   EMEA       8
    ## 102                             Oman   EMEA       1
    ## 103                         Pakistan   APAC       0
    ## 104                            Palau   APAC      10
    ## 105                           Panama  LATAM       7
    ## 106                 Papua New Guinea   APAC       4
    ## 107                         Paraguay  LATAM       8
    ## 108                             Peru  LATAM       7
    ## 109                      Philippines   APAC       6
    ## 110                            Qatar   EMEA       1
    ## 111                           Rwanda   EMEA      10
    ## 112 Saint Vincent and the Grenadines  LATAM       5
    ## 113                            Samoa   APAC       5
    ## 114            Sao Tome and Principe   EMEA       8
    ## 115                     Saudi Arabia   EMEA       0
    ## 116                          Senegal   EMEA       1
    ## 117                     Sierra Leone   EMEA       9
    ## 118                        Singapore   APAC       2
    ## 119                  Solomon Islands   APAC       1
    ## 120                          Somalia   EMEA       1
    ## 121                     South Africa   EMEA      10
    ## 122                        Sri Lanka   APAC       1
    ## 123                            Sudan   EMEA       3
    ## 124                         Suriname  LATAM       7
    ## 125                        Swaziland   EMEA       5
    ## 126                           Sweden   EMEA      10
    ## 127                            Syria   EMEA       1
    ## 128                       Tajikistan   EMEA       3
    ## 129                         Tanzania   EMEA       8
    ## 130                         Thailand   APAC       7
    ## 131                      Timor-Leste   APAC       1
    ## 132                             Togo   EMEA       2
    ## 133                            Tonga   APAC       4
    ## 134              Trinidad and Tobago  LATAM       6
    ## 135                          Tunisia   EMEA       1
    ## 136                           Turkey   EMEA       3
    ## 137                     Turkmenistan   EMEA       5
    ## 138                           Tuvalu   APAC       2
    ## 139             United Arab Emirates   EMEA       1
    ## 140                    United States             10
    ## 141                          Uruguay  LATAM       9
    ## 142                       Uzbekistan   EMEA       4
    ## 143                          Vanuatu   APAC       1
    ## 144                        Venezuela  LATAM       8
    ## 145                          Vietnam   APAC       4
    ## 146                      Yemen, Rep,   EMEA       0
    ## 147                           Zambia   EMEA       4
    ## 148                         Zimbabwe   EMEA       5

1.\[dataFrame\] filter pipe select

``` r
filter(dfAlc, region %in% c("EMEA", "APAC") & aconsum < 10 )%>% select(country, aconsum, rank)
```

    ##                   country aconsum  rank
    ## 1             Afghanistan       0 181.5
    ## 2                 Albania       7  81.5
    ## 3                 Algeria       1 164.0
    ## 4                  Angola       6  94.0
    ## 5                 Bahrain       4 122.5
    ## 6              Bangladesh       0 181.5
    ## 7                   Benin       2 147.5
    ## 8                  Bhutan       1 164.0
    ## 9                Botswana       7  81.5
    ## 10                 Brunei       2 147.5
    ## 11           Burkina Faso       7  81.5
    ## 12               Cambodia       5 107.5
    ## 13               Cameroon       8  71.5
    ## 14   Central African Rep,       3 136.5
    ## 15                   Chad       4 122.5
    ## 16                  China       6  94.0
    ## 17                Comoros       0 181.5
    ## 18       Congo, Dem, Rep,       3 136.5
    ## 19            Congo, Rep,       4 122.5
    ## 20           Cook Islands       3 136.5
    ## 21          Cote d'Ivoire       6  94.0
    ## 22                 Cyprus       9  61.0
    ## 23               Djibouti       2 147.5
    ## 24                  Egypt       0 181.5
    ## 25      Equatorial Guinea       6  94.0
    ## 26                Eritrea       2 147.5
    ## 27               Ethiopia       4 122.5
    ## 28                   Fiji       3 136.5
    ## 29                  Gabon       9  61.0
    ## 30                 Gambia       4 122.5
    ## 31                Georgia       7  81.5
    ## 32                  Ghana       3 136.5
    ## 33                 Guinea       1 164.0
    ## 34          Guinea-Bissau       4 122.5
    ## 35                Iceland       7  81.5
    ## 36                  India       3 136.5
    ## 37              Indonesia       1 164.0
    ## 38                   Iran       1 164.0
    ## 39                   Iraq       0 181.5
    ## 40                 Israel       3 136.5
    ## 41                  Japan       8  71.5
    ## 42                 Jordan       1 164.0
    ## 43                  Kenya       4 122.5
    ## 44               Kiribati       3 136.5
    ## 45                 Kuwait       0 181.5
    ## 46             Kyrgyzstan       5 107.5
    ## 47                   Laos       7  81.5
    ## 48                Lebanon       2 147.5
    ## 49                Lesotho       6  94.0
    ## 50                Liberia       5 107.5
    ## 51                  Libya       0 181.5
    ## 52         Macedonia, FYR       9  61.0
    ## 53             Madagascar       1 164.0
    ## 54                 Malawi       1 164.0
    ## 55               Malaysia       1 164.0
    ## 56                   Mali       1 164.0
    ## 57                  Malta       4 122.5
    ## 58             Mauritania       0 181.5
    ## 59              Mauritius       4 122.5
    ## 60  Micronesia, Fed, Sts,       5 107.5
    ## 61               Mongolia       3 136.5
    ## 62                Morocco       1 164.0
    ## 63             Mozambique       2 147.5
    ## 64                Myanmar       1 164.0
    ## 65                  Nauru       5 107.5
    ## 66                  Nepal       2 147.5
    ## 67                  Niger       0 181.5
    ## 68                   Niue       9  61.0
    ## 69                 Norway       8  71.5
    ## 70                   Oman       1 164.0
    ## 71               Pakistan       0 181.5
    ## 72       Papua New Guinea       4 122.5
    ## 73            Philippines       6  94.0
    ## 74                  Qatar       1 164.0
    ## 75                  Samoa       5 107.5
    ## 76  Sao Tome and Principe       8  71.5
    ## 77           Saudi Arabia       0 181.5
    ## 78                Senegal       1 164.0
    ## 79           Sierra Leone       9  61.0
    ## 80              Singapore       2 147.5
    ## 81        Solomon Islands       1 164.0
    ## 82                Somalia       1 164.0
    ## 83              Sri Lanka       1 164.0
    ## 84                  Sudan       3 136.5
    ## 85              Swaziland       5 107.5
    ## 86                  Syria       1 164.0
    ## 87             Tajikistan       3 136.5
    ## 88               Tanzania       8  71.5
    ## 89               Thailand       7  81.5
    ## 90            Timor-Leste       1 164.0
    ## 91                   Togo       2 147.5
    ## 92                  Tonga       4 122.5
    ## 93                Tunisia       1 164.0
    ## 94                 Turkey       3 136.5
    ## 95           Turkmenistan       5 107.5
    ## 96                 Tuvalu       2 147.5
    ## 97   United Arab Emirates       1 164.0
    ## 98             Uzbekistan       4 122.5
    ## 99                Vanuatu       1 164.0
    ## 100               Vietnam       4 122.5
    ## 101           Yemen, Rep,       0 181.5
    ## 102                Zambia       4 122.5
    ## 103              Zimbabwe       5 107.5

1.\[dataFrame\] pipe filter select

``` r
dfAlc %>% mutate(aconsum, rank) %>% filter(region %in% c('EMEA','APAC') & aconsum <10) %>% select(country,aconsum,rank)
```

    ##                   country aconsum  rank
    ## 1             Afghanistan       0 181.5
    ## 2                 Albania       7  81.5
    ## 3                 Algeria       1 164.0
    ## 4                  Angola       6  94.0
    ## 5                 Bahrain       4 122.5
    ## 6              Bangladesh       0 181.5
    ## 7                   Benin       2 147.5
    ## 8                  Bhutan       1 164.0
    ## 9                Botswana       7  81.5
    ## 10                 Brunei       2 147.5
    ## 11           Burkina Faso       7  81.5
    ## 12               Cambodia       5 107.5
    ## 13               Cameroon       8  71.5
    ## 14   Central African Rep,       3 136.5
    ## 15                   Chad       4 122.5
    ## 16                  China       6  94.0
    ## 17                Comoros       0 181.5
    ## 18       Congo, Dem, Rep,       3 136.5
    ## 19            Congo, Rep,       4 122.5
    ## 20           Cook Islands       3 136.5
    ## 21          Cote d'Ivoire       6  94.0
    ## 22                 Cyprus       9  61.0
    ## 23               Djibouti       2 147.5
    ## 24                  Egypt       0 181.5
    ## 25      Equatorial Guinea       6  94.0
    ## 26                Eritrea       2 147.5
    ## 27               Ethiopia       4 122.5
    ## 28                   Fiji       3 136.5
    ## 29                  Gabon       9  61.0
    ## 30                 Gambia       4 122.5
    ## 31                Georgia       7  81.5
    ## 32                  Ghana       3 136.5
    ## 33                 Guinea       1 164.0
    ## 34          Guinea-Bissau       4 122.5
    ## 35                Iceland       7  81.5
    ## 36                  India       3 136.5
    ## 37              Indonesia       1 164.0
    ## 38                   Iran       1 164.0
    ## 39                   Iraq       0 181.5
    ## 40                 Israel       3 136.5
    ## 41                  Japan       8  71.5
    ## 42                 Jordan       1 164.0
    ## 43                  Kenya       4 122.5
    ## 44               Kiribati       3 136.5
    ## 45                 Kuwait       0 181.5
    ## 46             Kyrgyzstan       5 107.5
    ## 47                   Laos       7  81.5
    ## 48                Lebanon       2 147.5
    ## 49                Lesotho       6  94.0
    ## 50                Liberia       5 107.5
    ## 51                  Libya       0 181.5
    ## 52         Macedonia, FYR       9  61.0
    ## 53             Madagascar       1 164.0
    ## 54                 Malawi       1 164.0
    ## 55               Malaysia       1 164.0
    ## 56                   Mali       1 164.0
    ## 57                  Malta       4 122.5
    ## 58             Mauritania       0 181.5
    ## 59              Mauritius       4 122.5
    ## 60  Micronesia, Fed, Sts,       5 107.5
    ## 61               Mongolia       3 136.5
    ## 62                Morocco       1 164.0
    ## 63             Mozambique       2 147.5
    ## 64                Myanmar       1 164.0
    ## 65                  Nauru       5 107.5
    ## 66                  Nepal       2 147.5
    ## 67                  Niger       0 181.5
    ## 68                   Niue       9  61.0
    ## 69                 Norway       8  71.5
    ## 70                   Oman       1 164.0
    ## 71               Pakistan       0 181.5
    ## 72       Papua New Guinea       4 122.5
    ## 73            Philippines       6  94.0
    ## 74                  Qatar       1 164.0
    ## 75                  Samoa       5 107.5
    ## 76  Sao Tome and Principe       8  71.5
    ## 77           Saudi Arabia       0 181.5
    ## 78                Senegal       1 164.0
    ## 79           Sierra Leone       9  61.0
    ## 80              Singapore       2 147.5
    ## 81        Solomon Islands       1 164.0
    ## 82                Somalia       1 164.0
    ## 83              Sri Lanka       1 164.0
    ## 84                  Sudan       3 136.5
    ## 85              Swaziland       5 107.5
    ## 86                  Syria       1 164.0
    ## 87             Tajikistan       3 136.5
    ## 88               Tanzania       8  71.5
    ## 89               Thailand       7  81.5
    ## 90            Timor-Leste       1 164.0
    ## 91                   Togo       2 147.5
    ## 92                  Tonga       4 122.5
    ## 93                Tunisia       1 164.0
    ## 94                 Turkey       3 136.5
    ## 95           Turkmenistan       5 107.5
    ## 96                 Tuvalu       2 147.5
    ## 97   United Arab Emirates       1 164.0
    ## 98             Uzbekistan       4 122.5
    ## 99                Vanuatu       1 164.0
    ## 100               Vietnam       4 122.5
    ## 101           Yemen, Rep,       0 181.5
    ## 102                Zambia       4 122.5
    ## 103              Zimbabwe       5 107.5

1.\[dataFrame\]

``` r
ind <- heights$height > mean(heights$height)#How many individuals in the dataset are above average height?
sum(ind)
```

    ## [1] 532

``` r
ind <- heights$height > mean(heights$height) & (heights$sex =="Female")#How many individuals in the dataset are above average height and are female?
sum(ind)
```

    ## [1] 31

``` r
mean(heights$sex == "Female")# proportion of individuals in the dataset are female
```

    ## [1] 0.2266667

``` r
minH<- min(heights$height) 
ind <- match(minH, heights$height)#Use the match() function to determine the index of the first individual with the minimum height.
heights$sex[ind]#Subset the sex column of the dataset by the index in 4b to determine the individual’s sex.
```

    ## [1] Male
    ## Levels: Female Male

``` r
maxH <- max(heights$height)#Write code to create a vector x that includes the integers between the minimum and maximum heights (as numbers).
minH <- min(heights$height)
intgr <- c(minH:maxH)
intgr
```

    ##  [1] 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
    ## [26] 75 76 77 78 79 80 81 82

``` r
sum(!(intgr %in% heights$height))#How many of the integers in x are NOT heights in the dataset?
```

    ## [1] 3

``` r
heights <- mutate(heights, ht_cm = height*2.54)#create a new column of heights in centimeters named ht_cm
head(heights)
```

    ##      sex height  ht_cm
    ## 1   Male     75 190.50
    ## 2   Male     70 177.80
    ## 3   Male     68 172.72
    ## 4   Male     74 187.96
    ## 5   Male     61 154.94
    ## 6 Female     65 165.10

``` r
heights$ht_cm[18]
```

    ## [1] 162.56

``` r
mean(heights$ht_cm) 
```

    ## [1] 173.5405

``` r
females <- filter(heights, sex == "Female") #females by filtering the heights2 data to contain only female individuals.
head(females)
```

    ##      sex height  ht_cm
    ## 1 Female     65 165.10
    ## 2 Female     66 167.64
    ## 3 Female     62 157.48
    ## 4 Female     66 167.64
    ## 5 Female     64 162.56
    ## 6 Female     60 152.40

``` r
nrow(females)#How many females are in the heights2 dataset?
```

    ## [1] 238

``` r
mean(females$ht_cm)#What is the mean height of the females in centimeters?
```

    ## [1] 164.9461

1.\[vector\] define 1.1.\[vector\] rank elements’ values, through
indexing 1.1.1.\[object\]keep index of the elements’ values in
increasing order 1.1.1.1.\[dataFrame\] create 2.\[ggplot\]

``` r
abbCountry <- dfAlc$abbrv 
suicidesPer100 <- dfAlc$suicideper100
urbanRT <- dfAlc$urbanrt
region <- dfAlc$region
ranksAConsumption <- rank(dfAlc$aconsum,na.last = NA)
i <- order(dfAlc$aconsum)
df<- data.frame(country = abbCountry[i], suicide = suicidesPer100[i], rank = ranksAConsumption[i],urbanrate = urbanRT[i],region = region[i])
df %>%
  ggplot(aes(urbanrate, suicide, label=country,color=rank)) + geom_label()
```

    ## Warning: Removed 28 rows containing missing values (geom_label).

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

2.\[ggplot\]

``` r
dfAlc %>%
  ggplot(aes(urbanrt, employrt, label=abbrv, color=region)) + geom_label()
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

2.\[ggplot\]

``` r
dfAlc %>%
  ggplot(aes(urbanrt, employrt, label=abbrv, color=region)) + geom_label()
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

2.\[scatterPlot\]

``` r
plot(dfAlc$suicideper100,dfAlc$aconsum, xlab = "suicides/100 people", ylab="alcohol consumption")
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

``` r
plot(dfAlc$employrt,dfAlc$aconsum, xlab = "employee rate", ylab="alcohol consumption")
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-69-2.png)<!-- -->

``` r
plot(dfAlc$urbanrt,dfAlc$aconsum, xlab = "urban rate", ylab="alcohol consumption")
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-69-3.png)<!-- -->

2.\[scatterPlot\] in logs

``` r
log10IncomePer1 <- log10(dfAlc$incomeper1)
log10Aconsum <- log10(dfAlc$aconsum)
plot(log10IncomePer1,log10Aconsum)
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

2.\[histogram\]

``` r
hist(dfAlc$aconsum,xlab = "alcohol consumption") 
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
dfAlc$country[which.max(dfAlc$aconsum)]
```

    ## [1] "Moldova"

2.\[boxplot\]

``` r
boxplot(aconsum~region, data = dfAlc,na.action = NULL) 
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

``` r
boxplot(suicidesPer100~region, data = dfAlc)
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-72-2.png)<!-- -->

``` r
boxplot(employrt~region, data = dfAlc)
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-72-3.png)<!-- -->

``` r
boxplot(urbanrt~region, data = dfAlc)
```

![](%5BR%5D-The-Notebook_files/figure-gfm/unnamed-chunk-72-4.png)<!-- -->

## II.

## III.

## References
