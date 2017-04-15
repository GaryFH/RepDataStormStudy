# Storm Data Analysis of National Weather Service Data



#                       Synopsis
#####    Storms and other severe weather events can cause both public health and economic problems for communities and municipalities.  
#####This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States.


#####    This project attemps to answer two questions:  
#####1) Across the United States, which types of events (as indicated in the "EVTYPE" variable) are most harmful with respect to population health?
#####2) Across the United States, which types of events have the greatest economic consequences?


#                       Data Processing
##### The data ("repdata_data_StormData.csv.bz2") was provided as part of the assignment documents and is included in the Github repo (https://github.com/GaryFH/RepDataStormStudy).  The data is "read" in, converted to a tbl_df and stored as "d1" with the following code:


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
c1<-read.csv("repdata_data_StormData.csv.bz2",stringsAsFactors = FALSE)
d1<-tbl_df(c1)
```
##### The data is initially examined with the following code:


```r
d1
```

```
## # A tibble: 902,297 × 37
##    STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
##      <dbl>              <chr>    <chr>     <chr>  <dbl>      <chr> <chr>
## 1        1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2        1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3        1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4        1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5        1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6        1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
## 7        1 11/16/1951 0:00:00     0100       CST      9     BLOUNT    AL
## 8        1  1/22/1952 0:00:00     0900       CST    123 TALLAPOOSA    AL
## 9        1  2/13/1952 0:00:00     2000       CST    125 TUSCALOOSA    AL
## 10       1  2/13/1952 0:00:00     2000       CST     57    FAYETTE    AL
## # ... with 902,287 more rows, and 30 more variables: EVTYPE <chr>,
## #   BGN_RANGE <dbl>, BGN_AZI <chr>, BGN_LOCATI <chr>, END_DATE <chr>,
## #   END_TIME <chr>, COUNTY_END <dbl>, COUNTYENDN <lgl>, END_RANGE <dbl>,
## #   END_AZI <chr>, END_LOCATI <chr>, LENGTH <dbl>, WIDTH <dbl>, F <int>,
## #   MAG <dbl>, FATALITIES <dbl>, INJURIES <dbl>, PROPDMG <dbl>,
## #   PROPDMGEXP <chr>, CROPDMG <dbl>, CROPDMGEXP <chr>, WFO <chr>,
## #   STATEOFFIC <chr>, ZONENAMES <chr>, LATITUDE <dbl>, LONGITUDE <dbl>,
## #   LATITUDE_E <dbl>, LONGITUDE_ <dbl>, REMARKS <chr>, REFNUM <dbl>
```
##### 985 different types of weather events - future investigations may want to look and combining some of these events (i.e all heat related events) The following code produces a plot that shows the top eight weather events based on fatalities.


```r
d2<-group_by(d1,EVTYPE)
d3<-select(d2,EVTYPE,FATALITIES,INJURIES)
d4<-filter(d3,FATALITIES,!is.na(FATALITIES))
d5<-summarise(d4,FATALITIES=sum(FATALITIES))
d6<-arrange(d5,desc(FATALITIES))
d7<-head(d6,8)
g<-ggplot(d7,aes(EVTYPE,FATALITIES))
plotfatal<-g+geom_col(fill="yellow")+ geom_text(aes(label=FATALITIES), vjust=1.5, color="black")

print(plotfatal,fig.width=8,fig.height=4)
```

![](RepDataStormStudy_files/figure-html/fatality-1.png)<!-- -->



