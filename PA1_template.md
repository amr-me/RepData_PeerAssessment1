# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
actData <- read.csv(unz("activity.zip", "activity.csv"), 
                    colClasses = c("numeric", "character","integer"), header = T)
```

Parse date in the order year, month, and days 

```r
library(lubridate)
actData$date <- ymd(actData$date)
```

## What is mean total number of steps taken per day?

```r
totSteps <- tapply(actData$steps, actData$date, FUN = sum, na.rm = TRUE)
mean(totSteps)
```

```
## [1] 9354.23
```

Also the median could be calculated as follows


```r
median(totSteps)
```

```
## [1] 10395
```

For the total number of steps taken by day 

```r
library(plyr)
```

```
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps <- print(summarize(group_by(filter(actData, !is.na(steps)), date), steps = sum(steps)))
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```
## What is the average daily activity pattern?
The histogram for the average daily activity


```r
library(ggplot2)
ggplot(steps, aes(x=date, y=steps)) + 
        geom_histogram(stat="identity", colour = "black", fill = "green") +
#        geom_vline(aes(xintercept=mean(, na.rm=T)),   # Ignore NA values for mean
#               color="red", linetype="dashed", size=1) +
        xlab("Dates") + ylab("Steps") + 
        labs(title= "Total numbers of Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
