---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data





```r
#load libraries
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.5.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
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
#load data from CSV file
activity_data <- read.csv('C:/Users/smatismi/OneDrive - Capgemini/Documents/R/win-library/3.5/rmarkdown/activity.csv')

str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
steps_day <- tapply(activity_data$steps, activity_data$date, sum, na.rm=TRUE)
steps_day <- steps_day[!is.na(steps_day)]

str(steps_day)
```

```
##  int [1:61(1d)] 0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
```


```r
hist(x=steps_day, col="green", breaks = 20, main= "Steps Per Day", xlab = "Total steps", ylab ="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#cleansteps_day <-steps_day[!is.na(steps_day$steps,)]
```

## What is mean total number of steps taken per day?


```r
mean_st <- mean(steps_day)
mean_st
```

```
## [1] 9354.23
```


```r
med_st <-median(steps_day)
med_st
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
#cleansteps_day <-steps_day[!is.na(steps_day$steps),]
#intervalTb <- steps_day[ ,c (lapply(.SD, mean, na.rm=TRUE)), .SDcols =c("steps"), by =.(interval)]
#intervalTb <- ddply(steps_day, .(interval), summarize, Ave = mean(steps))
interval <- activity_data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(Avg_steps = mean(steps))
  
ggplot(interval, aes(x=interval , y=Avg_steps)) + geom_line(color='green', size=1) + labs(title ="Average Steps per Day", x ="interval", y = "average steps per day" )
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
#interval[steps == max(steps), .(max_interval <- interval)]
max_steps <- max(interval$Avg_steps)
interval[interval$Avg_steps == max_steps, ]
```

```
## # A tibble: 1 x 2
##   interval Avg_steps
##      <int>     <dbl>
## 1      835      206.
```


## Imputing missing values

##imputing missing values, calculate the total number of missing values

```r
nrow(activity_data[is.na(activity_data$steps),])
```

```
## [1] 2304
```

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#substute missing values with the mean

```r
activity_data_fill <- activity_data
nmv <- is.na(activity_data_fill$steps)
step_ave <- tapply(activity_data_fill$steps, activity_data_fill$interval, mean, na.rm=TRUE)
activity_data_fill$steps[nmv] <- step_ave[as.character(activity_data_fill$interval[nmv])]

new_activity <- tapply(activity_data_fill$steps, activity_data_fill$date, sum, na.rm = TRUE)
hist(x=new_activity, col="green", breaks=20, xlab="Daily Steps", ylab="Frequency", main="Steps per Day missing values filled with mean")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
mean_fill <- mean(new_activity)
mean_fill
```

```
## [1] 10766.19
```


```r
median_fill <- median(new_activity)
median_fill
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
##Are there differences in activity patterns between weekdays and weekends?

```r
#is_wk_day <- function(d) {
 # wd <-weekdays(d)
#  ifelse (wd == "Saturday" | wd == "Sunday", #"weekend" , "weekday")
#}

#we <- sapply(activity_data_fill$date, is_wk_day)
#activity_data$wk <-  as.factor(we)
#head(activity_data_fill)
activity_data_fill$realdate <- as.Date(activity_data_fill$date, format = "%Y-%m-%d")
activity_data_fill$weekday <- weekdays(activity_data_fill$realdate)
activity_data_fill$weekorend <- ifelse(activity_data_fill$weekday =="Saturday" | activity_data_fill$weekday =="Sunday", 'weekend', 'weekday' )
head(activity_data_fill)
```

```
##       steps       date interval   realdate weekday weekorend
## 1 1.7169811 2012-10-01        0 2012-10-01  Monday   weekday
## 2 0.3396226 2012-10-01        5 2012-10-01  Monday   weekday
## 3 0.1320755 2012-10-01       10 2012-10-01  Monday   weekday
## 4 0.1509434 2012-10-01       15 2012-10-01  Monday   weekday
## 5 0.0754717 2012-10-01       20 2012-10-01  Monday   weekday
## 6 2.0943396 2012-10-01       25 2012-10-01  Monday   weekday
```


```r
steptable <- aggregate(steps~interval+weekorend, data=activity_data_fill, FUN = mean)
head(steptable)
```

```
##   interval weekorend      steps
## 1        0   weekday 2.25115304
## 2        5   weekday 0.44528302
## 3       10   weekday 0.17316562
## 4       15   weekday 0.19790356
## 5       20   weekday 0.09895178
## 6       25   weekday 1.59035639
```


```r
steptable$time <- steptable$interval/100
head(steptable)
```

```
##   interval weekorend      steps time
## 1        0   weekday 2.25115304 0.00
## 2        5   weekday 0.44528302 0.05
## 3       10   weekday 0.17316562 0.10
## 4       15   weekday 0.19790356 0.15
## 5       20   weekday 0.09895178 0.20
## 6       25   weekday 1.59035639 0.25
```

```r
J<-ggplot(steptable, aes(time, steps))
J+geom_line(col="green") + ggtitle("Average Steps per 5 Minute Interval, Weekday verses Weekend") +xlab("time")+ylab("steps")+facet_wrap(~weekorend, ncol= 1, nrow =2)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
