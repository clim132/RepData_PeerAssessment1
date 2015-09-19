---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This document describes the processing of the activity monitoring data of an individual.  Data is taken from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip on Sep 18 2015.  It is then unzipped into a single .csv file.  

Before running this markdown file in R studio, first run setwd() to get to the folder containing the .csv file.

------------------------------------

## Loading and preprocessing the data

## What is mean total number of steps taken per day?

1.  What is mean total number of steps taken per day?  We read in the CSV file and ignore the missing values.


```r
dataset <- read.csv("activity.csv")
sum(dataset$steps, na.rm=TRUE)
```

```
## [1] 570608
```

2.  Plot histogram of total number of steps taken each day


```r
summary <- tapply(dataset$steps, dataset$date, FUN=sum)
stepsPerDay <- array(summary)
hist(stepsPerDay)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

3a.  Mean of total number of steps taken each day - ignore missing values


```r
mean(stepsPerDay, na.rm=TRUE)
```

```
## [1] 10766.19
```

3b.  Mediean of total number of steps taken each day - ignore missing values


```r
median(stepsPerDay, na.rm=TRUE)
```

```
## [1] 10765
```

------------------------------------

## What is the average daily activity pattern?

1.  Time series plot of 5-min interval (x-axis) and average number of steps across all days (y-axis), ignoring NA values


```r
summary2 <- tapply(dataset$steps, dataset$interval, FUN=mean, na.rm=TRUE)
MeanSteps <- array(summary2)
TimeIntervals <- dimnames(summary2)[[1]]
plot(TimeIntervals, MeanSteps, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2.  5-min interval with max number of steps on average


```r
TimeIntervals[which.max(MeanSteps)]
```

```
## [1] "835"
```

------------------------------------

## Inputing missing values

1.  Total number of missing values in the dataset 


```r
sum(is.na(dataset$steps))
```

```
## [1] 2304
```

2.  Use the mean for that 5-min interval to fill in missing values; display first few values


```r
head(MeanSteps)
```

```
## [1] 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

3.  Create new dataset with missing values filled in as in #2; display first few values


```r
dataset2 <- dataset
dataset2$steps <- ifelse(is.na(dataset2$steps),MeanSteps,dataset2$steps)
head(dataset2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4a.  Redo histogram of total number of steps taken each day


```r
summary3 <- tapply(dataset2$steps, dataset2$date, FUN=sum)
stepsPerDay3 <- array(summary3)
hist(stepsPerDay3)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Histogram now looks different.

4b.  Mean of total number of steps taken each day - no need to ignore missing values


```r
mean(stepsPerDay3)
```

```
## [1] 10766.19
```

4c.  Mediean of total number of steps taken each day - no need to ignore missing values


```r
median(stepsPerDay3)
```

```
## [1] 10766.19
```

Mean has not changed from first part of assignment.
Median has changed from first part of assignment
Inputing missing data increases the total daily number of steps.

------------------------------------

## Are there differences in activity patterns between weekdays and weekends?

1. Create new factor variable in new dataset with 2 levels - "weekday" and "weekend".  Display first few values


```r
dataset2$weekdayOrWeekend <- ifelse((weekdays(as.Date(dataset2$date)) %in% 
                                       c("Saturday", "Sunday")),
                                    "weekend","weekday")
head(dataset2)                            
```

```
##       steps       date interval weekdayOrWeekend
## 1 1.7169811 2012-10-01        0          weekday
## 2 0.3396226 2012-10-01        5          weekday
## 3 0.1320755 2012-10-01       10          weekday
## 4 0.1509434 2012-10-01       15          weekday
## 5 0.0754717 2012-10-01       20          weekday
## 6 2.0943396 2012-10-01       25          weekday
```

2.  Time series plot of 5-min interval (x-axis) and average number of steps across all days (y-axis); no need to ignore NA values


```r
weekdayData <- dataset2[dataset2$weekdayOrWeekend=="weekday",]
weekendData <- dataset2[dataset2$weekdayOrWeekend=="weekend",]
summaryWeekday <- tapply(weekdayData$steps, weekdayData$interval, FUN=mean)
summaryWeekend <- tapply(weekendData$steps, weekendData$interval, FUN=mean)

par(mfrow=c(2,1))
MeanStepsWeekday <- array(summaryWeekday)
TimeIntervals <- dimnames(summaryWeekday)[[1]]
plot(TimeIntervals, MeanStepsWeekday, type="l",main="Weekday")
MeanStepsWeekend <- array(summaryWeekend)
TimeIntervals <- dimnames(summaryWeekend)[[1]]
plot(TimeIntervals, MeanStepsWeekend, type="l",main="Weekend")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

