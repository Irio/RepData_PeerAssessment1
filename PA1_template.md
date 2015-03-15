---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: yes
---

## Loading and preprocessing the data

1. Load the data (i.e. read.csv())

```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
data.by.day <- aggregate(data$steps, by=list(data$date), FUN=sum)
names(data.by.day) <- c("date", "steps")
data.by.day
```

```
##          date steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

2. Make a histogram of the total number of steps taken each day


```r
library("ggplot2")

ggplot(aes(x=steps), data=data.by.day) +
    geom_histogram(binwidth=max(data.by.day$steps, na.rm=T)/30) +
    labs(x="Number of steps", y="Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(data.by.day$steps, na.rm=T)
```

```
## [1] 10766.19
```


```r
median(data.by.day$steps, na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data.by.interval <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=T)
names(data.by.interval) <- c("interval", "steps")
ggplot(data.by.interval) +
    geom_line(aes(x=interval, y=steps)) +
    labs(x="Interval", y="Number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
lines.with.highest.average.of.steps <- data.by.interval$steps == max(data.by.interval$steps)
data.by.interval[lines.with.highest.average.of.steps, ]$interval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
rows.with.nas <- data[is.na(data$steps) | is.na(data$date) | is.na(data$interval), ]
nrow(rows.with.nas)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**We are going to use the mean for the given 5-minute interval as a strategy for filling NA values.**

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.without.nas <- data
for (index in 1:nrow(data.without.nas)) {
    if (is.na(data.without.nas$steps[index])) {
        same.interval <- data.by.interval$interval ==
            data.without.nas[index, ]$interval
        mean.value <- mean(data.by.interval[same.interval, ]$steps)
        data.without.nas[index, ]$steps <- mean.value
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
data.by.day.without.nas <- aggregate(data.without.nas$steps, by=list(data.without.nas$date), FUN=sum)
names(data.by.day.without.nas) <- c("date", "steps")

ggplot(aes(x=steps), data=data.by.day.without.nas) +
    geom_histogram(binwidth=max(data.by.day$steps, na.rm=T)/30) +
    labs(x="Number of steps", y="Frequency")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


```r
mean(data.by.day.without.nas$steps)
```

```
## [1] 10766.19
```


```r
median(data.by.day.without.nas$steps)
```

```
## [1] 10766.19
```

**The impact of giving values to missing data is that the bias in these days will be reduced. When represented by a histogram, the graph will extend vertically.**

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
day.type <- weekdays(data.without.nas$date, abbreviate=T) %in% c("Sat", "Sun")
day.type[day.type] <- "weekend"
day.type[day.type == "FALSE"] <- "weekday"
data.without.nas$day.type <- relevel(as.factor(day.type), "weekend")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
ggplot(data.without.nas) +
    geom_line(aes(x=interval, y=steps), color=rgb(81/256, 158/256, 252/256)) +
    facet_wrap(~ day.type, ncol=1) +
    labs(x="Interval", y="Number of steps") +
    theme_bw() +
    theme(strip.background = element_rect(fill=rgb(254/256, 229/256, 206/256)))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
