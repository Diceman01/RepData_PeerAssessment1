# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First, we will load the required libraries.  Second we must read and summarize the code, in order to draw the histogram of steps taken each day.

```r
library(stringr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
library(ggplot2)
raw <- read.csv("C:/Users/Pierre/Documents/Github/RepData_PeerAssessment1/activity/activity.csv")
by_date <- group_by(raw, date)
steps_by_day <- summarize(by_date, total_steps = sum(steps))
ggplot(steps_by_day, aes(total_steps)) + geom_histogram()
```

![](PA1_files/figure-html/unnamed-chunk-1-1.png) 


## What is mean total number of steps taken per day?

```r
daily_mean <- as.numeric(mean(steps_by_day$total_steps, na.rm = TRUE))
daily_median <- median(steps_by_day$total_steps, na.rm = TRUE)
```
The mean of the daily steps taken is 1.0766189\times 10^{4} and the median is 10765.

## What is the average daily activity pattern?

```r
by_interval <- group_by(raw,interval)
avg_steps_per_interval <- summarize(by_interval, steps = mean(steps, na.rm = TRUE))
ggplot(avg_steps_per_interval, aes(interval, steps)) + geom_line()
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png) 

```r
max_interval_avg <- avg_steps_per_interval[avg_steps_per_interval$steps == max(avg_steps_per_interval$steps),]
```
The interval containing the maximum, on average, activity is 835, which has a value of 206.1698113.

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
