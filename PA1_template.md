# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
stepsPerDay <- tapply(activity$steps, activity$date, sum)
hist(stepsPerDay, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## What is mean total number of steps taken per day?


```r
summary(stepsPerDay)[c('Mean','Median')]
```

```
##   Mean Median 
##  10770  10760
```

## What is the average daily activity pattern?


```r
meanIntervalSteps <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(names(meanIntervalSteps),meanIntervalSteps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The 5-minute interval that, on average, contains the maximum number of steps  

```r
names(meanIntervalSteps)[which.max(meanIntervalSteps)]
```

```
## [1] "835"
```

## Imputing missing values


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
activity$steps_imp <- ifelse(is.na(activity$steps), meanIntervalSteps[as.character(activity$interval)], activity$steps)
stepsPerDay <- tapply(activity$steps_imp, activity$date, sum)
hist(stepsPerDay, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
summary(stepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
It seems that the imputation did not affect the mean (as expected),  
but it caused the median to rise and become equal to the mean.

## Are there differences in activity patterns between weekdays and weekends?


```r
activity$weekday <- as.numeric(format(as.Date(activity$date),"%w"))
activity$is.weekend <- ifelse(activity$weekday == 6 | activity$weekday == 0, 1, 0)
wdActivity <- subset(activity, is.weekend == 0)
wdActivityIM <- tapply(wdActivity$steps_imp, wdActivity$interval, mean)
weActivity <- subset(activity, is.weekend == 1)
weActivityIM <- tapply(weActivity$steps_imp, weActivity$interval, mean)
par(mfrow = c(2,1), mar = c(5,5,2,2))
plot(names(wdActivityIM),wdActivityIM, type = "l", main = "Weekdays", xlab = "" , ylab = "Average Steps",xlim = c(0,2500))
plot(names(weActivityIM),weActivityIM, type = "l", main = "Weekends", xlab = "Interval", ylab = "Average Steps",xlim = c(0,2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
