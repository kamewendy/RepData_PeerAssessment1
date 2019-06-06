---
output: 
  html_document: 
    keep_md: yes
---
# Reproducible Research Course Project 1

=============================================================================



- Load the raw data into RStudio


```r
activity <- read.csv("/Users/xiaoweizhao/Downloads/activity.csv")
```

- Add one colume of the raw data to indicate the Date

```r
activity$Date <- as.Date(as.factor(activity$date))
```

- Remove all the NA rows 

```r
activitynona <- na.omit(activity)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
total <- aggregate(steps ~ Date, activitynona, sum)

## First 6 line of total number of steps 
head(total)
```

```
##         Date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. Make a histogram of the total number of steps taken each day

```r
hist(total$steps, main = "Histogram for total number of steps taken per day",
     xlab = "steps", breaks=c(seq(0,23000,1000)))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
## The mean steps:
meansteps <- mean(total$steps)
meansteps
```

```
## [1] 10766.19
```

```r
## The median steps:
mediansteps <- median(total$steps)
mediansteps
```

```
## [1] 10765
```
The mean of the total number of steps taken per day is 1.0766189\times 10^{4}.  The median of the total number of steps taken per day is 10765.

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
interval <- aggregate(steps ~ interval, activitynona, mean)
plot(interval$interval,interval$steps, type = "l",
     xlab = "5-minute interval", ylab = "steps",
     main = "Average number of steps taken during the 5-minute interval pattern",
     col = "red")
```

![](PA1_template_files/figure-html/calculation-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## The maximum number of average steps is:
max_step <- max(interval$steps)
max_step
```

```
## [1] 206.1698
```

```r
## The 5-minute interval contains the maximum number of steps is:
max_interval <- interval[which.max(interval$steps), "interval"]
max_interval
```

```
## [1] 835
```

Therefore, the 5-minute interval contains the maximum number of average steps (206.1698113 steps) is the interval 835.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
## Number of NAs in steps column in original data
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
## Number of NAs in interval column in original data
sum(is.na(activity$interval))
```

```
## [1] 0
```

```r
## Number of NAs in Date column in original data
sum(is.na(activity$Date))
```

```
## [1] 0
```

Based on above information, NAs only occurs in "steps" column, which is total 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*I will impute the missing values by using the mean for that 5-minute interval. *


```r
## Generate a function to get the mean steps based on corresponded 5-minute interval
getMeanSteps <- function(interval_number) {
        interval[which(interval$interval == interval_number), "steps"]
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
## Create a new dataset
complete.activity <- activity

## Fill in the missing data by its mean steps during that 5-minute interval
j <- 1
for (i in 1:nrow(activity)) {
        if(is.na(complete.activity$steps[j])) {
                complete.activity$steps[j] <- getMeanSteps(complete.activity$interval[j])
        }
        j <- j+1
}

## Show first 6 lines of completed dataset
head(complete.activity)
```

```
##       steps       date interval       Date
## 1 1.7169811 2012-10-01        0 2012-10-01
## 2 0.3396226 2012-10-01        5 2012-10-01
## 3 0.1320755 2012-10-01       10 2012-10-01
## 4 0.1509434 2012-10-01       15 2012-10-01
## 5 0.0754717 2012-10-01       20 2012-10-01
## 6 2.0943396 2012-10-01       25 2012-10-01
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## Calculate the total number of steps taken each day
total.complete.activity <- aggregate(steps ~ Date, complete.activity, sum)

## Make a histogram to show difference. 
## Plot the imputed dataset
hist(total.complete.activity$steps, 
     main = "Histogram for total number of steps taken per day", 
     col = "violet",
     xlab = "steps", 
     breaks=c(seq(0,23000,1000)),
     ylim = c(0,20))
## Add the non-imputed dataset
hist(total$steps, 
     main = "Histogram for total number of steps taken per day", 
     col = "wheat",
     xlab = "steps", 
     breaks=c(seq(0,23000,1000)),
     ylim = c(0,20),
     add = T)
## Add legend to compare the difference
legend("topright", c("Imputed", "Non-imputed"), col=c("violet", "wheat"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Based on the graph above, we could see that the shape of two histograms are quite similar. However, the frequency of the number of steps (10000-11000) is higher (about 8 more) in imputed dataset than that in non-imputed dataset. 


```r
## Mean total number of steps per day
mean(total.complete.activity$steps)
```

```
## [1] 10766.19
```

```r
## Median total number of steps per day
median(total.complete.activity$steps)
```

```
## [1] 10766.19
```

After imputing missing values, the mean total number of stpes of the complete dataset are not different from that of the original dataset with missing values, since we use the mean steps of 5-minute interval to impute the missing values. The median total number of steps is different from the original one, since the median index is now being changed after imputing missing values.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
## Create a new dataframe for this promblem set specifically. 
complete.activity.weekdays <- complete.activity

## Create a new factor variable with two levels: "Weekday" and "Weekend"
complete.activity.weekdays$which.day <- 
        as.factor(ifelse(weekdays(complete.activity.weekdays$Date) %in% 
                                 c("Saturday", "Sunday"), "Weekend", "Weekday"))

## Show first 6 lines of the dataset
head(complete.activity.weekdays)
```

```
##       steps       date interval       Date which.day
## 1 1.7169811 2012-10-01        0 2012-10-01   Weekday
## 2 0.3396226 2012-10-01        5 2012-10-01   Weekday
## 3 0.1320755 2012-10-01       10 2012-10-01   Weekday
## 4 0.1509434 2012-10-01       15 2012-10-01   Weekday
## 5 0.0754717 2012-10-01       20 2012-10-01   Weekday
## 6 2.0943396 2012-10-01       25 2012-10-01   Weekday
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
## Calculate the average number of steps for each interval across all weekend and weekday. 
graph <- aggregate(steps ~ interval + which.day, complete.activity.weekdays, mean)

## Make a panel plot
library(lattice)

xyplot(graph$steps ~ graph$interval|graph$which.day, main="Average number of steps taken across weekend and weekday",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
