#Course Project 1
##Loading and preprocessing the data:

```r
program1_file<-read.csv("activity.csv")
```

##What is mean total number of steps taken per day?
The total number of steps taken per day:

```r
steps_per_day<-aggregate(steps~date, program1_file, sum)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

A histogram of the total number of steps taken each day:

```r
hist(steps_per_day$steps, xlab="Steps", ylab="Days", main="Total Steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

The mean and median of the total number of steps taken per day:

```r
mean_steps<-mean(steps_per_day$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps<-median(steps_per_day$steps)
median_steps
```

```
## [1] 10765
```

##What is the average daily activity pattern?
A time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:

```r
steps_5minutes<-aggregate(steps~interval, program1_file, mean)
with(steps_5minutes, plot(interval, steps, type = "l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:

```r
steps_5minutes[which.max(steps_5minutes[,2]),1]
```

```
## [1] 835
```

##Imputing missing values by using median
The total number of missing values in the dataset:

```r
missing_data<-is.na(program1_file[,1])
count_of_missing_values <- sum(missing_data)
```
There are 'r count_of_missing_values' missing values

A strategy for filling in all of the missing values in the dataset:

```r
replacement_data<-median(steps_5minutes$steps)
```

A new dataset that is equal to the original dataset but with the missing data filled in:

```r
file_with_replaced_data<-program1_file
file_with_replaced_data[missing_data,1]<-replacement_data
```

A histogram of the total number of steps taken each day:

```r
steps_per_day_with_replaced_data<-aggregate(steps~date, file_with_replaced_data, sum)
hist(steps_per_day_with_replaced_data$steps, xlab="Steps", ylab="Days", main="Steps per day with Imputed Data")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

The mean and median total number of steps taken per day

```r
steps_per_day_with_replaced_data<-aggregate(steps~date, file_with_replaced_data, sum)
mean_with_imput<-mean(steps_per_day_with_replaced_data$steps)
mean_with_imput
```

```
## [1] 10642.7
```

```r
median_with_imput<-median(steps_per_day_with_replaced_data$steps)
median_with_imput
```

```
## [1] 10395
```
These values are slightly lower than those estimates from earlier without the imputed data.

##Are there differences in activity patterns between weekdays and weekends?

A new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day:

```r
library(dplyr)
file_with_replaced_data$date<-as.Date(file_with_replaced_data$date)
file_with_replaced_data_and_day_type<-file_with_replaced_data%>%
    mutate(day_type= ifelse(weekdays(file_with_replaced_data$date)=="Saturday" | weekdays(file_with_replaced_data$date)=="Sunday", "Weekend", "Weekday"))

average_step_by_day_type_and_interval<-file_with_replaced_data_and_day_type %>%
  group_by(day_type, interval) %>%
  summarize(average_step_by_day=sum(steps))
```

```
## `summarise()` has grouped output by 'day_type'. You can override using the `.groups` argument.
```

```r
head(average_step_by_day_type_and_interval)
```

```
## # A tibble: 6 x 3
## # Groups:   day_type [1]
##   day_type interval average_step_by_day
##   <chr>       <int>               <dbl>
## 1 Weekday         0                296.
## 2 Weekday         5                223.
## 3 Weekday        10                212.
## 4 Weekday        15                213.
## 5 Weekday        20                209.
## 6 Weekday        25                264.
```
A panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days:

```r
library(lattice)
with(average_step_by_day_type_and_interval, 
      xyplot(average_step_by_day ~ interval | day_type, 
      type = "l",      
      main = "Steps by Intervals by Day Type",
      xlab = "Daily Intervals",
      ylab = "Average Number of Steps"))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
