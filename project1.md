Reproducible Research Project 1
==================================

### Laoding the data 


```r
data = read.csv('activity.csv')
```

### What is mean total number of steps taken per day?

- Ignore the missing values 


```r
data_clean = na.omit(data)
data_clean$date = factor(data_clean$date)
step_total_day = tapply(data_clean$steps, data_clean$date, sum)
```

- Make a histogram of the total number of steps taken each day


```r
hist(step_total_day, 10)
```

![plot of chunk unnamed-chunk-3](./project1_files/figure-html/unnamed-chunk-3.png) 

- Calculate and report the mean and median total number of steps taken per day


```r
mean(step_total_day)
```

```
## [1] 10766
```

```r
median(step_total_day)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
step_interval = aggregate(data_clean$steps ~ data_clean$interval, FUN=mean)
interval = unique(data_clean$interval)
plot(interval, step_interval[,2], type = 'l', 
     xlab = 'interval', ylab = 'averaged step', 
     main = '5min interval averaged steps')
```

![plot of chunk unnamed-chunk-5](./project1_files/figure-html/unnamed-chunk-5.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval[which.max(step_interval[,2])]
```

```
## [1] 835
```

### Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
n_NA = dim(data)[1] - dim(data_clean)[1]
```

- Filling in all of the missing values in the dataset with the mean for that 5-minute interval

- Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
idx_na = is.na(data$steps)
data_na = data[idx_na,]
idx_temp = data_na$interval
idx_na_interval = (idx_temp %/% 100) * 12 + (idx_temp %% 100) / 5 + 1

data_new = data
data_new[idx_na,]$steps = step_interval[idx_na_interval, 2]
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
step_total_day1 = tapply(data_new$steps, data_new$date, sum)
hist(step_total_day1, 10)
```

![plot of chunk unnamed-chunk-9](./project1_files/figure-html/unnamed-chunk-9.png) 

```r
mean(step_total_day1)
```

```
## [1] 10766
```

```r
median(step_total_day1)
```

```
## [1] 10766
```

From the figures and numbers, since we are actually imputing with averaged 5min interval data, it has zero and very small impact on the mean and median values.

### Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekday = weekdays(as.Date(as.character(data_new$date)))
data_new$weekday = factor(weekday)
levels(data_new$weekday) = c('1', '1', '0', '0', '1', '1', '1')
```

- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
step_int1 = tapply(data_new[data_new$weekday=='1',]$steps, data_new[data_new$weekday=='1',]$interval, FUN=mean)

step_int2 = tapply(data_new[data_new$weekday=='0',]$steps, data_new[data_new$weekday=='0',]$interval, FUN=mean)

par(mfrow=c(2,1))
plot(interval, step_int1, type = 'l', xlab = 'interval', 
     ylab = 'averaged step', main = '5min averaged steps in Weekdays')
plot(interval, step_int2, type = 'l', xlab = 'interval', 
     ylab = 'averaged step', main = '5min averaged steps in Weekends')
```

![plot of chunk unnamed-chunk-11](./project1_files/figure-html/unnamed-chunk-11.png) 

