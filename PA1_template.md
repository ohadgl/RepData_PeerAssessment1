#Project assignment
Code for reading in the dataset and/or processing the data

```r
actv <- read.csv('activity.csv')
actv$date <- as.Date(actv$date , "%Y-%m-%d")
```
Histogram of the total number of steps taken each day

```r
total_steps <- tapply(actv$steps, actv$date, sum)
hist(total_steps , breaks = 10 , main = "Total steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
Mean and median number of steps taken each day

```r
mean(total_steps , na.rm = T)
```

```
## [1] 10766.19
```

```r
median(total_steps , na.rm = T)
```

```
## [1] 10765
```
Time series plot of the average number of steps taken

```r
steps_minute <- tapply(actv$steps, as.factor(actv$interval), mean , na.rm = T)
plot(names(steps_minute) , steps_minute, type = "l" , main = "Steps per interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
The 5-minute interval that, on average, contains the maximum number of steps

```r
intervals <- names(steps_minute)
intervals[steps_minute == max(steps_minute)]
```

```
## [1] "835"
```
Code to describe and show a strategy for imputing missing data

```r
sum(complete.cases(actv) == F)
```

```
## [1] 2304
```

```r
empty <- actv[is.na(actv$steps) , "interval"]
actvfull <- actv
actvfull[is.na(actvfull$steps) , "steps"] <- steps_minute[as.character(empty)]
```
Histogram of the total number of steps taken each day after missing values are imputed

```r
total_steps <- tapply(actvfull$steps, actv$date, sum)
hist(total_steps , breaks = 10 , main = "Total steps (imputed data)")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
mean(total_steps , na.rm = T)
```

```
## [1] 10766.19
```

```r
median(total_steps , na.rm = T)
```

```
## [1] 10766.19
```
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
days <- weekdays(as.Date(c("20200314","20200313","20200312","20200311","20200310","20200309","20200308") , "%Y%m%d"))
daytype <- factor(x = c("weekend" ,rep("weekday",5),"weekend") , levels = c("weekday","weekend"))
names(daytype) <- days
actvfull$daytype <- daytype[weekdays(actvfull$date)]
weekday <- subset(actvfull , daytype=="weekday")
weekend <- subset(actvfull , daytype=="weekend")

par(mfrow=c(1,2))
steps_weekday <- tapply(weekday$steps, as.factor(weekday$interval), mean , na.rm = T)
plot(names(steps_weekday) , steps_weekday, type = "l" , main = "Steps per interval (weekday)")
steps_weekend <- tapply(weekend$steps, as.factor(weekend$interval), mean , na.rm = T)
plot(names(steps_weekend) , steps_weekend, type = "l" , main = "Steps per interval (weekend)")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
