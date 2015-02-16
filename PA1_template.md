# Activity Monitoring Data - repdata-011- Assignment 1
AE  
Saturday, February 14, 2015  

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 

1. Load the data and perform transformations needed.


```r
# Clear the workspace
rm(list=ls())
# Load the raw activity data
amd_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)

# Transform the date attribute to an actual date format
amd_raw$date <- as.POSIXct(amd_raw$date, format="%Y-%m-%d")

# Compute the weekdays from the date attribute
amd_raw <- data.frame(date=amd_raw$date, 
    weekday=tolower(weekdays(amd_raw$date)), 
    steps=amd_raw$steps, 
    interval=amd_raw$interval)

# Compute the day type (weekend or weekday)
amd_raw <- cbind(amd_raw, daytype=ifelse(amd_raw$weekday == "saturday" | 
                amd_raw$weekday == "sunday", "weekend", "weekday"))

# Create the final data.frame
amd <- data.frame(date=amd_raw$date, 
     weekday = amd_raw$weekday, 
     daytype = amd_raw$daytype, 
     interval =amd_raw$interval,
     steps= amd_raw$steps)
```

In this part clear the workspace and display the first few rows of the activity data frame:


```r
# Clear the workspace
rm(amd_raw)

head(amd)
```

```
##         date weekday daytype interval steps
## 1 2012-10-01  monday weekday        0    NA
## 2 2012-10-01  monday weekday        5    NA
## 3 2012-10-01  monday weekday       10    NA
## 4 2012-10-01  monday weekday       15    NA
## 5 2012-10-01  monday weekday       20    NA
## 6 2012-10-01  monday weekday       25    NA
```


What is the mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day


```r
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(amd$steps, by=list(amd$date), FUN=sum, na.rm=TRUE)
# Rename the attributes
names(sum_data) <- c("date", "total")
```


In this part it's displayed the first few rows of the sum_data data frame:

```r
head(sum_data)
```

```
##         date total
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```


The histogram is given by the following lines of code:


```r
# Compute the histogram of the total number of steps each day
hist(sum_data$total, breaks=seq(from=0, to=25000, by=2500),
     col="green", xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Calculate and report the mean and median total number of steps taken per day
The mean and median are computed like


```r
mean(sum_data$total)
```

```
## [1] 9354.23
```

```r
median(sum_data$total)
```

```
## [1] 10395
```
These formulas gives a mean and median of 9354 and 10395 respectively.

What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Clear the workspace
rm(sum_data)

# Compute the means of steps accross all days for each interval
mean_data <- aggregate(amd$steps, by=list(amd$interval), FUN=mean, na.rm=TRUE)

# Rename the attributes
names(mean_data) <- c("interval", "mean")
```

Here display the first few rows of the mean_data data frame:

```r
head(mean_data)
```

```
##   interval      mean
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

The time serie plot is created by the following lines of code

```r
# Compute the time series plot
plot(mean_data$interval, 
     mean_data$mean, type="l", col="blue", lwd=2, 
     xlab="Interval [minutes]", ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# We find the position of the maximum mean
max_pos <- which(mean_data$mean == max(mean_data$mean))

# We lookup the value of interval at this position
max_interval <- mean_data[max_pos, 1]

# Clear the workspace
rm(max_pos, mean_data)
```

The 5-minute interval that contains the maximum of steps, on average across all days, is 835.

Inputing the missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAâs)


```r
# Clear the workspace
rm(max_interval)

# We use the trick that a TRUE boolean value is equivalent to 1 and a FALSE to 0.
NA_count <- sum(is.na(amd$steps))
```
The number of NAâs is 2304.

Device a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Clear the workspace
rm(NA_count)

# Find the NA positions
na_pos <- which(is.na(amd$steps))

# Create a vector of means
mean_vec <- rep(mean(amd$steps, na.rm=TRUE), times=length(na_pos))
```

We use the strategy to remplace each NA value by the mean of the steps attribute.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# Replace the NAs by the means
amd[na_pos, "steps"] <- mean_vec

# Clear the workspace
rm(mean_vec, na_pos)
```
We display the first few rows of the new activity data frame:

```r
head(amd)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(amd$steps, by=list(amd$date), FUN=sum)

# Rename the attributes
names(sum_data) <- c("date", "total")

# Compute the histogram of the total number of steps each day
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 

The mean and median are computed like

```r
mean(sum_data$total)
```

```
## [1] 10766.19
```

```r
median(sum_data$total)
```

```
## [1] 10766.19
```

These formulas gives a mean and median of 10766 and 10766 respectively.

These values differ greatly from the estimates from the first part of the assignment. The impact of imputing the missing values is to have more data, hence to obtain a bigger mean and median value.

Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - âweekdaysâ and âweekendâ indicating whether a given date is a weekday or weekend day.

```r
# The new factor variable "daytype" was already in the activity data frame
head(amd)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
# Clear the workspace
rm(sum_data)

# Load the lattice graphical library
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(amd$steps, 
                       by=list(amd$daytype, amd$weekday, amd$interval), mean)

# Rename the attributes
names(mean_data) <- c("daytype", "weekday", "interval", "mean")
```

We display the first few rows of the mean_data data frame:

```r
head(mean_data)
```

```
##   daytype  weekday interval     mean
## 1 weekday   friday        0 8.307244
## 2 weekday   monday        0 9.418355
## 3 weekend saturday        0 4.672825
## 4 weekend   sunday        0 4.672825
## 5 weekday thursday        0 9.375844
## 6 weekday  tuesday        0 0.000000
```

The time series plot take the following form:

```r
# Compute the time serie plot
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", lwd=1, xlab="Interval", 
       ylab="Number of steps", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png) 

```r
# Clear the workspace
rm(mean_data)
```

