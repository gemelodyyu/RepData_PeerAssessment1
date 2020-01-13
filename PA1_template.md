---
title: "Course Project 1 Solution and Results"
output: html_document
---

1.Loading and preprocessing the data 
=====================================

Show any code that is needed to

* Load the data 
* Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(dplyr)
#Load the data 
HWdata <- read.csv("activity.csv", header = T) 
#checking the data
dim(HWdata)
```

```
## [1] 17568     3
```

```r
head(HWdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
#Checking missing data 
missing_data <- HWdata[is.na(HWdata$steps),]
dim(missing_data)
```

```
## [1] 2304    3
```

2. What is mean total number of steps taken per day?
======================

For this part of the assignment, you can ignore the missing values in the dataset.

* Calculate the total number of steps taken per day


```r
total_steps <- with(HWdata, 
                    tapply(steps, as.factor(HWdata$date), 
                    sum, 
                    na.rm = T))
```

* If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
hist(total_steps, 
     main = "Histogram of the total number of steps taken each day", 
     xlab = "Total number of steps")
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

* Calculate and report the mean and median of the total number of steps taken per day

```r
summary(total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

    * The mean of the total number of steps taken per day is 9354, and the Median of the total number of steps taken per day is 10395.  

3. What is the average daily activity pattern?
===========

* Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
cleandata <- HWdata[!is.na(HWdata$steps), ]
mean_steps <- with(cleandata, 
                   tapply(steps, cleandata$interval, mean))
interval <- levels(as.factor(cleandata$interval))
plot(interval, mean_steps, 
     type = "l", 
     main = "Time series plot of the 5 minute interval and the average number of steps taken", 
     xlab = "interval", ylab = "mean steps")
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34-1.png)

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
table <- data.frame(mean_steps, interval) 
table[table$mean_steps == max(table$mean_steps), ] [2] 
```

```
##     interval
## 835      835
```

    * The 835th interval contains the maximum number of steps on average across all the days in the dataset. 

4. Imputing missing values
==========================

Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
length(missing_data$steps)
```

```
## [1] 2304
```

    * There are 2304 missing values in the dataset. 

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
#replace the missing values by the mean steps. 
meansteps <- with(cleandata, tapply(steps, cleandata$interval, mean))
missing_data$steps <- meansteps 
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in. 


```r
newdata <- rbind(cleandata, missing_data)
newdata <- newdata[order(newdata$date), ]
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Make a histogram. 
total_steps2 <- with(newdata, tapply(steps, as.factor(newdata$date), sum))
hist(total_steps2, 
     main = "Histogram of total number of steps taken per day", 
     xlab = "total number of steps")
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-39-1.png)

```r
#Calculate and report the mean and median total number os steps taken per day WITH imputing missing data. 
summary(total_steps2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
#Calculate and report the mean and median total number os steps taken per day WITHOUT imputing missing data. 
summary(total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

    * After filling in the missing data with mean, the mean total number of steps taken per day change from 9354 to 10766, and the medain change from 10395 to 10766. Imputing missing data increase both mean and median. 

5. Are there differences in activity patterns between weekdays and weekends?
====================================

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
newdata["day_type"] <- weekdays(as.Date(newdata$date))
newdata$datetype <- ifelse(newdata$day_type == "Saturday" | newdata$day_type == "Sunday", 
                           "Weekend", "Weekday")
newdata$datetype <- as.factor(newdata$datetype)
```

* Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
averagestep_datetpye <- newdata %>%
    group_by(datetype, interval) %>% 
    summarise(steps = mean(steps))

library(ggplot2)

ggplot(data = averagestep_datetpye, aes(x = interval, y = steps))+
  facet_grid(datetype ~ .)+
  geom_line()+
  ggtitle("Weekday vs. Weekend (Average number of steps taken)")
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-41-1.png)

    * On average, the activity pattern is more even over the whole day during weekends, verse there is a more obvious peak of activity at the beginning of the day during weekdays. 

