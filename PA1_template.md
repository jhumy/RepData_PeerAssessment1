---
title: "Reproducible Research: Peer Assessment 1"
author: "Olusoji oluwafemi Daniel"
output: html_document
date: "Monday, October 13, 2014"
html_document:
keep_md: true
---


## Loading and preprocessing the data

```r
#Remove all variables on the present workscreen
rm(list=ls())
#opening connection to the data 
activitycon <- file("C:/Users/USER/RepData_PeerAssessment1/activity.csv",open="r")
#reading the data
activity <- read.csv(activitycon,header=T)
#Closing the Opened COnnection
close(activitycon)
#Converting the date into a POIXct format
Sys.setlocale("LC_TIME","English_United States.1252")
```

```
## [1] "English_United States.1252"
```

```r
activity$date <- as.POSIXct(as.Date(activity$date,format="%Y-%m-%d"))
```


## What is mean total number of steps taken per day?
Plotting the total number of steps taken per day.

```r
#Obtaining the total number of steps taken per day
steps_sum <- tapply(activity$steps,activity$date,sum,na.rm=TRUE)
#Making a histogram of the total number of steps taken per day 
#using the gg plot2 system
library(ggplot2)
plot1 <- qplot(x=steps_sum,geom="histogram",binwidth=2000)
plot2 <- plot1+labs(title="Histogram of Total Number of Steps taken per Day")
plot3 <- plot2+labs(x="Total Number of Steps",y="Count")
#print the plot
print(plot3)
```

![plot of chunk histogram1](figure/histogram1.png) 
  
Reporting the mean and median total number of steps taken per day

```r
#Computing the mean total steps taken per day 
steps_mean <- tapply(activity$steps,activity$date,mean,na.rm=TRUE)
#Computing the median total steps taken per day 
steps_median <- tapply(activity$steps,activity$date,median,na.rm=TRUE)
steps_mean
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        NaN     0.4375    39.4167    42.0694    46.1597    53.5417 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##    38.2465        NaN    44.4826    34.3750    35.7778    60.3542 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##    43.1458    52.4236    35.2049    52.3750    46.7083    34.9167 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##    41.0729    36.0938    30.6285    46.7361    30.9653    29.0104 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##     8.6528    23.5347    35.1354    39.7847    17.4236    34.0938 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##    53.5208        NaN    36.8056    36.7049        NaN    36.2465 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    28.9375    44.7326    11.1771        NaN        NaN    43.7778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##    37.3785    25.4722        NaN     0.1424    18.8924    49.7882 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##    52.4653    30.6979    15.5278    44.3993    70.9271    73.5903 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##    50.2708    41.0903    38.7569    47.3819    35.3576    24.4688 
## 2012-11-30 
##        NaN
```

```r
steps_median
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0         NA          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0         NA          0          0         NA          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0         NA         NA          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0         NA          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##         NA
```
## What is the average daily activity pattern?
  
Computing the average number of steps taken every 5 minutes interval over all days considered

```r
#computing average number of steps taken every 5 minutes
stepsinterval_mean <- as.numeric(tapply(activity$steps,activity$interval,mean,na.rm=TRUE))
#creatin the x axis variable (5 minutes interval)
activity$interval <- factor(activity$interval)
steps_interval <- as.numeric(levels(activity$interval))
average_interval <- data.frame(steps_interval,stepsinterval_mean)
#Making the time series plot using ggplot2 
plot4 <- ggplot(data=average_interval,aes(x=as.numeric(steps_interval),y=stepsinterval_mean))
plot4 <- plot4+geom_line(colour="steelblue")
plot4 <- plot4 + labs(title="Time Plot of Total Number of Steps taken every 5 Mins.",x="5 Minutes Interval",y="Average Number of Steps Taken")
print(plot4)
```

![plot of chunk intervalplot](figure/intervalplot.png) 
  
Finding the 5 minutes interval with the maximum average number of steps.

```r
interval_mean <- tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
max_mean <- max(interval_mean)
max_find <- max_mean == interval_mean
max_interval <- interval_mean[max_find==TRUE]
names(max_interval)
```

```
## [1] "835"
```

## Imputing missing values

Counting the number of missing values in the data set.

```r
activity$test1 <- complete.cases(activity)
test2 <- table(activity$test1)
test2[c("FALSE")]
```

```
## FALSE 
##  2304
```
  
Obtaining a new dataset by filling in the missing values with the mean of the 5 mins interval across the days

```r
#A loop to check for where missing values and fill them with the mean of the 5 
#mins interval across the days
for(i in 1:length(activity$test))
  {
    if(activity$test[i]==FALSE)
      {
        a <- activity$interval[i]
        dat1 <- activity[(activity$interval==a),]
        activity$steps[i] <- mean(dat1$steps,na.rm=TRUE)
      }
  }
```
  
Plotting the Histogram of the filled Data Set

```r
steps_sum2 <- tapply(activity$steps,activity$date,sum,na.rm=TRUE)
#Making a histogram of the total number of steps taken per day 
plot11 <- qplot(x=steps_sum2,geom="histogram",binwidth=2000)
plot12 <- plot11+labs(title="Histogram of Total Number of Steps taken per Day")
plot13 <- plot12+labs(x="Total Number of Steps",y="Count")
#print the plot
print(plot13)
```

![plot of chunk newhistogram](figure/newhistogram.png) 
  
Reporting the mean and median total number of steps taken per day for the new dataset

```r
#Computing the mean total steps taken per day 
steps_mean2 <- tapply(activity$steps,activity$date,mean,na.rm=TRUE)
#Computing the median total steps taken per day 
steps_median2 <- tapply(activity$steps,activity$date,median,na.rm=TRUE)
steps_mean2
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##    37.3826     0.4375    39.4167    42.0694    46.1597    53.5417 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##    38.2465    37.3826    44.4826    34.3750    35.7778    60.3542 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##    43.1458    52.4236    35.2049    52.3750    46.7083    34.9167 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##    41.0729    36.0938    30.6285    46.7361    30.9653    29.0104 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##     8.6528    23.5347    35.1354    39.7847    17.4236    34.0938 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##    53.5208    37.3826    36.8056    36.7049    37.3826    36.2465 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    28.9375    44.7326    11.1771    37.3826    37.3826    43.7778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##    37.3785    25.4722    37.3826     0.1424    18.8924    49.7882 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##    52.4653    30.6979    15.5278    44.3993    70.9271    73.5903 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##    50.2708    41.0903    38.7569    47.3819    35.3576    24.4688 
## 2012-11-30 
##    37.3826
```

```r
steps_median2
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      34.11       0.00       0.00       0.00       0.00       0.00 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##       0.00      34.11       0.00       0.00       0.00       0.00 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##       0.00      34.11       0.00       0.00      34.11       0.00 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       0.00       0.00       0.00      34.11      34.11       0.00 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##       0.00       0.00      34.11       0.00       0.00       0.00 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-11-30 
##      34.11
```

## Are there differences in activity patterns between weekdays and weekends?
Creating the factor variable day with two levels "weekday" and "weekend"

```r
##Obtaining the days of the week the steps were taken
activity$days <- weekdays(activity$date,abbreviate=TRUE)
#A loop to group each dat to its appropriate week group i.e. weekend or weekdays
for(i in 1:length(activity$days))
  {
    if(activity$days[i]=="Sat" || activity$days[i]=="Sun")
      {
        activity$group[i] <- 2
      }
    else
      {
        activity$group[i] <- 1
      }  
  }
activity$group <- factor(activity$group,levels=c(1,2),labels=c("weekdays","weekend"))
```
Making a Panel time Plot of of the 5-minute interval and the average number of steps taken.

```r
##Dividing the data according the the day group i.e. weekday or weekend
steps_weekend <- activity[(activity$group=="weekend"),]
#
steps_weekday <- activity[(activity$group=="weekday"),]
#
averagesteps_weekend <- as.numeric(tapply(steps_weekend$steps,steps_weekend$interval,mean,na.rm=TRUE))
#
averagesteps_weekday <- as.numeric(tapply(steps_weekday$steps,steps_weekday$interval,mean,na.rm=TRUE))
#Computing the required averages and obtaining the plot data frame. 
average_steps <- as.numeric(cbind(averagesteps_weekday,averagesteps_weekend))
group <- gl(2,288,labels=c("weekday","weekend"))
tsteps <- as.numeric(cbind(steps_interval,steps_interval))
plot_frame <- data.frame(tsteps,average_steps,group)
#Plotting the Data using the ggplot2 system
plot21 <- ggplot(plot_frame,aes(x=tsteps,y=average_steps))
plot21 <- plot21 +geom_line(colour="steelblue")
plot21 <- plot21 + labs(title="Time Plot of Average Number of Steps Taken",x="5-Minutes Interval",y="Average Steps Taken")
plot21 <- plot21+facet_grid(group~.)
print(plot21)
```

![plot of chunk plotgroup](figure/plotgroup.png) 

```r
#Remove all variables from screen
rm(list=ls())
```
  
Thank You.

### Olusoji Oluwafemi Daniel,
### University of Ibadan, Oyo State, Nigeria.
