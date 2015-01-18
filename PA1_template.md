---
title: Reproducible Research Peer Assessment 1
author: "Angie Zhu"
date: "January 16, 2015"
output: html_document
---
1. Loading data


```r
#download and read file
require("graphics")
require("utils")
if(!file.exists("activity.csv")){
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","./activity.zip")
  unzip("activity.zip")
}
data<-read.csv("activity.csv")
```

2. Mean total number of steps taken per day 

```r
total_steps<-aggregate(data$steps,by=list(data$date), FUN=sum) #total steps by day
mean_total_steps<-mean(total_steps$x,na.rm=TRUE) #mean number of steps per day
median_total_steps<-median(total_steps$x,na.rm=TRUE) #median number of steps per day
data.frame("mean"= mean_total_steps, "median"=median_total_steps)
```

```
##       mean median
## 1 10766.19  10765
```

```r
hist(total_steps$x,10,xlab="total steps",main="Daily Total Steps") #histogram plot
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

3. Average Daily Activity Pattern

```r
int_total_steps<-aggregate(steps~interval,data,mean) #average steps taken per interval
int_total_steps$time<-with(int_total_steps, strptime(paste((interval-interval%%100)/100,interval%%100),format="%H %M")) #new column converting intervals to POSIXct time
maxsteps<-int_total_steps[which(int_total_steps$steps==max(int_total_steps$steps)),c("steps","time")]
maxsteps$time<-strftime(maxsteps$time,format="%H:%M") #convert to POSIXlt
```
Interval containing maximum number of steps

```r
maxsteps
```

```
##        steps  time
## 104 206.1698 08:35
```

```r
plot(int_total_steps$time,int_total_steps$steps,type="l",xlab="time",ylab="average steps",main="Average Number of Steps Taken per Time Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

4. Inputing missing values

```r
ind<-which(is.na(data$steps)) #vector of indices of missing steps data
length(ind) #number of rows with missing steps data
```

```
## [1] 2304
```

```r
data2<-data #duplicate data frame

#replace na values with average for the 5-minute interval
for(row in ind){
  data2[row,"steps"]<-int_total_steps[which(int_total_steps$interval==data2[row,"interval"]),"steps"]
}

total_steps2<-aggregate(data2$steps,by=list(data2$date),FUN=sum) #total steps taken for each day with NA values filled in
hist(total_steps2$x,10,xlab="total steps", main="Daily Total Steps (NAs replaced)")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
data.frame("mean"=mean(total_steps2$x),"median"=median(total_steps2$x))
```

```
##       mean   median
## 1 10766.19 10766.19
```
The mean and median of total steps taken each day are now identical with the NA values filled in

```r
data2$date<-strptime(data2$date,"%Y-%m-%d") #convert dates to POSIXct values
data2$day<-weekdays(data2$date) #add column of weekdays
data2$day<-as.factor(ifelse(data2$day %in% c("Saturday","Sunday"),"weekend","weekday")) #label weekdays and weekends
daysteps<-aggregate(steps ~ interval+day,data2,FUN=mean) #mean steps taken by interval and day
daysteps$time<-with(daysteps, strptime(paste((interval-interval%%100)/100,interval%%100),format="%H %M")) #convert time interval to POSIXct values


#plots by weekday and weekend
par(mfrow=c(2,1), mar=c(5,4,1,2))
with(daysteps[which(daysteps$day=="weekday"),], plot(time,steps,type="l",main="Weekday",ylab="average steps"))
with(daysteps[which(daysteps$day=="weekend"),], plot(time,steps,type="l",main="Weekend",xlab="time",ylab="average steps"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

