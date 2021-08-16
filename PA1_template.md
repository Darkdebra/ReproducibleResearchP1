---
title: "PA1_template"
author: "Jesús Antonio Leal Cázares"
date: "15/8/2021"
output: html_document
---

## 1. Loading and preprocessing the data

The first step is to obtain the csv file to read the data:

```{r unzip, cache= TRUE}
unzip("repdata_data_activity.zip",exdir="data")
```

Once we have the csv file, we read the data frame and show the summary as well as preprocessing the data:

```{r reading and preprocessing, cache=TRUE}
library(ggplot2)
library(plyr)
activity <- read.csv("data/Activity.csv",stringsAsFactors=FALSE)
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime <- as.POSIXct(activity$date, format="%Y-%m-%d")
clean <- activity[!is.na(activity$steps),]

names(activity)
str(activity)
summary(activity)
```

Once we get here, we can start with answering more questions.

## 2. What is mean total nomber of steps per day?

```{r steps mean}
sumTable <- aggregate(activity$steps ~activity$date, FUN=sum,)
colnames(sumTable) <- c("Date","Steps")
```

Here is a histogram of the total number of steps taken each day.

```{r histogram}
hist(sumTable$Steps, main= "Histogram of total steps taken per day", xlab="Total steps per day", ylab="Number of days", breaks=15, col="darkolivegreen4")
abline(v=as.integer(mean(sumTable$Steps)),lty=1,lwd=2,col="darkslategray")
abline(v=as.integer(median(sumTable$Steps)),lty=2,lwd=2,col="black")
legend(x="topright",c("Mean","Median"),col=c("darkslategray","black"),lty=c(1,2),lwd=c(2,2))
```

## 3. What is the average daily activity pattern?

After the last histogram, we procceed to make a time series plot of the 5-minute interval to see if there is a daily activity pattern.

```{r time series plot}
library(plyr)
library(ggplot2)
clean <- activity[!is.na(activity$steps),]
intervalTable <- ddply(clean,.(interval),summarize,Avg=mean(steps))
p <- ggplot(intervalTable,aes(x=interval,y=Avg),xlab="Interval",ylab="Average number of steps")
p+geom_line()+xlab("Interval")+ylab("Average number of steps")+ggtitle("Average number of steps per interval")
```

The time interval which the maximun number of steps is taken is:

```{r max steps}
maxSteps <- max(intervalTable$Avg)
intervalTable[intervalTable$Avg==maxSteps,1]
```

## 4. Imputing missing values.

Now, let's impute missing values, first of all, let's find if there is days with all time intervals reporting NA step values so we can replace de missing data for a day by the time average over all other days.

```{r missng values}
nrow(activity[is.na(activity$steps),])
```

That's the total of steps rows with NAs 

```{r steps mean 2}
avgTable <- ddply(clean,.(interval,day), summarize, Avg=mean(steps))
NAData <- activity[is.na(activity$steps),]
newData <- merge(NAData, avgTable, by=c("interval","day"))
```

Then, let's get our new data set.

```{r histogram 2}
newData2 <- newData[,c(6,4,1,2,5)]
colnames(newData2)<-c("steps","date","interval","day","DateTime")
mergeData <- rbind(clean,newData2)
```

Whit that information we can make a new histogram of the total number of steps taken each day:

```{r NAs}
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date,FUN=sum,)
colnames(sumTable2) <- c("Date","Steps")
as.integer(mean(sumTable2$Steps))
as.integer(median(sumTable2$Steps))
hist(sumTable2$Steps,breaks=5,xlab="Steps",main="Total steps per day with NAs fixed",col="darkolivegreen4")
hist(sumTable$Steps,breks=5,xlab="Steps",main="Total steps per day with NAs fixed",col="darkslategray",add=T)
legend("topright",c("Imputed Data","Non-NA Data"),fill=c("darkolivegreen4","darkslategray"))
```

## 5. Are there differences in activity patterns between weekdays and weekends?

Finally, we are gonna see if there is differences in activity patterns between weekdays and weekends by identifying the date as a weekday or weekend and make a plot of it.

```{r weekdays plot}
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
library(lattice)
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

