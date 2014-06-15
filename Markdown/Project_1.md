Peer Assessment 1 - Reproducible Research
============

### Loading and preprocessing the data

```{r}
setwd("/home/isgauper/Downloads/Coursera/Reproducible Research/Project1")

activity <- read.csv("activity.csv", colClasses = c("integer","Date","integer"))
```

### What is the mean total number of steps taken per day?

```{r}
#calculate total steps for each day
daysum <- rowsum(activity$steps, format(activity$date, '%m-%d'),na.rm=T)

#plot histogram
plot(daysum,type="h",lwd=10,lend="square", 
	main = "Total Number of Steps Taken Each Day",
	ylab="Total Steps", 
	xlab = "Day")   				
							
daymean <- mean(daysum) #mean of all days
daymedian <- median(daysum) #median of all days


```

The mean number of steps taken per day was `r daymean`.
The median number of steps taken per day was `r daymedian`.

### What is the average daily activity pattern?

```{r}

intmean <- rowsum(activity$steps, activity$interval,na.rm=T)/61 #mean per interval
plot.ts(intmean, main = "Average daily activity pattern", 
	xlab = "Interval",
	ylab = "Steps") #plot time series

maxint <- which(intmean==max(intmean)) #find the interval with the highest maximum

#create a data frame for using later
intmean2 <-as.data.frame(cbind(interval = as.numeric(rownames(intmean)), 
	average = as.numeric(intmean[,1])))
```
On average, interval `r maxint` contains the maximum number of steps.


# Imputing missing values

```{r}

na.sum <- sum(is.na(activity$steps)) #calculate the total number of NAs

#replace nas with interval mean

merge = merge(activity, intmean2, by = "interval") #merge activity data with interval means

merge$steps = ifelse(is.na(merge$steps), floor(merge$average), merge$steps) #replace NAs

#create data set that is equivalent to original data set with NAs removed

activity.na = data.frame(steps = merge$steps, date=merge$date, interval = merge$interval)
activity.na = activity.na[with(activity.na, order(date,interval)),]

head(activity.na)


#calculate total steps for each day
daysum.na <- rowsum(activity.na$steps, format(activity.na$date, '%m-%d'),na.rm=T)

#plot histogram
plot(daysum.na,type="h",lwd=10,lend="square", 
	main = "Total Number of Steps Taken Each Day",
	ylab="Total Steps", 
	xlab = "Day")   				
							
daymean.na <- mean(daysum.na) #mean of all days
daymedian.na <- median(daysum.na) #median of all days

```

The total number of NAs is `r na.sum`.

The mean number of steps taken per day was `r daymean.na`.
The median number of steps taken per day was `r daymedian.na`.

These values are larger than the mean and median values computed before imputing the NAs. 
Imputing the NAs increases the average number of steps per day.


### Are there differences in activity patterns between weekdays and weekends?

```{r}

#labeling days as weekend or weekday


activity$daytype <- ifelse(weekdays(activity$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")

weekend <- subset(activity, daytype == "Weekend")
weekday <- subset(activity, daytype == "Weekday")

weekend.intmean <- rowsum(weekend$steps, weekend$interval,na.rm=T)/61 #mean per interval
weekday.intmean <- rowsum(weekday$steps, weekday$interval, na.rm=T)/61 

par(mfrow = c(2,1))
plot.ts(weekend.intmean, main = "Average Daily Activity - Weekend", ylab = "Steps")
plot.ts(weekday.intmean, main = "Average Daily Activity - Weekday", ylab = "Steps")
```























