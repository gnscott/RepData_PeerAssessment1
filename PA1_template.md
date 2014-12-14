---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
itimes <- seq(0,287) / 12 # 5-minute intervals expressed as hours

adf <- read.csv('activity.csv')
adf$date <- as.POSIXct(adf$date)
adf$wkday <- weekdays(adf$date)
adf$wkend <- 'weekday'
adf$wkend[adf$wkday=='Saturday' | adf$wkday=='Sunday'] <- 'weekend'
```


## What is mean total number of steps taken per day?


```r
d_adf <- group_by(adf, date)
d_stats <- summarise(d_adf, 
                     avsteps=mean(steps,na.rm=TRUE),
                     totsteps=sum(steps,na.rm=TRUE),
                     count=n(),
                     nintvl=length(steps[!is.na(steps)]))

hist(d_stats$totsteps, breaks=seq(0,25000,by=2500),
     main='Histogram of Steps per Day',
     xlab='Steps per Day', ylab='Number of Days')
```

![plot of chunk dailyStats](figure/dailyStats.png) 

```r
meanSpD <- mean(d_stats$totsteps, na.rm=TRUE)
medSpD <- median(d_stats$totsteps, na.rm=TRUE)
```

Median number of steps per day: 10395

Mean number of steps per day: 9354.2295

## What is the average daily activity pattern?


```r
i_adf <- group_by(adf, interval)
i_stats <- summarise(i_adf, 
                     avsteps=mean(steps,na.rm=TRUE),
                     totsteps=sum(steps,na.rm=TRUE),
                     count=n(),
                     ndays=length(steps[!is.na(steps)]))
imax <- which.max(i_stats$avsteps)
```

Interval with max number of steps is 835

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

## Imputing missing values



```r
iMiss <- which(is.na(adf$steps))
nMiss <- length(iMiss)
```

There are 2304 intervals with missing data, representing 8 24-hour days with no data.
We'll fill them with the average of the corresponding
non-missing values for that interval. (Note that day 2012-11-15 has only 41 steps and 
286 no-step intervals, but *NO* missing intervals.)


```r
intvlMiss <- adf$interval[iMiss]
fillVal <- sapply(intvlMiss, function(iv) i_stats$avsteps[which(i_stats$interval==iv)])

adfFill <- adf
adfFill$steps[iMiss] <- fillVal
```

## Are there differences in activity patterns between weekdays and weekends?


```r
iwd_adf <- group_by(adfFill[adfFill$wkend=='weekday',], interval)
iwd_stats <- summarise(iwd_adf, 
                     avsteps=mean(steps,na.rm=TRUE),
                     totsteps=sum(steps,na.rm=TRUE),
                     count=n(),
                     ndays=length(steps[!is.na(steps)]))

iwe_adf <- group_by(adfFill[adfFill$wkend=='weekend',], interval)
iwe_stats <- summarise(iwe_adf, 
                     avsteps=mean(steps,na.rm=TRUE),
                     totsteps=sum(steps,na.rm=TRUE),
                     count=n(),
                     ndays=length(steps[!is.na(steps)]))
plot(itimes, iwd_stats$avsteps, type='l', xaxt='n',
     xlab='Hour of Day', 
     ylab='Steps per Interval')
axis(1, seq(0,24,by=2), labels=sprintf('%d',seq(0,24,2)))
lines(itimes, iwe_stats$avsteps, col='red')
legend('topleft', c('Weekday','Weekend'), col=c('black','red'), lty=1)
```

![plot of chunk weekends](figure/weekends1.png) 

```r
oldpar <- par(mfrow=c(2,1))
par(mar = c(0,4,4,2))
plot(itimes, iwd_stats$avsteps, type='l', xaxt='n', ylim=c(0,250))
abline(h=seq(0,250,by=50),lty=2,col='gray')
par(mar = c(5,4,0,2))
plot(itimes, iwe_stats$avsteps, type='l', xaxt='n', ylim=c(0,250))
abline(h=seq(0,250,by=50),lty=2,col='gray')
axis(1, seq(0,24,by=2), labels=sprintf('%d',seq(0,24,2)))
```

![plot of chunk weekends](figure/weekends2.png) 

```r
par(oldpar)
```
