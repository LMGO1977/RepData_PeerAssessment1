## Read activity data file which was unziped and download to a R directory
MainDataAtivity <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric")) 

## ----What is mean total number of steps taken per day?---

## Exclude NA steps rows 
names(MainDataAtivity) 
str(MainDataAtivity) 
head(MainDataAtivity[which(!is.na(MainDataAtivity$steps)), ]) 

## Total steps per day
totalstepsday <- aggregate(steps ~ date, MainDataAtivity, sum) 
head(totalstepsday) 

## Histogram of the total number of steps taken each day

paletteBlue <- colorRampPalette(c("skyblue", "darkblue", "skyblue")) 
hist(totalstepsday$steps, breaks=10, xlab="Number of steps",  ylab="Number of days",
main="Histogram - Total number of steps per Day", 
col=paletteBlue(22), family="serif") 

## Mean and median number of steps taken each day

library(dplyr) 
stepsstatistics <- summarise(totalstepsday, meantotalsteps=mean(totalstepsday$steps),                              
mediantotalsteps=median(totalstepsday$steps)) 
print(stepsstatistics) 

## -----What is the average daily activity pattern?----

## Time series plot of the average number of steps taken
meanstepsinterval <- aggregate(steps ~ interval, MainDataAtivity, mean) 
head(meanstepsinterval) 

plot(x=meanstepsinterval$interval, y=meanstepsinterval$steps, type="l", 
main="Time Series Plot - Average steps by per Interval", 
ylab="Number of steps", xlab="Intervals (in 5 minutes)", 
col="darkblue", lwd=1.5, family="serif")

## The 5-minute interval that, on average, contains the maximum number of steps
meanstepsinterval[grep(max(meanstepsinterval$steps), meanstepsinterval$steps), ]

## -----Imputing missing values -----
## Code to describe and show a strategy for imputing missing data
## Replace NA values in the steps variable by the mean of the interval

adjusteddata <- MainDataAtivity 
for(x in 1:17568) { 
   if(is.na(adjusteddata[x, 1])==TRUE) { adjusteddata[x, 1] <- meanstepsinterval[meanstepsinterval$interval %in% adjusteddata[x, 3], 2] } } 
head(adjusteddata) 

## Histogram of the total number of steps taken each day after missing values are imputed

adjustedtotalstepsday <- aggregate(steps ~ date, adjusteddata, sum) 
head(adjustedtotalstepsday) 

paletteRed <- colorRampPalette(c("deeppink", "darkred", "deeppink")) 
hist(adjustedtotalstepsday$steps, breaks=20, xlab="Number of Steps Taken",  ylab="Number of Days",
main="Histogram - Total Number of Steps per Day (Adjust. values)", 
col=paletteRed(22), family="serif") 

## Compare Histograms to see the impact of imputing vakues in missing data

par(mfrow = c(1, 2)) 
hist(totalstepsday$steps, breaks=20, xlab="Number of Steps Taken",  ylab="Number of Days",
col=paletteBlue(22), family="serif", ylim=c(0, 20), main=NULL) 
hist(adjustedtotalstepsday$steps, breaks=20, xlab="Number of Steps Taken Adjusted",  ylab="Number of Days",
col=paletteRed(22), family="serif", ylim=c(0, 20), main=NULL) 
mtext("Histograms -Total Number of Steps Taken per Day, Original vs Adjusted Data", 
adj=0.95, family="serif", font=2) 

##Are there differences in activity patterns between weekdays and weekends?

daysdata <- adjusteddata 
daysdata$days <- weekdays(daysdata$date) 
daysdata$weekday <- as.character(rep(0, times=17568)) 
for(x in 1:17568) { 
    if(daysdata[x, 4] %in% c("Saturday", "Sunday")) { daysdata[x, 5] <- "Weekend" } else { daysdata[x, 5] <- "Weekday" } } 
daysdata$weekday <- factor(daysdata$weekday) 
head(daysdata) 

## Compare the weekday and weekend data

weekdaydata <- daysdata[daysdata$weekday=="Weekday", ] 
weekenddata <- daysdata[daysdata$weekday=="Weekend", ] 

weekdayMean <- aggregate(steps ~ interval, weekdaydata, mean) 
weekendMean <- aggregate(steps ~ interval, weekenddata, mean) 

## Making the chart to compare the two series

par(mar = rep(2, 4))
plot(weekdayMean$interval, weekdayMean$steps, type="l", 
main="Avg. Steps per Interval - Weekdays", 
xlab="Intervals (5 mins)", ylab="Number Steps", family="serif", 
col="darkred", lwd=1.5, ylim=c(0, 230)) 
plot(weekendMean$interval, weekendMean$steps, type="l", 
main="Avg. Steps per Interval - Weekends", 
xlab="Intervals (5 mins)", ylab="Number Steps", family="serif", 
col="darkblue", lwd=1.5, ylim=c(0, 230)) 

