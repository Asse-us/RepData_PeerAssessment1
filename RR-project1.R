
#install the necessary packages
install.packages("data.table", "dplyr", "ggplot2")
packages<- c("data.table", "dplyr", "ggplot2")
lapply(packages, library, character.only = TRUE)

# Loading and preprocessing the data

filePath<- getwd()
# look at the name of the data set in the directory
list.files(filePath) 

# unzip the already loaded data
unzip("activity.zip")

# look at the unzipped data set
list.files(filePath) 

# read the csv data and assign the data set name activity 
activity<- read.csv("activity.csv")

# Explore the data and get some overview 
summary(activity)
str(activity)
head(activity)
tail(activity)
colSums(is.na(activity)) #column based NAs counts

## What is mean total number of steps taken per day?

#Calculate the total number of steps taken per day
DailySteps<- aggregate(activity$steps, by = list(activity$date), sum)
colnames(DailySteps)<- c("Date", "Total_Steps")
head(DailySteps)
tail(DailySteps)

# Make a histogram of the total number of steps taken each day

Non_NAData <- activity[complete.cases(activity), ]
dim(Non_NAData)

png(plot1.png)
ggplot(Non_NAData, aes(as.factor(date),steps))+geom_bar(fill="orange", stat="identity")+xlab("Dates") + ylab("Total Steps")+ggtitle("Histogram of total steps taken each day")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
# # or x-axis (dates) values are removed from the chart and each date is represented by tick mark
# 
# ggplot(Non_NAData, aes(as.factor(date),steps))+geom_bar(fill="orange", stat="identity")+xlab("Date") + ylab("Total Steps")+ggtitle("Histogram of total steps taken each day")+ theme(axis.text.x = element_blank())

# Calculate and report the mean and median of the total number of steps taken per day
Non_NADailySteps<- aggregate(steps~date,Non_NAData, FUN=sum)

mean(Non_NADailySteps$steps)
#Mean of daily total steps is 10766.19.

median(Non_NADailySteps$steps)
#Median of daily total steps is 10765.

## What is the average daily activity pattern?
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
    # and the average number of steps taken, averaged across all days (y-axis)

StepsPerInterval <- aggregate(steps~interval, Non_NAData, FUN=mean)
png(plot2.png)
plot(StepsPerInterval$steps ~ StepsPerInterval$interval, type="l", xlab="Time Intervals (5 minutes)", 
     ylab="Total Steps", 
     main = "Average Number of Steps Taken at 5 minutes Interval")
 dev.off()
 
#2. Which 5-minute interval, on average across all the days in the dataset, 
    # contains the maximum number of steps?

StepsPerInterval[which(StepsPerInterval$steps == max(StepsPerInterval$steps)), ]

## Imputing missing values
#1. Calculate and report the total number of missing values in the dataset 
    # (i.e. the total number of rows with NAs)

# The total number of rows with NAs in the dataset is
sum(is.na(activity$steps)) 

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
    # The strategy does not need to be sophisticated. For example, 
    # you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# I will use the mean for that 5 -minute interval to replace all the missing values 
# and check if all the NAs have been replaced. The code for this strategy is as follows:

NA_Data <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
NA_Replaced <- (activity %>% group_by(interval) %>% mutate(steps = NA_Data(steps)))
summary(NA_Replaced)
head(NA_Replaced)
tail(NA_Replaced)
#check whether there is NA or not
sum(is.na(NA_Replaced))

# 3. Create a new dataset that is equal to the original dataset 
    # but with the missing data filled in.

NAFilledActivity <- as.data.frame(NA_Replaced)
dim(NAFilledActivity)
str(NAFilledActivity)
summary(NAFilledActivity)
head(NAFilledActivity)
tail(NAFilledActivity)

NAFilledDailySteps<- aggregate(steps ~ date, NAFilledActivity, FUN = sum)

# 4. Make a histogram of the total number of steps taken each day and 
    # Calculate and report the mean and median total number of steps taken per day. 
    # Do these values differ from the estimates from the first part of the assignment? 
    # What is the impact of imputing missing data on the estimates of the total daily 
    # number of steps?

png(plot3.png)
ggplot(NAFilledDailySteps, aes(as.factor(date),steps))+geom_bar(fill="purple", stat="identity")+xlab("Date") + ylab("Total Steps")+ggtitle("Histogram of total steps taken each day after filling NAs")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

#Mean comparisons
NewMean<- mean(NAFilledDailySteps$steps) 
OldMean<- mean(Non_NADailySteps$steps)
NewMean
OldMean
# The means of each dataset are same

#Median comparison
NewMedian<- median(NAFilledDailySteps$steps)
OldMedian<- median(Non_NADailySteps$steps)
NewMedian
OldMedian
# The new median ofter filling the NAs values is one step higher than the previos mean
# with NAs.


#5. Are there differences in activity patterns between weekdays and weekends?

    # 1. Create a new factor variable in the dataset with two levels – “weekday” 
    # and “weekend” indicating whether a given date is a weekday or weekend day.

NAFilledActivity$DayType <- ifelse(as.POSIXlt(NAFilledActivity$date)$wday %in% c(0,6), 'weekend', 'weekday')

AvgNAFilledActivity <- aggregate(steps ~ DayType +interval , data=NAFilledActivity, mean)

head(AvgNAFilledActivity)

# 2. Make a panel plot containing a time series plot (i.e.type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days(y-axis). 

png(plot4.png)
ggplot(AvgNAFilledActivity, aes(interval, steps, color = DayType)) + 
    geom_line(lwd = 1) + 
    facet_wrap(~DayType, ncol=1) +
    xlab("5 minutes interval") + 
    ylab("Number of steps") +
    ggtitle("Day Type Comparison of Steps in 5 minutes Interval")
dev.off()






