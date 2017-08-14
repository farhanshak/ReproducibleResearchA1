
# load and reprocess the data
# set working directory and data csv file

setwd("C:/Users/Farhan/Documents/R/Data science/Reproducible Research")
data <- read.csv("activity.csv")


# R code for the mean of total steps taken per day
library(ggplot2)

total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

mean(total.steps, na.rm=TRUE)
# answer for mean [1] 9354.23

median(total.steps, na.rm=TRUE)
# answer for median [1] 10395


# R code to get the average of daily activity patterns
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

averages[which.max(averages$steps),]
# answer     interval = 104    steps = 835 206.1698


# R code to insert and/or impute missing values for data showing as NA

missing <- is.na(data$steps)
table(missing)
#answer for missing data False = 15264 True = 2304 < this is what needs to be replaced

# R code to replace missing value NA with the mean
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)


# R code to create a plot showing total # of steps taken each day
# R code to also calculate the mean and median of these steps
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

mean(total.steps)
# answer for mean = [1] 10766.19
median(total.steps)
# answer for median = [1] 10766.19

# compared to before replacing NA with mean, the mean and meadian have gone up
# mean was 9354.23 and median was 10395


# R code to show if there was any difference in activity from weekdays to weekend
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)


# R code to plot average number of steps for weekdays and weekend
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
# answer = there is much more activity on the weekends than the weekdays
# this may be caused by possibly having more leisure time, shopping, or other phsycial activities
