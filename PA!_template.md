## Code for reading in the dataset and/or processing the data
library(knitr)
library(ggplot2)

data <- read.csv('activity.csv')
data <- read.csv('activity.csv')

## Histogram of the total number of steps taken each day
hist(steps_per_day$steps, main = "Total Steps per day", xlab = "Steps", ylim = c(0,40), labels = TRUE)
See Histogram_Total_Steps.PNG
## Mean and median number of steps taken each day
Mean: 10766.19
Median: 10765

## Time series plot of the average number of steps taken
plot(mean_steps_interval$interval, mean_steps_interval$steps, type = "l", main = "Average number of steps per 5-min interval", xlab = "Interval", ylab = "Average Steps")
See Time_Series_Plot.PNG

## The 5-minute interval that, on average, contains the maximum number of steps
835

## Code to describe and show a strategy for imputing missing data
sum(is.na(data)) ## shows 2304 NA values

imputed_data <- data
for (i in 1:length(imputed_data$steps)) {
  if (is.na(imputed_data$steps[i])) {
    imputed_data$steps[i] <- mean_steps_interval$steps[mean_steps_interval$interval == imputed_data$interval[i]]
  }
}
imp_steps_per_day <- aggregate(steps ~ date, imputed_data, sum, na.rm = TRUE)

sum(is.na(imp_steps_per_day$steps)) ## is now 0 NA values
## Histogram of the total number of steps taken each day after missing values are imputed
hist(imp_steps_per_day$steps, main = "Total Steps per day", xlab = "Steps", ylim = c(0,40), labels = TRUE)
See Histogram_Total_Steps_Imputed.PNG
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
g <- ggplot(imputed_data_interval, aes(interval, steps))
g + facet_grid(wkdy ~ .) + geom_line() + ggtitle("Average number of steps per 5-min interval")
See Panel_Plot.PNG
## All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
See Project1.R