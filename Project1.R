## Load Required Libaries
	library(knitr)
	library(ggplot2)

## Load the Data
	data <- read.csv('activity.csv')

## Transform the Data
	data <- transform(data, date = as.Date(date))

## What is mean total number of steps taken per day?
## Aggregate the table for total steps per day, remove NA’s
	steps_per_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

## Create the plot
	hist(steps_per_day$steps, main = "Total Steps per day", xlab = "Steps", ylim = c(0,40), labels = TRUE)

## Mean
	mean(steps_per_day$steps)

## Median
	median(steps_per_day$steps)

## What is the average daily activity pattern?
## Aggregate data for time series plot, remove NA’s
	mean_steps_interval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

## Create time series plot
	plot(mean_steps_interval$interval, mean_steps_interval$steps, type = "l", main = "Average number of steps per 5-min interval", xlab = "Interval", ylab = "Average Steps")

## Which 5-minute interval, on average across all days, contains the max number of steps?
	max_interval <- mean_steps_interval$interval[which(mean_steps_interval$steps == max(mean_steps_interval$steps))]
	max_interval

## Imputing missing values
## Calculate and report the total number of missing values in the dataset.
	sum(is.na(data))

## Devise a strategy for filling in missing data values.
## Replace NA’s with the mean for that 5-minute interval.
	imputed_data <- data
	for (i in 1:length(imputed_data$steps)) {
		if (is.na(imputed_data$steps[i])) {
    			imputed_data$steps[i] <- mean_steps_interval$steps[mean_steps_interval$interval == imputed_data$interval[i]]
  		}
	}
## Aggregate the filled data and make sure there are no NA steps
	imp_steps_per_day <- aggregate(steps ~ date, imputed_data, sum, na.rm = TRUE)
	sum(is.na(imp_steps_per_day$steps))

## Create the plot with the imputed data
	hist(imp_steps_per_day$steps, main = "Total Steps per day", xlab = "Steps", ylim = c(0,40), labels = TRUE)

## Mean Imputed
	mean(imp_steps_per_day$steps)

## Median Imputed
	median(imp_steps_per_day$steps)

## Are there differences in activity patterns between weekdays and weekends?
## Add a factor variable that will be either weekend or weekday, and create aggregate for plot
	imputed_data$date <- as.Date(imputed_data$date)
	imputed_data$wkdy <- "weekday"
	imputed_data$wkdy[weekdays(imputed_data$date) == "Saturday" | weekdays(imputed_data$date) == "Sunday"] <- "weekend"
	imputed_data$wkdy <- as.factor(imputed_data$wkdy)
	imputed_data_interval <- aggregate(steps ~ interval + wkdy, imputed_data, mean, na.rm = TRUE)

## Create the panel plot
	g <- ggplot(imputed_data_interval, aes(interval, steps))
	g + facet_grid(wkdy ~ .) + geom_line() + ggtitle("Average number of steps per 5-min interval")







