# Read data
data <- read.csv("activity.csv")
# Change date format to Date
data$date <- as.Date(data$date, "%Y-%m-%d")
# Check the sturcture of the data
str(data)

# Calculate the total number of steps taken per day
total_steps <- aggregate(data$steps, by = list(data$date), sum)
names(total_steps) <- c("Date", "TotalSteps")
head(total_steps, 10)
# plot histogram of the total number of steps taken per day
library(ggplot2)
library(scales)
library(ggthemr)
ggthemr("dust")
g <- ggplot(total_steps, aes(x = Date, y = TotalSteps))
g + geom_bar(stat="identity") + scale_x_date(labels=date_format("%Y-%m-%d")) +
        ylab("Total steps")
# Calculate the mean and median of the total number of steps taken per day
summary(total_steps$TotalSteps)

# Calculate average steps accross all days
ave_steps <- aggregate(data$steps, by = list(data$interval), mean, na.rm = TRUE)
names(ave_steps) <- c("interval", "averagesteps")
max_step <- ave_steps[which.max(ave_steps$averagesteps),]
# make a time series plot
ag <- ggplot(ave_steps, aes(x = interval, y = averagesteps))
ag + geom_line(size = 1) + xlab("5-minute interval") + ylab("average steps across all days") +
        geom_vline(xintercept = max_step$interval, linetype = "dashed") +
        geom_text(aes(max_step$interval,0,label = max_step$interval, vjust = -1))
# 5-minute interval contains maximum number of steps
max_step

# Calculate the total number rows of missing values in the dataset
sum(is.na(data))
# fill in the NAs with mean for that 5-minute interval
na <- data[is.na(data),]
nadate <- unique(na$date)
new_data <- data
for (i in 1:8){
new_data[new_data$date == nadate[i],]$steps <- ave_steps$averagesteps
}
# Calculate the total number of steps taken per day
new_total_steps <- aggregate(new_data$steps, by = list(new_data$date), sum)
names(new_total_steps) <- c("Date", "TotalSteps")
head(new_total_steps, 10)
# make a new histogram for the new dataset
n_g <- ggplot(new_total_steps, aes(x = Date, y = TotalSteps))
n_g + geom_bar(stat="identity") + scale_x_date(labels=date_format("%Y-%m-%d")) +
        ylab("Total steps")
# Calculate the mean and median of the total number of steps taken per day
summary(new_total_steps$TotalSteps)

# Create new factor variable "weekday" "weekend"
Sys.setlocale("LC_TIME", "English")
new_data$day <- weekdays(new_data$date)
new_data$week <- ""
new_data[new_data$day == "Saturday" | new_data$day == "Sunday", ]$week <- "weekend"
new_data[!(new_data$day == "Saturday" | new_data$day == "Sunday"), ]$week <- "weekday"
new_data$week <- factor(new_data$week)
head(new_data)
average <- aggregate(new_data$steps, by = list(new_data$interval, new_data$week), mean)
names(average) <- c("interval", "week", "steps")
# Make a panel plot
library(ggplot2)
ggplot(average, aes(interval, steps, group = week)) +
        geom_line(size = 1) + facet_grid(week ~.) + 
        xlab("5-minute interval") + ylab("Average number of steps")