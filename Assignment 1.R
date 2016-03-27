setwd("C:\\Users\\zcapozzi002\\Documents\\Misc (Private)\\Hopkins Coursera\\Reproducible Research\\RepData_PeerAssessment1")


# Loading and preprocessing the data

unzip("activity.zip")
raw_data <- read.csv("activity.csv")
data <- subset(raw_data, !is.na(raw_data$steps))
summary(data)

# What is mean total number of steps taken per day?

total_steps_per_day <- aggregate(raw_data$steps, by=list(raw_data$date), FUN=sum)
names(total_steps_per_day) = c('Date','TotalSteps')

total_steps_per_day <- total_steps_per_day[!is.na(total_steps_per_day$TotalSteps),]
total_steps_per_day$TotalSteps <- as.numeric(total_steps_per_day$TotalSteps)

hist(total_steps_per_day$TotalSteps)

mean_steps <- mean(total_steps_per_day$TotalSteps)
print(paste("Calculated mean steps per day: ",mean_steps))
median_steps <- median(total_steps_per_day$TotalSteps)
print(paste("Calculated median steps per day: ",median_steps))

# What is the average daily activity pattern?

steps_by_interval <- aggregate(data$steps, by=list(data$interval), FUN=mean)
names(steps_by_interval) = c('Interval', 'AvgSteps')
plot(steps_by_interval)

maxSteps <- steps_by_interval[steps_by_interval$AvgSteps == max(steps_by_interval$AvgSteps),]
print(paste("The interval which, on average, has the most steps daily: ", maxSteps$Interval))


# Imputing missing values

missing <- nrow(subset(raw_data, is.na(raw_data$steps)))
print(paste("There are", missing, "rows that have invalid or missing step counts."))

imputed_data <- merge(raw_data, steps_by_interval, by.x='interval', by.y='Interval')
imputed_data[is.na(imputed_data$steps), 'steps'] <- imputed_data[is.na(imputed_data$steps), 'AvgSteps']

new_missing <- nrow(subset(imputed_data, is.na(imputed_data$steps)))
print(paste("After imputation, there are", new_missing, "rows that have invalid or missing step counts."))

imputed_data <- subset(imputed_data, select=c('interval', 'steps', 'date'))
summary(imputed_data)

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

imputed_steps_per_day <- aggregate(imputed_data$steps, by=list(imputed_data$date), FUN=sum)
names(imputed_steps_per_day)<- c('date', 'steps')
hist(imputed_steps_per_day$steps)

print(paste("The (imputed) mean number of steps per day is", round(mean(imputed_steps_per_day$steps))))
print(paste("The (imputed) median number of steps per day is", round(median(imputed_steps_per_day$steps))))

print(paste("The original mean was",mean_steps,", the imputed mean was",mean(imputed_steps_per_day$steps)))
print(paste("The original median was",median_steps,", the imputed median was",median(imputed_steps_per_day$steps)))


# Are there differences in activity patterns between weekdays and weekends?

imputed_data$day_of_week <- weekdays(as.Date(imputed_data$date))
imputed_data[imputed_data$day_of_week=='Sunday' | imputed_data$day_of_week=='Saturday','day_type'] <- 'weekend'
imputed_data[imputed_data$day_type!='weekend','day_type'] <- 'weekday'
imputed_data$day_type <- as.factor(imputed_data$day_type)

imputed_steps_by_interval <- aggregate(imputed_data$steps, by=list(imputed_data$interval, imputed_data$day_type), FUN=mean)
names(imputed_steps_by_interval) = c('Interval', 'Day_Type', 'AvgSteps')

par(mfrow=c(2,1))
plot(imputed_steps_by_interval[imputed_steps_by_interval$Day_Type=='weekday', 'Interval'],
     imputed_steps_by_interval[imputed_steps_by_interval$Day_Type=='weekday', 'AvgSteps'], 
     main="Weekday Steps by Interval",xlab='Interval', ylab='Avg Steps', type ="l")


plot(imputed_steps_by_interval[imputed_steps_by_interval$Day_Type=='weekend', 'Interval'],
     imputed_steps_by_interval[imputed_steps_by_interval$Day_Type=='weekend', 'AvgSteps'], 
     main="Weekend Steps by Interval",xlab='Interval', ylab='Avg Steps', type ="l")
