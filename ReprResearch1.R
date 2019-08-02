#import libraries
library(zip)

# filenames
zipfilename <- "./activity.zip"
datafilename <- "./activity.csv"

# 1. Read dataset
if(!file.exists(datafilename)) {
    unzip(zipfilename)
}
df <- read.csv(datafilename)
df$date <- as.Date(df$date, "%Y-%m-%d")


# 2. Histogram of the total number of steps taken each day
stepsPerDay <- tapply(X = df$step, INDEX = df$date, FUN = sum)
barplot(stepsPerDay)

# 3. Mean and median number of steps taken each day
tapply(X = df$step, INDEX = df$date, FUN = mean)
tapply(X = df$step, INDEX = df$date, FUN = median)

# 4. Time series plot of the average number of steps taken
stepsPerInterval <- tapply(X = df$step, INDEX = df$interval, FUN = sum, na.rm = T)
meanNumberOfStepsPerDay <- mean(stepsPerDay, na.rm = TRUE)
averageStepsPerInterval <- stepsPerInterval / meanNumberOfStepsPerDay
barplot(averageStepsPerInterval, type = "l")

# 5. The 5-minute interval that, on average, contains the maximum number of steps
# 6. Code to describe and show a strategy for imputing missing data
# 7. Histogram of the total number of steps taken each day after missing values are imputed
# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report