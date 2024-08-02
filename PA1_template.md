---
title: "My_rmarkdown"
author: "Bartolomé Mestre Fons"
date: "2024-08-02"
output:
  html_document: default
  pdf_document: default
---

## Loading and preprocessing the data

Load the data:

```{r importdata}
data <- read.csv('activity.csv')
```
Process/transform the data (if necessary) into a format suitable for your analysis
```{r preprocessing}
data$date <- as.Date(data$date, format = '%Y-%m-%d')
```
## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day

```{r total_steps}
library(dplyr)
sol <- data %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE))
sol
```
Make a histogram of the total number of steps taken each day

```{r total_steps_plot}
hist(sol$total_steps/10^4, xlab = 'Number of steps x 10^4', main = 'Frequency of steps taken per day', breaks = 20)
```

```{r mean_and_median}
mea_val <- mean(sol$total_steps)
med_val <- median(sol$total_steps)
print(paste('Mean value:', mea_val, sep = ' '))
print(paste('Median value:', med_val, sep = ' '))
```

## What is the average daily activity pattern?
Make a time series plot (i.e. 
type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r activity_pattern_plot}
new_sol <- as.data.frame(data %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE)))
plot(new_sol[,1], new_sol[,2], type = 'l', col = 'red', main = 'Avergage steps per interval', xlab = 'Interval', ylab = 'Steps')
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r max_interval_steps}
final_interval <- new_sol[new_sol$steps == max(new_sol$steps),1]
print(paste('Max steps interval:', final_interval, sep = ' '))
```
##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as 
NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r number_of_NAS}
sumofnas <- sum(is.na(data))
print(paste('Total number of NAS:', sumofnas, sep = ' '))
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. In this case, we will replace the missing values for the mean value of the number of steps per interval. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r nas_strategy}
data_nonas <- data
for(i in new_sol$interval){
  data_nonas[data_nonas$interval == i & is.na(data_nonas$steps), 1] <- new_sol[new_sol$interval == i, 2] 
  
}
```
Make a histogram of the total number of steps taken each day.
```{r fill_nas_plot}
sol_nonas <- as.data.frame(data_nonas %>% group_by(date) %>% summarise(total_steps = sum(steps)))
hist(sol_nonas[,2], breaks = 20, xlab = 'Steps', main = 'Total Number of Steps Taken Each Day (Imputed)')
```
Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r mean_non_nas}
mea_val_nonas <- mean(sol_nonas[,2])
med_val_nonas <- median(sol_nonas[,2])
print(paste('New mean:',mea_val_nonas, 'New Median:', med_val_nonas, sep = ' '))
```
The mean is the same as the mean from the first part of the assignment, but the median is not, although their values are close. Imputing missing data using the average of the 5-minute interval results in more data points equal to the mean and smaller variation of the distribution. Since many data points have the same values as the mean, the median is much likely to be the same as the mean as well.
##Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r factor_weekdays}
data_nonas$day <- weekdays(data_nonas$date)
data_nonas$week <- ""
data_nonas[data_nonas$day == "sábado" | data_nonas$day == "domingo", ]$week <- "weekend"
data_nonas[!(data_nonas$day == "sábado" | data_nonas$day == "domingo"), ]$week <- "weekday"
data_nonas$week <- factor(data_nonas$week)
```
Make a panel plot containing a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r plot_weekday_results}
library(ggplot2)
final_data <- aggregate(steps ~ interval + week, data = data_nonas, mean)
ggplot(final_data, aes(interval, steps)) + geom_line(colour = 'red') + facet_grid(.~week) + labs(title = 'Average Number of Steps Taken (across all weekday days or weekend days)', x = 'Interval', y = 'Steps')
```
