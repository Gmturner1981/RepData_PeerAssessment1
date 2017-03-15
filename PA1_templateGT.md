---
title: 'Reproducible Research: Peer Assessment 1'
author: "G Turner"
date: "12/03/2017"
output:
  pdf_document: default
  html_document: default
---


### Loading and pre-processing the data

In order to complete the assignment, it is necessary to load and pre-process the data:
Step (1.) read the data in to [R]
Step (2.) amend date column to the [R] date format
Step (3.) create a new df where NA's are removed
Step (4.) check the set up of the data tables

```{r load_data}
library(readr)
activity.df <- read.csv("~/activity.csv", stringsAsFactors = FALSE)
activity.df$date <- as.Date(activity.df$date, format = "%Y-%m-%d")
activity.df_no_na <- activity.df[!is.na(activity.df$steps),]
## table with NA's
head(activity.df)
summary(activity.df)
## table without NA's
head(activity.df_no_na)
summary(activity.df_no_na)
```

### What is mean total number of steps taken per day?

```{r countsteps}
library(ggplot2)
activity.df_sum <- aggregate(activity.df_no_na$steps, by = list(activity.df_no_na$date), FUN = sum)
names(activity.df_sum) <- c("Date", "Sum.Steps")
hist(activity.df_sum$Sum.Steps,
     xlab = "Total # of steps",
     main = "Histogram: Total # of steps p/day",
     cex.axis = .8,
     cex.lab = .8,
     cex.main = .8,
     breaks = 20,
     border = "dark red",
     col = "red",
     las = 1)
## statistics of the new df
summary(activity.df_sum)
options("scipen"=100, "digits"=4) # code ensures data is not shown in exponential notation, on the below in-line values.
mean0 <- mean(activity.df_sum$Sum.Steps)
median0 <- median(activity.df_sum$Sum.Steps)
```

Mean:     `r mean0`
Median:   `r median0`



## What is the average daily activity pattern?

To verify the avg daily activty pattern per 5 minutes intervals, we can run a line plot.

This code generates this step:
a new data frame calculating the mean grouped by the variable interval, and plot this in a time plot:

```{r avgdailypattern}
library(ggplot2)
## calculate the mean grouped by the variable internal
activity.df_avg <- aggregate(x=list(steps=activity.df_no_na$steps), by=list(interval=activity.df_no_na$interval),
                      FUN=mean, na.rm=TRUE)
names(activity.df_avg) <- c("interval2", "Average.Steps")
## plot a time series
ggplot(data=activity.df_avg, aes(x=interval2, y=Average.Steps, col="red")) +
    geom_line() +
    xlab("5-min interval") +
    ylab("Avg # of steps")
summary(activity.df_avg)
```

On average across all the days in the dataset, the 5-minute interval containing the maximum number of steps can be calculated via:

```{r Avgsteps}
data <- activity.df_avg[activity.df_avg$Average.Steps == max(activity.df_avg$Average.Steps),]
data
```

The interval with the highest amount of steps is `r data$inerval2` totaling steps of `r data$Average.Steps`.


## Input missing values

There are many days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

```{r fixingmissing}
## count the number of rows missing data
answer_cnt <- nrow(activity.df[is.na(activity.df$steps),])
## calculate the percentage of the data this represents
answer_pct <- paste(round(nrow(activity.df[is.na(activity.df$steps),])/nrow(activity.df) * 100,2), "%")
```

There are `r answer_cnt` rows missing data.  This represents `r answer_pct` of the data.

We can fill all of the missing values with the mean value for that 5-minute interval.
```{r mean_5min_interval}
## Replace each missing value with the mean value of its 5-minute interval
## checking the tables for compatability and field names
summary(activity.df)
summary(activity.df_avg)
## Run a merge
activity.df_fix2 <-  merge(activity.df, activity.df_avg, by.x = "interval", by.y = "interval2")
activity.df_fix2$steps.fix <- with(activity.df_fix2, ifelse(is.na(steps), Average.Steps, steps))
## Check output
summary(activity.df_fix2)
```

The new column steps.fix contains the count of steps after the correct for NA's made.  The next step will run a check on using this:

```{r check on steps_fix}
activity.df_sum2 <- aggregate(activity.df_fix2$steps.fix, by = list(activity.df_fix2$date), FUN = sum)
names(activity.df_sum2) <- c("Date", "Sum.Steps")
summary(activity.df_sum2)
mean2 <- mean(activity.df_sum2$Sum.Steps)
median2 <- median(activity.df_sum2$Sum.Steps)
```

Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.

```{r historgram}
hist(activity.df_sum2$Sum.Steps,
     xlab = "Total # of steps",
     main = "Histogram: Total # of steps p/day",
     cex.axis = .8,
     cex.lab = .8,
     cex.main = .8,
     breaks = 20,
     border = "dark green",
     col = "green",
     las = 1, 
     ylim = c(0, 20))
```

First mean & median results:
Mean:     `r mean0`
Median:   `r median0`
Second mean& median results:
Mean:     `r mean2`
Median:   `r median2`

The Mean and median values are higher after imputing missing data. The reason is that in the original data there are some days with step values NA for any interval.

The total number of steps taken in such days are set to 0s by default. However, after replacing missing steps values with the mean steps of associated interval value, these 0 values are removed from the histogram of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?

First, let's find the day of the week for each measurement in the dataset. In this part, we use the dataset with the filled-in values.

```{r weekenddaydiff}
activity.df_fix2$weekday <- factor(with(activity.df_fix2, ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekdays")))
```

Then we calculate the mean per interval and weekday:

```{r weekendday_mean}
summary(activity.df_fix2)
activity.df_avg2 <- aggregate(steps.fix~interval+weekday, data = activity.df_fix2, FUN = mean)
names(activity.df_avg2) <- c("Interval", "Weekday", "Average.Steps")
```

Now, let's make a panel plot containing plots of average number of steps taken on weekdays and weekends.

```{r panelplot}
timeplot <- ggplot(data=activity.df_avg2, aes(x=Interval, y=Average.Steps)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    geom_line() +
    facet_grid(Weekday ~ ., scale ="free")
print(timeplot)
```


The results will show that weekday activity is concentrated at a particular interval/time - but activity at the weekend has more variability of the course of time.
