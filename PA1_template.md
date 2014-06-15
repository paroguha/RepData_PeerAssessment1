Title: PA1_template (Peer Assessment1 for reproducible research)
========================================================

This is Peer Assessment1 for the Reproducible Research course. The data is collected from a personal activity device of an anonymous individual from October through  November, 2012 monitoring the number of steps taken in 5 minute intervals each day. 

## Loading and processing the data

```{r read_data, echo=TRUE}
library(ggplot2) # we shall use ggplot2 for plotting figures

# download and read the data, convert columns for convenience
read_data <- function() {
    fname = "activity.zip"
    source_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if(!file.exists(fname)) {
        download.file(source_url, destfile=fname, method="curl")
    }
    con <- unz(fname, "activity.csv")
    tbl <- read.csv(con, header=T, colClasses=c("numeric", "character", "numeric"))
    tbl$interval <- factor(tbl$interval)
    tbl$date <- as.Date(tbl$date, format="%Y-%m-%d")
    tbl
}
tbl <- read_data()
```

Summarize data
```{r examine_data, echo=TRUE}
summary(tbl)
str(tbl)
```


##What is mean total number of steps taken per day?

Histogram of the daily total number of steps taken including the mean and median of the daily total steps.


```{r steps_per_day, echo=TRUE}
calc_steps_per_day <- function(tbl) {
    steps_per_day <- aggregate(steps ~ date, tbl, sum)
    colnames(steps_per_day) <- c("date", "steps")
    steps_per_day

}

plot_steps_per_day <- function(steps_per_day, mean_steps, median_steps) {
    col_labels=c(paste("Mean:", mean_steps), paste("Median:", median_steps))
    cols = c("green", "yellow")
    
    ggplot(steps_per_day, aes(x=steps)) + 
        geom_histogram(fill="steelblue", binwidth=1500) + 
        geom_point(aes(x=mean_steps, y=0, color="green"), size=4, shape=15) + 
        geom_point(aes(x=median_steps, y=0, color="yellow"), size=4, shape=15) + 
        scale_color_manual(name=element_blank(), labels=col_labels, values=cols) + 
        labs(title="Histogram of Steps Taken per Day", x="Number of Steps", y="Count") + 
        theme_bw() + theme(legend.position = "bottom")    
}

steps_per_day <- calc_steps_per_day(tbl)
mean_steps = round(mean(steps_per_day$steps), 2)
median_steps = round(median(steps_per_day$steps), 2)
plot_steps_per_day(steps_per_day, mean_steps, median_steps)
```

**For the total number of steps taken per day:**  
**`r paste("Mean:", mean_steps)`**
**`r paste("Median:", median_steps)`**


## What is the average daily activity pattern?

Plot of the average daily activity pattern.

```{r steps_per_interval, echo=TRUE}
calc_steps_per_interval <- function(tbl) {
    steps_pi <- aggregate(tbl$steps, by=list(interval=tbl$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    steps_pi$interval <- as.integer(levels(steps_pi$interval)[steps_pi$interval])
    colnames(steps_pi) <- c("interval", "steps")
    steps_pi

}

plot_activity_pattern <- function(steps_per_interval, max_step_interval) {
    col_labels=c(paste("Interval with Maximum Activity: ", max_step_interval))
    cols = c("red")
    
    ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="steelblue", size=1) +  
        geom_point(aes(x=max_step_interval, y=0, color="red"), size=4, shape=15) +  
        scale_color_manual(name=element_blank(), labels=col_labels, values=cols) +     
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw() + theme(legend.position = "bottom")
}

steps_per_interval <- calc_steps_per_interval(tbl)
max_step_interval <- steps_per_interval[which.max(steps_per_interval$steps),]$interval

plot_activity_pattern(steps_per_interval, max_step_interval)

```

The **`r max_step_interval`<sup>th</sup> interval** has the maximum activity on the average.

## Imputing missing values

Missing values are replaced by the mean values. 

```{r impute_data, echo=TRUE}
impute_means <- function(tbl, defaults) {
    na_indices <- which(is.na(tbl$steps))
    defaults <- steps_per_interval
    na_replacements <- unlist(lapply(na_indices, FUN=function(idx){
        interval = tbl[idx,]$interval
        defaults[defaults$interval == interval,]$steps
        }))
    imp_steps <- tbl$steps
    imp_steps[na_indices] <- na_replacements
    imp_steps
}
complete_tbl <- data.frame(  
    steps = impute_means(tbl, steps_per_interval),  
    date = tbl$date,  
    interval = tbl$interval)
```

Histogram of the daily total number of steps With the imputed dataset.

```{r complete_steps_per_day, echo=TRUE}
complete_steps_per_day <- calc_steps_per_day(complete_tbl)
complete_mean_steps = round(mean(complete_steps_per_day$steps), 2)
complete_median_steps = round(median(complete_steps_per_day$steps), 2)
plot_steps_per_day(complete_steps_per_day, complete_mean_steps, complete_median_steps)


```

Although mean value remains unchanghed, the median has shifted closer to the mean.

## Are there differences in activity patterns between weekdays and weekends?

We do this comparison with the table with filled-in missing values.

1. Augment the table with a column that indicates the day of the week
2. Subset the table into two parts - weekends (Saturday and Sunday) and weekdays (Monday through Friday).
3. Tabulate the average steps per interval for each dataset.
4. Plot the two datasets side by side for comparison.

```{r weekday_compare, echo=TRUE}
calc_day_of_week_data <- function(tbl) {
    tbl$weekday <- as.factor(weekdays(tbl$date))
    weekend_data <- subset(tbl, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(tbl, !weekday %in% c("Saturday","Sunday"))
    
    weekend_spi <- calc_steps_per_interval(weekend_data)
    weekday_spi <- calc_steps_per_interval(weekday_data)
    
    weekend_spi$dayofweek <- rep("weekend", nrow(weekend_spi))
    weekday_spi$dayofweek <- rep("weekday", nrow(weekday_spi))
    
    day_of_week_data <- rbind(weekend_spi, weekday_spi)
    day_of_week_data$dayofweek <- as.factor(day_of_week_data$dayofweek)
    day_of_week_data
}
plot_day_of_week_comparison <- function(dow_data) {
    ggplot(dow_data, 
        aes(x=interval, y=steps)) + 
        geom_line(color="steelblue", size=1) + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
}
day_of_week_data <- calc_day_of_week_data(complete_tbl)
plot_day_of_week_comparison(day_of_week_data)
```

Activity on the weekends tends to be more spread out compared to the weekdays since weekends are more adhoc than weekdays with regular work schedule.
