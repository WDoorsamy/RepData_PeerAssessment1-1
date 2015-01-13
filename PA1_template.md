---
title: "Test"
author: "WDoorsamy"
date: "Monday, January 12, 2015"
output: html_document
---


## Loading and preprocessing the data

```r
if (!file.exists("./data")) {
    dir.create("./data")
}
fileUrl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/activity.zip")
unzip("./data/activity.zip", exdir = "./data", overwrite = TRUE)
act1 <- read.csv("./data/activity.csv", sep = ",", na.strings = "NA")
act <- act1[complete.cases(act1), ]
```

## What is mean total number of steps taken per day?


```r
aggrstep <- aggregate(act$steps, list(act$date), sum)
names(aggrstep) <- c("date", "totsteps")
```
What is the average daily activity pattern?
Make histogram for total steps for each day:

```r
hist(aggrstep$totsteps, main = "Frequency for total number of steps taken each day", 
    xlab = "total step")
```

![](Test_files/figure-latex/unnamed-chunk-7-1.pdf) 


## Imputing missing values

## Are there differences in activity patterns between weekdays and weekends?


