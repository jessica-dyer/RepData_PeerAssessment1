---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
#******************************************************************
#Step 0. Downloading dependencies
#******************************************************************
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.0.3
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.4     v dplyr   1.0.2
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.0
```

```
## Warning: package 'ggplot2' was built under R version 4.0.3
```

```
## Warning: package 'tibble' was built under R version 4.0.3
```

```
## Warning: package 'forcats' was built under R version 4.0.3
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(dplyr)
library(readr)
library(ggplot2)
# 1. Download data in to data folder (git will ignore this folder)
if(!file.exists("./data")){dir.create("./data")}

if(!exists("activity")) {
  fileUrl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  
  zipF <- "./data/activity_monitoring_data.zip"
  
  if(!file.exists(zipF)) {
    download.file(fileUrl, destfile = "./data/activity_monitoring_data.zip", method = "curl")
    outDir <- "./data"
    activity <- unzip(zipF, exdir = outDir)
  }
  
  # 2. Read data into global environment
  activity <- read.table("data/activity.csv", header = TRUE, sep = ",", dec = ".")
}

# 3. Format the date variable with as.Date
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?


```r
hist_data <- aggregate(steps ~ date, data = activity, sum)

hist <- ggplot(hist_data, aes(x=steps)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef") +
    ggtitle("Steps per day")
hist
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
# mean_steps <- aggregate(activity[, 1], list(activity$date), mean)
mean <- mean(activity$steps)
median <- median(activity$steps)
```
The average number of steps taken per day is {r mean} and the median is {r median}

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
