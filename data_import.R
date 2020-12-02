#******************************************************************
#Step 0. Downloading dependencies
#******************************************************************
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

#******************************************************************
#Step 1. Downloading & unzipping data
#******************************************************************
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