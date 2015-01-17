---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
# Read raw data 
activity.data.raw <- read.csv("./activity.csv", header=TRUE,na.strings = "NA", stringsAsFactors = FALSE)

# Clean the data
# Take only the rows that have no NA values
ok <- complete.cases(activity.data.raw)
activity.data.modified <- activity.data.raw[ok,]
```
## What is mean total number of steps taken per day?


```r
# Change "date" column from 'character' type to 'Date' type
activity.data.modified$date <- as.Date(activity.data.modified$date)

# Calculate 'total number of steps' for each date
# Group the data according to "date" column and apply function 'sum' to 
# the "steps" column in each group

total.steps.in.a.day <- aggregate(steps ~ date, data=activity.data.modified, FUN=sum)
# Create a histogram plot on the 'total number of steps'
hist(total.steps.in.a.day[,2], main = "Histogram for 53 days (Excluding Missing Value dates) ",col = "lightblue", xlab = "Total number of steps in a day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# mean and median total number of steps taken per day
mean.total.steps <- mean (total.steps.in.a.day[,2])
median.total.steps <- median (total.steps.in.a.day[,2])
```
The **mean** is 1.0766189 &times; 10<sup>4</sup> and the **median** is 10765


## What is the average daily activity pattern?

```r
average.data <- aggregate(steps ~ interval, data=activity.data.modified, FUN=mean)

plot(average.data$interval, average.data$steps, type="l", main="Average daily activity pattern", xlab="5-minute interval", ylab="Average number of steps",col = "dark red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max.five.min.interval <- which.max(average.data$steps)
```
 104  **-th 5-minute interval**, on average across all the days in the dataset, contains the maximum number of steps

## Imputing missing values

Report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
not.ok <- sum(!ok)
```
Total number of rows with **missing values (NAs)** is 2304


Strategy used here:   Missing values are filled with mean for that 5-minute interval 


```r
# New data set with filled values would be activity.data.raw.copy
activity.data.raw.copy <- activity.data.raw
row.number <- 1L
for (i in average.data$interval){
    index <- which(activity.data.raw.copy$interval == i)
    index <- index[is.na(activity.data.raw.copy[index,1])]
    
    activity.data.raw.copy[index,1] <- average.data[row.number,2]
    
    row.number <- row.number + 1
}
```
Make a histogram of the total number of steps taken each day 

```r
# Change "date" column from 'character' type to 'Date' type
activity.data.raw.copy$date <- as.Date(activity.data.raw.copy$date)

# Calculate 'total number of steps' for each date
# Group the data according to "date" column and apply function 'sum' to 
# the "steps" column in each group
total.steps.in.a.day.complete.data <- aggregate(steps ~ date, data=activity.data.raw.copy, FUN=sum)
# Create a histogram plot on the 'total number of steps'
hist(total.steps.in.a.day.complete.data[,2], main = "Histogram for all days  ",col = "gray", xlab = "Total number of steps in a day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
# mean and median total number of steps taken per day
mean.total.steps.complete.data <- mean (total.steps.in.a.day.complete.data[,2])
median.total.steps.complete.data <- median (total.steps.in.a.day.complete.data[,2])
```

Calculate and report the mean and median total number of steps taken per day.
The **mean** is 1.0766189 &times; 10<sup>4</sup> and the **median** is 1.0766189 &times; 10<sup>4</sup>

Mean and Median Comparison


```r
mean.vector <- c(mean.total.steps,mean.total.steps.complete.data)
median.vector <- c(median.total.steps,median.total.steps.complete.data)
mean.median.table <- data.frame(mean.vector, median.vector, row.names = c("Without imputing missing values", "Imputing missing values") )
names(mean.median.table) <- c("Mean", "Median")
library("xtable")
disp.table <- xtable(mean.median.table)
print(disp.table,type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sat Jan 17 15:39:06 2015 -->
<table border=1>
<tr> <th>  </th> <th> Mean </th> <th> Median </th>  </tr>
  <tr> <td align="right"> Without imputing missing values </td> <td align="right"> 10766.19 </td> <td align="right"> 10765.00 </td> </tr>
  <tr> <td align="right"> Imputing missing values </td> <td align="right"> 10766.19 </td> <td align="right"> 10766.19 </td> </tr>
   </table>
Impact of imputing missing data on the estimates of the total daily number of steps

```r
# Summary of 'without imputing' total number of steps
s1 <- summary(total.steps.in.a.day$steps)
s1 <- as.matrix(s1)
s1 <- data.frame(s1)
# Summary of 'with imputing' total number of steps
s2 <- summary(total.steps.in.a.day.complete.data$steps)
s2 <- as.matrix(s2)
s2 <- data.frame(s2)

s1 <- cbind(s1,s2[,1])
colnames(s1) <- c("Without imputing","With imputing" )

print(xtable(s1), type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sat Jan 17 15:39:06 2015 -->
<table border=1>
<tr> <th>  </th> <th> Without imputing </th> <th> With imputing </th>  </tr>
  <tr> <td align="right"> Min. </td> <td align="right"> 41.00 </td> <td align="right"> 41.00 </td> </tr>
  <tr> <td align="right"> 1st Qu. </td> <td align="right"> 8841.00 </td> <td align="right"> 9819.00 </td> </tr>
  <tr> <td align="right"> Median </td> <td align="right"> 10760.00 </td> <td align="right"> 10770.00 </td> </tr>
  <tr> <td align="right"> Mean </td> <td align="right"> 10770.00 </td> <td align="right"> 10770.00 </td> </tr>
  <tr> <td align="right"> 3rd Qu. </td> <td align="right"> 13290.00 </td> <td align="right"> 12810.00 </td> </tr>
  <tr> <td align="right"> Max. </td> <td align="right"> 21190.00 </td> <td align="right"> 21190.00 </td> </tr>
   </table>
## Are there differences in activity patterns between weekdays and weekends?
