Course Project 1
======================================================
### Installing the different packages that may be required in the course project

```r
library(ggplot2)
library(plyr)
library(dplyr)
```
### Loading the dataset


```r
counts <- read.csv("C:/Users/amank/Downloads/reproducible_week2/activity.csv", na.strings = "")
summary(counts)
```

```
##      steps               date          interval     
##  0      :11014   2012-10-01:  288   Min.   :   0.0  
##  NA     : 2304   2012-10-02:  288   1st Qu.: 588.8  
##  7      :   87   2012-10-03:  288   Median :1177.5  
##  8      :   83   2012-10-04:  288   Mean   :1177.5  
##  15     :   68   2012-10-05:  288   3rd Qu.:1766.2  
##  16     :   65   2012-10-06:  288   Max.   :2355.0  
##  (Other): 3947   (Other)   :15840
```

```r
str(counts)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : Factor w/ 618 levels "0","1","10","100",..: 618 618 618 618 618 618 618 618 618 618 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##Total number of steps per day

```r
counts$day <- as.Date(counts$date)
counts$DateTime <- as.POSIXct(counts$date,format = "%Y-%m-%d")
clean <- counts[!is.na(counts$steps),]
```
## Converting the steps into numeric


```r
counts$steps <- as.numeric(as.character(counts$steps))
```

```
## Warning: NAs introduced by coercion
```

```r
total <- aggregate(counts$steps ~ counts$date, FUN = sum,)
colnames(total) <- c("Date","Steps")
```
## Plotting the graph


```r
hist(total$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

##Mean and median of the total number of steps


```r
as.integer(mean(total$Steps))
```

```
## [1] 10766
```

```r
as.integer(median(total$Steps))
```

```
## [1] 10765
```

## Average Daily Pattern Activity

```r
clean <- counts[!is.na(counts$steps),]
average_pattern <- ddply(clean, .(interval), summarize, Avg = mean(steps))
a <- ggplot(data = average_pattern, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Count of Steps") 
a +geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
maximum <- max(average_pattern$Avg)
average_pattern[average_pattern$Avg==maximum,1]
```

```
## [1] 835
```

##Inputing missing values

```r
nrow(counts[is.na(counts$steps),])
```

```
## [1] 2304
```

```r
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
nadata<- counts[is.na(counts$steps),]
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
```
### Getting the summary of the new dataset

```r
summary(newdata2)
```

```
##      steps             date      interval        day        DateTime 
##  Min.   : NA   2012-10-01:0   Min.   : NA   Min.   :NA   Min.   :NA  
##  1st Qu.: NA   2012-10-02:0   1st Qu.: NA   1st Qu.:NA   1st Qu.:NA  
##  Median : NA   2012-10-03:0   Median : NA   Median :NA   Median :NA  
##  Mean   :NaN   2012-10-04:0   Mean   :NaN   Mean   :NA   Mean   :NA  
##  3rd Qu.: NA   2012-10-05:0   3rd Qu.: NA   3rd Qu.:NA   3rd Qu.:NA  
##  Max.   : NA   2012-10-06:0   Max.   : NA   Max.   :NA   Max.   :NA  
##                (Other)   :0
```

```r
str(newdata2)
```

```
## 'data.frame':	0 obs. of  5 variables:
##  $ steps   : num 
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 
##  $ interval: int 
##  $ day     :Class 'Date'  num(0) 
##  $ DateTime:Classes 'POSIXct', 'POSIXt'  atomic (0) 
##   ..- attr(*, "tzone")= chr ""
```


### Joining the datasets

```r
mergeData <- rbind(clean, newdata2)
counts$steps <- as.numeric(as.character(counts$steps))

counts$date <- as.Date(counts$date)
mergeData$steps <- as.numeric(as.character(mergeData$steps))
total2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(total2)<- c("Date", "Steps")
##Mean
as.integer(mean(total2$Steps))
```

```
## [1] 10766
```

```r
#Median
as.integer(median(total2$Steps))
```

```
## [1] 10765
```

```r
hist(total2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(total$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

##Are there differences in activity patterns between weekdays and weekends?

```r
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

library(lattice)

intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


