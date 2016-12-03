Course Project 1
======================================================
### Installing the different packages that may be required in the course project
```{r}

library(ggplot2)
library(plyr)
library(dplyr)
```
### Loading the dataset

```{r echo=TRUE}
counts <- read.csv("C:/Users/amank/Downloads/reproducible_week2/activity.csv", na.strings = "")
summary(counts)
str(counts)
```

##Total number of steps per day
```{r echo=TRUE}
counts$day <- as.Date(counts$date)
counts$DateTime <- as.POSIXct(counts$date,format = "%Y-%m-%d")
clean <- counts[!is.na(counts$steps),]
```
## Converting the steps into numeric

```{r echo=TRUE}
counts$steps <- as.numeric(as.character(counts$steps))
total <- aggregate(counts$steps ~ counts$date, FUN = sum,)
colnames(total) <- c("Date","Steps")
```
## Plotting the graph

```{r echo=TRUE}
hist(total$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```

##Mean and median of the total number of steps

```{r echo=TRUE}

as.integer(mean(total$Steps))
as.integer(median(total$Steps))
```

## Average Daily Pattern Activity
```{r echo=TRUE}
clean <- counts[!is.na(counts$steps),]
average_pattern <- ddply(clean, .(interval), summarize, Avg = mean(steps))
a <- ggplot(data = average_pattern, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Count of Steps") 
a +geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

maximum <- max(average_pattern$Avg)
average_pattern[average_pattern$Avg==maximum,1]
```

##Inputing missing values
```{r echo=TRUE}
nrow(counts[is.na(counts$steps),])

avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
nadata<- counts[is.na(counts$steps),]
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
```
### Getting the summary of the new dataset
```{r echo=TRUE}
summary(newdata2)
str(newdata2)
```


### Joining the datasets
```{r echo=TRUE}
mergeData <- rbind(clean, newdata2)
counts$steps <- as.numeric(as.character(counts$steps))

counts$date <- as.Date(counts$date)
mergeData$steps <- as.numeric(as.character(mergeData$steps))
total2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(total2)<- c("Date", "Steps")
##Mean
as.integer(mean(total2$Steps))
#Median
as.integer(median(total2$Steps))


hist(total2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(total$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )

```

##Are there differences in activity patterns between weekdays and weekends?
```{r echo=TRUE}
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

library(lattice)

intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")

```

