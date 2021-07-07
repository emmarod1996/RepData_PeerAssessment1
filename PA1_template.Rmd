```{r include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```


##Course project 1
#Setting the directory
```{r}
if (getwd() != "C:/Users/rodriguezm.150/Documents/R/repdata_data_activity" ) { 
setwd("C:/Users/rodriguezm.150/Documents/R/repdata_data_activity") }
```

##Loading and preprocessing the data
#Reading the DB
```{r}
Activity <- read.csv("activity.csv")
```

#Converting the date to a date class
```{r}
Activity$date <- as.Date(Activity$date,"%Y-%m-%d")
```


#making the histogram
```{r}
library(ggplot2)
weekday <- weekdays(Activity$date)
Activity <- cbind(Activity,weekday)
```
## What is mean total number of steps taken per day?
#steps taken per day
```{r}
Activity_total_steps <- with(Activity, aggregate(steps,by=list(date), FUN=sum, na.rm=TRUE))
names(Activity_total_steps) <- c("Date","Steps")
hist(Activity_total_steps$Steps,main="Total number of steps taken per day",xlab = "Steps per day",
     col="darkgreen",ylim = c(0,20), breaks = seq(0,25000,by = 2500))
```

#mean and median of the steps taken each day
```{r}
mean(Activity_total_steps$Steps)
median(Activity_total_steps$Steps)
```


## What is the average daily activity pattern?
#daily activity pattern
```{r}
average_daily <- with(Activity, aggregate(steps,by=list(interval), FUN=mean, na.rm=TRUE))
names(average_daily) <- c("Interval","Mean")
plot(average_daily$Interval,average_daily$Mean, type = "l",col="darkgreen", lwd=2,
     xlab = "Interval",ylab = "Average number o steps", main = "Average number of steps per interval")
```
     

#Maximun number of steps
```{r}
average_daily[which.max(average_daily$Mean),]$Interval
```

##Imputting Missing values
```{r}
sum(is.na(Activity$steps))
```

#filling in missing values
```{r}
imputed_steps <- average_daily$Mean[match(Activity$interval,average_daily$Interval)]
```

#missing data filled in
```{r}
activity_imputed <- transform(Activity,steps=ifelse(is.na(Activity$steps),
                                                    imputed_steps,Activity$steps))
total_steps_imputed <- aggregate(steps~date,activity_imputed,sum)
names(total_steps_imputed) <- c("date","daily_steps")
```

#histogram total number of steps
```{r}
hist(total_steps_imputed$daily_steps,main="Total number of steps taken per day",xlab = "Steps per day",
     col="darkgreen",ylim = c(0,30), breaks = seq(0,25000,by = 2500))
```

#Median and mean of total number of steps taken
```{r}
mean(total_steps_imputed$daily_steps)
median(total_steps_imputed$daily_steps)
```

## Are there differences in activity patterns between weekdays and weekends?
#differences weekdays and weekends
```{r}
Activity_with_day <- transform(activity_imputed,day_of_week=ifelse(weekdays(activity_imputed$date)=="Saturday"|                                                                     weekdays(activity_imputed$date) == "Sunday","weekend","weekday"))
```


#time series by weekday
```{r}
Activity_by_date <- aggregate(steps~interval+day_of_week, Activity_with_day,mean,na.rm=TRUE)
plot <- ggplot(Activity_by_date,aes(x = interval,y = steps,color=day_of_week))+geom_line()+
  labs(title = "Average daily steps by type of date",x="Interval",y="Average number of steps")+
  facet_wrap(~day_of_week,ncol = 1, nrow = 2)
print(plot)
```
