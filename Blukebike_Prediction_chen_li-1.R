# Import Libraries
library(forecast)
library(tidyverse)
library(lubridate)
library(scales)
library(geosphere)
library(ggmap)
library('lubridate')
library(gains)
library(imputeTS)
library(caret)
library(ggplot2)
library(reshape2)

lift_chart <-function(pred,testdata){ 
  gain <- gains(testdata$num_rides, pred)
  ride <- testdata$num_rides
  plot(c(0,gain$cume.pct.of.total*sum(ride))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Ride number", main="Lift Chart", type="l")
  # baseline
  lines(c(0,sum(ride))~c(0,dim(testdata)[1]), col="gray", lty=2)
}

r2 <-function(preds,actual){ 
  return(1- sum((preds - actual) ^ 2,na.rm=T)/sum((actual - mean(actual,na.rm=T))^2,na.rm=T))
}



############### Process weather data ###########################################


weather_df =  read_csv("2935830.csv", na = c("","NA","\\N","NULL"))
weather_df = weather_df[(weather_df$REPORT_TYPE == 'FM-15' | weather_df$REPORT_TYPE == 'SY-MT'),]
weather_df$Year = year(weather_df$DATE)
weather_df$Month = month(weather_df$DATE)
weather_df$Day = day(weather_df$DATE)
weather_df$Hour = hour(weather_df$DATE)
weather_df$weekdays = weekdays(weather_df$DATE)

weather_df = weather_df[,names(weather_df) %in% c("Year","Month","Day","Hour","HourlyRelativeHumidity","weekdays",
                                     "HourlyVisibility","HourlyDryBulbTemperature","HourlyDewPointTemperature",
                                     "HourlyWetBulbTemperature","HourlyWindSpeed","HourlyPrecipitation")]



weather_df$HourlyPrecipitation <- str_replace(weather_df$HourlyPrecipitation, "T", "0.00")
weather_df$HourlyPrecipitation <- as.numeric(weather_df$HourlyPrecipitation)

glimpse(weather_df)
write_csv(weather_df,"Cleaned_Boston_Weather.csv")


############### Process bluebike data ###########################################
df_201901 <- read_csv("201901-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201902 <- read_csv("201902-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201903 <- read_csv("201903-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201904 <- read_csv("201904-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201905 <- read_csv("201905-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201906 <- read_csv("201906-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201907 <- read_csv("201907-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201908 <- read_csv("201908-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201909 <- read_csv("201909-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201910 <- read_csv("201910-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201911 <- read_csv("201911-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_201912 <- read_csv("201912-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))

df_202001 <- read_csv("202001-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202002 <- read_csv("202002-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202003 <- read_csv("202003-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202004 <- read_csv("202004-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202005 <- read_csv("202005-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202006 <- read_csv("202006-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202007 <- read_csv("202007-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202008 <- read_csv("202008-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202009 <- read_csv("202009-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202010 <- read_csv("202010-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202011 <- read_csv("202011-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202012 <- read_csv("202012-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))

df_202101 <- read_csv("202101-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202102 <- read_csv("202102-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202103 <- read_csv("202103-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202104 <- read_csv("202104-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202105 <- read_csv("202105-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202106 <- read_csv("202106-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202107 <- read_csv("202107-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202108 <- read_csv("202108-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202109 <- read_csv("202109-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202110 <- read_csv("202110-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202111 <- read_csv("202111-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202112 <- read_csv("202112-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))


df_202201 <- read_csv("202201-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202202 <- read_csv("202202-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))
df_202203 <- read_csv("202203-bluebikes-tripdata.csv", na = c("","NA","\\N","NULL"))


bike_df = rbind(df_201901[1:13],df_201902[1:13],df_201903[1:13],df_201904[1:13],df_201905[1:13],
                df_201906[1:13],df_201907[1:13],df_201908[1:13],df_201909[1:13],df_201910[1:13],
                df_201911[1:13],df_201912[1:13],df_202001[1:13],df_202002[1:13],df_202003[1:13],
                df_202004[1:13],df_202005[1:13],df_202006[1:13],df_202007[1:13],df_202008[1:13],
                df_202009[1:13],df_202010[1:13],df_202011[1:13],df_202012[1:13],df_202101[1:13],
                df_202102[1:13],df_202103[1:13],df_202104[1:13],df_202105[1:13],df_202106[1:13],
                df_202107[1:13],df_202108[1:13],df_202109[1:13],df_202110[1:13],df_202111[1:13],
                df_202112[1:13],df_202201[1:13],df_202202[1:13],df_202203[1:13])

#### Date cleaning on merged bike data
# Convert startime into time format and extract year, month, day & hour information 
bike_df$starttime = as.POSIXct(bike_df$starttime)
bike_df$Year = year(bike_df$starttime)
bike_df$Month = month(bike_df$starttime)
bike_df$Day = day(bike_df$starttime)
bike_df$Hour = hour(bike_df$starttime)
bike_df$weekdays = weekdays(bike_df$starttime)

# factorized categorical variables
bike_df$usertype = factor(bike_df$usertype)



write_csv(bike_df,"Merged_bike_data.csv")



############### Create final dataset used for prediction  ###########################################

ride_df = summarize(group_by(bike_df, Year, Month, Day,Hour), num_rides = n())
ride_df  <- ungroup(ride_df)

final_df =left_join(ride_df,weather_df)

final_df = final_df[
  order(final_df[,1], final_df[,2],final_df[,3],final_df[,4] ),
]


# Add Holiday indicator - only counting for federal holidays
final_df = mutate(final_df, Holiday = if_else((final_df$Month==1 & final_df$Day==1 & final_df$Year==2019) |
                                              (final_df$Month==1 & final_df$Day==21 & final_df$Year==2019) |
                                              (final_df$Month==5 & final_df$Day==27 & final_df$Year==2019) |
                                              (final_df$Month==7 & final_df$Day==4 & final_df$Year==2019) |
                                              (final_df$Month==9 & final_df$Day==2 & final_df$Year==2019) |
                                              (final_df$Month==11 & final_df$Day==28 & final_df$Year==2019) |
                                              (final_df$Month==12 & final_df$Day==25 & final_df$Year==2019) |
                                              (final_df$Month==1 & final_df$Day==1 & final_df$Year==2020) |
                                              (final_df$Month==1 & final_df$Day==20 & final_df$Year==2020) |
                                              (final_df$Month==5 & final_df$Day==25 & final_df$Year==2020) |
                                              (final_df$Month==7 & final_df$Day==3 & final_df$Year==2020) |
                                              (final_df$Month==9 & final_df$Day==7 & final_df$Year==2020) |
                                              (final_df$Month==11 & final_df$Day==26 & final_df$Year==2020) |
                                              (final_df$Month==12 & final_df$Day==25 & final_df$Year==2020) |
                                              (final_df$Month==1 & final_df$Day==1 & final_df$Year==2021) |
                                              (final_df$Month==1 & final_df$Day==18 & final_df$Year==2021) |
                                              (final_df$Month==5 & final_df$Day==31 & final_df$Year==2021) |
                                              (final_df$Month==7 & final_df$Day==4 & final_df$Year==2021) |
                                              (final_df$Month==9 & final_df$Day==6 & final_df$Year==2021) |
                                              (final_df$Month==11 & final_df$Day==25 & final_df$Year==2021) |
                                              (final_df$Month==12 & final_df$Day==25 & final_df$Year==2021) |
                                              (final_df$Month==12 & final_df$Day==31 & final_df$Year==2021) |
                                              (final_df$Month==1 & final_df$Day==1 & final_df$Year==2022) |
                                              (final_df$Month==1 & final_df$Day==17 & final_df$Year==2022), 1, 0))


# change order of columns 
final_df <- final_df[, c("Year", "Month", "Day","Hour","weekdays","Holiday","HourlyRelativeHumidity",
                         "HourlyVisibility","HourlyDryBulbTemperature","HourlyDewPointTemperature",
                         "HourlyWetBulbTemperature","HourlyWindSpeed","HourlyPrecipitation","num_rides")]

final_df$weekdays = factor(final_df$weekdays)


summary(final_df)
glimpse(final_df)

write_csv(final_df,"final.csv")


############### Plots & Data visualization  ###########################################

##########	How ride volumes has been change though time 
bike_df$starttime = date(bike_df$starttime)
day_volumn_df = group_by(bike_df, starttime)
day_volumn_df <- summarize(day_volumn_df, num_rides = n())

ggplot(data = day_volumn_df) +
  geom_line(mapping = aes(x = starttime, y = num_rides), color = "blue") + 
  labs(title = "Daily Number of Rides from Jan 2019 - Mar 2022", x = "Day", y = "Number of Trips") +
  scale_x_date(date_labels="%y-%m",minor_breaks = "1 month") +
  theme_light(base_size = 10)

# seperated by customer type 

day_volumn_df2 = group_by(bike_df, starttime,usertype)
day_volumn_df2 <- summarize(day_volumn_df2, num_rides = n())

ggplot(data = day_volumn_df2) +
  geom_line(mapping = aes(x = starttime, y = num_rides, color = usertype)) + 
  labs(title = "Daily Number of Rides from Jan 2019 - Mar 2022", x = "Day", y = "Number of Trips") +
  scale_x_date(date_labels="%y-%m",minor_breaks = "1 month") +
  theme_light(base_size = 10)

##########	How ride volumes vary by year

# Bar plot
ggplot(final_df) + geom_bar(aes(x = Year, y = num_rides), stat = "summary", fun = "mean") + 
  theme_light(base_size = 10)+labs(x = "Year", y = "Average number of Ride", title = "Average Yearly Number of Trips")


ggplot(data = bike_df) +
  geom_bar(mapping = aes(x = Year, y = (..count..), fill = usertype)) + 
  labs(x = "Year", y = "Total number of Ride", title = "Total number of ride by Year")


########### How ride volumes vary by week

final_df$weekday <- factor(final_df$weekdays, levels=c("Monday", "Tuesday", "Wednesday"
                                                   ,"Thursday","Friday","Saturday","Sunday"))

ggplot(final_df) + geom_bar(aes(x = weekday, y = num_rides,na.rm = TRUE), stat = "summary", fun = "mean") + 
  theme_light(base_size = 10) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(x = "Weekdays", y = "Average number of Ride by Weekdays", title = "Average Weekly Number of Trips")

bike_df$weekday <- factor(bike_df$weekdays, levels=c("Monday", "Tuesday", "Wednesday"
                                                       ,"Thursday","Friday","Saturday","Sunday"))
ggplot(data = bike_df) +
  geom_bar(mapping = aes(x = weekday, y = (..count..), fill = usertype)) + 
  labs(x = "Weekdays", y = "Total number of Ride", title = "Total number of ride by weekdays")  + 
  theme_light(base_size = 10) +
  facet_wrap(~Year) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


##########  How ride volumes vary by Month

ggplot(data = final_df) +
  geom_bar(mapping = aes(x = factor(Month), y = num_rides), stat = "summary", fun = "mean") + 
  labs(title = "Average Monthly Number of Trips", x = "Month", y = "Average Number of Trips") +
  theme_light(base_size = 10)

ggplot(data = bike_df) +
  geom_bar(mapping = aes(x = factor(Month), y = (..count..), fill = usertype)) + 
  labs(x = "Month", y = "Total number of Ride", title = "Total number of ride by Month")  + 
  theme_light(base_size = 10) +
  facet_wrap(~Year) 

# Box plot
ggplot(final_df) + geom_boxplot(aes(x = factor(Month), y = num_rides )) + xlab("Month") 


##########   How ride volumes vary by Hour 

ggplot(data = final_df) +
  geom_bar(mapping = aes(x = factor(Hour), y = num_rides), stat = "summary", fun = "mean") + 
  labs(title = "Average Hourly Number of Trips", x = "Hour", y = "Average Number of Trips") +
  theme_light(base_size = 10)

ggplot(data = bike_df) +
  geom_bar(mapping = aes(x = factor(Hour), y = (..count..), fill = usertype)) + 
  labs(x = "Hour", y = "Total number of Ride", title = "Total number of ride by Hour")  + 
  theme_light(base_size = 10) +
  facet_wrap(~Year) 

# Box plot
ggplot(final_df) + geom_boxplot(aes(x = factor(Hour), y = num_rides )) + xlab("Hour") 


##########  	Model correlation , heatmap 
corr = cor(na.omit(final_df[,-c(1,2,3,4,5,6,13,15)]))
corr
heatmap(corr, Rowv = NA, Colv = NA)

### higher than 0.75 correlation
## HourlyDryBulbTemperature vs HourlyDewPointTemperature: 0.8951062
## HourlyDryBulbTemperature vs HourlyWetBulbTemperature: 0.9752065
## HourlyWetBulbTemperature  vs HourlyDewPointTemperature: 0.967432298
## Will only keep HourlyDryBulbTemperature as it is mostly correlated with num_ride 0.46879442


# pairplot on numeric values 
library(GGally)
ggpairs(final_df[, c(7,8,9,12,13,14)],upper = "blank")



##########  How ride volumes change via temperature, windspeed and 
ggplot(final_df) + geom_point(colour = "navy", alpha = 0.7,aes(x = num_rides, y = HourlyDryBulbTemperature))

ggplot(final_df) + geom_point(colour = "navy", alpha = 0.7,aes(x = num_rides, y = HourlyWindSpeed))

ggplot(final_df) + geom_point(colour = "navy", alpha = 0.7,aes(x = num_rides, y = HourlyPrecipitation))




##########  	How bike demand varies at different station using map

num_bike_Station_total = group_by(bike_df,`start station id`)
num_bike_Station_total <- summarize(num_bike_Station_total, rides_num = n())
bike_loc_num_total <- full_join(num_bike_Station_total,location)
bike_loc_num_total$Indicator = ifelse(bike_loc_num_total$rides_num>30000, "High","Low")

## plot in all years
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = rides_num, color = Indicator), data = bike_loc_num_total, alpha = 0.5) +
  scale_size(range = c(0, 10)) +
  labs(title = "Bike Demand by stations") +
  theme_light(base_size = 10)



num_bike_Station = group_by(bike_df,Year, `start.station.id`)
num_bike_Station <- summarize(num_bike_Station, rides_num = n())

location = select(bike_df, `start.station.id`, `start.station.longitude`, `start.station.latitude`)
location = group_by(location, `start.station.id`)
location <- summarize(location, lon = mean(`start.station.longitude`), lat = mean(`start.station.latitude`))

bike_loc_num <- full_join(num_bike_Station,location)

bike_loc_num$Indicator = ifelse(bike_loc_num$rides_num>8000, "High","Low")

map <- get_map(location = c(left = -71.15, bottom = 42.34, right = -71.05, top = 42.38),
                 source = "google", maptype = "terrain")
ggmap(map)


## plot in 2019 
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = rides_num, color = Indicator), data = bike_loc_num[bike_loc_num$Year==2019,], alpha = 0.5) +
  scale_size(range = c(0, 10)) +
  labs(title = "Bike Demand by stations in 2019") +
  theme_light(base_size = 10)

## plot in 2020 
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = rides_num, color = Indicator), data = bike_loc_num[bike_loc_num$Year==2020,], alpha = 0.5) +
  scale_size(range = c(0, 10)) +
  labs(title = "Bike Demand by stations in 2020") +
  theme_light(base_size = 10)

## plot in 2021
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = rides_num, color = Indicator), data = bike_loc_num[bike_loc_num$Year==2021,], alpha = 0.5) +
  scale_size(range = c(0, 10)) +
  labs(title = "Bike Demand by stations in 2021") +
  theme_light(base_size = 10)


## plot in 2022 
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = rides_num, color = Indicator), data = bike_loc_num[bike_loc_num$Year==2022,], alpha = 0.5) +
  scale_size(range = c(0, 10)) +
  labs(title = "Bike Demand by stations in 2022") +
  theme_light(base_size = 10)

############### Data Preprocessing  ###########################################

final_df$Year = factor(final_df$Year )
final_df$Month = factor(final_df$Month)
final_df$Day = factor(final_df$Day)
final_df$Hour = factor(final_df$Hour)
final_df$weekdays = factor(final_df$weekdays)
final_df$HourlyVisibility = as.numeric(final_df$HourlyVisibility)
final_df = na.omit(final_df)
glimpse(final_df)


##########  Train/test Split   ###########################################

#### 60/40 split & Get rid of dew point,  wet bulb temp and Day
set.seed(1)
train.index = sample(row.names(final_df), 0.6*dim(final_df)[1])  
test.index = setdiff(row.names(final_df), train.index)  
train_df = final_df[train.index,-c(3,10,11)]
test_df = final_df[test.index,-c(3,10,11)]

glimpse(train_df)
glimpse(test_df)






############### Evaluate Model performance  ###########################################


##########  Linear Regression   ###########################################

## Build model
lr_mod = lm(num_rides~., data = train_df)
summary(lr_mod)
# R square: 0.5035

## Predict
lm_pred <- predict(lr_mod, newdata=test_df)

## Evaluate
# accuracy table
accuracy(lm_pred, test_df$num_rides)
# RMSE: 210.6181

# R square
r2(lm_pred, test_df$num_rides)
# 0.5042584

# lift chart 
lift_chart(lm_pred, test_df)



##########  Linear Regression with regulation   ###########################################

library(glmnet)

# convert dummy variables
m <- model.matrix( ~ Year + Month + Hour + weekdays , data = train_df)
train_x = cbind(m[,-1],train_df[,5:10])
train_x  = data.matrix(train_x )
m <- model.matrix( ~ Year + Month + Hour + weekdays , data = test_df)
test_x = cbind(m[,-1],test_df[,5:10])
test_x  = data.matrix(test_x )


##################### Lasso

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(train_x, train_df$num_rides, alpha = 1)

#produce plot of test MSE by lambda value
plot(cv_model) 


#find optimal lambda value that minimizes test MSE
minlambda <- cv_model$lambda.min
minlambda

# build model with best lambda
l1_model <- glmnet(train_x, train_df$num_rides, alpha = 1, family="gaussian", lambda = minlambda)
coef(l1_model)

## predict
l1_pred = predict(l1_model, s = minlambda, newx = test_x)

## Evaluate
# accuracy table
RMSE(l1_pred, test_df$num_rides)
# RMSE: 210.6293

# R square
r2(l1_pred, test_df$num_rides)
# 0.5042054

# lift chart 
lift_chart(l1_pred, test_df)




##################### ridge

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(train_x, train_df$num_rides, alpha = 0)

#produce plot of test MSE by lambda value
plot(cv_model) 


#find optimal lambda value that minimizes test MSE
minlambda <- cv_model$lambda.min
minlambda
# 13.84091


# build model with best lambda
l2_model <- glmnet(train_x, train_df$num_rides, alpha = 0, family="gaussian", lambda = minlambda)
coef(l2_model)

## predict
l2_pred = predict(l2_model, s = minlambda, newx = test_x)

## Evaluate
# accuracy table
RMSE(l2_pred, test_df$num_rides)
# RMSE: 211.1612

# R square
r2(l2_pred, test_df$num_rides)
# 0.5016982

# lift chart 
lift_chart(l2_pred, test_df)



##################### elastic net

control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

# Training ELastic Net Regression model
elastic_model <- train( num_rides ~ .,
                       data = train_df,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 10,
                       trControl = control)

elastic_model
# The final values used for the model were alpha = 0.06049776 and lambda = 0.01189846.


## predict
elastic_pred = predict(elastic_model, test_df)

## Evaluate
# accuracy table
RMSE(elastic_pred, test_df$num_rides)
# RMSE:  210.6342

# R square
r2(elastic_pred, test_df$num_rides)
# 0.5041823

# lift chart 
lift_chart(elastic_pred, test_df)



##########  SVM   ###########################################

# fit SVM model
library (e1071)
svm_mod = svm(num_rides~., data = train_df,kernel = "radial") 

## Predict
svm_pred = predict(svm_mod, test_df)


## Evaluate

# accuracy table
accuracy(svm_pred, test_df$num_rides)
# RMSE: 185.6588

# R square
r2(svm_pred, test_df$num_rides)
# 0.6147921

# lift chart 
lift_chart(svm_pred, test_df)




##########  Random Forest     ###########################################
library(randomForest)
rf_mod <- randomForest(num_rides~., data = train_df, ntree = 100)

## Predict
rf_pred = predict(rf_mod, test_df)

## Evaluate
# accuracy table
accuracy(rf_pred, test_df$num_rides)
# RMSE:120.5527

# R square
r2(rf_pred, test_df$num_rides)
# 0.837588

# lift chart 
lift_chart(rf_pred, test_df)

# feature importance plot
varImpPlot(rf_mod)




##########  XGB boost    ###########################################
library(xgboost)

dtrain <- xgb.DMatrix(data = data.matrix(train_df[,-11]), label=train_df$num_rides)
dtest <- xgb.DMatrix(data = data.matrix(test_df[,-11]), label=test_df$num_rides)

result = data.frame(matrix(NA,nrow = 20, ncol = 2))
colnames(result) = c("RMSE", "R2")

for (rounds in seq(1,40,1)) {
    # train
    xbg_mod <- xgboost(data = dtrain, max.depth=20, nround=rounds)
    xgb_pred = predict(xbg_mod, dtest)
    result[rounds, 1] = accuracy(xgb_pred, test_df$num_rides)[2]
    result[rounds, 2] = r2(xgb_pred, test_df$num_rides)
}

# build model, 20 is most optimal 
xbg_mod  = xgboost(data = dtrain, max.depth = 20,  nrounds = 20)

## Predict
xgb_pred = predict(xbg_mod, dtest)

## Evaluate
# accuracy table
accuracy(xgb_pred, test_df$num_rides)
# RMSE:94.90862

# R square
r2(xgb_pred, test_df$num_rides)
# 0.8993357

result$round = as.numeric(rownames(result))
par(mfcol = c(2,1))
ggplot(result, aes(x=round, y=RMSE)) + geom_line() + geom_point() 
ggplot(result, aes(x=round, y=R2)) + geom_line() + geom_point() 

# lift chart 
lift_chart(xgb_pred, test_df)





##########  Neuron Net   ###########################################
library(keras)
library(tfdatasets)

# convert dummy variables
m <- model.matrix( ~ Year + Month + Hour + weekdays , data = train_df)
train.dummy.df = cbind(m[,-1],train_df[,5:11])

m <- model.matrix( ~ Year + Month + Hour + weekdays , data = test_df)
test.dummy.df = cbind(m[,-1],test_df[,5:11])

## build model
spec <- feature_spec(train.dummy.df, num_rides ~ . ) %>% 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

spec

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    


input <- layer_input_from_dataset(train.dummy.df %>% select(-num_rides))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )

model

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
  

history <- model %>% fit(
  x = train.dummy.df %>% select(-num_rides),
  y = train.dummy.df$num_rides,
  epochs = 100,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop)
)

plot(history)


test_predictions <- model %>% predict(test.dummy.df %>% select(-num_rides))
test_predictions[ , 1]

## Evaluate
# accuracy table
RMSE(test_predictions[ , 1], test_df$num_rides)
# RMSE:93.69632

# R square
r2(test_predictions[ , 1], test_df$num_rides)
# 0.9018909

# lift chart 
lift_chart(test_predictions[ , 1], test_df)

df = data.frame(cbind(test_predictions[ , 1], test_df$num_rides))
colnames(df) = c("NN prediction","Ride number in Test")
ggplot(df,aes(x=`NN prediction`,y=`Ride number in Test`),color = 'blue')+geom_point()+geom_smooth(method = "lm")



