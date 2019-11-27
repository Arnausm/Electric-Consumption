pacman::p_load("forecast","plotly","esquisse",
               "dplyr","lubridate","RMySQL",
               "ggplot2","padr","zoo","imputeTS","tseries","caret")

#### Connection to sql ####

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
# dbListTables(con)

#### Data ####

yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

##  Dataset ##

newDF <- bind_rows(yr_2007, yr_2008, yr_2009, yr_2010)

#### Combine Date and Time attribute values in a new attribute column ####

newDF <- cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 

colnames(newDF)[11] <- "DateTime"

## Move the DateTime attribute within the dataset

newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]

## Give format ymd_hms to new column

newDF$DateTime <- ymd_hms(newDF$DateTime)

#### Preprocess: missing values ####

  # Not a single NA found but temporal serie isn't complete

## Added missing rows to data

data <- pad(x = newDF, by = "DateTime", break_above = 3)
                  # break_above hace referencia a Millones

#### Set data in the same units ####

# Globar active/reactive power: Watts per hour
# Sub_metering: watts per hour. sum every min to get 1h

newDF$Global_active_power <- round(newDF$Global_active_power*1000/60, digits = 3)
data$Global_active_power <- round(data$Global_active_power*1000/60, digits = 3)

newDF$Global_reactive_power <- round(newDF$Global_reactive_power*1000/60, digits = 3)
data$Global_reactive_power <- round(data$Global_reactive_power*1000/60, digits = 3)

# Creation new columns #

data$Date <- ymd(data$Date)
newDF$Date <- ymd(newDF$Date)

data$Time <- hms(data$Time)
newDF$Time <- hms(newDF$Time)

## Create "year", "month", "hour", "Day" ##

newDF$Year <- year(newDF$DateTime)
data$Year <- year(data$DateTime)

newDF$Month <- month(newDF$DateTime, label = F, abbr = F)
data$Month <- month(data$DateTime, label = F, abbr = F)
# label == T: el output será texto en vez de número

newDF$Hour <- hour(newDF$DateTime)
data$Hour <- hour(data$DateTime)

newDF$Day <- day(newDF$DateTime)
data$Day <- day(data$DateTime)

## Add names week ##

newDF$Day_of_the_week <- wday(newDF$Date, label = T, abbr = F)
data$Day_of_the_week <- wday(data$Date, label = T, abbr = F)

# newDF$Day_of_the_week[newDF$Day_of_the_week == 1] <- "Domingo"

#### Information % ####

  # The column percentage of DF_test shows the % of energy
  # that represents the three submeterings

DF_test <- data %>% group_by(Year,Month,Day) %>%
            summarise(Sub_1 = sum(Sub_metering_1),
                      Sub_2 = sum(Sub_metering_2),
                      Sub_3 = sum(Sub_metering_3),
                      Global_act_power = sum(Global_active_power),
                      Global_react_power = sum(Global_reactive_power)
                      )

for(i in 1:nrow(DF_test)) {
  
  DF_test$Percent[i] <- DF_test$Global_act_power[i] - (sum(DF_test$Sub_1[i] + DF_test$Sub_2[i] + DF_test$Sub_3[i]))
  
  DF_test$Percent[i] <- round((DF_test$Percent[i]/DF_test$Global_act_power[i])*100,digits = 2)
  
  DF_test$Percent[i] <-  100 - DF_test$Percent[i]
      }

#### Missing values ####

  # "na table" shows where the NA's are located (days) and the number of them.

na <- data %>% group_by(Year,Month,Day) %>% select("id") %>% filter(is.na(id)) %>% count(is.na(id))

na$Date <- ymd(paste(na$Year, na$Month, na$Day))
                 
## 

data$Date <- date(data$DateTime)

data$Time <- hms(paste(hour(data$DateTime),minute(data$DateTime), second(data$DateTime)))

data$Day_of_the_week <- wday(data$Date, label = T, abbr = F)

#### Applying na.locf ####

dates <- c()

for(i in 1:nrow(data)){
  
  if(data$Date[i] %in% na$Date) {
    
    a <- data$Date[i]
    
    dates <- append(dates,a)
  }
}

    rm(a)

    dates <- unique(dates)
    
for(i in 1:length(dates)) {
  
  if(na$n[i] < 10) {
  
    data[which(data$Date == na$Date[i]),] <- zoo::na.locf(data[which(data$Date == na$Date[i]),])
    
      }
    }
    
#### Detecting and removing outlier ####
    
## Visualization ##
    
Outlier <- data %>% group_by(Year,Month) %>% dplyr::summarise(sum(Global_active_power))
    # low values of august 2008
    
August_month <- data[data$Year == 2008 & data$Month == 8,]
    
## Distribution on power consumption divided by submeters ## 
    
plot_ly(August_month, x = ~August_month$DateTime, y = ~August_month$Sub_metering_1,
      name = 'Sub_1', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~August_month$Sub_metering_2,
      name = 'Sub_2', mode = 'lines') %>%
    add_trace(y = ~August_month$Sub_metering_3,
      name = 'Sub_3', mode = 'lines') %>%
    add_trace(y = ~August_month$Global_active_power,
      Name = 'Global Active Power', mode = 'lines') %>%
    layout(title = "Power Consumption per month",
      xaxis = list(title = "Time"),
      yaxis = list (title = "Power (watt-hours)"))
    
## AP ##
    
values_mean_ap <- round(mean(c(mean_07 = mean(data[data$Year == 2007 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Global_active_power,na.rm = TRUE),
                              mean_09 = mean(data[data$Year == 2009 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Global_active_power,na.rm = TRUE),
                              mean_10 = mean(data[data$Year == 2010 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Global_active_power,na.rm = TRUE)
                              )),digits = 2)
    
data[838081:876960,]$Global_active_power <- values_mean_ap
    
## Sub 1 ##
    
values_mean_sub1 <- round(mean(c(mean_07 = mean(data[data$Year == 2007 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_1,na.rm = TRUE),
                                mean_09 = mean(data[data$Year == 2009 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_1,na.rm = TRUE),
                                mean_10 = mean(data[data$Year == 2010 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_1,na.rm = TRUE)
                                )),digits = 2)
    
data[838081:876960,]$Sub_metering_1 <- values_mean_sub1
    
## Sub 2 ##
    
values_mean_sub2 <- round(mean(c(mean_07 = mean(data[data$Year == 2007 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_2,na.rm = TRUE),
                                 mean_09 = mean(data[data$Year == 2009 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_2,na.rm = TRUE),
                                mean_10 = mean(data[data$Year == 2010 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_2,na.rm = TRUE)
                                  )),digits = 2)
    
data[838081:876960,]$Sub_metering_2 <- values_mean_sub2
    
## Sub 3 ##
    
values_mean_sub3 <- round(mean(c(mean_07 = mean(data[data$Year == 2007 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_3,na.rm = TRUE),
                                  mean_09 = mean(data[data$Year == 2009 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_3,na.rm = TRUE),
                                  mean_10 = mean(data[data$Year == 2010 & data$Month == 8 & data$Day >= 5 & data$Day <= 31,]$Sub_metering_3,na.rm = TRUE)
                                  )),digits = 2)
    
data[838081:876960,]$Sub_metering_3 <- values_mean_sub3


#### Applying na.kalman for the rest of NAs ####
    

data$id <- na.interpolation(data$id, option = "linear")


for (i in 5:11) {

data[,i] <- na.ma(x = data[,i], k= 30000, weighting = "simple" )

    }

#### Group_by ####

## Year ##

df_year <- data %>% group_by(DateTime=floor_date(DateTime,"year")) %>% 
                    summarise(sub_1 = sum(Sub_metering_1, na.rm = T),
                              sub_2 = sum(Sub_metering_2, na.rm = T),
                              sub_3 = sum(Sub_metering_3, na.rm = T),
                              active_power = sum(Global_active_power, na.rm = T),
                              reactive_power = sum(Global_reactive_power, na.rm = T)
                              )

## Month ##

df_month <- data %>% group_by(DateTime=floor_date(DateTime,"month")) %>%
                      summarise(sub_1 = sum(Sub_metering_1, na.rm = T),
                                sub_2 = sum(Sub_metering_2, na.rm = T),
                                sub_3 = sum(Sub_metering_3, na.rm = T),
                                active_power = sum(Global_active_power, na.rm = T),
                                reactive_power = sum(Global_reactive_power, na.rm = T)
                                )

## Week ##

df_week <- data %>% group_by(DateTime=floor_date(DateTime,"week")) %>% 
                    summarise(sub_1 = sum(Sub_metering_1, na.rm = T),
                        sub_2 = sum(Sub_metering_2, na.rm = T),
                        sub_3 = sum(Sub_metering_3, na.rm = T),
                        active_power = sum(Global_active_power, na.rm = T),
                        reactive_power = sum(Global_reactive_power, na.rm = T)
                        )

## Day ##

df_day <- data %>% group_by(DateTime=floor_date(DateTime,"day")) %>%
                    summarise(sub_1 = sum(Sub_metering_1, na.rm = T),
                              sub_2 = sum(Sub_metering_2, na.rm = T),
                              sub_3 = sum(Sub_metering_3, na.rm = T),
                              active_power = sum(Global_active_power, na.rm = T),
                              reactive_power = sum(Global_reactive_power, na.rm = T)
                              )

#### Visualization ####

## Plot by month ##

plot_ly(df_month, x = ~df_month$DateTime, y = ~df_month$sub_1,
        name = 'Sub_1', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~df_month$sub_2,
            name = 'Sub_2', mode = 'lines') %>%
  add_trace(y = ~df_month$sub_3,
            name = 'Sub_3', mode = 'lines') %>%
  add_trace(y = ~df_month$active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption per month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Plot by day

plot_ly(df_day, x = ~df_day$DateTime, y = ~df_day$sub_1,
        name = 'Sub_1', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~df_day$sub_2,
            name = 'Sub_2', mode = 'lines') %>%
  add_trace(y = ~df_day$sub_3,
            name = 'Sub_3', mode = 'lines') %>%
  add_trace(y = ~df_day$active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption per day",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Plot by week ##

plot_ly(df_week, x = ~df_week$DateTime, y = ~df_week$sub_1,
        name = 'Sub_1', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~df_week$sub_2,
            name = 'Sub_2', mode = 'lines') %>%
  add_trace(y = ~df_week$sub_3,
            name = 'Sub_3', mode = 'lines') %>%
  add_trace(y = ~df_week$active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption per weeek",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Plot by year ##

plot_ly(df_year, x = ~df_year$DateTime, y = ~df_year$sub_1,
        name = 'Sub_1', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~df_year$sub_2,
            name = 'Sub_2', mode = 'lines') %>%
  add_trace(y = ~df_year$sub_3,
            name = 'Sub_3', mode = 'lines') %>%
  add_trace(y = ~df_year$active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption per year",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#### Time series ####

## Time series per month ##

# Month submeter 1
ts_month_sub1 <- ts(df_month$sub_1, frequency= 12, start= 2007, end = 2010)
# frequency = 12 meses = 1 año
autoplot(ts_month_sub1, xlab = "", ylab = "Watts",
         main = "Consumition of Sub 1 per month")

# Month submeter 2
ts_month_sub2 <- ts(df_month$sub_2, frequency= 12, start= 2007, end = 2010)
# frequency = 12 meses = 1 año
autoplot(ts_month_sub2, xlab = "", ylab = "Watts",
         main = "Consumition of Sub 2 per month")

# Month submeter 3
ts_month_sub3 <- ts(df_month$sub_3, frequency= 12, start= 2007, end = 2010)
# frequency = 12 meses = 1 año
autoplot(ts_month_sub3, xlab = "", ylab = "Watts",
         main = "Consumition of Sub 3 per month")

# Month Active Power
ts_month_AP <- ts(df_month$active_power, frequency= 12, start= 2007, end = 2010)
# frequency = 12
autoplot(ts_month_AP, xlab = "", ylab = "Watts", main = "Distribution of Active Power per month")

## Time series per week ##

# Week Submeter 1 
ts_day_of_week_sub1 <- ts(df_week$sub_1, frequency= 84, start= 2007, end = 2010)
# frequency = 12*7= 84
autoplot(ts_day_of_week_sub1)

plot.ts(ts_day_of_week_sub1)

# Week Submeter 2
ts_day_of_week_sub2 <- ts(df_week$sub_2, frequency= 84, start= 2007, end = 2010)
# frequency = 12*7= 84
autoplot(ts_day_of_week_sub2)

plot.ts(ts_day_of_week_sub2)

# Week Submeter 3 
ts_day_of_week_sub3 <- ts(df_week$sub_3, frequency= 84, start= 2007, end = 2010)
# frequency = 12*7= 84
autoplot(ts_day_of_week_sub3)

plot.ts(ts_day_of_week_sub3)

# Week Global Active Power
ts_week_AP <- ts(df_week$active_power, frequency= 12, start= 2007, end = 2010)
# frequency = 12
autoplot(ts_week_AP, xlab = "", ylab = "Watts", main = "Distribution of Active Power per month")

plot.ts(ts_week_AP)

## Descomposition ##

comp_month_sub1 <- stl(ts_month_sub1, s.window = "period")
autoplot(comp_month_sub1, xlab = "", main = "Submetering 1")

comp_month_sub2 <- stl(ts_month_sub2, s.window = "period")
autoplot(comp_month_sub2, xlab = "", main = "Submetering 2")

comp_month_sub3 <- stl(ts_month_sub3, s.window = "period")
autoplot(comp_month_sub3, xlab = "", main = "Submetering 3")

comp_month_AP <- stl(ts_month_AP, s.window = "period")
autoplot(comp_month_AP, xlab = "", main = "Active Power")
#plot(comp_month_AP)


#### Forecasting ####

## Forecast for sub-meter 1. Forecast ahead 20 time periods 
forecastSub_1 <- forecast(comp_month_sub1, h=20, level = c(80,90))

## Plot the forecast for sub-meter 1
plot(forecastSub_1, main = "Forecast Sub 1")

## Forecast for sub-meter 2. Forecast ahead 20 time periods 
forecastSub_2 <- forecast(comp_month_sub2, h=20, level = c(80,90))

## Plot the forecast for sub-meter 2
plot(forecastSub_2, main = "Forecast Sub 2")

## Forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastSub_3 <- forecast(comp_month_sub3, h=20, level = c(80,90))

## Plot the forecast for sub-meter 3
plot(forecastSub_3, main = "Forecast Sub 3")

## Forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastAP <- forecast(comp_month_AP, h=20, level = c(80,90))

## Plot the forecast for AP
plot(forecastAP, main = "Forecast Active Power")


#### Forecast Hw and Arima for AP monthly ####

train <- window(ts_month_AP,start = c(2007,1), end = c(2008,12))
test <- window(ts_month_AP,start = c(2009,1), end = c(2010,1))

## Holtwinters ##

hwModel <- HoltWinters(train)

predicthw <- forecast(hwModel, h = 13, seasonal = "multiplicative")

accuracy(predicthw,test)

## ARIMA ##

arimaModel <- auto.arima(train)

predictArima <- forecast(arimaModel, h = 13)

accuracy(predictArima,test)


#### ADF test submeter 3 ####

### Augmented Dickey-Fuller unit root test
# The p-value resulting from the ADF test
# has to be less than 0.05 or 5% for a time series to be stationary.
# If the p-value is greater than 0.05 or 5%, you conclude that the time series
# has a unit root which means that it is a non-stationary process.

adf.test(ts_month_sub3, alternative = "stationary")

# # Augmented Dickey-Fuller Test
# 
# data:  count_ma
# Dickey-Fuller = -0.2557, Lag order = 8, p-value = 0.99
# alternative hypothesis: stationary

month_sub3 = diff(log(ts_month_sub3),lag=1)
plot(month_sub3)

adf.test(month_sub3, alternative = "stationary")


# We know that for AR models, the ACF will dampen exponentially
# and the PACF plot will be used to identify the order (p)
# of the AR model. For MA models, the PACF will dampen exponentially
# and the ACF plot will be used to identify the order (q) of the MA model.

acf_test <- acf(month_sub3, lag.max = 36, plot = T)
  
