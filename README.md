# Electric-Consumption

**Project goal**: build predictive models to forecast future electricity consumption in Smart Homes and demonstrate the client how data can be used to help households make decisions regarding power usage.

**Data characteristics**: a multivariate time series containing power usage records of a Smart Home. The data was collected between December 2006 and November 2010 and observations of power consumption within the household were collected every minute. <br />

Source: http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption

**Technical Approach** <br />
Language used: R programming 

1. **PRE-PROCESSING** (DATA QUALITY)
* SQL queries
* Data types conversion and scaling
* Missing values treatment: pad + NA interpolation
* Feature engineering
* Group_by to visualize different data granularities

2. **TIME SERIES CREATION & DECOMPOSITION** 

3. **FORECASTING**
* Model ETS
* Model Arima
* Model Holt Winters
