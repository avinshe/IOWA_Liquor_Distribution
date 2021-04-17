library(rio)

library(ggplot2)

library(forecast)

library(tseries)

library(tidyverse)

library(lubridate)

library(zoo)

file <- "/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/IOWA LIQUOR SALES/IOWA_LIQUOR_CATOR_CLEANED_MERGED_DATA.csv"



df <- read.csv(file = file, sep = ",", header = TRUE)

names(df)[names(df)=="Invoice.Item.Number"] <- "Invoice Item Number"
names(df)[names(df)=="Store.Name"] <- "Store Name"
names(df)[names(df)=="Category.Name"] <- "Category Name"
names(df)[names(df)=="Vendor.Number.New"] <- "Vendor Number"
names(df)[names(df)=="Vendor.Name.Cleaned.y"] <- "Vendor Name"
names(df)[names(df)=="Item.Number"] <- "Item Number"
names(df)[names(df)=="Item.Description"] <- "Item Description"
names(df)[names(df)=="Bottle.Volume..ml."] <- "Bottle Volume (ml)"
names(df)[names(df)=="State.Bottle.Cost"] <- "State Bottle Cost"
names(df)[names(df)=="State.Bottle.Retail"] <- "State Bottle Retail"
names(df)[names(df)=="Bottles.Sold"] <- "Bottles Sold"
names(df)[names(df)=="Sale..Dollars."] <- "Sale (Dollars)"
names(df)[names(df)=="Volume.Sold..Litres."] <- "Volume Sold (Liters)"
names(df)[names(df)=="Volume.Sold..Gallons."] <- "Volume Sold (Gallons)"
names(df)[names(df)=="Store.Number"] <- "Store Number"
names(df)[names(df)=="AchoholTypes"] <- "AlchoholTypes"


df[str_detect(df$`Item Description`, 'Root Beer'),'AlchoholTypes'] <- 'Schnapps'
df[str_detect(df$`Item Description`, 'Rootbeer'),'AlchoholTypes'] <- 'Schnapps'
df[str_detect(df$`Item Description`, 'Vodka'),'AlchoholTypes'] <- 'Vodka'

df <- df[-as.numeric(rownames(df[df$`Item Description` == 'Dekuyper Blood Orange',])),]

df$Date <- as.Date(x = df$Date, format = '%m/%d/%Y')



df$TotalSale <- df$`State Bottle Retail` * df$`Bottles Sold`

df <- df[!is.na(df$TotalSale),]

df <- df[order(df$Date),]

agg_data_city_county <- aggregate(df$TotalSale, by = list(df$Date, df$City, df$County), FUN=sum)

names(agg_data_city_county)[names(agg_data_city_county) == "Group.1"] <- 'Date'
names(agg_data_city_county)[names(agg_data_city_county) == "Group.2"] <- 'City'
names(agg_data_city_county)[names(agg_data_city_county) == "Group.3"] <- 'County'
names(agg_data_city_county)[names(agg_data_city_county) == "x"] <- 'TotalSales'

sum(is.na(agg_data_city_county$TotalSales))

agg_data_city_county$Month <- month(agg_data_city_county$Date)

agg_data_date <- aggregate(agg_data_city_county$TotalSale, by = list(agg_data_city_county$Date), FUN=sum)
names(agg_data_date)[names(agg_data_date) == "Group.1"] <- 'Date'
names(agg_data_date)[names(agg_data_date) == "x"] <- 'TotalSales'

agg_data_date$Month <- month(agg_data_date$Date)

view(df, title = "Total Records")

view(agg_data_city_county, title = "Aggregated Data based on Date, City and County")

view(agg_data_date, title = "Aggregated Data based on Date")

sum(is.na(agg_data_date$TotalSales))

#------------------------- Exploratory Data Analysis --------------------------#

ggplot(agg_data_date, aes(Date,TotalSales)) + geom_line() + 
  scale_x_date('Month')  + xlab("") + ylab("Daily Total Sales")

ggplot(agg_data_date, aes(Date,TotalSales)) + geom_point(color = "navyblue") +
  facet_wrap( ~ Month) + scale_x_date('Month')  + ylab("Daily Total Sales") +
  xlab("")

agg_data_date[year(agg_data_date$Date) == "2013",]

p <- ggplot(agg_data_date, aes(x=Date, y=TotalSales)) + 
  geom_boxplot() + facet_wrap( ~ Month) + scale_x_date('Month')  + ylab("Daily Total Sales") +
  xlab("")

p + coord_flip()

countTSObjectDate <- ts(agg_data_date[,c('TotalSales')])


agg_data_date$CleanedSales <- tsclean(countTSObjectDate)


ggplot() + geom_line(data = agg_data_date, aes(x=Date, y= CleanedSales)) + ylab("Cleaned Sales")


agg_data_date$CleanedSalesMa <- ma(agg_data_date$CleanedSales, order = 7)
agg_data_date$CleanedSalesMa30 <- ma(agg_data_date$CleanedSales, order = 30)

ggplot() + geom_line(data = agg_data_date, aes(x=Date, y= CleanedSales, colour="Cleaned Sales")) +
  geom_line(data = agg_data_date, aes(x=Date, y= CleanedSalesMa, colour="Weekly Moving Average")) +
  geom_line(data = agg_data_date, aes(x=Date, y= CleanedSalesMa30, colour="Monthly Moving Average")) +
  ylab("Total Sales")

#------------------------- Decomposition of the data --------------------------#

cleanedSalesMa <- ts(na.omit(agg_data_date$CleanedSalesMa), frequency = 7) #chosed daily
decomp <- stl(cleanedSalesMa, s.window = "periodic") 
deseasonalCleanedSales <- seasadj(decomp)

plot(decomp)

adf.test(cleanedSalesMa, alternative = "stationary")

#------------------------- Autocorrelations and choosing model order --------------------------#

autoplot(Acf(cleanedSalesMa, lag.max =100, main = ""))

autoplot(Pacf(cleanedSalesMa, lag.max =100, main = ""))

#1st order difference

cleanedSalesD1 = diff(deseasonalCleanedSales, differences = 1)

plot(cleanedSalesD1)

adf.test(cleanedSalesD1, alternative = "stationary")

autoplot(Acf(cleanedSalesD1, lag.max =100, main = "1 differencing ACF"))

autoplot(Pacf(cleanedSalesD1, lag.max =100, main = "1 differencing PACF"))


#------------------------- Fitting ARIMA Model --------------------------#


auto.arima(deseasonalCleanedSales, seasonal = FALSE)

fit <- Arima(deseasonalCleanedSales, c(5, 1, 0))
tsdisplay(residuals(fit), lag.max = 60, main = "Default")

fit1 <- Arima(deseasonalCleanedSales, c(5, 1, 7))
tsdisplay(residuals(fit1), lag.max = 60, main = "Default")

fit2 <- Arima(deseasonalCleanedSales, c(9, 1, 7))
tsdisplay(residuals(fit2), lag.max = 60, main = "Default")

fit3 <- Arima(deseasonalCleanedSales, c(9, 1, 10))
tsdisplay(residuals(fit3), lag.max = 60, main = "Default")

fit4 <- Arima(deseasonalCleanedSales, c(10, 1, 10))
tsdisplay(residuals(fit4), lag.max = 60, main = "Default")

fit5 <- Arima(deseasonalCleanedSales, c(30, 1, 10))
tsdisplay(residuals(fit5), lag.max = 60, main = "Default")

fit6 <- Arima(deseasonalCleanedSales, c(0, 1, 7))
tsdisplay(residuals(fit6), lag.max = 60, main = "Default")

fit7 <- Arima(deseasonalCleanedSales, c(9, 1, 11))
tsdisplay(residuals(fit7), lag.max = 60, main = "Default")

fit8 <- Arima(deseasonalCleanedSales, c(10, 1, 11))
tsdisplay(residuals(fit8), lag.max = 60, main = "Default")

fit9 <- Arima(deseasonalCleanedSales, c(10, 1, 15))
tsdisplay(residuals(fit9), lag.max = 60, main = "Default")

fit10 <- Arima(deseasonalCleanedSales, c(30, 1, 15))
tsdisplay(residuals(fit10), lag.max = 60, main = "Default")

fit11 <- Arima(deseasonalCleanedSales, c(30, 1, 11))
tsdisplay(residuals(fit11), lag.max = 60, main = "Default")

fit12 <- Arima(deseasonalCleanedSales, c(30, 1, 9))
tsdisplay(residuals(fit12), lag.max = 60, main = "Default")

fit13 <- Arima(deseasonalCleanedSales, c(30, 1, 8))
tsdisplay(residuals(fit13), lag.max = 60, main = "Default")


#----------------------------Monthly--------------------------#


agg_data_month <- aggregate(agg_data_date$TotalSales, by = list(as.yearmon(agg_data_date$Date)), FUN=sum)

names(agg_data_month)[names(agg_data_month) == "Group.1"] <- 'YearMonth'
names(agg_data_month)[names(agg_data_month) == "x"] <- 'TotalSales'

view(agg_data_month)

ts_mnth_sales <- ts(log10(agg_data_month$TotalSales), start = c(2012,1), frequency = 12)

plot.ts(ts_mnth_sales)

ts_mnth_sales_cleaned <- tsclean(ts_mnth_sales)

plot(ts_mnth_sales_cleaned)

#------------------------- Decomposition of the data --------------------------#

decomp_mon_cleaned <- stl(ts_mnth_sales_cleaned, s.window = "periodic") 
deseasonalMonCleanedSales <- seasadj(decomp_mon_cleaned)
plot(decomp_mon_cleaned)

adf.test(ts_mnth_sales_cleaned, alternative = "stationary")

#------------------------- Autocorrelations and choosing model order --------------------------#

autoplot(Acf(ts_mnth_sales_cleaned, lag.max =100, main = ""))

autoplot(Pacf(ts_mnth_sales_cleaned, lag.max =100, main = ""))

autoplot(Acf(deseasonalMonCleanedSales, lag.max =300, main = ""))

autoplot(Pacf(deseasonalMonCleanedSales, lag.max =300, main = ""))

#1st order difference

cleanedMthSalesD1 = diff(deseasonalMonCleanedSales, differences = 1)

plot(cleanedMthSalesD1)

adf.test(cleanedMthSalesD1, alternative = "stationary")

autoplot(Acf(cleanedMthSalesD1, lag.max =100, main = "1 differencing ACF"))

autoplot(Pacf(cleanedMthSalesD1, lag.max =100, main = "1 differencing PACF"))

#2nd order difference

cleanedMthSalesD2 = diff(deseasonalMonCleanedSales, differences = 2)

plot(cleanedMthSalesD2)

adf.test(cleanedMthSalesD2, alternative = "stationary")

autoplot(Acf(cleanedMthSalesD2, lag.max =100, main = "2 differencing ACF"))

autoplot(Pacf(cleanedMthSalesD2, lag.max =100, main = "2 differencing PACF"))

#------------------------- Fitting ARIMA Model --------------------------#

fit <- auto.arima(deseasonalMonCleanedSales, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 60, main = "Default")

fit_mon <- Arima(deseasonalMonCleanedSales, c(2, 1, 1))
tsdisplay(residuals(fit_mon), lag.max = 60, main = "Default")

fit_mon1 <- Arima(deseasonalMonCleanedSales, c(2, 1, 4))
tsdisplay(residuals(fit_mon1), lag.max = 60, main = "Monthly Model 1")

fit_mon2 <- Arima(deseasonalMonCleanedSales, c(2, 1, 10))
tsdisplay(residuals(fit_mon2), lag.max = 60, main = "Monthly Model 2")

fit_mon3 <- Arima(deseasonalMonCleanedSales, c(3, 1, 10))
tsdisplay(residuals(fit_mon3), lag.max = 60, main = "Monthly Model 3")

fit_mon4 <- Arima(deseasonalMonCleanedSales, c(3, 2, 10))
tsdisplay(residuals(fit_mon4), lag.max = 60, main = "Monthly Model 4")

fit_mon5 <- Arima(deseasonalMonCleanedSales, c(3, 3, 10))
tsdisplay(residuals(fit_mon5), lag.max = 60, main = "Monthly Model 5")

fit_mon6 <- Arima(deseasonalMonCleanedSales, c(2, 2, 4)) #optimum
tsdisplay(residuals(fit_mon6), lag.max = 60, main = "Monthly Model 6")

fit_mon7 <- Arima(deseasonalMonCleanedSales, c(3, 2, 4))
tsdisplay(residuals(fit_mon7), lag.max = 60, main = "Monthly Model 7")

fit_mon8 <- Arima(deseasonalMonCleanedSales, c(1, 2, 4))
tsdisplay(residuals(fit_mon8), lag.max = 60, main = "Monthly Model 8")

fit_mon9 <- Arima(deseasonalMonCleanedSales, c(14, 2, 5))
tsdisplay(residuals(fit_mon9), lag.max = 60, main = "Monthly Model 9")

fit_mon10 <- Arima(deseasonalMonCleanedSales, c(0, 1, 1))
tsdisplay(residuals(fit_mon10), lag.max = 60, main = "Monthly Model 10")

fit_mon11 <- Arima(deseasonalMonCleanedSales, c(10, 1, 1))
tsdisplay(residuals(fit_mon11), lag.max = 60, main = "Monthly Model 11")

fit_mon12 <- Arima(deseasonalMonCleanedSales, c(13, 2, 4))
tsdisplay(residuals(fit_mon12), lag.max = 60, main = "Monthly Model 12")

fit_mon13 <- Arima(deseasonalMonCleanedSales, c(0, 2, 1))
tsdisplay(residuals(fit_mon13), lag.max = 60, main = "Monthly Model 13")

fit_mon14 <- Arima(deseasonalMonCleanedSales, c(3, 2, 1))
tsdisplay(residuals(fit_mon14), lag.max = 60, main = "Monthly Model 14")

fit_mon15 <- Arima(deseasonalMonCleanedSales, c(0, 0, 0))
tsdisplay(residuals(fit_mon15), lag.max = 60, main = "Default")

fit_mon16 <- Arima(deseasonalMonCleanedSales, c(0, 1, 0))
tsdisplay(residuals(fit_mon16), lag.max = 60, main = "Default")

fit_mon17 <- Arima(deseasonalMonCleanedSales, c(0, 1, 1))
tsdisplay(residuals(fit_mon17), lag.max = 60, main = "Q = 1")

fit_mon18 <- Arima(deseasonalMonCleanedSales, c(0, 2, 1))
tsdisplay(residuals(fit_mon18), lag.max = 60, main = "Default")

fit_mon19 <- Arima(deseasonalMonCleanedSales, c(1, 2, 1))
tsdisplay(residuals(fit_mon19), lag.max = 60, main = "Default")

fit_mon20 <- Arima(deseasonalMonCleanedSales, c(2, 2, 1))
tsdisplay(residuals(fit_mon20), lag.max = 60, main = "Default")

fit_mon21 <- Arima(deseasonalMonCleanedSales, c(2, 2, 3))
tsdisplay(residuals(fit_mon21), lag.max = 60, main = "Default")

fit_mon22 <- Arima(deseasonalMonCleanedSales, c(2, 1, 11))
tsdisplay(residuals(fit_mon22), lag.max = 60, main = "Default")

fit_mon23 <- Arima(deseasonalMonCleanedSales, c(2, 1, 12))
tsdisplay(residuals(fit_mon23), lag.max = 60, main = "Default")

fit_mon24 <- Arima(deseasonalMonCleanedSales, c(2, 2, 3))
tsdisplay(residuals(fit_mon24), lag.max = 60, main = "Default")

fit_mon25 <- Arima(deseasonalMonCleanedSales, c(2, 2, 1))
tsdisplay(residuals(fit_mon25), lag.max = 60, main = "Default")

fit_mon26 <- Arima(deseasonalMonCleanedSales, c(2, 2, 0))
tsdisplay(residuals(fit_mon26), lag.max = 60, main = "Default")

fit_mon27 <- Arima(deseasonalMonCleanedSales, c(2, 2, 10))
tsdisplay(residuals(fit_mon27), lag.max = 60, main = "Default")

fit_mon28 <- Arima(deseasonalMonCleanedSales, c(2, 2, 9))
tsdisplay(residuals(fit_mon28), lag.max = 60, main = "Default")

fit_mon29 <- Arima(deseasonalMonCleanedSales, c(2,2,4))
tsdisplay(residuals(fit_mon29), lag.max = 60, main = "Default")

fcast <- forecast(fit_mon6, h = 12)

plot(fcast)

#------------------------- Test Model With Hold Out --------------------------#
par(mfrow=c(1,1))
hold <- window(ts(deseasonalMonCleanedSales), start = 85)
fit_no_holdout <- Arima(ts(deseasonalMonCleanedSales[-c(85:97)]), order = c(2, 2, 4))
fcast_no_holdout <- forecast(fit_no_holdout, h = 13)

plot(fcast_no_holdout, main="")
lines(ts(deseasonalMonCleanedSales))

y <- as.vector(hold)

yhat <- as.vector(fcast_no_holdout)


accuracy(yhat, y)





#------------------------- Adding Seasonality back --------------------------#

fit_w_seasonality <- auto.arima(deseasonalMonCleanedSales, seasonal = TRUE)
seas_fcast <- forecast(fit_w_seasonality, h =13)
plot(seas_fcast)
lines(deseasonalMonCleanedSales)

#------------------------- Further Testing and Analysis of the model --------------------------#

tsdisplay(residuals(fit_w_seasonality), lag.max = 60, main = "Seasonal Models Residual")

fit_mon15 <- auto.arima(deseasonalMonCleanedSales, seasonal = FALSE)
tsdisplay(residuals(fit_mon15), lag.max = 60, main = "Seasonal Models Residual")

fit_mon16 <- Arima(deseasonalMonCleanedSales, c(2, 2, 4))
tsdisplay(residuals(fit_mon16), lag.max = 60, main = "Seasonal Models Residual")

fit_mon17 <- Arima(deseasonalMonCleanedSales, c(1, 1, 1))
tsdisplay(residuals(fit_mon17), lag.max = 60, main = "Seasonal Models Residual")


#------------------------- Final Fit and Tested ARIMA --------------------------#

par(mfrow = c(2,2))


fcast <- forecast(fit_w_seasonality, h = 12)
plot(fcast)


fcast1 <- forecast(fit_mon15, h = 12)
plot(fcast1)

fcast2 <- forecast(fit_mon16, h = 12)
plot(fcast2)

fcast3 <- forecast(fit_mon17, h = 12)
plot(fcast3)

