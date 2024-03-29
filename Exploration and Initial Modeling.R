#-------------------------------------------------------------------------------
# Atlas-Copco Demand Forecasting 
# Data exploration and initial modeling
#-------------------------------------------------------------------------------
# Aleksey Shipitsyn
# Data Scientist at Combient (MiX)
# mailto: aleksey.shipitsyn@combient.com
# phone: +460720796811

#-------------------------------------------------------------------------------
library(forecast)
library(readxl)
library(seasonal)
library(prophet)
library(bsts)     

# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)


#-------------------------------------------------------------------------------
# Data exploration and preprocessing
#-------------------------------------------------------------------------------
rm(list = ls())
setwd("~/Documents/Atlas-Copco Demand Forecast")

# read xls
data <- readxl::read_xlsx("SEB_DemandHistory.xlsx")

# names(data)
# # [1] "OrderNum"               "OrderType"              "Description"            "Item"                   "Plnt"                  
# # [6] "eHubRegion"             "eHubCty"                "Material"               "MaterialDescription"    "Requiredquantity"      
# # [11] "Customer"               "Name1"                  "CustomerCty"            "SalesOrg"               "ShipTo"                
# # [16] "ShipToName1"            "Market Sector"          "TopFam"                 "conk"                   "HighLevelProductFamily"
# # [21] "LowLevelProductFamily"  "eHubGP"                 "eHubZGP"                "eHubMKT"                "eHubMKTlong"           
# # [26] "eHubZMKT"               "Reqdlvdt"               "Annee"                  "Mois"                   "Period"                
# # [31] "Quarter"                "YQ"                     "Ac GI date"             "Annee GI"               "Mois GI"               
# # [36] "Period GI"              "Quarter GI"             "YQ GI"                  "Date Rec" 


# Missing values in columns
na_count <- apply(data, 2, function(x) { sum(is.na(x)) })
na_count[na_count > 0]

# MaterialDescription          Ac GI date            Annee GI             Mois GI           Period GI          Quarter GI 
#             4                 716                 716                 716                 716                 716 
# YQ GI            Date Rec 
# 716                6131 

# QUESTION: reason of missing values 


# QUESTIONS on columns selection:
# 1. Description column needed ?
# 2. What is Material - Material Description ?
# 3. 


# Dates
range(data$Reqdlvdt ) # "2007-01-01 UTC" ... "2020-12-31 UTC"
range(data$Annee) # 2007 ... 2020
range(data$Period) # "2007-01-01 UTC" "2020-12-01 UTC"
range(data$`Ac GI date`, na.rm=T) # "2006-12-15 UTC" "2019-02-28 UTC"
range(data$`Annee GI`, na.rm=T) # 2006 ... 2019
range(data$`Period GI`, na.rm=T) # "2006-12-01 UTC" "2019-02-01 UTC"
range(data$`Date Rec`, na.rm=T) # "2006-12-08 UTC" "2019-02-28 UTC"

# QUESTION: year 2020 in dates? Which date column is correct?


#-------------------------------------------------------------------------------
# Columns of interest:
# eHubRegion = region 
# LowLevelProductFamily = product
# Requiredquantity = quantity 
# Reqdlvdt = date 

df <- data[, c("Reqdlvdt","eHubRegion", "LowLevelProductFamily", "Requiredquantity")]
names(df) <- c('date','region','product','demand')

# Aggregate demand monthly
df$date <- as.POSIXlt(df$date)
df$year <- df$date$year + 1900
df$month <- df$date$mon + 1
df <- aggregate(demand ~ ., 
                df[,c('year','month','region','product','demand')], sum)
df$date <- as.POSIXlt(paste(df$year, df$month, '01', sep='-'))

# restore the month number "1" to "01"
restore_month <- function(x) {
  out <- character(length(x))
  less10 <- x < 10
  out[less10] <- paste0('0', x[less10])
  out[!less10] <- as.character(x[!less10])
  return(out)
}
df$yearmon <- as.integer(paste0(df$year, restore_month(df$month)))

#-------------------------------------------------------------------------------
# reshape to horizontal, each time series in row 
rp <- unique(df[, c('region','product')]) # 21
row.names(rp) <- NULL

# time series matrix with months in columns and time series in rows
r <- range(df$year)
nyears <- length(r[1]:r[2])
m <- matrix(nrow=nrow(rp), ncol=nyears * 12)
periods <- paste0(sort(rep(r[1]:r[2], 12)), restore_month(rep(1:12, nyears)))
dimnames(m)[[2]] <- periods


# put time series into matrix
for (i in 1:nrow(rp)) {
  # select demand time series for region and product
  ser <- df[df$region == rp$region[i] & df$product == rp$product[i], c("demand","yearmon")]
  # fill in months without demand
  ser <- merge(x=data.frame(yearmon=periods), y=ser, by='yearmon', all.x=T)  
  # write into time series matrix
  m[i,] <- ser$demand
}


# delete periods from future
future_start <- 201903
m <- m[, as.integer(dimnames(m)[[2]]) < future_start]


#-------------------------------------------------------------------------------
# Trend
#-------------------------------------------------------------------------------
# Function to compute trend with different methods
# Input: y = numeric vector
#        method = {'loess', 'GP'}
# Output: numeric vector with trend values

trend <- function(y, method='loess', ...) {
  x <- 1:length(y)
  fit <- NULL
  if (method == 'loess') { 
    fit <- loess(y ~ x, na.action=na.omit) 
  }
  if (method == 'GP') { 
    require(kernlab)
    fit <- gausspr(x=x, y=y, type="regression")
  }
  trend <- as.numeric(predict(fit, x))
  return(trend)
}


loess_trend <- trend(m[1,], method='loess')
gp_trend <- trend(m[1,], method='GP')

par(mfrow=c(1,1))
plot(x=1:ncol(m), y=m[1,], type='l')
lines(x=1:ncol(m), y=loess_trend, col='red',lwd=2)
lines(x=1:ncol(m), y=gp_trend, col='blue',lwd=2)



#-------------------------------------------------------------------------------
# Visualization of time series
#-------------------------------------------------------------------------------
# Input: mat = matrics with time series in rows

visualize_ts <- function(mat) {
  par(mfrow=c(3,3))
  x <- as.Date(paste0(dimnames(mat)[[2]],'01'), format='%Y%m%d')
  for (i in 1:nrow(rp)) {
    plot(x=x, y=mat[i,], type='l', las=1, lwd=2, pch=20, 
         main=paste('region', rp$region[i],':  product', rp$product[i]), 
         xlab='', ylab='demand')
    lines(x=x, y=trend(mat[i,], method='loess'), col='red',lwd=2)
    lines(x=x, y=trend(mat[i,], method='GP'), col='blue',lwd=2)
    legend('topleft', col=c('red','blue'), lwd=c(2,2), legend=c('loess trend','GP trend'))
  }
  par(mfrow=c(1,1))
}

visualize_ts(m)

#-------------------------------------------------------------------------------
# Missing values imputation
#-------------------------------------------------------------------------------
# Interpolate missing values with Loess
# Input: y = numeric vector
# Output: numeric vector

loess_interpolation <- function(y) {
  x <- 1:length(y)
  fit <- loess(y ~ x)
  pred <- predict(fit, x)
  mis <- is.na(y)
  y[mis] <- pred[mis]
  return(y)
}

# y <- c(NA, 3, 5, NA, 7, 8, 10, NA, 8, 7, 9, NA)
# loess_interpolation(y)
## NA  3.000000  5.000000  5.785063  7.000000  8.000000 10.000000  8.814745  8.000000  7.000000  9.000000  NA

# # loess interpolation
# m1 <- m
# for (i in 1:nrow(m1)) {  
#   m1[i,] <- loess_interpolation(m1[i,]) 
# }
# visualize_ts(m1)

#-------------------------------------------------------------------------------
# ASSUMPTIONS:
# 1) NA in the begining == no product at market, leave them
# 2) NA in between == zerous 
# 3) NA in the end == missing data or zeroes, leave them 

# function to fill NA in the middle, leaving NA in the beginning and the end  
# Input: x = numeric vector 
#        fill = value to fill
# Output: numeric vector

fill_middle_na <- function(x, fill=0) {
  if (sum(is.na(x)) == 0 | all(is.na(x))) { return(x) }
  
  # start and end of non-NA values in x
  startend <- range(which(!is.na(x)))
  
  # fill all
  x[is.na(x)] <- fill
  
  # restore NA in th begining
  x[1:(startend[1]-1)] <- NA
  
  # restore NA in the end
  if (startend[2] < length(x)) {
    x[(startend[2]+1):length(x)] <- NA
  }
  return(x)  
}

# fill_middle_na(c(1,1,1,1)) # 1 1 1 1
# fill_middle_na(c(NA,NA,NA)) # NA NA NA
# fill_middle_na(c(NA,NA,1,1,NA,1,NA,NA,1,NA)) # NA NA  1  1  0  1  0  0  1 NA

rowSums(is.na(m))
# 34 12 62 26 31 18  0 15  0 37 21 13 15  0 17  6 38 19 13 14  0

m <- t(apply(m, MARGIN=1, fill_middle_na))

rowSums(is.na(m))
# 18  1 38 21 14 16  0 12  0 36 21 13 15  0 16  1 38 13 13 12  0

visualize_ts(m)



# QUESTION: is it correct to impute missing values with zeros?

#-------------------------------------------------------------------------------
# Scaling time series 
#-------------------------------------------------------------------------------
# Function for matching to [0..1] interval 
# Input: numeric vector or time series
# Output: numeric vector or time series

scale_ts <- function(x) {
  r <- range(x, na.rm=T)
  scaled <- (x - r[1]) / (r[2] - r[1])
  return(scaled)
}

# a <- c(NA,NA,0,1,4,0,1,2)
# scale_ts(a)
# # NA   NA 0.00 0.25 1.00 0.00 0.25 0.50


#-------------------------------------------------------------------------------
# Scaling is required for metric comparisson between different time series
# We use matching to [0..1] interval for scaling

for (i in 1:nrow(m)) {
  m[i,] <- scale_ts(m[i,])
}

visualize_ts(m)


#-------------------------------------------------------------------------------
# Performance metric
#-------------------------------------------------------------------------------
# Perfomance metric from residuals
# Input: r = numeric vector of residuals 
#        type = metric type {'rmse','mae'}
# Output: numeric number

performance_metric <- function(r, type='rmse') {
  out <- NULL
  if (type == 'rmse') {
    out <- sqrt(mean(r * r, na.rm=T))
  }
  if (type == 'mae') {
    out <- mean(abs(r), na.rm=T)
  }
  return(out)
}


#-------------------------------------------------------------------------------
# Non-linear Auto-Regression models
#-------------------------------------------------------------------------------
# Function to slice time series into data frame
# Input: x = numeric vector of time series
#        win = integer, window length for slicing
# Output: matrix with slices of time series in a row 

ts2df <- function(x, win=24) {
  N <- length(x)-win+1
  out <- matrix(0, nrow=N, ncol=win)
  for (i in 1:N) {
    out[i,] <- x[i:(i+win-1)]
  }
  return(out)
}

# ts2df(1:10, 1)
# ts2df(1:10, 3)
# ts2df(1:10, 5)
# ts2df(1:10, 10)


#-------------------------------------------------------------------------------
# Function to predict time series with Random Forest
# Input: x = time series
#        h = number of points to predict in the future
#        win = number of past points for input 
# Output: numeric vector of predictions

predict_RFts <- function(x, h=12, win=24, ...) {
  require(randomForest)
  
  # select non-missing values for input
  x <- x[!is.na(x)]
  # slicing
  d <- ts2df(x, win=win)
  # model
  rf_model <- randomForest(x=d[,-ncol(d)], y=d[,ncol(d)], ...)
  
  # prediction one by one point in the future
  pred <- numeric(length=h)
  xnew <- d[nrow(d), -1]
  pred[1] <- predict(rf_model, xnew)
  for (i in 2:h) {
    xnew <- c(xnew[-1], pred[i-1])
    pred[i] <- predict(rf_model, xnew)
  }
  return(pred)
} 


#-------------------------------------------------------------------------------
# Function to predict time series with XGBoost
# Input: x = time series
#        h = number of points to predict in the future
#        win = number of past points for input 
# Output: numeric vector of predictions

predict_XGBts <- function(x, h=12, win=24, ...) {
  require(xgboost)
  
  # select non-missing values for input
  x <- x[!is.na(x)]
  # slicing
  d <- ts2df(x, win=win)
  # model
  xgb_model <- xgboost(data=d[,-ncol(d)], label=d[,ncol(d)], ...)
  
  # prediction one by one point in the future
  pred <- numeric(length=h)
  xnew <- matrix(d[nrow(d), -1], nrow=1)
  pred[1] <- predict(xgb_model, xnew)
  for (i in 2:h) {
    xnew <- matrix(c(xnew[-1], pred[i-1]), nrow=1)
    pred[i] <- predict(xgb_model, xnew)
  }
  return(pred)
} 


#-------------------------------------------------------------------------------
# Function to predict time series with SVM
# Input: x = time series
#        h = number of points to predict in the future
#        win = number of past points for input 
# Output: numeric vector of predictions

predict_SVMts <- function(x, h=12, win=24, ...) {
  require(e1071)
  
  # select non-missing values for input
  x <- x[!is.na(x)]
  # slicing
  d <- ts2df(x, win=win)
  # model
  svm_model <- svm(x=d[,-ncol(d)], y=d[,ncol(d)], ...)
  
  # prediction one by one point in the future
  pred <- numeric(length=h)
  xnew <- matrix(d[nrow(d), -1], nrow=1)
  pred[1] <- predict(svm_model, newdata=xnew)
  for (i in 2:h) {
    xnew <- matrix(c(xnew[,-1], pred[i-1]), nrow=1)
    pred[i] <- predict(svm_model, xnew)
  }
  return(pred)
} 


#-------------------------------------------------------------------------------
# Function to create polynomial features

poly_features <- function(x, degree=3) {
  x_poly <- x
  X <- x_poly
  for (k in 2:degree) {
    x_poly <- x_poly * x  
    X <- c(X, x_poly)
  }
  return(X)
}

# poly_features(1:5, 3)


#-------------------------------------------------------------------------------
# Function to predict time series with Polinomial regression
# Input: x = time series
#        h = number of points to predict in the future
#        win = number of past points for input 
# Output: numeric vector of predictions

predict_Polyts <- function(x, h=12, win=24, degree=3, ...) {
  # select non-missing values for input
  x <- x[!is.na(x)]
  
  # slicing
  d <- ts2df(x, win=win)
  
  # polynomial features
  d_poly <- matrix(0, nrow=nrow(d), ncol=degree * (ncol(d)-1))
  for (i in 1:nrow(d_poly)) {
    d_poly[i,] <- poly_features(d[i,-ncol(d)], degree=degree)
  }
  
  # model
  data <- data.frame(d_poly, y=d[,ncol(d)])
  lm_model <- lm(y ~ ., data=data)
  summary(lm_model)
  
  # prediction one by one point in the future
  pred <- numeric(length=h)
  # time series window which is shifting in the future
  dnew <- d[nrow(d),-1]
  # polynomial features data.frame for input into lm_model
  xnew <- data[nrow(data),-ncol(data)]
  
  # predict one by one
  for (i in 1:h) {
    xnew[1,] <- poly_features(dnew, degree=degree)
    pred[i] <- predict(lm_model, newdata=xnew)
    dnew <- c(dnew[-1], pred[i])              
  }
  # make sure predictions are positive or zeros
  pred[pred < 0 | is.na(pred)] <- 0
  return(pred)
} 


#-------------------------------------------------------------------------------
# Function to convert time series to data.frame
# Input: x = time series or data.frame(ds, y) to convert
# Output: converted x

cast_ts2df <- function(x) {
  data.frame(ds = as.Date(paste0(names(x),'01'), format='%Y%m%d'), y=as.vector(x))
}

#-------------------------------------------------------------------------------
# Function to correct anomalies in time series by multiplication 
# Looks and corrects positive amonalies 
# Input: x = data.frame(date, value) of time series
#        correction_multiplier = multiplier to use for values correction
# Output: numeric vector or time series with imputed anomalies

anomalycorrection <- function(x, correction_multiplier=0.9, ...) {
  require(AnomalyDetection)
  
  # data.frame for input into anomalies detection
  dfx <- cast_ts2df(x)
  # cast dates for propper input into anomaly detection
  dfx$ds <- as.POSIXct(as.POSIXlt(dfx$ds, tz='GMT'), tz='GMT')
  # anomalies detection
  res <- AnomalyDetectionTs(dfx, direction='pos', ...)
  
  # anomalies correction
  if (nrow(res$anoms) > 0) {
    ind <- which(as.Date(dfx$ds) %in% as.Date(res$anoms$timestamp))
    x[ind] <- res$anoms$anoms * correction_multiplier
  }
  return(x)
}

# Y <- m[1,]
# Y1 <- anomalycorrection(Y, max_anoms=0.02, )
# plot(Y, type='l', lwd=2)
# lines(Y1, col='red')


#-------------------------------------------------------------------------------
# Test set is the last 12 months in 2018-2019
split_start <- as.integer(dimnames(m)[[2]][(ncol(m)-11):ncol(m)][1])
train <- as.integer(dimnames(m)[[2]]) < split_start 
test <- !train 

# number of time points
sum(train) # 134
sum(test) # 12 

# starting and ending months for training set 
start_month <- as.integer(c(substr(dimnames(m)[[2]][1], 1, 4), 
                            substr(dimnames(m)[[2]][1], 5, 6)))

end_month <- as.integer(c(substr(dimnames(m)[[2]][sum(train)], 1, 4), 
                            substr(dimnames(m)[[2]][sum(train)], 5, 6)))

#-------------------------------------------------------------------------------
# Performance metrics data frame
metrics <- matrix(0, ncol=8, nrow=nrow(rp))
dimnames(metrics)[[2]] <- c(#'arima_rmse','arima_mae', 
                            #'snaive_rmse','snaive_mae',
                            #'lm_rmse','lm_mae',
                            'stl_rmse','stl_mae',
                            'ets_rmse','ets_mae',
                            #'nnet_rmse','nnet_mae',
                            #'prophet_rmse','prophet_mae',
                            #'bsts_rmse','bsts_mae',
                            'rf_rmse','rf_mae',
                            #'xgb_rmse','xgb_mae',
                            #'svm_rmse','svm_mae',
                            'poly_rmse','poly_mae')
                            #'bag_rmse','bag_mae')

# Models and predictions
for (i in 1:nrow(m)) {
  print(i)
  # time series for training
  Y <- ts(m[i, train], start=start_month, end=end_month, frequency=12)
  # anomaly correction
  Y <- anomalycorrection(Y, max_anoms=0.02, correction_multiplier=0.9)
  
  # # Seasonal naive model
  # snaive_pred <- snaive(Y, h=sum(test))
  # snaive_pred <- as.numeric(snaive_pred$mean)
  # metrics[i,'snaive_rmse'] <- performance_metric(m[i, test] - snaive_pred, type='rmse')
  # metrics[i,'snaive_mae'] <- performance_metric(m[i, test] - snaive_pred, type='mae')

  # # Linear model
  # lm_model <- tslm(Y ~ trend + season)
  # lm_pred <- forecast(lm_model, h=sum(test))
  # lm_pred <- as.numeric(lm_pred$mean)
  # metrics[i,'lm_rmse'] <- performance_metric(m[i, test] - lm_pred, type='rmse')
  # metrics[i,'lm_mae'] <-  performance_metric(m[i, test] - lm_pred, type='mae')

  # Seasonal Decomposition by Loess
  stl_model <- stl(Y, t.window=13, s.window='periodic', robust=T, na.action = na.omit)
  stl_pred <- forecast(stl_model, h=sum(test))
  stl_pred <- as.numeric(stl_pred$mean)
  metrics[i,'stl_rmse'] <- performance_metric(m[i, test] - stl_pred, type='rmse')
  metrics[i,'stl_mae'] <- performance_metric(m[i, test] - stl_pred, type='mae')

  # State space model
  ets_model <- ets(Y)
  ets_pred <- forecast(ets_model, h=sum(test))
  ets_pred <- as.numeric(ets_pred$mean)
  metrics[i,'ets_rmse'] <- performance_metric(m[i, test] - ets_pred, type='rmse')
  metrics[i,'ets_mae'] <- performance_metric(m[i, test] - ets_pred, type='mae')

  # # Auto Arima
  # arima_model <- auto.arima(Y, d=1, D=1, max.order=7, stepwise=F, parallel=T)
  # arima_pred <- forecast(arima_model, h=sum(test))
  # arima_pred <- as.numeric(arima_pred$mean)
  # metrics[i,'arima_rmse'] <- performance_metric(m[i, test] - arima_pred, type='rmse')
  # metrics[i,'arima_mae'] <- performance_metric(m[i, test] - arima_pred, type='mae')

  # # Neural Net
  # nnet_model <- nnetar(Y, na.action=na.omit)
  # nnet_pred <- forecast(nnet_model, PI=T, h=sum(test))
  # nnet_pred <- as.numeric(nnet_pred$mean)
  # metrics[i,'nnet_rmse'] <- performance_metric(m[i, test] - nnet_pred, type='rmse')
  # metrics[i,'nnet_mae'] <- performance_metric(m[i, test] - nnet_pred, type='mae')

  # # Prophet model
  # # time series as data frame
  # dfts <- cast_ts2df(Y)
  # prophet_model <- prophet(dfts, seasonality.mode = 'additive', growth='linear',
  #                          yearly.seasonality=T, weekly.seasonality=F, daily.seasonality=F)
  # df_future <- make_future_dataframe(prophet_model, freq='month', periods=12, include_history=F)
  # prophet_pred <- predict(prophet_model, df_future)
  # prophet_pred <- as.numeric(prophet_pred$yhat)
  # metrics[i,'prophet_rmse'] <- performance_metric(m[i, test] - prophet_pred, type='rmse')
  # metrics[i,'prophet_mae'] <- performance_metric(m[i, test] - prophet_pred, type='mae')

  # # BSTS model
  # set.seed(56)
  # ss <- AddStudentLocalLinearTrend(list(), Y)
  # ss <- AddAutoAr(ss, Y, lags = 12)
  # bsts_model <- bsts(Y, state.specification=ss, niter=1000, na.action=na.omit)
  # bsts_pred <- predict(bsts_model, horizon=12, burn=100)
  # bsts_pred <- as.numeric(bsts_pred$median)
  # metrics[i,'bsts_rmse'] <- performance_metric(m[i, test] - bsts_pred, type='rmse')
  # metrics[i,'bsts_mae'] <- performance_metric(m[i, test] - bsts_pred, type='mae')

  # Random Forest for time series
  set.seed(52)
  rf_pred <- predict_RFts(Y, h=sum(test), win=24, ntree=100, proximity=T, nodesize=7)
  rf_pred <- as.numeric(rf_pred)
  metrics[i,'rf_rmse'] <- performance_metric(m[i, test] - rf_pred, type='rmse')
  metrics[i,'rf_mae'] <- performance_metric(m[i, test] - rf_pred, type='mae')

  # # XGBoost for time series
  # set.seed(52)
  # xgb_pred <- predict_XGBts(Y, h=sum(test), win=24, nrounds=200,
  #                           objective='reg:linear', eta=0.1, verbose=0, nthread=8)
  # xgb_pred <- as.numeric(xgb_pred)
  # metrics[i,'xgb_rmse'] <- performance_metric(m[i, test] - xgb_pred, type='rmse')
  # metrics[i,'xgb_mae'] <- performance_metric(m[i, test] - xgb_pred, type='mae')
  # 
  # # SVM for time series
  # svm_pred <- predict_SVMts(Y, h=sum(test), win=24, type='eps-regression')
  # svm_pred <- as.numeric(svm_pred)
  # metrics[i,'svm_rmse'] <- performance_metric(m[i, test] - svm_pred, type='rmse')
  # metrics[i,'svm_mae'] <- performance_metric(m[i, test] - svm_pred, type='mae')

  # Polinomial Linear model for time series
  poly_pred <- predict_Polyts(Y, h=sum(test), win=4, degree=3)
  poly_pred <- as.numeric(poly_pred)
  metrics[i,'poly_rmse'] <- performance_metric(m[i, test] - poly_pred, type='rmse')
  metrics[i,'poly_mae'] <- performance_metric(m[i, test] - poly_pred, type='mae')
  
  # # Bag of models
  # bag_pred <- (#arima_pred + bsts_pred + snaive_pred + lm_pred + nnet_pred + prophet_pred + xgb_pred + svm_pred +
  #               stl_pred + ets_pred + rf_pred + poly_pred) / 4
  # metrics[i,'bag_rmse'] <- performance_metric(m[i, test] - bag_pred, type='rmse')
  # metrics[i,'bag_mae'] <- performance_metric(m[i, test] - bag_pred, type='mae')
}


# save(metrics, file='metrics.RData')
# load('metrics.RData')

# reshape for each metric separately
metrics <- as.data.frame(metrics)
# mean 
data.frame(colMeans(metrics))
# median
data.frame(apply(metrics, 2, median))

res_rmse <- metrics[, grep(names(metrics), pattern='rmse')]
names(res_rmse) <- gsub(names(res_rmse), pattern='_rmse', replacement='')

res_mae <- metrics[, grep(names(metrics), pattern='mae')]
names(res_mae) <- gsub(names(res_mae), pattern='_mae', replacement='')

#-------------------------------------------------------------------------------
# Metrics averages
avg_rmse <- data.frame(rmse=colMeans(res_rmse))
# avg_rmse <- data.frame(rmse=apply(res_rmse, 2, median))
avg_rmse$model <- row.names(avg_rmse)
avg_rmse <- avg_rmse[order(avg_rmse$rmse, decreasing=F), c('model','rmse')]
row.names(avg_rmse) <- NULL

avg_mae <- data.frame(mae=colMeans(res_mae))
# avg_mae <- data.frame(mae=apply(res_mae, 2, median))
avg_mae$model <- row.names(avg_mae)
avg_mae <- avg_mae[order(avg_mae$mae, decreasing=F), c('model','mae')]
row.names(avg_mae) <- NULL

# with Anomaly correction
avg_rmse
#      model      rmse
# 1      ets 0.1537587
# 2      stl 0.1558784
# 3     bsts 0.1576505
# 4       rf 0.1680170
# 5     poly 0.1743050
# 6     nnet 0.1770203
# 7    arima 0.1771315
# 8      xgb 0.1779876
# 9  prophet 0.1790823
# 10      lm 0.1995760
# 11     svm 0.2009215
# 12  snaive 0.2068051

avg_mae
#      model       mae
# 1      stl 0.1261853
# 2      ets 0.1281934
# 3     bsts 0.1294022
# 4       rf 0.1372784
# 5     poly 0.1413468
# 6    arima 0.1473214
# 7     nnet 0.1480254
# 8      xgb 0.1494120
# 9  prophet 0.1497299
# 10     svm 0.1656886
# 11      lm 0.1697478
# 12  snaive 0.1747639

#-------------------------------------------------------------------------------
# without Anomaly correction - from previous run
# avg_rmse
# model      rmse
# 1      ets 0.1566491
# 2     bsts 0.1611585
# 3      stl 0.1611642
# 4       rf 0.1680000
# 5     nnet 0.1720533
# 6     poly 0.1786707
# 7      xgb 0.1789332
# 8  prophet 0.1790652
# 9    arima 0.1844207
# 10      lm 0.1992631
# 11     svm 0.2013704
# 12  snaive 0.2095188

# avg_mae
# model       mae
# 1      ets 0.1302879
# 2      stl 0.1314065
# 3     bsts 0.1332030
# 4       rf 0.1378589
# 5     nnet 0.1437690
# 6     poly 0.1458369
# 7  prophet 0.1497307
# 8      xgb 0.1518210
# 9    arima 0.1550234
# 10     svm 0.1657332
# 11      lm 0.1692371
# 12  snaive 0.1771783


#-------------------------------------------------------------------------------
# The best model for each time series
#-------------------------------------------------------------------------------
best_models <- data.frame( 
  cbind( 
    t(apply(res_rmse, MARGIN=1, function(x) { 
      names(res_rmse)[order(x, decreasing=F)][1:2] })),
    t(apply(res_mae, MARGIN=1, function(x) { 
      names(res_mae)[order(x, decreasing=F)][1:2] }))),
  stringsAsFactors=F)

names(best_models) <- c('best_rmse','second_rmse','best_mae','second_mae')  


best_models
#    best_rmse second_rmse best_mae second_mae
# 1       bsts       arima    arima       bsts
# 2        stl          rf       rf        stl
# 3        svm         stl      svm        stl
# 4        stl          lm      stl         lm
# 5        stl          lm      stl         lm
# 6        ets        bsts      ets       bsts
# 7       bsts         ets     bsts        stl
# 8      arima         xgb    arima        xgb
# 9      arima        bsts    arima       bsts
# 10      nnet        poly     nnet       bsts
# 11      poly         xgb     poly        xgb
# 12       stl       arima      stl      arima
# 13       ets        bsts      ets       bsts
# 14       ets        bsts      ets       bsts
# 15       stl          rf       rf        stl
# 16        rf         svm      svm         rf
# 17       stl         ets      stl         rf
# 18      bsts         ets     bsts        ets
# 19      poly        nnet     nnet       poly
# 20       stl      snaive   snaive        ets
# 21   prophet          lm  prophet         lm


# best models overall
unique(c(best_models$best_rmse, best_models$best_mae))
# "bsts"    "stl"     "svm"     "ets"     "arima"   "nnet"    "poly"    "rf"      "prophet" "snaive"  

# best models mrse
unique(best_models$best_rmse)
# "bsts"    "stl"     "svm"     "ets"     "arima"   "nnet"    "poly"    "rf"      "prophet"

# best models mae
unique(best_models$best_mae)
# "arima"   "rf"      "svm"     "stl"     "ets"     "bsts"    "nnet"    "poly"    "snaive"  "prophet"



#-------------------------------------------------------------------------------
# best model for each time series accuracy
res_best <- data.frame(best_mrse=apply(res_rmse, MARGIN=1, min), 
                       best_mae=apply(res_mae, MARGIN=1, min))
colMeans(res_best)

# best_mrse  best_mae 
# 0.1353341 0.1079975 

# Light models only:
# best_mrse  best_mae 
# 0.1397781 0.1141051 

#-------------------------------------------------------------------------------
# MODEL SELECTION SUMMARY
#-------------------------------------------------------------------------------
# 1. The best models in average are: 

# - stl (Seasonal Decomposition by Loess) 
# - ets (State space model)
# - bsts (Bayesian Structural Time Series) 

# 2. The best accuracy is achived by using different model for each time series

# 3. Using only light models, different for each time series has also good accuracy

# Bag of models has average accuracy, no sense to use

#-------------------------------------------------------------------------------
