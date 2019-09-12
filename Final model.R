#-------------------------------------------------------------------------------
# Atlas-Copco Demand Forecasting
# Final model: fill NA with zeros + Anomaly correction + Seasonal Decomposition by Loess
#-------------------------------------------------------------------------------
# Aleksey Shipitsyn
# Data Scientist at Combient (MiX)
# mailto: aleksey.shipitsyn@combient.com
# phone: +460720796811

#-------------------------------------------------------------------------------
library(forecast)
library(readxl)
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

#-------------------------------------------------------------------------------
# Data preprocessing
#-------------------------------------------------------------------------------
rm(list = ls())
setwd("~/Documents/Atlas-Copco Demand Forecast")

# read xls
data <- readxl::read_xlsx("SEB_DemandHistory.xlsx")

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
# Missing values imputation
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# ASSUMPTIONS:
# 1) NA in the begining == no product at market, leave them
# 2) NA in between == zerous
# 3) NA in the end == missing data or zeroes, leave them

m <- t(apply(m, MARGIN=1, fill_middle_na))

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
# Test set is the last 12 months
split_test <- as.integer(dimnames(m)[[2]][(ncol(m)-11):ncol(m)][1])
train <- as.integer(dimnames(m)[[2]]) < split_test
test <- !train

# starting and ending months for training set
start_month <- as.integer(c(substr(dimnames(m)[[2]][1], 1, 4),
                            substr(dimnames(m)[[2]][1], 5, 6)))

end_month <- as.integer(c(substr(dimnames(m)[[2]][sum(train)], 1, 4),
                          substr(dimnames(m)[[2]][sum(train)], 5, 6)))

#-------------------------------------------------------------------------------
# stl model (Seasonal Decomposition by Loess)
#-------------------------------------------------------------------------------
# prediction on test set
pred <- matrix(0, nrow(m), ncol=sum(test))
dimnames(pred)[[2]] <- dimnames(m)[[2]][test]
stl_pred <- list() 

for (i in 1:nrow(m)) {
   print(i)
   
  # time series for training
   Y <- ts(m[i, train], start=start_month, end=end_month, frequency=12)
   
   # anomaly correction
   Y <- anomalycorrection(Y, max_anoms=0.02, correction_multiplier=0.9, plot=F)

   # Seasonal Decomposition by Loess
   stl_model <- stl(Y, t.window=13, s.window='periodic', robust=T, na.action = na.omit)
   stl_pred[[i]] <- forecast(stl_model, h=sum(test))
   p <- ceiling(as.numeric(stl_pred[[i]]$mean))
   p[p < 0] <- 0
   pred[i,] <- p
}


#-------------------------------------------------------------------------------
# Output
#-------------------------------------------------------------------------------
# Plot time series and predictions with 95% interval
# Input: y = time series or numeric vector
#        s = stl prediction object 

visualize_ts_pred <- function(y, s) {
  
  yhat <- ceiling(as.numeric(s$mean))
  yhat[yhat < 0] <- 0
  hi95 <- floor(as.numeric(s$upper[,2]))
  lo95 <- ceiling(as.numeric(s$lower[,2]))
  lo95[lo95 < 0] <- 0 
  
  x <- as.Date(paste0(names(y),'01'), format='%Y%m%d')
  x_pred <- x[(length(x)-ncol(pred)+1):length(x)]
  
  # actual time series
  plot(x=x, y=y, type='l', las=1, lwd=2, col='black', ylim=c(0, max(c(y, hi95), na.rm=T)),
       main=paste('region', rp$region[i],':  product', rp$product[i]),
       xlab='', ylab='demand', )
  points(x=x, y=y, pch=20, col='black')
  
  # predictions start
  abline(v=x_pred[1], lty='dashed', lwd=1, col='blue')
  
  # predictions - mean
  lines(x=x_pred, y=yhat, col='orange', lwd=2)
  points(x=x_pred, y=yhat, pch=20, col='orange')
  
  # high 95% interval
  lines(x=x_pred, y=hi95, col='red', lwd=2)
  points(x=x_pred, y=hi95, pch=20, col='red')
  
  # low 95% interval
  lines(x=x_pred, y=lo95, col='red', lwd=2)
  points(x=x_pred, y=lo95, pch=20, col='red')
  
  legend('topleft', col=c('black','orange','red'), 
         lwd=c(2,2,2), legend=c('actual','predicted','95% interval'))
}


#-------------------------------------------------------------------------------
# visualize time series and predictions
par(mfrow=c(3,3))
for (i in 1:nrow(m)) {
  visualize_ts_pred(y=m[i,], s=stl_pred[[i]])
} 
par(mfrow=c(1,1))

# mean of predictions
pred
#       201803 201804 201805 201806 201807 201808 201809 201810 201811 201812 201901 201902
#  [1,]      9     10      9     10     10      9      9     10     10      9     10     10
#  [2,]     53     52     51     53     53     51     52     53     53     51     51     54
#  [3,]      5      6      5      5      4      5      6      6      4      4      3      4
#  [4,]     20     22     20     20     21     19     20     22     23     24     21     24
#  [5,]      5      4      5      4      5      5      5      5      4      3      4      5
#  [6,]     15     15     17     15     16     17     13     11     14     16     15     12
#  [7,]    124    122    129    123    122    134    127    136    130    134    136    137
#  [8,]     37     37     35     39     42     41     44     37     41     37     40     39
#  [9,]     13     12      9     15     15     13     11     11     13     14     14     13
# [10,]     35     34     30     36     30     30     30     33     30     32     24     28
# [11,]    161    166    154    150    158    149    149    145    153    160    150    150
# [12,]     30     30     27     29     31     30     27     28     28     29     29     23
# [13,]     55     59     63     58     62     63     58     60     62     62     63     52
# [14,]     50     42     58     49     45     53     44     46     47     44     50     50
# [15,]     23     25     27     25     25     27     24     29     21     25     22     24
# [16,]      5      4      5      6      4      4      5      4      5      5      2      5
# [17,]     10     13     10     12     15     12     16     12     10     12     11     12
# [18,]     87     92     74     87     86     92     76     78     91    105     87     87
# [19,]     27     29     32     28     31     29     31     29     30     33     31     29
# [20,]    107    117    105     98    108    103    101    100    107    106    105     86
# [21,]     32     36     41     32     30     33     30     30     30     30     26     30

#-------------------------------------------------------------------------------
