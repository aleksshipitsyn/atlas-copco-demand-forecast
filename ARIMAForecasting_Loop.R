############################################
# This Section is quick loop to period for a Prod and Region
# and generate consolidated table of ARIMA Forecast 
###########################################

library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DescTools)

rm(list = ls())

setwd("C:/Users/almirall/Documents/NEWPROJECTS")

dataset <- read.csv("SEB_DemandHistory.txt")
#y <-ts(inf_data$Requiredquantity,start = c(2010,1),end=c(2019,3),frequency = 12)
dataset$Date <- as.Date(dataset$Reqdlvdt, "%d/%m/%y %H:%M:%S")
dataset$Reqdlvdt<-gsub("/","-",dataset$Reqdlvdt)
dataset$Date <- parse_date_time(dataset$Reqdlvdt, orders="dmy HMS")

dataset$Requiredquantity <- as.numeric(gsub('[£,]', '', dataset$Requiredquantity))


rm(dfAutoArimaNewTxt,dfAutoArimaTotal,dfdataset3Txt,dfActualTotal)
dfAutoArimaTotal<-data.frame(Mois="",value=0,Prod="",Reg="",Lag=0)
dfActualTotal<-data.frame(Mois="",value=0,Prod="",Reg="")

#region <-"KR"
product <-"iGX600"
PeriodLoop <- c(1:23)
#RegionLoop <- c("CN","KR","JP","EU","SG","TW","US")
#RegionLoop <- c("SG")

debutTrain <- as.Date("2010-01-01")
finTrain0 <- as.Date("2016-12-31")
debutTest0 <- as.Date("2017-01-01")
finTest0 <- as.Date("2017-11-30")

#Regions Loop
for (r in 1:length(RegionLoop))
{
  region <- RegionLoop[r]

for (i in 1:length(PeriodLoop))
  {
  finTrain <- AddMonths(finTrain0,i)
  debutTest <- AddMonths(debutTest0,i)
  finTest <- AddMonths(finTest0,i)
  t0<-c(year(debutTrain),month(debutTrain),day(debutTrain))
  t1<-c(year(finTrain),month(finTrain),day(finTrain))
  t2<-c(year(finTrain),month(finTrain),28)
  t3<-c(year(finTest),month(finTest),Day(finTest))
  TrainSample= data.frame(date = seq.Date(from =debutTrain, 
                                          to=finTrain, by="month"),value="a")
  Train_Sample=NROW(TrainSample)
  TestSample= data.frame(date = seq.Date(from =debutTest, 
                                         to=finTest, by="month"),value="a")
  Test_Sample=NROW(TestSample)
  Total_Sample=Test_Sample+Train_Sample
  #dataset2 Is filered records in the total windows (Train+Test)
  dataset2 <- filter(dataset, dataset$Date >= debutTrain & dataset$Date <= finTest & eHubRegion ==region & LowLevelProductFamily==product)
  #dataset2 Is Summarized filered records in the total windows (Train+Test)
  #in other words Actuals!
  dataset3 <- dataset2 %>% 
    group_by(month=floor_date(Date, "month")) %>%
    summarise(Requiredquantity = sum(Requiredquantity))
  #dataset2 Is filered records for ARIMA Training
  dataset2Trained <- filter(dataset, dataset$Date >= debutTrain & dataset$Date < debutTest & eHubRegion ==region & LowLevelProductFamily==product)
  #dataset2 Is Summarized for ARIMA Training
  dataset3Trained  <- dataset2Trained %>% 
    group_by(month=floor_date(Date, "month")) %>%
    summarise(Requiredquantity = sum(Requiredquantity))
  Y <- ts(dataset3Trained[,2],start = t0, end = t2,frequency = 12)
  #autoplot(Y)
  
  ##ARIMA method
  fit_arima <- auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace=TRUE)
  #print(summary(fit_arima))
  #checkresiduals(fit_arima)
  ##Forecast with ARIMA
  fcst <- forecast(fit_arima,h=Test_Sample)
  #autoplot(fcst)
  #fcst
  dfAutoArima <- data.frame(date = seq.Date(from =debutTest, 
                                            to=finTest, by="month"),
                            value = fcst$mean)
  #dfAutoArima
  dfAutoArimaNew<-data.frame(date=dfAutoArima$date,value=dfAutoArima$value,Prod=product,Reg=region)
  dfAutoArimaNew <- dfAutoArimaNew %>% mutate(Lag = -row_number())
  dfAutoArimaNewTxt<-data.frame(Mois= as.character(dfAutoArima$date),value=dfAutoArima$value,Prod=product,Reg=region, Lag=dfAutoArimaNew$Lag)
  dfAutoArimaTotal<-rbind(dfAutoArimaTotal,dfAutoArimaNewTxt)
}

#Re-write DataSet2 for the final Demand in the original scope
dataset2 <- filter(dataset, dataset$Date >= "2010-01-01" & dataset$Date <= "2018-12-31" & eHubRegion ==region & LowLevelProductFamily==product)
#dataset2 Is Summarized filered records in the total windows (Train+Test)
#in other words Actuals!
dataset3 <- dataset2 %>% 
  group_by(month=floor_date(Date, "month")) %>%
  summarise(Requiredquantity = sum(Requiredquantity))
#dfdataset3 <- data.frame(date = seq.Date(from = as.Date("2010-01-01"), 
#                                         to=as.Date("2018-12-31"), by="month"),
#                         value = dataset3$Requiredquantity)
dfdataset3 <- data.frame(date=dataset3$month,value=dataset3$Requiredquantity)


dfdataset3Txt<-data.frame(Mois= as.character(dfdataset3$date),value=dfdataset3$value,Prod=product,Reg=region)
#dfActualTotal<-data.frame(Mois="",value=0,Prod="",Reg="")
dfActualTotal<-rbind(dfActualTotal,dfdataset3Txt)

}

# Write data to csv files:
write.table(dfAutoArimaTotal, "clipboard-16384", sep="\t", row.names=TRUE)
write.table(dfActualTotal, "clipboard-16384", sep="\t", row.names=TRUE)


