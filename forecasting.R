
# packages included 
library(fpp2)
library(dplyr)
library(readxl)





Retail=read_xlsx("RetailDataIndividual.xlsx",skip=3) %>%
  pull('29416000')%>%
  ts(start=c(1982,4),frequency = 12)
autoplot(Retail)+
  ggtitle("Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")+ ylab ( "Turnover in Millions")

#Seasonal Plot 
ggseasonplot(Retail,year.labels = TRUE,year.labels.left = TRUE)+ggtitle("Seasonal Plot- Turnover of Food Retailing in Australian Capital Territory ") +
  xlab ("Months")+ ylab ( "Turnover in Millions")

#finding frequency and Maximum Value
frequency(Retail)
which.max(Retail)

#Subseries Plot
ggsubseriesplot(Retail)+ggtitle("Subseries Plot- Turnover of Food Retailing in Australian Capital Territory ")+
  xlab ("Months")+ ylab ( "Turnover in Millions")

#Auto corelation 
ggAcf(Retail,lag.max=60)+ggtitle("Correlation Plot ")
gglagplot(Retail) + ggtitle("Lag Plot")

#Q2

#log Tranformation
LogT=Retail %>% BoxCox(lambda=0) %>% autoplot() + ggtitle('Log Tranformation of Food Retail')

#Square Root
Power_0.5=Retail %>% BoxCox(lambda=1/2) %>% autoplot + ggtitle('Power Tranformation of Food Retail with lambda=1/2')

# Cube Root
Power_0.25=Retail %>% BoxCox(lambda=1/4) %>% autoplot + ggtitle('Power Tranformation of Food Retail with lambda=1/3')

#BoxCOX
lambda <- BoxCox.lambda(Retail)
AutoLambda=Retail %>% BoxCox(lambda) %>%
  autoplot() + 
  ylab(paste("BoxCox(Turnover in Million$,", round(lambda, 2), ")")) + ggtitle('BoxCox Tranformation of Food Retail with Auto selection of Lambda')

original=autoplot(Retail)+ggtitle("Original Data")
gridExtra::grid.arrange(original,LogT,Power_0.5,Power_0.25,AutoLambda,nrow=3)

#Q3 - Dividing the dataset into Train and Test
Retail_Train <- window(Retail, end = c(2014,12))
Retail_Test <- window(Retail, start = c(2015,1))
autoplot(Retail) + ggtitle("Plot Showing Test and Train datasets")+
  autolayer(Retail_Train, series="Training") +
  autolayer(Retail_Test, series="Test")

#Q4 - Fitting the Model
f3 <- snaive(Retail_Train, h=length(Retail_Test))
f4 <- rwf(Retail_Train, drift=TRUE, h=length(Retail_Test))

# Calcuating the Accuracy of snaive
snaive=accuracy(f3,Retail_Test)
snaive=as.data.frame(snaive)
snaive$method='SNaive'

#Calculating the accuracy of RWF

rwf=accuracy(f4,Retail_Test)
rwf=as.data.frame(rwf)
rwf$method='rwf'

#Putting the two into one dataframe
Benchmark_Methods <- rbind(rwf,snaive)

#PLotting both sniave and rwf
autoplot(Retail_Train) + ggtitle("Forecast plot of Random Walk with Drift and Seasonal naive")+
  xlab("years")+ylab("Turnover in Millions")+
  autolayer(f3, series="snaive", PI=FALSE)+
  autolayer(f4, series="rwf", PI=FALSE)+
  autolayer(Retail_Test,series = 'Test')


#Q5
checkresiduals(f3)

#Q6- Plotting the forecasts
autoplot(snaive(Retail,h=24)) +ggtitle("Forecasts of Retailing from Seasonal naive method")+
  xlab("Years")+ylab("Turnover in Millions")

#-------------------------------------Assignment 3-----------------------------------------
#Q1- Explorind data using STL and MSTL
stlFit <- stl(Retail, s.window = "periodic")
autoplot(stlFit) + ggtitle("Trend,Seasonal and Remainder STL plot for Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")

mstlFit <- mstl(Retail)
autoplot(stlFit) +ggtitle("Trend,Seasonal and Remainder MSTL plot for Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")

#Q2
autoplot(ets(Retail, model="MAM", damped=FALSE))
ETS_MAM=ets(Retail, model="MAM", damped=FALSE)
summary(ETS_MAM)

#Q3
checkresiduals(ETS_MAM)

#Q5

#Building alternative Model 
ETS_MNM=ets(Retail, model="MNM", damped=FALSE)
autoplot(ETS_MNM)

#Summary of both
summary(ETS_MNM)
summary(ETS_MAM)

#checking Residuals of both
checkresiduals(ETS_MNM)
checkresiduals(ETS_MAM)


#Forcasting using both
ETS_MAM_forecast=forecast(ETS_MAM,h=24)
ETS_MNM_forecast=forecast(ETS_MNM,h=24)

#Generating Residuals obtained from forecast function
x <- ETS_MNM_forecast$residuals 
h<-hist(x, breaks=10, col="red", xlab="Residual Range", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
hist(ETS_MAM_forecast$residuals, col=rgb(0,0,1,0.5), add=T)
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)



#Q6
Retail %>% window(start=2015) %>% ets("MAM") %>% forecast() %>% autoplot()+ggtitle("Forecasts from ETS(MAM) Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")+ ylab ( "Turnover in Millions")


#Q7

Retail %>% window(start=2015) %>% ets("MAM") %>% forecast() %>% autoplot()+ggtitle("Forecasts from ETS(MAM) Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")+ ylab ( "Turnover in Millions")


Retail %>% window(start=2015) %>% ets("MNM") %>% forecast() %>% autoplot()+ggtitle("Forecasts from ETS(MNM) Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")+ ylab ( "Turnover in Millions")

#-------------------------------------Assignment 4---------------------------------------------------------------------------------

#Q1
Retail=read_xlsx("RetailDataIndividual.xlsx",skip=3) %>%
  pull('29416000')%>%
  ts(start=c(1982,4),frequency = 12)
autoplot(Retail)+
  ggtitle("Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")+ ylab ( "Turnover in Millions")

lambda <- BoxCox.lambda(Retail)
Retail %>% BoxCox(lambda=lambda) %>% autoplot() + ggtitle("Variance Stabilized Plot")+  xlab ("Years")

gglagplot(Retail)+ ggtitle("Lag Plot") 


lambda <- BoxCox.lambda(Retail)
Retail %>% BoxCox(lambda=lambda)%>% ggAcf(lag.max = 60)+ggtitle("Correlation Plot ")


Retail %>% BoxCox(lambda=lambda)%>% diff(lag=12) %>%  autoplot() +
  ggtitle("First seasonal Difference ")

Retail %>% BoxCox(lambda=lambda)%>% diff(lag=12)  %>% ggAcf() +
  ggtitle("Correlaion of First seasonal Difference ")

#Q2
Retail %>% BoxCox(lambda=lambda)%>%  diff(lag=12) %>% ggtsdisplay(main="Series,ACF and PACF plots")

#Q3
Fit=Retail %>% BoxCox(lambda=lambda) %>%Arima(order=c(2,0,0),seasonal=c(3,1,0))
Fit
checkresiduals(Fit)

#Q4
Fit1=Retail %>% BoxCox(lambda=lambda) %>%  Arima(order=c(2,0,1),seasonal=c(3,1,0))
Fit2=Retail %>% BoxCox(lambda=lambda) %>%  Arima(order=c(2,0,1),seasonal=c(2,1,0))
Fit3=Retail %>% BoxCox(lambda=lambda) %>%  Arima(order=c(2,0,3),seasonal=c(0,1,3))
Fit1
Fit2
Fit3



#Q5
Fit_auto=Retail %>% BoxCox(lambda=lambda) %>% auto.arima()
Fit_auto
checkresiduals(Fit_auto)

#Q6

Fit_auto=Retail %>% BoxCox(lambda=lambda) %>% auto.arima(stepwise = FALSE,approximation = FALSE)
Fit_auto
checkresiduals(Fit_auto)

#Q7

Retail_Train <- window(Retail, end = c(2014,12))
Retail_Test <- window(Retail, start = c(2015,1))
autoplot(Retail) + ggtitle("Plot Showing Test and Train datasets")+
  autolayer(Retail_Train, series="Training") +
  autolayer(Retail_Test, series="Test")

Fit1=Retail_Train %>% BoxCox(lambda=lambda) %>% Arima(order=c(2,0,1),seasonal=c(3,1,0))
Fit2=Retail_Train %>% BoxCox(lambda=lambda) %>% Arima(order=c(2,0,1),seasonal=c(2,1,0))
Fit3=Retail_Train %>% BoxCox(lambda=lambda) %>% Arima(order=c(2,0,3),seasonal=c(0,1,3))
Fit4=Retail_Train %>% BoxCox(lambda=lambda) %>% Arima(order=c(2,0,4),seasonal=c(0,1,2))
Fit5=Retail_Train %>% BoxCox(lambda=lambda) %>% Arima(order=c(3,0,0),seasonal=c(0,1,2))

fit1_accuracy=accuracy(Arima(Retail_Test, model=Fit1))
fit2_accuracy=accuracy(Arima(Retail_Test, model=Fit2))
fit3_accuracy=accuracy(Arima(Retail_Test, model=Fit3))
fit4_accuracy=accuracy(Arima(Retail_Test, model=Fit4))
fit5_accuracy=accuracy(Arima(Retail_Test, model=Fit5))

Models <- rbind(fit1_accuracy,fit2_accuracy,fit3_accuracy,fit4_accuracy,fit5_accuracy)

#Q8
Fit_arima=Retail %>%window(start=2015)%>% Arima(order=c(2,0,4),seasonal=c(0,1,2),lambda = lambda)
Arima_fore=forecast(Fit_arima,h=24)
autoplot(Arima_fore)+ xlab ("Years")+ ylab ( "Turnover in Millions")

#Q9

Retail_full=read_xlsx("RetailDataIndividualFull.xlsx",skip=3) %>%
  pull('29416000')%>%
  ts(start=c(1982,4),frequency = 12)
autoplot(Retail)+
  ggtitle("Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")+ ylab ( "Turnover in Millions")

#Q10
snaive = Retail %>% window(start=2015) %>% snaive()
snaive_fore= forecast(snaive,h=24)

ETS_MAM=Retail %>% window(start=2015) %>% ets(model = "MAM", damped=FALSE)
ETS_fore= forecast(ETS_MAM,h=24)

Retail_full_2015= Retail_full  %>% window(start=2015)

autoplot(Retail_full_2015) +
  ggtitle("Forecast comparision of ETS,snaive and Arima models developed")+
  xlab("years")+ylab("Turnover in Millions")+
  autolayer(snaive_fore, series="snaive", PI=TRUE)+
  autolayer(ETS_fore, series="ETS_MAM", PI=TRUE)+
  autolayer(Arima_fore,series="Arima",PI=TRUE)+
  autolayer(Retail_full_2015,series = 'Data')

#Q11

arima_accuracy=accuracy(Arima_fore,Retail_full)
snaive_accuracy=accuracy(snaive_fore,Retail_full)
ets_accuracy=accuracy(ETS_fore,Retail_full)

Final_Models <- rbind(arima_accuracy,snaive_accuracy,ets_accuracy)

#Q12

Retail_full %>% window(start=2015) %>% snaive(level=80,h=24)  %>% autoplot()+
  ggtitle("Forecasts from Seasonal Naive- Food Retailing Turnover of Australian Capital Territory") +
  xlab ("Years")+ ylab ("Turnover in Millions")

