install.packages("vars")
library(vars)
library(tseries)
library(forecast)
install.packages('urca')
library(urca)
install.packages('mFilter')
library(mFilter)
library(tidyverse)
attach(time_series_data)
pce = ts(PCE, start = c(1970,3), frequency = 4)
pdi = ts(PDI, start = c(1970,3), frequency = 4)

#plotting the independent variables
ts.plot(pce)
ts.plot(pdi)
autoplot(cbind(pce,pdi))

ols1 = lm(pdi ~ pce)
summary(ols1)

#testing the stationarity
adf.test(pdi)
adf.test(PDI)
pp.test(pdi)
#we find PDI is not staionary.
plot.ts(PDI)
adf.test(pce)
adf.test(PCE)
#we find data is not stationary
plot.ts(PCE)

#to make it stationary we will take lag values
dpdi = diff(pdi)
DPDI = diff((PDI))
adf.test(dpdi)
adf.test(DPDI)
plot.ts(DPDI)
#PDI is now stationary after one lagged difference

dpce = diff(pce)
DPCE = diff(PCE)
adf.test(DPCE)
adf.test(dpce)
plot.ts(DPCE)
#PCE is still not stationary. so we will take second lag
DPCE
ddpce= diff(pce, lag = 2, differences = 2) 
DDPCE= diff(PCE, lag=2, differences = 2)
adf.test(DDPCE)
adf.test(ddpce)
plot.ts(DDPCE)
DDPCE
autoplot(cbind(ddpce,dpdi))

#his is after second lag difference 
#we fac with the problem after difference is that we need to ignore certain values from the bottom as they are neglected.
#hence we will need to slice the data in order to further proceed in the analysis.
nDPDI= DPDI[1:84]
nDDPCE= DDPCE[1:84]
#following commnd will staore 1 to 84 data in the new variable


acf(dpdi)
pacf(dpdi)
acf(ddpce)
pacf(ddpce)
#now will create model to enforce vector auto regression (VAR)
lagselect = VARselect(data.frame(nDPDI,nDDPCE), lag.max = 10, type = 'const')
lagselect
lagselect$selection
#firstly we will see to create lag selection in order to get appropriate lag while modeling.
model1 <- VAR(data.frame(nDPDI,nDDPCE), p = 5, type = "const", season = NULL, exog = NULL)
summary(model1)
#we find the equation of the endogenous variables based on 5 lag variables of both. 




#after that we try to perform model diagnostics of th VAR nodel created above,
#aim is to check from auto correlation   
Serial1 <- serial.test(model1, lags.pt = 5, type = "PT.asymptotic")
Serial1
#we find that p value is less than 0.05 which confirms the absence of residuals being auto corelated with the variables
#when we increase the lag values then we get auto correlation iin the model. this is due to white noise coming from
#extra lags taken

#Another aspect to consider is the presence of heteroscedasticity
#this tests whether during rapid fluctuations esiduals variance remain grounded/constant as per assumption or not.
Arch1 <- arch.test(model1, lags.multi = 5, multivariate.only = TRUE)
Arch1
#the results of the ARCH test signify no degree of heteroscedasticity as we fail to reject the null hypothesis.
#as p value is more than 0.05

Norm1 <- normality.test(model1, multivariate.only = TRUE)
Norm1
plot(Norm1)
#we find that residual lag have no significant impact, histogram is almost normal and residual is evenly spread
#the model consist of the cofficients of various lags of i=our independent variable.

#The stability test is some test for the presence of structural breaks. 
#We know that if we are unable to test for structural breaks and if there happened to be one, the whole estimation may be thrown off
Stability1 <- stability(model1, type = "OLS-CUSUM")
plot(Stability1)
#a plot of the sum of recursive residuals. If at any point in the graph, 
#the sum goes out of the red critical bounds, then a structural break at that point was seen.





#granger casulty test
#Granger causality testing each variable in the system against all the others. As we said, 
#there could be a unidirectional, bidirectional, or no causality relationships between variables.
GrangernDPDI<- causality(model1, cause = "nDDPCE")
GrangernDPDI
GrangernDDPCE <- causality(model1, cause = "nDPDI")
GrangernDDPCE

#the null hypothesis would be: X does not granger cause Y or the other way. Also, you accept or reject your null hypothesis depending on the level of significance.
#f P value < Significance level, then Null hypothesis would be rejected.
#if P value > Significance level, then Null hypothesis cannot be rejected.


#impulse response functions

nDPDIirf <- irf(model1, impulse = "nDPDI", response = "nDPDI", n.ahead = 20, boot = TRUE)
plot(RRPirf, ylab = "nDPDI", main = "nDPDIs shock to nDPDI")

nDDPCEirf <- irf(model1, impulse = "nDDPCE", response = "nDDPCE", n.ahead = 20, boot = TRUE)
plot(nDDPCEirf, ylab = "nDDPCE", main = "nDDPCE's shock to nDDPCE")

XYirf <- irf(model1, impulse = "nDDPCE", response = "nDPDI", n.ahead = 20, boot = TRUE)
plot(XYirf, ylab = "nDPDI", main = "nDDPCE's shock to nDPDI")

YXirf <- irf(model1, impulse = "nDPDI", response = "nDDPCE", n.ahead = 20, boot = TRUE)
plot(YXirf, ylab = "nDDPCE", main = "nDPDI's shock to nDDPCE")
#the IRFs where we need to specify the model, what the impulse series will be


#forecast error variance decomposition. 
#Again, we can trace the development of shocks in our system to explaining the forecast error variances 
#of all the variables in the system.
FEVD1 <- fevd(model1, n.ahead = 10)
FEVD1
plot(FEVD1)

#forecaste
forecast <- predict(model1, n.ahead = 12, ci = 0.95)
forecast
plot(forecast)

fanchart(forecast, names = "nDDPCE", 
main = "Fanchart for nDDPCE", 
xlab = "Horizon", ylab = "nDDPCE")

fanchart(forecast, names = "nDPDI", 
         main = "Fanchart for nDPDI", 
         xlab = "Horizon", ylab = "nDPDI")
