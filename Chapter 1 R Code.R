install.packages("TSA")
install.packages("tseries")

####### 1.1.1 ANNUAL RAINFALL IN L.A. ####

# Describe the data. What can we learn from the plot?
library(TSA)
data(larain)
larain
help(larain)
help(plot)
plot(larain,ylab="Inches",xlab="Year",type="b")

# What is zlag doing?
cbind(zlag(larain), larain)
plot(larain,x=zlag(larain),ylab="Inches",
     xlab="Previous year inches")
abline(0,1)

####### 1.1.2 Abundance of Canadian Hare by Year ####

# Do data look dependent from year to year based on the plot?
data(hare)
plot(hare,ylab="Abundance", xlab="Year",type="o")

# Does the above plot support you answer to the previous question?
plot(x=zlag(hare),y=hare ,xlab="Previous Year Abundance",
     ylab="Abundance")
abline(0,1)

# Does abundance in the current year depend on abundance 2 years ago? How
# about 3 years ago, 4 years ago, ...
plot(x=zlag(hare, 2),y=hare ,xlab="Abun 2 Yrs ago", ylab="Abun")

####### 1.1.3 Unemployment Rate ####

# Do data look dependent from month to month?
unemp = ts(read.table("C:/Users/nickd/OneDrive - Oklahoma A and M System/Time Series Analysis/Data/unemp.txt"), start = 2005, frequency =12)
plot(unemp, ylab = "Monthly Unemp.", xlab = "year", type = "o")
plot(x=zlag(unemp),y=unemp ,xlab="Previous Month
Unemployment Rate", ylab="Unemployment Rate")

####### 1.1.4 Dow Jones ####

# Do data look dependent from month to month?
# the [[2]] below tells R to only look at the second column in the txt file
DJIA = ts(read.table("DJIA.txt")[[2]],frequency = 12,
          start = c(1896, 6))
plot(x=zlag(DJIA),y=DJIA ,xlab="Prev Month DJIA", ylab="DJIA")

# Sometimes we model the returns of a time series. Are the returns dependent
# from month to month?
returns = diff(log(DJIA))
plot(returns, main = "DJIA Returns")
plot(x=zlag(returns),y=returns ,xlab="Prev Month Ret", ylab="Ret")

####### 1.1.5 Beer Sales Index by Month ####

# Do you notice a pattern in the plot? This regular pattern is called monthly
# seasonality. What other types of times series might be seasonal?
data(beersales)
plot(y=beersales,x=time(beersales),
     pch=as.vector(season(beersales)),type="o")

# Suppose you own the liquor store downtown. Should you use last month’s sales
# or last year’s sales to determine how much beer to stock.
par(mfrow=c(1,2))
plot(y=beersales,x=zlag(beersales,d=1), xlab="previous month sales",
     ylab="beer sales")
abline(0,1)
plot(y=beersales,x=zlag(beersales,d=12),xlab="previous years sales",
     ylab="beer sales")
abline(0,1)

####### 1.1.6 Sun Spot Radiation Measurements by Year ####

# Do you see a seasonal trend?
# How are the seasonal trends for beersales and sunspots different?
data(spots)
plot(spots, type = "o")

#################### 1.2 Regression and Time Series ################
####### 1.2.1 Regression ####

# Lets consider the annual unemployment rate over the last 12 years.
# Does the linear model yt = β0 + β1t + episolon(underscore t) fit the data?
y= c(6.0,5.5,5.1,4.6,4.6,5.8,9.3,9.6,8.9, 8.1, 7.4, 6.8)
t=2003:2014
plot(t,y, type = "b", main = "annual unemployment rate")
abline(lm(y~t))

# What about this linear model: yt = β0 + β1t + β2t^2 + episolon(underscore t)
# what are the assumptions for the linear model?
# What do the above assumptions imply about the relationship between Yt and Yt−1?
# is this assumption reasonable?
t.sq=t*t
plot(t,y, type = "b", main = "annual unemployment rate", ylim = c(4,10))
par(new = T)
plot(t, predict(lm(y~t+t.sq)),type="l", ylab = "", ylim = c(4,10))

####### 1.2.2 Time Series ####
# The following set of plots better illustrates how these data arise
# Do you still believe that the assumptions of the linear model are satisfied?
# Is the above question relevant?
t=2003:2014
for( i in 1:12){
  plot(t[1:i], y[1:i], type="b", xlim=c(2003,2014),
       ylim=c(3,11), ylab="y",xlab="t")
  readline()}

###################### 1.3 The Box-Jenkins Strategy  ###############
# Our Box Jenkins (1976) model building strategy can be broken down into 3 steps,
# which will be iterated until we are satisfied with our model. We will adhere to the
# principle of parsimony throughout our model building process.
# 1. Model Specification. This step is tentative. Loosely speaking, can you think
# of a model for beersales?
# 2. Model Fitting. We must estimate parameters for the specified model. Do
# you know of any methods for estimating parameters in a statistical model?
# 3. Model Diagnostics. Diagnostic tools will be used to determine if the putative
# model fits, and if not, what model might fit better.

###################### 1.4 The Outline for the Course  ###############
# 1. Develop some foundational tools: Mean functions, variance functions, autocovariance functions, autocorrelation functions, random walks, 
# stationarity.
# 2. Develop some useful models: Regression trend models, autoregressive moving
# average (ARMA) models, autoregressive integrated moving average (ARIMA) models.
# 3. Model specification, model fitting, and model diagnostics
# 4. Forecasting with ARIMA models.
# 5. Inference for general linear models for time series: Outlier detection, shocks to
# the system.



