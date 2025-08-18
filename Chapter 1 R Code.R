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
# plot is mainly 

# What is zlag doing?
cbind(zlag(larain), larain)
plot(larain,x=zlag(larain),ylab="Inches",
     xlab="Previous year inches")
abline(0,1)
lagged <- cbind(zlag(larain), larain)
# 1:114 is the first value to the 114th value, 2:115 takes the 2nd value to the 115th value, when using cbind this created each 
# row that has the current year and the previous year values 
# first column will be used as the previous time period to compare to the second column which will be the current time period
manual_lag <- cbind(larain[1:114], larain[2:115])
cor(lagged, use = "complete.obs") 
# zlag is creating lagged data, so it is taking yt and subtracting one (so time point one value is now time point 0)
# then we correlate the lagged and non-lagged values to see if the value preceeding the current value is correlated with the current value
# correlation is very low indicating previous values (y(t - 1)) is not very useful in predicting the value for the current value

####### 1.1.2 Abundance of Canadian Hare by Year ####

# Do data look dependent from year to year based on the plot?
data(hare)
hare
plot(hare,ylab="Abundance", xlab="Year",type="o")
# this plot shows a cyclical pattern, and shows that there is a dependence between the current year and the previous year

# Does the above plot support you answer to the previous question?
plot(x=zlag(hare),y=hare ,xlab="Previous Year Abundance",
     ylab="Abundance")
abline(0,1)
cor(zlag(hare), hare, use = "complete.obs") 
# there is more of a correlation observed in this plot compared to the first plot, 
# the current year is positively correlated with the previous year 

# Does abundance in the current year depend on abundance 2 years ago? How
# about 3 years ago, 4 years ago, ...
# the zlag(hare, 2) is taking the lag from not the previous year but last year
plot(x=zlag(hare, 2),y=hare ,xlab="Abun 2 Yrs ago", ylab="Abun")
abline(0,1)
# lag 3 - this year vs 3 years ago
plot(x=zlag(hare, 3),y=hare ,xlab="Abun 3 Yrs ago", ylab="Abun")
abline(0,1)
# lag 4 
plot(x=zlag(hare, 4),y=hare ,xlab="Abun 4 Yrs ago", ylab="Abun")
abline(0,1)

####### 1.1.3 Unemployment Rate ####

# Do data look dependent from month to month?
# ts function converts data into a time series and needs the start and frequency of observations (12 per year - monthly unemployment)
unemp = ts(read.table("C:/Users/nickd/OneDrive - Oklahoma A and M System/Time Series Analysis/Data/unemp.txt"), start = 2005, frequency =12)
unemp
plot(unemp, ylab = "Monthly Unemp.", xlab = "year", type = "o")
plot(x=zlag(unemp),y=unemp ,xlab="Previous Month
Unemployment Rate", ylab="Unemployment Rate")
abline(0,1)
cor(zlag(unemp), unemp, use="complete.obs")
#re-produce manually
plot(x=unemp[1:126],y=unemp[2:127] ,xlab="Previous Month
Unemployment Rate", ylab="Unemployment Rate")
abline(0,1)

####### 1.1.4 Dow Jones ####

# Do data look dependent from month to month?
# the [[2]] below tells R to only look at the second column in the txt file
DJIA = ts(read.table("C:/Users/nickd/OneDrive - Oklahoma A and M System/Time Series Analysis/Data/DJIA.txt")[[2]],frequency = 12,
          start = c(1896, 6))
# above is telling R that the first observation occurs in 1896 (year), and that the sub-period is 6 - which is that the first month with an 
# observation is June (out of 12 months per year (frequency = 12))
DJIA
plot(DJIA, ylab="index", xlab="year", type="o")
plot(x=zlag(DJIA),y=DJIA ,xlab="Prev Month DJIA", ylab="DJIA")
abline(0,1)
cor(zlag(DJIA), DJIA, use="complete.obs")

# Sometimes we model the returns of a time series. Are the returns dependent
# from month to month?
returns = diff(log(DJIA)) # this is taking the difference of log(yt) - log(yt - 1) - lagged difference
plot(returns, main = "DJIA Returns")
plot(x=zlag(returns),y=returns ,xlab="Prev Month Ret", ylab="Ret")
# no correlation observed

####### 1.1.5 Beer Sales Index by Month ####

# Do you notice a pattern in the plot? This regular pattern is called monthly
# seasonality. What other types of times series might be seasonal?
data(beersales)
plot(y=beersales,x=time(beersales),type="o")
plot(y=beersales,x=time(beersales),
     pch=as.vector(season(beersales)),type="o")
# seasonal pattern is identified by up and down pattern per year, summer occurs in peaks, winter occurs in valleys 
# overall trend is still increasing because the sales are rising as year increasing
# dependence now occurs between the same month of the previous year, not yt - yt-1 (august compared to July),
# but instead is yt - yt-12 (aug this year vs aug last year)

# Suppose you own the liquor store downtown. Should you use last month’s sales
# or last year’s sales to determine how much beer to stock.
par(mfrow=c(1,2))
plot(y=beersales,x=zlag(beersales,d=1), xlab="previous month sales",
     ylab="beer sales")
abline(0,1)
plot(y=beersales,x=zlag(beersales,d=12),xlab="previous years sales",
     ylab="beer sales")
abline(0,1)
# stronger correlation observed by year

####### 1.1.6 Sun Spot Radiation Measurements by Year ####

# Do you see a seasonal trend?
# How are the seasonal trends for beersales and sunspots different?
data(spots)
spots
plot(spots, type = "o")
# yes there is a seasonal trend
# the data spans from 1945 - 2005, and has data points of one per year
# in contrast to the beer sales, the cyclical nature is about every 10 years opposed to every 12 months with beer sales

#################### 1.2 Regression and Time Series ################
####### 1.2.1 Regression ####

# Lets consider the annual unemployment rate over the last 12 years.
# Does the linear model yt = β0 + β1t + episolon(underscore t) fit the data?
y= c(6.0,5.5,5.1,4.6,4.6,5.8,9.3,9.6,8.9, 8.1, 7.4, 6.8)
t=2003:2014
plot(t,y, type = "b", main = "annual unemployment rate")
abline(lm(y~t))
# the linear model does not fit the data, as the dots do not follow along with the linear regression line

# What about this linear model: yt = β0 + β1t + β2t^2 + episolon(underscore t)
  # still a linear model, but we are adding a quadratic term
# what are the assumptions for the linear model?
  # independence, normality of error term (residuals), homoscedasticity (constant variance) of error (residuals), lack of multicollinearity,
  # and error term (residuals) should be centered at 0
# What do the above assumptions imply about the relationship between Yt and Yt−1?
  # yt and yt - 1 are not independent 
# is this assumption reasonable?
  # no, yt and yt-1 are not independent 
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
  readline()
}
# yes, the above question is relevant, we must deal with the dependence/correlation amongst observations in our model
# THIS IS THE FOCUS OF THE COURSE

###################### 1.3 The Box-Jenkins Strategy  ###############
# Our Box Jenkins (1976) model building strategy can be broken down into 3 steps,
# which will be iterated until we are satisfied with our model. We will adhere to the
# principle of parsimony throughout our model building process.
# 1. Model Specification. This step is tentative. Loosely speaking, can you think
# of a model for beersales?
  # THEORY OF EACH MODEL IS COVERED IN CHAPTERS 1 - 5, CHAPTER 6 COVERS MODEL SPECIFICATION
# 2. Model Fitting. We must estimate parameters for the specified model. Do
# you know of any methods for estimating parameters in a statistical model? MAXIMUM LIKELIHOOD ESTIMATION, SUM OF LEAST SQUARES, ETC.
  # THIS IS COVERED IN CHAPTER 7
# 3. Model Diagnostics. Diagnostic tools will be used to determine if the putative
# model fits, and if not, what model might fit better.
  # THIS IS COVERED IN CHAPTER 8
# CHAPTER 9 IS FORECASTING 

###################### 1.4 The Outline for the Course  ###############
# 1. Develop some foundational tools: Mean functions, variance functions, autocovariance functions, autocorrelation functions, random walks, 
# stationarity.
# 2. Develop some useful models: Regression trend models, autoregressive moving
# average (ARMA) models, autoregressive integrated moving average (ARIMA) models.
# 3. Model specification, model fitting, and model diagnostics
# 4. Forecasting with ARIMA models.
# 5. Inference for general linear models for time series: Outlier detection, shocks to
# the system.



