# Homework 1 - Time Series Analysis#
# Nick Hollman#
library(TSA)

##### PROBLEM 1 ######
# 1. Construct a scatter plot of (unemployment rate, previous months unemployment rate), (unemployment rate, previous year’s unemployment rate), 
# and (unemployment rate, unemployment rate 2 years ago) and draw a 45 degree line on each plot. Suppose that you would like to forecast or predict 
# the unemployment rate next month, next year, and 2 years from now. Based on your plots, which forecast will be the most trustworthy: 1 month from now, 
# 1 year from now, 2 years from now? Explain why in a sentence or two.
unemp = ts(read.table("Data/unemp.txt"), start = 2005, frequency =12)
unemp
# plot unemployment rate by year
plot(unemp, ylab = "Monthly Unemp.", xlab = "year", type = "o")
# plot unemployment rate by previous month unemployment rate w/ 45 degree line
plot(x=zlag(unemp),y=unemp ,xlab="Previous Month Unemployment Rate", ylab="Unemployment Rate")
abline(0,1)
# plot unemployment rate by previous year unemployment rate w/ 45 degree line 
# because one lag is one month, a year is 12 lag
plot(x=zlag(unemp, 12),y=unemp ,xlab="Previous Year's Unemployment Rate", ylab="Unemployment Rate")
abline(0,1)
# # create dataset to verify results
# lagged_unemp <- zlag(unemp, 12)  # unemployment rate 12 months ago
# 
# # Combine into a dataset
# df <- data.frame(
#   prev_year = lagged_unemp,
#   current   = unemp
# )
# plot unemployment rate by unemployment rate 2 years ago w/ 45 degree line 
# because one lag is one month, two years is 24 lag
plot(x=zlag(unemp, 24),y=unemp ,xlab="Unemployment rate 2 years ago", ylab="Unemployment Rate")
abline(0,1)
# # create dataset to verify results
# lagged_unemp_b <- zlag(unemp, 24)  # unemployment rate 24 months ago
# 
# # Combine into a dataset
# df_b <- data.frame(
#   prev_year = lagged_unemp_b,
#    current   = unemp
#  )
###### FORECAST NEXT MONTH WILL BE MOST TRUSTWORTHY BECAUSE THIS HAS THE HIGHEST CORRELATION #######


##### PROBLEM 2 ######
# Plot the sun spots data and plot lags 1 -15 vs. original data #
# Observe that the correlation is positive for neighboring time points, 
# negative when time points are about 5 lags apart, and then positive again
# when time points are about 10 points apart. Does this provide support
# for or against a sunspot cycle. If it provides evidence for a cycle, then
# what is the period of a cycle. That is, how many years does it take to
# go through 1 cycle? For more on periodicity see the section on spectral
# density estimation in our book.
data(spots)
spots
plot(spots, type = "o")
# Observe that the correlation is positive for neighboring time points,
plot(x=zlag(spots, 1),y=spots ,xlab="Sun Spots lag 1", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 2),y=spots ,xlab="Sun Spots lag 2", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 3),y=spots ,xlab="Sun Spots lag 3", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 4),y=spots ,xlab="Sun Spots lag 4", ylab="Sun Spots")
abline(150, -1)
# negative when time points are about 5 lags apart
plot(x=zlag(spots, 5),y=spots ,xlab="Sun Spots lag 5", ylab="Sun Spots")
abline(150,-1)
plot(x=zlag(spots, 6),y=spots ,xlab="Sun Spots lag 6", ylab="Sun Spots")
abline(150,-1)
plot(x=zlag(spots, 7),y=spots ,xlab="Sun Spots lag 7", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 8),y=spots ,xlab="Sun Spots lag 8", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 9),y=spots ,xlab="Sun Spots lag 9", ylab="Sun Spots")
abline(0,1)
# then positive again when time points are about 10 points apart
plot(x=zlag(spots, 10),y=spots ,xlab="Sun Spots lag 10", ylab="Sun Spots")
abline(0,1)
cor(zlag(spots, 10), spots, use="complete.obs")
plot(x=zlag(spots, 11),y=spots ,xlab="Sun Spots lag 11", ylab="Sun Spots")
abline(0,1)
cor(zlag(spots, 11), spots, use="complete.obs")
plot(x=zlag(spots, 12),y=spots ,xlab="Sun Spots lag 12", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 13),y=spots ,xlab="Sun Spots lag 13", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 14),y=spots ,xlab="Sun Spots lag 14", ylab="Sun Spots")
abline(0,1)
plot(x=zlag(spots, 15),y=spots ,xlab="Sun Spots lag 15", ylab="Sun Spots")
abline(0,1)
# yes there is a seasonal trend
# the data spans from 1945 - 2005, and has data points of one per year
# in contrast to the beer sales, the cyclical nature is about every 10 years opposed to every 12 months with beer sales

###### PROBLEM 3 ########
# Find a time series data set that you are interested in and describe it in a
# sentence or two. Construct some plots (you only need to hand in 2 or 3 plots
# but can discuss more) to determine if
  # (a) there is seasonality and
  # (b) if data are independent across time points.

# Read the data
zillow <- read.csv("Metro_mlp_uc_sfrcondo_month.csv")

# Extract just the the monthly values
values <- as.numeric(zillow[1, 6:ncol(zillow)])

# Turn into a time series object
zillow_ts <- ts(values, start = c(2018, 1), frequency = 12)

# Plot
zillow_ts
plot(zillow_ts, ylab = "Median Monthly Home List Price in the U.S.", xlab = "Month", type = "o")

# scatter plots #
plot(x=zlag(zillow_ts, 1),y=zillow_ts ,xlab="Median Monthly Home List Price lag 1", ylab="Median Monthly Home List Price")
abline(0,1)
cor(zlag(zillow_ts, 1), zillow_ts, use="complete.obs")
plot(x=zlag(zillow_ts, 2),y=zillow_ts ,xlab="Median Monthly Home List Price lag 2", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 3),y=zillow_ts ,xlab="Median Monthly Home List Price lag 3", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 4),y=zillow_ts ,xlab="Median Monthly Home List Price lag 4", ylab="Median Monthly Home List Price")
abline(0, 1)
plot(x=zlag(zillow_ts, 5),y=zillow_ts ,xlab="Median Monthly Home List Price lag 5", ylab="Median Monthly Home List Price")
abline(0, 1)
plot(x=zlag(zillow_ts, 6),y=zillow_ts ,xlab="Median Monthly Home List Price lag 6", ylab="Median Monthly Home List Price")
abline(0, 1)
cor(zlag(zillow_ts, 6), zillow_ts, use="complete.obs")
plot(x=zlag(zillow_ts, 7),y=zillow_ts ,xlab="Median Monthly Home List Price lag 7", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 8),y=zillow_ts ,xlab="Median Monthly Home List Price lag 8", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 9),y=zillow_ts ,xlab="Median Monthly Home List Price lag 9", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 10),y=zillow_ts ,xlab="Median Monthly Home List Price lag 10", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 11),y=zillow_ts ,xlab="Median Monthly Home List Price lag 11", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 12),y=zillow_ts ,xlab="Median Monthly Home List Price lag 12", ylab="Median Monthly Home List Price")
abline(0,1)
cor(zlag(zillow_ts, 12), zillow_ts, use="complete.obs")
plot(x=zlag(zillow_ts, 13),y=zillow_ts ,xlab="Median Monthly Home List Price lag 13", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 14),y=zillow_ts ,xlab="Median Monthly Home List Price lag 14", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 15),y=zillow_ts ,xlab="Median Monthly Home List Price lag 15", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 16),y=zillow_ts ,xlab="Median Monthly Home List Price lag 16", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 17),y=zillow_ts ,xlab="Median Monthly Home List Price lag 17", ylab="Median Monthly Home List Price")
abline(0,1)
plot(x=zlag(zillow_ts, 18),y=zillow_ts ,xlab="Median Monthly Home List Price lag 18", ylab="Median Monthly Home List Price")
abline(0,1)
cor(zlag(zillow_ts, 18), zillow_ts, use="complete.obs")

# syntax for submission
plot(zillow_ts, ylab = "Median Monthly Home List Price in the U.S.", xlab = "Month", type = "o")
plot(x=zlag(zillow_ts, 1),y=zillow_ts ,xlab="Median Monthly Home List Price lag 1", ylab="Median Monthly Home List Price")
abline(0,1)
cor(zlag(zillow_ts, 1), zillow_ts, use="complete.obs")
plot(x=zlag(zillow_ts, 12),y=zillow_ts ,xlab="Median Monthly Home List Price lag 12", ylab="Median Monthly Home List Price")
abline(0,1)
cor(zlag(zillow_ts, 12), zillow_ts, use="complete.obs")

