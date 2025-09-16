library(TSA)

# Create the vector of daily returns
DJIA_returns <- c(0.2, -0.2, 0.4, -0.9, -0.3, -0.5, 0.0, 1.3, 0.2, -0.8, -0.6, 0.6, 0.2, 1.0, -0.6, 0.6, 0.1, -1.0, -0.4, 0.3,
                  -2.0, 0.1, 0.8, -0.2, 0, 0, 0.5, -1.5, 0.2, 1.2, 1.2, 0.4, 0.0, 0.4, -0.2, 0.1, -1.0, -0.4, -0.7, -0.9, -0.7,
                  1.1, 0.7, 0.0, -0.3, -0.5, -0.3, -0.1, -0.7, -0.3, 1.4, -1.2, 0.0, 0.0, 0.4, 0.4, -0.2, -0.9, -2.1, -3.1, -3.6,
                  -1.3, 4.0, 2.3, -0.1, -0.7, -2.8, 1.8)

# Create a date sequence from July 1, 2015 to Sept 3, 2015
DJIA_seq <- seq(from = as.Date("2015-07-01"), by = "day", length.out = length(DJIA_returns))

# Create a timeSeries object, frequency = 365 for daily data, format "%j" tells R to extract the day of the year (182 for July 1, 2015)
DJIA_ts <- ts(DJIA_returns, start = c(2015, as.numeric(format(DJIA_seq[1], "%j"))), frequency = 365)

# Convert to data frame
DJIA_df <- data.frame(Date = DJIA_seq, Return = as.numeric(DJIA_ts))

# View result
print(DJIA_ts)
head(DJIA_df)
tail(DJIA_df)

#	Consider the model Yt = µt+Xt where Xt is some zero-mean stationary process and µt is either 
####	Model 1: µt = β0 + β1t 
####	Model 2: µt = µ 
### A.) Get ˆµt for each model and construct a plot with each estimated mean function superimposed. 
plot(DJIA_returns, ylab = "Daily Returns", xlab = "Time")
abline(h = c(mean(DJIA_returns)), lty = 2, lwd = 2)
print(mean(DJIA_returns))
linearmodel = lm(DJIA_returns~time(DJIA_returns))
abline(linearmodel, lwd = 2)
legend("topleft",
       legend = c(expression(hat(mu)[t]~" (constant)"),
                  expression(hat(mu)[t]~" (linear)")),
       lty = c(2,1), lwd = 2, bty = "n")

# B.) Provide four plots (one for each assumption) for each model and use them
# to assess the assumption that Xt (Residuals)
### Constant mean model (model 2) is the intercept only
fit_const <- lm(DJIA_returns ~ 1)

  # i. has mean 0,
plot(y=rstudent(linearmodel), x=as.vector(time(DJIA_returns)), xlab = "Time", ylab="Stand. Resid.", type = "o")
plot(y=rstudent(fit_const), x=as.vector(time(DJIA_returns)), xlab = "Time", ylab="Stand. Resid.", type = "o")

  # ii. has constant variance
plot(y=rstudent(linearmodel), x = as.vector(fitted(linearmodel)), xlab=" mu hat ", ylab="Stand. Resid")
plot(y=rstudent(fit_const), x = as.vector(fitted(fit_const)), xlab=" mu hat ", ylab="Stand. Resid")

  # iii. is normally distributed
hist(rstudent(linearmodel), xlab = "Stand. Residuals")
qqnorm(rstudent(linearmodel))
abline(0,1)
hist(rstudent(fit_const), xlab = "Stand. Residuals")
qqnorm(rstudent(fit_const))
abline(0,1)

  # iv. is white noise.
acf(rstudent(linearmodel))
acf(rstudent(fit_const))

# D.)  Perform a hypothesis test to determine if Model 1 or Model 2 is better.
# Make sure to report a p-value and to assess the validity of the p-value
# based on your answers in problem 1b.
summary(linearmodel)
summary(fit_const)
