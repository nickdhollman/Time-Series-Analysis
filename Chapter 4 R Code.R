curve(-x/(1+x^2), xlim=c(-1, 1), lwd=3, ylab = expression(rho[1]),
      xlab = expression(theta), main = "Corr. for MA(1)")

#### First order MA 
library(TSA)
x = arima.sim(list(ma = .9), n=500) # Yt = et + 0.9et-1
y = arima.sim(list(ma = -.9), n=500) # Yt = et - 0.9et-1
par(mfrow = c(2,1))
plot(x)
plot(y)
acf(x) # this is generating r - sample correlation 
acf(y) 
# MA(1) behavior is observed for both plots because r1 for both plots is significant, with no other 
# correlations significant beyond lag 1
ARMAacf(ma = .9) # moving average model with theta = 0.9 (this generates the theoretical rho, not the sample correlation
#### theoretical rho is what we "should" see in our data if our data follows moving average process)
ARMAacf(ma = -.9) # moving average model with theta = -0.9
plot(data.frame(x, zlag(x,1), zlag(x,2), zlag(x,3)))
plot(data.frame(y, zlag(y,1), zlag(y,2), zlag(y,3)))

#### Second order MA
library(TSA)
x = arima.sim(list(ma = c(-1, .8)), n=500) # this is Yt = et - et-1 + 0.8et-2 (-1 represents the -1 * et-1)
y = arima.sim(list(ma = c(0,-.9)), n=500) # Yt = et + 0 * et-1 - 0.9et-2 = et - 0.9et-2
par(mfrow = c(2,1))
plot(x)
plot(y)
acf(x)
acf(y)
# MA(2) behavior is observed for both plots because r2 (sample correlation) for both plots is significant, with no other 
# correlations significant beyond lag 2
ARMAacf(ma = c(-1, .8)) # calculates theoretical values for rho 1 
ARMAacf(ma = c(0,-.9))
# how to read first row of plot: 1 row, 2nd col: t vs t-1 correlation (lag 1), 1 row, 3rd col: t vs t-2 correlation (lag 2), 1 row, 4th col: t vs t-3 correlation (lag 3)
# how to read second row of plot: 2 row, 3rd col: t-1 vs t-2 correlation (lag 1), 2 row, 4th col: t-1 vs t-3 correlation (lag 2)
plot(data.frame(x, zlag(x,1), zlag(x,2), zlag(x,3))) # correlations for first model 
plot(data.frame(y, zlag(y,1), zlag(y,2), zlag(y,3))) # correlations for 2nd model 


#### First Order Auto-regressive (AR) Process
x<-arima.sim(list(ar = .9), n=500) # Yt = 0.9*Yt-1 + et
y<-arima.sim(list(ar = -.9), n=500) # Yt = -0.9*Yt-1 + et
par(mfrow = c(2,1))
plot(x)
plot(y)
acf(x) # behavior of AR model is correlation at lag 1 = 0.9, correlation at lag 2 = 0.9^2 = 0.81, and so on...
acf(y)
ARMAacf(ar = 0.9, lag.max = 5) # these are the theoretical rho values, whereas acf above is sample correlation (r)
ARMAacf(ar = -.9, lag.max = 5)

#### Second Order Auto-regressive (AR) process
data(spots)
par(mfrow = c(2,1))
plot(spots)
acf(arima.sim(list(ar = c(1.4,-.8)), n = 100), 15) # this is specifying AR(2) model, where Yt = 1.4*Yt-1 - 0.8*Yt-2 + et
acf(spots,15)
ARMAacf(ar = c(1.4,-.8), lag.max = 15)

## Canadian hare data
data(hare)
par(mfrow = c(2,1))
acf(arima.sim(list(ar = c(1.3,-.8)), n = 100), 15) # Yt = 1.3*Yt-1 - 0.8*Yt-2 + et
acf (hare, 15)
ARMAacf (ar = c(1.3,-.8), lag.max = 15)


#### ARMA Model Process
par(mfrow = c(1,1))
acf(arima.sim(list(ar = .9, ma = 0), n =1000)) # Yt = 0.9*Yt-1 + et - 0*et-1 = Yt = 0.9*Yt-1
acf(arima.sim(list(ar = .9, ma = -.2), n =1000)) # Yt = 0.9*Yt-1 + et - (-0.2)*et-1 = Yt = 0.9*Yt-1 + et + 0.2*et-1
acf(arima.sim(list(ar = .9, ma = -.5), n =1000)) # Yt = 0.9*Yt-1 + et - (-0.5)*et-1 = Yt = 0.9*Yt-1 + et + 0.5*et-1
