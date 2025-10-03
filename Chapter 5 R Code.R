library(TSA)
data(oil.price)
data(wages)
DJIA = ts(read.table("Data/DJIA.txt")[[2]],frequency = 12,
          start = c(1896, 6))
plot (DJIA)
plot(oil.price)
plot(wages)
# all the above or non-stationary 
# because they are non-stationary we can't use ARMA, but maybe we can apply transformations to make the data stationary 

# Plot the difference of the data sets above. Do the differences look stationary?
plot(diff(DJIA))
plot(diff(oil.price))
plot(diff(wages))
# these models look a lot better and more stationary than before, though not perfect, we could try and then use ARMA model
# especially would be concerned about the DJIA data 

#plot the returns for DJIA, oil.price, and wages data. Do the returns look stationary?
plot(diff(log(DJIA)))
plot(diff(log(oil.price)))
plot(diff(log(wages)))
# these look even better than just doing the difference, we can feel good trying ARMA model with these that assumes
# staionarity 

data(ima22.s)
plot(ima22.s, type="o")
w <- diff(ima22.s)
plot(w, type="o")
# looks slighltly better, still non-stationary 
w2 <- diff(w)
plot(w2, type="o")
# this plot is much better

# below is simulating IMA(2,2) to show that it is unstable and can go up or down 
y = arima.sim(n=100, model=list(order=c(0,2,2), ma=c(1, -0.6))) # this is IMA(2, 2)
plot(y, type="o")

# page 14 of Chapter 5 pdf
data("electricity")
plot(electricity)
# mean and variance increase with time 

x<-seq(-.2,.2,.01)
plot(x, log(1+x))
abline(0,1)

# Consider the DJIA. Do the returns look stationary?
returns = diff(log(DJIA))
plot(returns)
# Yes (mostly) - still some heteroscedasticity (extreme shocks) - this can be modelled by GARCH models in model 12 

plot(electricity)
BoxCox.ar(electricity) # we are wanting the max point (maximum likelihood estimate) of lambda which is the highest
# point on the y-axis for log likelihood
# 0 would be the most convenient lambda value
data(hare)
BoxCox.ar(hare)
# 1/2 would be the most convenient lambda value

# page 15 of Chapter 5 pdf
BoxCox.ar (unemp)
BoxCox.ar(DJIA)
BoxCox.ar(DJIA, lambda = seq(-1,3,.01))
data(hare)
BoxCox.ar(hare)

