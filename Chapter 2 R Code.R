x = rnorm(1000)
y = rnorm(1000)
y2 = .8*x + sqrt(1-.8^2)*y
hist(x)
hist(y)
hist (y2)
plot(x,y)
plot(x,y2)

par(mfrow = c(1,2))
plot(x,y)
abline(h = 0, v = 0)
plot(x,y2)
abline(h = 0, v = 0)

e = rnorm(20)
y = cumsum(e)
plot(y, ylim = c(-10,10), type = "b")

while(T){plot(cumsum(rnorm(20)), ylim = c(-10,10), type = "b")
  readline()
  }
# hit escape in console to stop loop
y = arima.sim(list(ma = 0.5), n = 100)
plot(y)

