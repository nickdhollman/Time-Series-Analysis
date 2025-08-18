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
