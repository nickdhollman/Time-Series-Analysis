library(TSA)
unemp = ts(read.table("Data/unemp.txt"), frequency =1)
#unemp
plot(unemp, ylab = "Monthly Unemp.", xlab = "time", type = "o")
acf(unemp)
y <- unemp
print(acf(y))


