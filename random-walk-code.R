#simulating random number 
set.seed(2)
whit <- rnorm(100)
acf(rnorm(100))

#or 
rnorm(10, mean = 70, sd = 5)

# simlate a random walk
x <- w <- rnorm(1000)
for (t in 2:1000)
  x[t] <- x[t-1]+w[t]
plot(x,type="l")

#check the autocorrelation
acf(x)


#   simulate random walk wth drift
for (t in 2:1000)
  x[t] <- 0.5+ x[t-1]+w[t]
plot(x,type="l")


#ABC company has IPO'd!
#On average it gains 1.001 times its opening price during the trading day,
#but that can vary by a standard deviation of 0.005 on any given day
#(this is its volatility). We can simulate a single sample path for ABC compnay
#by taking the cumulative product from a Normal distribution with a mean 
#of 1.001 and a sd of 0.005. Assuming ABC compnay opens at $20/per share.
#here is a sample path for 200 days of ABC trading.

days <- 200
changes <- rnorm(200,mean=1.001,sd=0.005)
plot(cumprod(c(20,changes)),type='l',ylab="Price",xlab="day",main=" ABC closing price 
     (sample path)")


runs <- 100
#simulates future movements and returns the closing price on day 200
generate.path <- function(){
  days <- 200
  changes <- rnorm(200,mean=1.001,sd=0.005)
  sample.path <- cumprod(c(20,changes))
  closing.price <- sample.path[days+1] #+1 because we add the opening price
  return(closing.price)
}

mc.closing <- replicate(runs, generate.path())

ts.plot(mc.closing)
#The median price of ABC at the end of 200 days is simply median(mc.closing).
# but we can also see the upper and lower 95th percentiles 

quantile(mc.closing, 0.95)
quantile(mc.closing, 0.05) 







