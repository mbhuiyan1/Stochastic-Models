# simulating brownian motion
alpha=0
sigma = 1 
T = 50
n = 4
x0 = 0.1 # initial value of your stock
dt= T/n
t=seq(0,T,by=dt)
x=c(x0, mu * dt + sigma*sqrt(dt)*rnorm(n, mean=0, sd=1))
xt=cumsum(x)
plot(t,xt,type="l",xlab="time", ylab="X(t)")


# prediction
(price = xt[5])
mean(xt)
quantile(xt, 0.95)
quantile(xt, 0.05) 



# simulating Geometric Brownian motion
# library(sde)
mu=0.16; 
sigma=0.2; 
P0=40;  # initial value of your stock
T = 10 # 10 days
nt=50; # number of runs
n=25 # number of time points
##Generate nt trajectories
dt=T/n; t=seq(0,T,by=dt)
X=matrix(rep(0,length(t)*nt), nrow=nt) # columns= number of time points and rows= # number of runs
for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T,N=n)}
##Plot
ymax=max(X); ymin=min(X) #bounds for simulated prices
plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1, ylab="Price P(t)",xlab="time t")
for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}


# plot 1 run for n time points
plot(t, X[1,],t='l',ylim=c(ymin, ymax), col=1, ylab="Price P(t)",xlab="time t")

# prediction
X # all the simulaiton
mean(X[, 26]) # mean of prices for multiple runs
quantile(X[, 26], 0.95)
quantile(X[, 26], 0.05) 



