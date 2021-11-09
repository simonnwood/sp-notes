x <- nhtemp
a <- 1.2; b <- .6 ## gamma prior shape and scale
c <- 50; d <- 100 ## normal prior mean and variance

## check data and priors...

par(mfrow=c(1,3),mar=c(4,4,1,1))
hist(x,main="")
tau <- seq(0,10,length=400)
plot(tau,dgamma(tau,a,b),type="l")
mu <- seq(0,100,length=400)
plot(mu,dnorm(mu,c,d^.5),type="l")

## Gibbs loop..

ns <- 10000 ## number of samples
th <- matrix(0,2,ns) ## sample storage
mu <- 0;tau <- 0 ## initial states
n <- length(x) 
dn <- d*n;dnx <- dn*mean(x) ## store constants needed repeatedly
for (i in 1:ns) { ## Gibbs loop
  mu <- rnorm(1,(dnx*tau+c)/(dn*tau+1),sqrt(d/(dn*tau+1)))
  tau <- rgamma(1,n/2+a,sum((x-mu)^2)/2 + b)
  th[,i] <- c(mu,1/sqrt(tau)) ## store as mean and sd
}

## trace plots..

par(mfrow=c(2,1),mar=c(4,4,1,1))
## see ?plotmath for adding maths to plots
plot(th[1,],type="l",ylab=expression(mu))
plot(th[2,],type="l",ylab=expression(sigma))

## acfs...

par(mfrow=c(1,2))
acf(th[1,])
acf(th[2,])


## check posteriors against priors

par(mfrow=c(1,2))
hist(th[1,],xlab=expression(mu),main="",probability=TRUE)
mu <- seq(50,52,length=100)
lines(mu,dnorm(mu,c,d^.5),col=2)

hist(1/th[2,]^2,xlab=expression(tau),main="",probability=TRUE)
tau <- seq(0.1,1.2,length=100)
lines(tau,dgamma(tau,a,b),col=2)

## posterior mean and CI...

pm <- rowMeans(th)
names(pm) <- c("mu","sigma")
pm ## posterior mean

ci <- apply(th,1,quantile,probs=c(.025,.975))
colnames(ci) <- c("mu","sigma")
ci ## confidence intervals
