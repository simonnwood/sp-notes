## nhtemp

ll <- function(theta,temp) {
## log likelihood for model (T_i - mu)/sig ~ t_df
  mu <- theta[1];sig <- exp(theta[2]); df = 1 + exp(theta[3])
  sum(dt((temp-mu)/sig,df=df,log=TRUE) - log(sig)) 
}


ns <- 10000
th <- matrix(0,3,ns)
th[,1] <- th[,ns]
llth <- ll(th[,1],nhtemp)
lprior.th <- dnorm(th[3,1],mean=3,sd=2,log=TRUE)
p.sd <- c(.1,.1,.1)
accept <- 0
for (i in 2:ns) { ## Metropolis Hastings iteration
  thp <- th[,i-1] + rnorm(3)*p.sd ## proposal
  lprior.p <- dnorm(thp[3],mean=3,sd=2,log=TRUE) 
  llp <- ll(thp,nhtemp) ## proposal log lik
  if (runif(1) < exp(llp + lprior.p - llth - lprior.th)) {
    th[,i] <- thp;llth <- llp;lprior.th <- lprior.p
    accept <- accept  + 1
  } else {
    th[,i] <- th[,i-1]
  }
}
accept/ns


par(mfrow=c(3,1),mar=c(4,4,2,2))
plot(th[1,],type="l")
plot(th[2,],type="l")
plot(th[3,],type="l")

par(mfrow=c(2,3))
acf(th[1,]);acf(th[2,]);acf(th[3,]);
hist(th[1,]);hist(exp(th[2,]));hist(th[3,]);

plot(nhtemp)
## straight line increase in t


ll2 <- function(theta,temp) {
## log likelihood when mu increases linearly with year
  n <- length(temp)
  mu <- (1:n-n/2)*theta[2] + theta[1]
  sig <- exp(theta[3]); df = 1 + exp(theta[4])
  sum(dt((temp-mu)/sig,df=df,log=TRUE) - log(sig)) 
}


ns <- 100000
th <- matrix(0,4,ns)
th[,1] <- c(mean(nhtemp),0,log(sd(nhtemp)),log(6))
llth <- ll2(th[,1],nhtemp)
lprior.th <- dnorm(th[4,1],mean=3,sd=2,log=TRUE)
p.sd <- c(.2,.2,.3,4)/5
accept <- 0
for (i in 2:ns) {
  thp <- th[,i-1] + rnorm(4)*p.sd
  lprior.p <- dnorm(thp[4],mean=3,sd=2,log=TRUE) 
  llp <- ll2(thp,nhtemp)
  if (runif(1) < exp(llp + lprior.p - llth - lprior.th)) {
    th[,i] <- thp;llth <- llp;lprior.th <- lprior.p
    accept <- accept  + 1
  } else {
    th[,i] <- th[,i-1]
  }
}
accept/ns


par(mfrow=c(2,2),mar=c(4,4,1,1))
plot(th[1,],type="l")
plot(th[2,],type="l")
plot(th[3,],type="l")
plot(th[4,],type="l")

hist(th[1,]);hist(th[2,]);
hist(exp(th[3,]));hist(th[4,]);

## CI for increase in temperature per year
quantile(th[2,],c(.025,.5,.975))


