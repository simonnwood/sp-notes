## Parametric bootstrap
setwd("~sw283/lnotes/StatProg/sp-notes")
set.seed(7)

mu <- mean(nhtemp)  ## mean temperature
sd <- sd(nhtemp)    ## standard dev temp
n <- length(nhtemp) ## number of data 
nb <- 10000         ## number of bootstrap samples
sdb <- mub <- rep(0,nb) ## vectors for replicate estimates
for (i in 1:nb) { ## parametric bootstrap loop
  yb <- rnorm(n,mu,sd)  ## generate replicate data
  mub[i] <- mean(yb)    ## estimate mean
  sdb[i] <- sd(yb)      ## and standard deviation
}
ps <- FALSE
if (ps) postscript("nht-para-bs.eps",height=2,width=8)
par(mfrow=c(1,3),mar=c(4,4,1,1))
hist(nhtemp,main="") ## plot data
hist(mub,breaks=20,freq=FALSE,xlab=expression(hat(mu)),main="")
lines(density(mub)) ## distribution of estimated mean
hist(sdb,breaks=20,freq=FALSE,xlab=expression(hat(sigma)),main="")
lines(density(sdb)) ## distribution of estimated sd
if (ps) dev.off()

## check SD
c(sd(mub),sd(nhtemp)/sqrt(n))

quantile(mub,c(.025,.975))
quantile(sdb,c(.025,.975))

## non-parametric boostrap

nb <- 10000; n <- length(nhtemp)
iqb <- rep(0,nb)
for (i in 1:nb) {
  yb <- sample(nhtemp,n,replace=TRUE)
  iqb[i] <- diff(quantile(yb,c(.25,.75)))
}


ps <- FALSE
if (ps) postscript("nht-np-bs.eps",height=3,width=4)
par(mar=c(4,4,1,1))
hist(iqb,main="",xlab="interquartile range",freq=FALSE);
lines(density(iqb,adjust=2))
if (ps) dev.off()

iq <- diff(quantile(nhtemp,c(.25,.75))) ## original iqr
pci <- quantile(iqb,c(.025,.975)) ## 0.25 and 0.975 quantiles of b.s. irq
names(iq) <- names(iq) <- NULL ## avoid confusing names
b <- iq - pci[1]; c <- pci[2] - iq ## get upper and lower interval margins
c(iq-c,iq+b)  ## CI
pci  ## equivalent percentile CI


## multivariate data...

nb <- 10000; n <- nrow(iris)
pvb <- rep(0,nb)
for (i in 1:nb) {
  ii <- sample(1:n,n,replace=TRUE) ## bootstrap resample indices
  V <- cov(iris[ii,1:4]) ## bootstrap cov matrix
  ev <- eigen(V,symmetric=TRUE,only.values=TRUE)$values
  pvb[i] <- sum(ev[1:2])/sum(ev) ## prop var explained by PCA 1 & 2
}

ps <- FALSE
if (ps) postscript("pca-bs.eps",height=3,width=4)
par(mar=c(4,4,1,1))
hist(pvb,main="",xlab="proportion variance explained",freq=FALSE,breaks=20);
lines(density(pvb,adjust=1))
if (ps) dev.off()

## Metropolis Hastings...

ll <- function(theta,temp) { ## log of pi(T|theta)
  mu <- theta[1];sig <- exp(theta[2]) 
  df = 1 + exp(theta[3])
  sum(dt((temp-mu)/sig,df=df,log=TRUE) - log(sig)) ## log likelihood
}

ns <- 10000; th <- matrix(0,3,ns)
th[,1] <- c(mean(nhtemp),log(sd(nhtemp)),log(6)) ## initial parameters
llth <- ll(th[,1],nhtemp)   ## initial log likelihood
lprior.th <- dnorm(th[3,1],mean=3,sd=2,log=TRUE) ## initial prior
p.sd <- c(.5,.1,1.2) ## proposal SD (tuned)
accept <- 0 ## acceptance counter
for (i in 2:ns) { ## MH sampler loop
  thp <- th[,i-1] + rnorm(3)*p.sd ## proposal
  lprior.p <- dnorm(thp[3],mean=3,sd=2,log=TRUE) ## prior for proposal
  llp <- ll(thp,nhtemp) ## log lik of proposal
  if (runif(1) < exp(llp + lprior.p - llth - lprior.th)) { ## accept with MH acceptance prob
    th[,i] <- thp;llth <- llp;lprior.th <- lprior.p ## updating parameters and corresponding probs
    accept <- accept  + 1
  } else { ## reject 
    th[,i] <- th[,i-1] ## leave chain at previous value
  }
}
accept/ns ## about 1/4 is ideal

## check chains...
par(mfrow=c(3,1),mar=c(4,4,1,1))
plot(th[1,],type="l") ## mu
plot(th[2,],type="l") ## log sigma
plot(th[3,],type="l") ## nu - the DoF of t

par(mfrow=c(2,3))
acf(th[1,]);acf(th[2,]);acf(th[3,]);
hist(th[1,]);hist(exp(th[2,]));hist(th[3,]);

pm <- rowMeans(th) ## posterior mean

## transform to original scale...
pm[2:3] <- exp(pm[2:3])
pm[3] <- pm[3] + 1
names(pm) <- c("mu","sig","df")
pm
 
## 95% Credible Intervals...
ci <- apply(th,1,quantile,prob=c(.025,.975))
ci[,2:3] <- exp(ci[,2:3]) ;ci[,3] <- ci[,3]+1
colnames(ci) <- c("mu","sig","df")
ci

## Simple Gibbs...

## check priors and data...
x <- nhtemp ## just to save later typing
a <- 1.2; b <- .6 ## gamma prior shape and scale
c <- 50; d <- 100 ## normal prior mean and variance
## check data and priors...
par(mfrow=c(1,3),mar=c(4,4,1,1))
hist(x,main="")
tau <- seq(0,10,length=400)
plot(tau,dgamma(tau,a,b),type="l")
mu <- seq(0,100,length=400)
plot(mu,dnorm(mu,c,d^.5),type="l")

## Actual sampler...

ns <- 10000 ## number of samples
th <- matrix(0,2,ns) ## sample storage
mu <- 0;tau <- 0 ## initial states
n <- length(x) 
## store constants needed repeatedly...
dn <- d*n;dnx <- dn*mean(x) 

for (i in 1:ns) { ## Gibbs loop
  ## update mu | tau, x ... 
  mu <- rnorm(1,(dnx*tau+c)/(dn*tau+1),sqrt(d/(dn*tau+1)))
  ## update tau | mu, x ... 
  tau <- rgamma(1,n/2+a,sum((x-mu)^2)/2 + b)
  
  th[,i] <- c(mu,1/sqrt(tau)) ## store as mean and sd
}
## Trace plots...
par(mfrow=c(2,1))
## see ?plotmath for adding maths to plots
plot(th[1,],type="l",ylab=expression(mu))
plot(th[2,],type="l",ylab=expression(sigma))

par(mfrow=c(1,2))
acf(th[1,]);acf(th[2,])

## do the data matter....

hist(th[1,],xlab=expression(mu),main="",probability=TRUE) ## posterior for mu
mu <- seq(50,52,length=100)
lines(mu,dnorm(mu,c,d^.5),col=2)  ## prior for mu
hist(1/th[2,]^2,xlab=expression(tau),main="",probability=TRUE) ## posterior for tau
tau <- seq(0.1,1.2,length=100) 
lines(tau,dgamma(tau,a,b),col=2) ## prior for tau = 1/sigma^2
