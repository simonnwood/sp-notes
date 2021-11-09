## Solution File.
## Exercises. These are all based on the nhtemp example in lecture 15, the
## code for which is provided below.
## 
## 1. Use optim to fit the model by maximum likelihood estimation, using BFGS
## 2. Compute confidence intervals for the model parameters using
##    large sample MLE results. Hint: use the inverse of the hessian of the
##    negative log likelihood.
## 3. Use bootstrapping to obtain the confidence intervals (this will involve,
##    recomputing the MLEs for each bootstrap resample.)
## 4. Try out the MH loop from different starting values, to see the case
##    in which the chains take some time to converge.
## 5. Modify the code to only store every Mth simulated vector from the chain,
##    where M is an integer that can be set (try it out with M=10, and a larger
##    total number of chain steps, for example)
## 6. Modify the code so that the posterior means are computed on the parameters
##    after transforming back to the mu, sigma, nu scale (rather than taking
##    means and then transforming).

hist(nhtemp)

ll <- function(theta,temp) { 
  mu <- theta[1];sig <- exp(theta[2]); df = 1 + exp(theta[3])
  sum(dt((temp-mu)/sig,df=df,log=TRUE) - log(sig)) 
}

## Q1.
nll <- function(theta,temp) -ll(theta,temp) ## negative log likelihood
th0 <- c(40,.5,.5) ## initial values 
fit <- optim(th0,nll,method="BFGS",temp=nhtemp,hessian=TRUE)
fit

## Q2.
th.hat <- fit$par
th.sd <- diag(solve(fit$hessian))^.5
ci <- rbind(th.hat-1.96*th.sd,th.hat+1.96*th.sd)
ci[,2:3] <- exp(ci[,2:3]);ci[,3] <- ci[,3] + 1
colnames(ci) <- c("mu","sigma","df")
rownames(ci) <- c("lower","upper")
ci

## Q3.

nb <- 1000
n <- length(nhtemp)
thb <- matrix(0,3,nb)
for (i in 1:nb) { ## bootstrap loop
  tempb <- sample(nhtemp,nb,replace=TRUE) ## resample
  fb <- optim(fit$par,nll,method="BFGS",temp=tempb) ## fit to resample
  thb[,i] <- fb$par ## extract MLEs
}

## Now compute 95% percentile confidence intervals....
par(mfrow=c(1,3))
for (i in 1:3) hist(thb[i,])
cib <- apply(thb,1,quantile,prob=c(.025,.975))
cib[,2:3] <- exp(cib[,2:3]);cib[,3] <- cib[,3] + 1
colnames(cib) <- c("mu","sigma","df")
cib

## end of Q3

ns <- 10000
th <- matrix(0,3,ns)

## Q4. simply comment out the following line (M=1) to start the run
## a long way from the posterior mode. Plotting the chains then
## shows an initial burn-in period, that would need to be discarded.

th[,1] <- c(mean(nhtemp),log(sd(nhtemp)),log(6))

## Q5. modified loop as below...
thi <- th[,1] ## vector for current chain state
llth <- ll(thi,nhtemp)
lprior.th <- dnorm(thi[3],mean=3,sd=2,log=TRUE)
p.sd <- c(.5,.1,1.2)
accept <- 0;M <- 10
for (i in 2:(ns*M)) {
  thp <- thi + rnorm(3)*p.sd
  lprior.p <- dnorm(thp[3],mean=3,sd=2,log=TRUE) 
  llp <- ll(thp,nhtemp)
  if (runif(1) < exp(llp + lprior.p - llth - lprior.th)) {
    thi <- thp;llth <- llp;lprior.th <- lprior.p
    accept <- accept  + 1
  } ## nothing to do on reject
  if (i%%M == 0) th[,i/M] <- thi ## store current state every M steps
}
accept/(ns*M)


par(mfrow=c(3,1),mar=c(4,4,1,1))
plot(th[1,],type="l")
plot(th[2,],type="l")
plot(th[3,],type="l")


par(mfrow=c(2,3))
acf(th[1,]);acf(th[2,]);acf(th[3,]);
hist(th[1,]);hist(exp(th[2,]));hist(th[3,]);

## 95% Credible Intervals...
ci <- apply(th,1,quantile,prob=c(.025,.975))
ci[,2:3] <- exp(ci[,2:3]) ;ci[,3] <- ci[,3]+1
colnames(ci) <- c("mu","sig","df")
ci

## Q6...

tht  <- th ## copy
tht[2:3,] <- exp(tht[2:3,]) ## transform
tht[3,] <- tht[3,] + 1
pm <- rowMeans(tht) ## posterior mean on transformed scale
names(pm) <- c("mu","sig","df")
pm

