## nhtemp

ll <- function(theta,temp) { 
  mu <- theta[1];sig <- exp(theta[2]); df = 1 + exp(theta[3])
  sum(dt((temp-mu)/sig,df=df,log=TRUE) - log(sig)) 
}

nll <- function(theta,temp) -ll(theta,temp)

theta0 <- c(mean(nhtemp),log(sd(nhtemp)),log(6))
#theta0 <- rep(0,3)
fit <- optim(theta0,nll,method="BFGS",temp=nhtemp,hessian=TRUE)
fit
theta <- fit$par
sd <- diag(solve(fit$hessian))^.5
ci <- rbind(theta+2*sd,theta-2*sd)
ci[,2:3] <- exp(ci[,2:3])  

ns <- 10000
th <- matrix(0,3,ns)
th[,1] <- theta0
llth <- ll(theta0,nhtemp)
lprior.th <- dnorm(th[3],mean=3,sd=2,log=TRUE)
p.sd <- c(.5,.1,1.2)
accept <- 0
for (i in 2:ns) {
  thp <- th[,i-1] + rnorm(3)*p.sd
  lprior.p <- dnorm(thp[3],mean=3,sd=2,log=TRUE) 
  llp <- ll(thp,nhtemp)
  if (runif(1) < exp(llp + lprior.p - llth - lprior.th)) {
    th[,i] <- thp;llth <- llp;lprior.th <- lprior.p
    accept <- accept  + 1
  } else {
    th[,i] <- th[,i-1]
  }
}
accept/ns

setwd("~sw283/lnotes/StatProg/MH")
ps <- TRUE
if (ps) postscript("chains.eps")
par(mfrow=c(3,1),mar=c(4,4,1,1))
plot(th[1,],type="l")
plot(th[2,],type="l")
plot(th[3,],type="l")
if (ps) dev.off()

if (ps) postscript("acf-hist.eps")
par(mfrow=c(2,3))
acf(th[1,]);acf(th[2,]);acf(th[3,]);
hist(th[1,]);hist(exp(th[2,]));hist(th[3,]);
if (ps) dev.off()

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

