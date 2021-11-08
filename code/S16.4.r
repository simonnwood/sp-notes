library(rjags)                ## load rjags (and coda)
setwd("~sw283/lnotes/StatProg/sp-notes/code")  ## set working directory
## compile model...
mod <- jags.model("basic.jags",data=list(x=nhtemp,N=length(nhtemp)))
sam <- jags.samples(mod,c("mu","tau"),n.iter=10000)
str(sam)
sam.coda <- coda.samples(mod,c("mu","tau"),n.iter=10000)
str(sam.coda)
plot(sam.coda)
acfplot(sam.coda)
crosscorr(sam.coda)
effectiveSize(sam.coda)
HPDinterval(sam.coda[[1]])
apply(sam.coda[[1]],2,quantile,prob=(c(0.025,.975)))

## less basic model...
N <- length(nhtemp)
mod2 <- jags.model("basic2.jags",data=list(x=nhtemp,t=1:N,N=N))
sam2.coda <- coda.samples(mod2,c("alpha","beta","tau","df"),n.iter=10000)
plot(sam2.coda)
plot(sam2.coda[[1]][,1:2])
effectiveSize(sam2.coda)
crosscorr(sam2.coda)
acfplot(sam2.coda,aspect=1)

mod2 <- jags.model("basic2.jags",data=list(x=nhtemp,t=1:N-N/2,N=N))
sam2.coda <- coda.samples(mod2,c("alpha","beta","tau","df"),n.iter=10000)
plot(sam2.coda)
effectiveSize(sam2.coda)
crosscorr(sam2.coda)
acfplot(sam2.coda,aspect=1)

## DIC...
mod <- jags.model("basic.jags",data=list(x=nhtemp,N=length(nhtemp)),n.chains=2)
mod2 <- jags.model("basic2.jags",data=list(x=nhtemp,t=1:N,N=N),n.chains=2)
dic.samples(mod,n.iter=10000)
dic.samples(mod2,n.iter=10000)
