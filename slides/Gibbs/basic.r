library(rjags)
setwd("~sw283/lnotes/StatProg/2020/Gibbs")
setwd("~sw283/lnotes/StatProg/sp-notes")

mod <- jags.model("basic.jags",data=list(x=nhtemp,N=length(nhtemp)))

sam <- jags.samples(mod,c("mu","tau"),n.iter=10000)
str(sam)

sam.coda <- coda.samples(mod,c("mu","tau"),n.iter=10000)
str(sam.coda)

ps <- TRUE
if (ps) postscript("coda-trace.eps",height=6)
plot(sam.coda)
if (ps) dev.off()

if (ps) postscript("coda-acf.eps",height=4)
acfplot(sam.coda)
if (ps) dev.off()

autocorr(sam.coda)
crosscorr(sam.coda)
effectiveSize(sam.coda)
HPDinterval(sam.coda[[1]])
apply(sam.coda[[1]],2,quantile,prob=(c(0.025,.975)))

N <- length(nhtemp)
mod2 <- jags.model("basic2.jags",data=list(x=nhtemp,t=1:N-N/2,N=N))

#sam2 <- jags.samples(mod2,c("alpha","beta","tau","df"),n.iter=10000)

sam2.coda <- coda.samples(mod2,c("alpha","beta","tau","df"),n.iter=10000)
if (ps) postscript("nht-trace2.eps")
plot(sam3.coda)
if (ps) dev.off()

autocorr(sam2.coda)
crosscorr(sam2.coda)
if (ps) postscript("nht-acf2.eps",height=3.5)
acfplot(sam2.coda,aspect=1)
if (ps) dev.off()

effectiveSize(sam2.coda)
HPDinterval(sam2.coda)
apply(sam2.coda[[1]],2,quantile,prob=(c(0.025,.975)))

## model comparison

mod <- jags.model("basic.jags",data=list(x=nhtemp,N=length(nhtemp)),n.chains=2)
mod2 <- jags.model("basic2.jags",data=list(x=nhtemp,t=1:N,N=N),n.chains=2)

dic.samples(mod,n.iter=10000)
dic.samples(mod2,n.iter=10000)

