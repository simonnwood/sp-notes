setwd("~sw283/lnotes/StatProg/sp-notes")
## vector manipulation exercise...
## split numbers as single and double digits into single digits
x <- c(10,2,7,89,43,1)
ii <- which(x%/%10 > 0) ## where are the double digits?

xs <- rep(0,length(ii)+length(x)) ## vector to store the single digits
iis <- ii+1:length(ii)-1 ## where should first digit go in xs?
xs[iis] <- x[ii]%/%10 ## insert first digits
xs[-iis] <- x%%10  ## insert the rest

xs

## Suppose we want to do the same with a character vector
x <- c("10","2","7","89","43","1")
ii <- which(nchar(x)>1)
xs <- rep("",length(ii)+length(x)) ## vector to store the single digits
iis <- ii+1:length(ii) ## where should second digit go in xs?
xs[iis] <- substr(x[ii],2,2)
xs[-iis] <- substr(x,1,1)
xs

poem <- "Inside me is a skeleton, of this I have no doubt, now it's got my flesh on, but it's waiting to get out."
pow <- strsplit(poem," ")[[1]]
n.words <- length(pow)
freq <- tabulate(nchar(pow))
ie <- grep("e",pow,fixed=TRUE)
n.e <- length(ie)
ia <- grep("a",pow,fixed=TRUE)
iea <- ia[ia %in% ie]
pow[iea] <- paste(pow[iea],"*",sep="")
paste(pow,collapse=" ")





## basic vector examples

a3d <- c(TRUE,FALSE,FALSE) ## create an example logical vector
class(a3d)   ## its class
typeof(a3d)  ## its type
a3d[2:3]  ## print its 2nd and 3rd elements 
a3d[2] <- TRUE ## change the 2nd element
a3d ## print the resulting vector

bb <- 1:10 ## create numeric example
class(bb) ## check class
bb[c(1,4,9)] <- c(.1,-3,2.2) ## change selected elements
class(bb)   ## note automatic change of class 
typeof(bb)  ## how it is actually being stored
bb[3:8] ## print elements 3:8 

## recycling

a <- 1:4 # a 4-vector 
b <- 5:6 # a 2-vector
a*b      # multiplication with recycling
b <- 5:7 # a 3 vector
a*b      # multiplication with recycling

## array 
a <- array(1:24,c(3,2,4))
d <- dim(a)
a[3+1*d[1]+2*d[1]*d[2]]

B <- matrix(1:6,2,3); B ## create a matrix (filled by col)
B[1,2] <- -1            ## change elment 1,2
a <- c(.3,-1.2,2.3)    ## a 3-vector
B %*% a                 ## matrix multiplication
B*a                     ## element wise multiplication with recycling!!

## list

stuff <- list(a=1:6,txt="furin cleavage site",l2 = function(x) log(x^2),
         more = list(a="er",b=42))
stuff[[1]]
stuff[["l2"]]
stuff$a
stuff[c(1,2)]

## factors

fac <- factor(c("fred","sue","sue","bill","fred"))
class(fac)
fac         ## default printing for class factor 
levels(fac) ## extract the levels attribute
as.numeric(fac)  ## look at the underlying coding




## data.frame

dat <- data.frame(y = c(.3,.7,1.2),x = 1:3,fac = factor(c("a","b","a")))
dat      ## a data.frame
dim(dat) ## like a matrix
dat$fac  ## and lie a list!

## if/loop

if (runif(1)>.5) {
  cat("heads\n")
} else if (runif(1)>.7) {
  cat("tails\n")
} else cat("also tails\n")

n <- 100;p <- rep(.5,n)
for (i in 2:n) p[i] <- 3.7*p[i-1]*(1-p[i-1]) 
plot(p,type="l")

## ...
foo <- function(x,a,b) x/a + b

mat.fun <- function(A,fun=I,...) {
  ea <- eigen(A,symmetric=TRUE)
  ea$vectors %*% (fun(ea$values,...)*t(ea$vectors)) ## note use of re-cycling rule!
}

mat.fun(B,fun=foo,a=2,b=3)

## plot an empirical cdf
set.seed(8)
n <- 100
x <- rgamma(n,shape=3,scale=2)

ecdf <- function(x) {
## function to plot empirical cdf of sample x
  n <- length(x)
  p <- 1:n/n ## create cumulative probability vector 
  xs <- sort(x) ## sort the x values
  ## plot the x, p points
  plot(xs,p,pch=19,cex=.5,xlab="x",ylab=expression(P(X<=x)))
  ## now add in the lines of constant probability between x values
  lines(c(2*xs[1]-xs[n],xs[1]),c(0,0))
  for (i in 1:(n-1)) lines(c(xs[i],xs[i+1]),c(p[i],p[i]))
  lines(c(2*xs[n]-xs[1],xs[n]),c(1,1))
} ## ecdf

## test it
set.seed(8);
x <- rgamma(500,shape=3,scale=2) ## test data
ps <- FALSE
if (ps) postscript("ecdf.eps",width=6,height=3.5)
par(mar=c(4,4,1,1))
ecdf(x)
if (ps) dev.off()
?lines

## so we could avoid the loop
ecdf1 <- function(x) {
## function to plot empirical cdf of sample x
  n <- length(x)
  p <- 1:n/n ## create cumulative probability vector 
  xs <- sort(x) ## sort the x values
  ## plot the x, p points 
  p1 <- x1 <- rep(NA,3*n+2)
  p1[1:n*3+2] <- p1[1:n*3+1] <- p
  x1[1:n*3+1] <- xs
  x1[1:n*3+2] <- c(xs[-1],2*xs[n]-xs[1])
  p1[1:2] <- 0
  x1[1:2] <- c(2*xs[1]-xs[n],xs[1])
  plot(xs,p,pch=19,cex=.5,xlab="x",ylab=expression(P(X<=x)))
  lines(x1,p1)
} ## ecdf1


## why did we avoid the loop?

n <- 10000000
x <- rep(0,n)
system.time(for(i in 1:n) x[i] <- 2-i*1e-5)
system.time(x <- 2-1:n*1e-5)
## even worse...
x <- 0
system.time(for(i in 1:n) x[i] <- 2-i*1e-5)

## simulation
## sample

set.seed(0) ## just so you can reproduce what I got
sample(1:100,10)


## Birthday problem...

n <- 1000000 ## number of classes
bd <- matrix(sample(1:366,30*n,replace=TRUE,prob=c(rep(4,365),1)),n,30) ## birthdays
mean(apply(bd,1,function(x) length(unique(x)))!=30)

## SEIR simulation

seir <- function(n=10000,ni=10,nt=100,gamma=1/3,delta=1/5,bmu=5e-5,bsc=1e-5) {
## SEIR stochastic simulation model.
## n = population size; ni = initially infective; nt = number of days
## gamma = daily prob E -> I; delta = daily prob I -> R;
## bmu = mean beta; bsc = var(beta) = bmu * bsc
  x <- rep(0,n) ## initialize to susceptible state
  beta <- rgamma(n,shape=bmu/bsc,scale=bsc) ## infectivity
  x[1:ni] <- 2 ## create some infectives
  S <- E <- I <- R <- rep(0,nt)
  S[1] <- n-ni;I[1] <- ni
  for (i in 2:nt) { ## loop over days
    u <- runif(n)
    x[x==2&u<delta] <- 3 ## I -> R with prob delta
    x[x==1&u<gamma] <- 2 ## E -> I with prob gamma
    x[x==0&u<beta*I[i-1]] <- 1 ## S -> E with prob beta*I[i-1]
    S[i] <- sum(x==0); E[i] <- sum(x==1)
    I[i] <- sum(x==2); R[i] <- sum(x==3)
  }
  list(S=S,E=E,I=I,R=R,beta=beta)
} ## seir

ps <- TRUE
if (ps) postscript("seir.eps",width=8,height=3.5)

par(mfcol=c(2,3),mar=c(4,4,1,1))
epi <- seir(bmu=7e-5,bsc=1e-7);hist(epi$beta,xlab="beta",main="")
plot(epi$S,ylim=c(0,max(epi$S)),xlab="day",ylab="N")
points(epi$E,col=4);points(epi$I,col=2)

epi <- seir(bmu=7e-5,bsc=7e-5);hist(epi$beta,xlab="beta",main="")
plot(epi$S,ylim=c(0,max(epi$S)),xlab="day",ylab="N")
points(epi$E,col=4);points(epi$I,col=2)

epi <- seir(bmu=4.6e-5,bsc=2e-6);hist(epi$beta,xlab="beta",main="")
plot(epi$S,ylim=c(0,max(epi$S)),xlab="day",ylab="N")
points(epi$E,col=4);points(epi$I,col=2)

if (ps) dev.off()

n.rep <- 10000; n.ci <- 0; n <- 20;p <- .2
for (i in 1:n.rep) {
  x <- rbinom(1,n,p)
  p.hat <- x/n
  sig.p <- sqrt(p.hat*(1-p.hat)/n)
  if (p.hat - 1.96*sig.p <p && p.hat+1.96*sig.p > p) n.ci <- n.ci + 1
}
n.ci/n.rep

## all versus covid UK deaths March 2020-August 2021 ONS, by "5 year" age bands:
## 0-1, 1-5, 5-10, etc, finally 90+


## all.deaths
d <- c(3362,428,290,369,978,1723,2471,3731,5607,7926,12934,20688,29905,40456,53579,84784,106448,136269,150325,178684)

## covid.deaths
cd <- c(3,1,3,10,26,65,133,257,438,740,1418,2587,4250,6337,8528,13388,18622,25220,27718,30247)

par(mfrow=c(1,2))
ind <- 1:10
a <- c(1,1:19*5-1)
c1 <- 1
barplot(rbind(cd,d-cd),names.arg=c(a[-20],""),ylim=c(0,250000),xlab="age")
text(0,200000,"All deaths during pandemic, COVID-19 in black",pos=4,cex=c1)

barplot(rbind(cd[ind],(d-cd)[ind]),names.arg=a[ind],ylim=c(0,10000),xlab="age")
text(0,8000,"All deaths during pandemic, COVID-19 in black",pos=4,cex=c1)

## how to actually show the tiny numbers at low ages
## ?barplot to get width and spacing - crap idea areas distorted.

sfac <- sqrt(cd/d)
for (i in 1:10) {
  x0 <- (i-1)*1.2 + .2
  rect(x0,0,sfac[i]+x0,d[i]*sfac[i],col="grey",border="grey")
}


## Linear models

library(ggplot2)
mpg1 <- data.frame(mpg)
## fl is fuel, but levels are undocumented, so this is unusable
head(mpg1)
mpg1$trans <- factor(gsub("\\(.*\\)","",mpg1$trans)) 
head(model.matrix(cty~trans+displ,mpg1))
head(model.matrix(cty~trans*displ,mpg1))
head(model.matrix(cty~trans+class+displ,mpg1))

m1 <- lm(cty~trans+displ,mpg1)
m1
ps <- TRUE
if (ps) postscript("mpg-check1.eps",width=9,height=7)
par(mfrow=c(2,2))
plot(m1)
if (ps) dev.off()

ps <- TRUE
if (ps) postscript("mpg-resid1.eps",width=5,height=4)
par(mar=c(4,4,1,1))
plot(mpg1$displ,residuals(m1))
if (ps) dev.off()


m2 <- lm(cty~trans+displ+I(displ^2),mpg1)
par(mfrow=c(2,2))
plot(m2)
mpg1[c(213,222,223),]
m3 <- lm(cty~(trans+displ+I(displ^2))^2,mpg1)
m0 <- lm(cty~displ+I(displ^2),mpg1)
anova(m0,m3)

a <- list(a = 1:3, sd = c(.1,.2,.1))
class(a) <- "foo"

print.foo <- function(x) {
  cat(x$a-1.96*x$sd,"\n")
  cat(x$a+1.96*x$sd,"\n")
}

## matrices

n <- 2000
A <- matrix(runif(n*n),n,n)
B <- matrix(runif(n*n),n,n)
y <- runif(n)
system.time(a1 <- A %*% B %*% y)
system.time(a2 <- A %*% (B %*% y))
range(a1-a2)

## Cholesky

n <- 2000
A <- crossprod(matrix(runif(n*n),n,n)) ## example +ve def matrix
b <- runif(n)  ## example n vector
system.time(c0 <- solve(A,b)) ## solve 
system.time({R <- chol(A);    ## solve with cholesky
c1 <- backsolve(R,forwardsolve(t(R),b))})
range(c0-c1)/norm(A) ## compare relative difference

y <- c(3,4.5,2)
S <- matrix(c(1,.5,.3,.5,2,1,.3,1,1.5),3,3)
m <- c(.6,.8,1.2)


ldmvn <- function(y,m,S) { 
## Cholesky based evaluation of log density of MVN at y. 
## m is mean and S the covariance matrix.
  R <- chol(S) ## Get Cholesky factorization R'R = S
  z <- forwardsolve(t(R),y-m) ## R^{-T}(y-m)
  -sum(z^2)/2 - sum(log(diag(R))) - length(m)*log(2*pi)/2
} ## ldmvn

y <- c(3,4.5,2); m <- c(.6,.8,1.2)
S <- matrix(c(1,.5,.3,.5,2,1,.3,1,1.5),3,3)
ldmvn(y,m,S)

## QR
n <- 1000
A <- matrix(runif(n*n),n,n)
x <- runif(n)
b <- A%*%x
x1 <- solve(A,b)
qra <- qr(A)
x2 <- backsolve(qr.R(qra),qr.qty(qra,b))
mean((x1-x)^2)^.5;mean((x2-x)^2)^.5

## lm
X <- model.matrix(~group,PlantGrowth)
qrx <- qr(X); y <- PlantGrowth$weight
p <- ncol(X)
beta <- backsolve(qr.R(qrx),qr.qty(qrx,y)[1:p])


## PCA
setwd("~sw283/lnotes/StatProg/sp-notes")
ps <- FALSE
if (ps) postscript("iris.eps",width=8,height=8)
pairs(iris)
if (ps) dev.off()
X <- sweep(as.matrix(iris[,1:4]),2,colMeans(X1))
ec <- eigen(t(X)%*%X/(nrow(X)-1))
U <- ec$vectors;lambda <- ec$values
Z <- X %*% U;

if (ps) postscript("iris-pca.eps",width=5,height=5)
plot(Z[,1],Z[,2],col=c(1,2,4)[as.numeric(iris$Species)],main="iris PCA",
     xlab="PCA 1",ylab="PCA 2")
if (ps) dev.off()
apply(Z,2,var);lambda
sum(lambda[1:2])/sum(lambda)

## on correlation
X <- sweep(as.matrix(iris[,1:4]),2,colMeans(X1))
X <- sweep(X,2,apply(X,2,sd),FUN="/") ## standardize to unit var

ec <- eigen(t(X)%*%X/(nrow(X)-1))
U <- ec$vectors;lambda <- ec$values
Z <- X %*% U;

plot(Z[,1],Z[,2],col=c(1,2,4)[as.numeric(iris$Species)],main="iris PCA",
     xlab="PCA 1",ylab="PCA 2") ## separation not so clean
sum(lambda[1:2])/sum(lambda) ## less explained by first 2 components



pointless <- function(A,B,y) { 
  C <- A%*%B%*%y  ## compute ABy
  D <- solve(A,B)%*%y  ## compute A^{-1}By
  E <- A + B
  F <- A + t(B)
}
n <- 2000; A <- matrix(runif(n*n),n,n)
B <- matrix(runif(n*n),n,n); y <- runif(n)
## Profile a call to pointless...
Rprof()
pointless(A,B,y)
Rprof(NULL)
summaryRprof()

pointless2 <- function(A,B,y) { 
  C <- A%*%(B%*%y)  ## compute ABy
  D <- solve(A,B%*%y)  ## compute A^{-1}By
  E <- A + B
  F <- A + t(B)
}
Rprof()
pointless2(A,B,y)
Rprof(NULL)
summaryRprof()
