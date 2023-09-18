
## 5.8.1 following looks long, but only 3 lines are modified from notes 

ecdf3 <- function(x,h=.1) {
## function to plot empirical cdf of sample x, smoothed with
## bandwidth h
  x <- x + rnorm(length(x)*100,sd=h) ## changed line!!
  n <- length(x)
  p <- 1:n/n ## create cumulative probability vector 
  xs <- sort(x) ## sort the x values
  ## plot the x, p points
  p1 <- x1 <- rep(NA,3*n+2) ## create p1 and x1 with NAs everywhere
  p1[1:n*3+2] <- p1[1:n*3+1] <- p ## fill in the step heights
  x1[1:n*3+1] <- xs ## step start
  x1[1:n*3+2] <- c(xs[-1],2*xs[n]-xs[1]) ## step ends
  p1[1:2] <- 0 ## initial prob is zero
  x1[1:2] <- c(2*xs[1]-xs[n],xs[1]) ## x co-ords for zero prob
  ## changed plot symbol!!...
  plot(xs,p,pch=".",cex=.5,xlab="x",ylab=expression(P(X<=x)),col=4)
  lines(x1,p1,col=2)
} ## ecdf3

set.seed(8);
x <- rgamma(50,shape=3,scale=2) ## test data
ecdf3(x,h=1.5)
x0 <- seq(0,25,length=200)
lines(x0,pgamma(x0,shape=3,scale=2))


## 6.1.1

n <- length(morley$Speed)
bs.ls <- matrix(sample(morley$Speed,n*1000,replace=TRUE),n,1000)
bs.mean <- colMeans(bs.ls)
quantile(bs.mean,c(.025,.975))

## 6.1.2 One of many alternatives for doing this...

n <- 2^(1:9) ## the n sequence
m <- 10000   ## sample sizes
U <- matrix(rexp(m*max(n)),m) ## exponential deviate to sum
par(mfrow=c(3,3)) ## split the plot window
x <- seq(0,6,length=1000) ## extra bit: sequence of x values for pdf plotting
for (i in n) { ## loop over n
  Xn <- rowMeans(U[,1:i]) ## sum n=2, 4, ... , 512 U values
  hist(Xn,main="",xlab=paste("X",i),freq=FALSE,breaks=30) ## visualize dist
  lines(x,dnorm(x,mean=mean(Xn),sd=sd(Xn))) ## plot normal pdf with same mean and sd
}

## 6.3

n.rep <- 10000 ## number of CIs to compute
n.ci <- 0 ## counter for number that include the truth
n <- 20;p <- .2
x <- rbinom(n.rep,n,p) ## generate x
p.hat <- x/n ## compute the estimate of p
sig.p <- sqrt(p.hat*(1-p.hat)/n) ## and its standard error
cp <- mean(p.hat - 1.96*sig.p <p & p.hat+1.96*sig.p > p)
cp ## observed coverage probability 