ecdf <- function(x) {
## function to plot empirical cdf of sample x
  n <- length(x)
  p <- 1:n/n ## create cumulative probability vector 
  xs <- sort(x) ## sort the x values
  ## plot the x, p points
  plot(xs,p,pch=19,cex=.5,xlab="x",ylab=expression(P(X<=x)),col=4)
  ## now add in the lines of constant probability between x values
  lines(c(2*xs[1]-xs[n],xs[1]),c(0,0))
  for (i in 1:(n-1)) lines(c(xs[i],xs[i+1]),c(p[i],p[i]),col=2)
  lines(c(2*xs[n]-xs[1],xs[n]),c(1,1),col=2)
} ## ecdf

## test it
set.seed(8);
x <- rgamma(50,shape=3,scale=2) ## test data
ecdf(x)


ecdf2 <- function(x) {
## function to plot empirical cdf of sample x
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
  plot(xs,p,pch=19,cex=.5,xlab="x",ylab=expression(P(X<=x)),col=4)
  lines(x1,p1,col=2)
} ## ecdf

ecdf2(x)
