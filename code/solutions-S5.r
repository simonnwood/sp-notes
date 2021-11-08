## 5.2.2

set.seed(5); n <- 2000
w <- runif(n)
A <- matrix(runif(n*n),n,n)
system.time(B <- diag(w) %*% A )

## Let W = diag(w). Generically B_{ij} = \sum_k W_{ik} B_{kj}
## But W_{ik} = 0 except for W_{ii} = w_i, so B_{ij} = w_i B_{ij}
## given that B is actually stored column-wise that is in the order
## B_11 B_21, B_31, ... B_12 B_22 ... then the product can be
## computed using the recycling rule an vector multiplication...

system.time(B1 <- w * A )
range(B1-B)

## The huge speed up is because we have avoided all those multiplications
## by zero that contribute nothing anyway.


## 5.2.4.

a <- factor(sample(c("fred","george","sue","ann"),20,replace=TRUE))
a
b <- factor(a,levels=c("ann","sue","fred","george"))
b
as.numeric(a)
as.numeric(b)
## The codes are 1 for the first level, 2 for the second and so on
## so the change results in 4 -> 2, 2 -> 3 and 3 -> 4

al <- levels(a)
an <- as.numeric(a)
al[an]

## 5.4
x <- rnorm(20); z <- rnorm(20); y <- rnorm(20)
ii <- z!=0 & (z<1 | y/z <0)
x[ii] <- x[ii]^2
## Note that the fact that z is tested for equality to zero before y/z test
## ensures that we don't have a problem with y/z being undefined if z==0 -
## y/z will not be evaluated in that case.

## 5.5 other options are possible...

f <- function(x) log(x) - 1/x

xr <- c(.1,10) ## current bracketing interval
xt <- mean(xr) ## mid-point of interval
ft <- 1
while(abs(ft)>1e-8) {
  xt <- mean(xr) ## mid-point of interval
  ft <- f(xt)
  if (ft>0) xr[2] <- xt else xr[1] <- xt
}
xt


## 5.6 mostly as above... 

f <- function(x) log(x) - 1/x
g <- function(x,a=1,b=1) x^b - exp(a*x) + 2 

root <- function(xr,f,...) {
## finds the solution of f(x,...) = 0 in the interval xr
  f0 <- f(xr[1],...);f1 <- f(xr[2],...)
  if (f0==0) return(xr[1])
  if (f1==0) return(xr[2])
  if (f0*f1>0) stop("Interval does not appear to bracket solution")
  up <- sign(f1) ## +1 if function increasing, -1 otherwise 
  repeat { ## bisection loop
    xt <- mean(xr) ## mid-point of interval
    ft <- f(xt,...)
    if (up*ft>0) xr[2] <- xt else xr[1] <- xt
    if (abs(ft)<1e-8) break
  }
  xt
}

root(c(.1,10),f)
root(c(0,10),g,a=1,b=2)
