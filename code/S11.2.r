n <- 3000
A <- crossprod(matrix(runif(n*n),n,n)) ## example +ve def matrix
b <- runif(n)  ## example n vector

system.time(c0 <- solve(A,b)) ## solve 
system.time({R <- chol(A);    ## solve with cholesky
c1 <- backsolve(R,forwardsolve(t(R),b))}) 
range(c0-c1)/norm(A) ## confirm same result


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

