## code example for debugging and profiling...

fit.ridge <- function(y,X,sp) {
## fits model y = X b + e by ridge regression, with penalty parameter sp
## and computes GCV score.
  p <- ncol(X);n <- nrow(X)
  ## Compute hat matrix...
  A <- X %*% solve(crossprod(X) + sp*diag(p),t(X))
  trA <- sum(diag(A)) ## Effective degrees of freedom
  ## compute coeff estimates...
  b.hat <- solve(crossprod(X) + sp*diag(p),t(X)*y)
  fv <- X %*% b.hat ## fitted values
  gcv <- sum(n*(y-fv)^2)/(n-trA)^2 
  list(b.hat=b.hat,fv=fv,gcv=gcv,edf=trA)
} ## fit.ridge

plot.gcv <- function(edf,lsp,gcv) {
## plot GCV against EDF and log s.p. showing minimum 
  par(mfrow=c(1,2),mar=c(4,4,1,1)) 
  plot(lsp,gcv,xlab=expression(log(lambda)),ylab="GCV",type="l")
  i.min <-which(gcv==min(gcv)) 
  points(lsp[i.min],gcv[i.min],col=2,pch=19)
  plot(edf,gcv,xlab="EDF",ylab="GCV",type="l")
  points(edf[i.min],gcv[i.min],col=2,pch=19)
}

ridge <- function(y,X,lsp=seq(-5,5,length=100)) {
## function for ridge regression of y on X with generalized
## cross validation lsp is the set of smoothing parameters to try
  edf <- gcv <- lsp*0 ## vectors for edf and gcv
  for (i in 1:length(lsp)) { ## loop over log smoothing parameters
    fr <- fit.ridge(y,X,lsp[i]) ## fit
    gcv[i] <- fr$gcv
    edf[i] <- fr$edf
  }
  plot.gcv(edf,lsp,gcv) ## plot results
  i.opt <- max(which(gcv==min(gcv))) ## locate minimum
  fit.ridge(y,X,lsp[i.opt]) ## return fit at minimum
} ## ridge

## test code. First simulate some data...
set.seed(0)
n <- 1000;p <- 100
X <- matrix(runif(n*p),n,p)
beta <- runif(p);beta[round(p/5):p] <- 0
mu <- X %*% beta
y <- mu + rnorm(n)

## fit model
fv <- ridge(y,X)

