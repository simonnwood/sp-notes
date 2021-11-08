## Section 12.2 ridge regression, basically after debugging and profiling
## as in debugging lecture...

fit.ridge <- function(y,X,XX,sp) {
## fits model y = X b + e by ridge regression, with penalty parameter sp
## and computes GCV score.
  p <- ncol(X);n <- nrow(X)
  ## Compute hat matrix... tr(BD) = tr(DB)
  ## tr{X(X'X+sI)^{-1}X'} = tr{(X'X+sI)^{-1}X'X}
  A1 <- solve(XX + sp*diag(p),XX)
  trA <- sum(diag(A1)) ## Effective degrees of freedom
  ## compute coeff estimates...
  b.hat <- solve(XX + sp*diag(p),drop(t(y)%*%X))
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
  XX <- crossprod(X)
  for (i in 1:length(lsp)) { ## loop over log smoothing parameters
    fr <- fit.ridge(y,X,XX,exp(lsp[i])) ## fit
    gcv[i] <- fr$gcv
    edf[i] <- fr$edf
  }
  plot.gcv(edf,lsp,gcv) ## plot results
  i.opt <- max(which(gcv==min(gcv))) ## locate minimum
  fit.ridge(y,X,lsp[i.opt]) ## return fit at minimum
} ## ridge


## Section 12.3

n <- 10;A <- matrix(runif(n*n),n,n)
B <- try(mat.fun(A),silent=TRUE)
OK <- FALSE
if (inherits(B,"try-error")) { ## failed
  ## but need to check if it failed correctly by catching this case
  ## and producing appropriate error message...
  if (grep("matrix not symmetric",as.character(attr(B,"condition")))==1) OK <- TRUE
}
if (!OK) warning("non-symmetric matrix error handling failure")
