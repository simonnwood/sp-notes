## optim....

nll <- function(theta,t,y) {
## -ve log likelihood for AIDS model y_i ~ Poi(alpha*exp(beta*t_i))
## theta = (alpha,beta)  
  mu <- theta[1] * exp(theta[2] * t) ## mu = E(y)
  -sum(dpois(y,mu,log=TRUE)) ## the negative log likelihood
} ## nll

t80 <- 1:13 ## years since 1980
y <- c(12,14,33,50,67,74,123,141,165,204,253,246,240) ## AIDS cases

fit <- optim(c(10,.1),nll,y=y,t=t80,hessian=TRUE)

fit 

V <- chol2inv(chol(fit$hessian)) ## solve(fit$hessian) also possible
se <- diag(V)^.5 ## extract standard deviations
se

## optim with grad...

gll <- function(theta,t,y) {
## grad of -ve log lik of Poisson AIDS early epidemic model
  alpha <- theta[1];beta <- theta[2] ## enhances readability
  ebt <- exp(beta*t) ## avoid computing twice
  -c(sum(y)/alpha - sum(ebt),     ## -dl/dalpha
    sum(y*t) - alpha*sum(t*ebt)) ## -dl/dbeta
} ## gll

fd <- th0 <- c(10,.1) ## test param value, and approx grad vec
nll0 <- nll(th0,t=t80,y=y) ## nll at th0
eps <- 1e-7  ## finite difference interval 
for (i in 1:length(th0)) { ## loop over parameters
  th1 <- th0; th1[i] <- th1[i] + eps ## increase th0[i] by eps
  nll1 <- nll(th1,t=t80,y=y) ## compute resulting nll
  fd[i] <- (nll1 - nll0)/eps ## approximate -dl/dth[i]
}
fd;gll(th0,t80,y) ## compare


fit <- optim(c(10,.1),nll,gr=gll,y=y,t=t80,method="BFGS",hessian=TRUE)
fit

## nlm...

hll <- function(theta,t,y) {
## Hessian of -ve log lik of Poisson AIDS early epidemic model
  alpha <- theta[1];beta <- theta[2] ## enhances readability
  ebt <- exp(beta*t) ## avoid computing twice
  H <- matrix(0,2,2) ## matrix for Hessian of -ve ll
  H[1,1] <- sum(y)/alpha^2
  H[2,2] <-  alpha*sum(t^2*ebt)
  H[1,2] <- H[2,1] <- sum(t*ebt)
  H
} ## hll

gll0 <- gll(th0,t=t80,y=y) ## gran of nll at th0
eps <- 1e-7  ## finite difference interval 
Hfd <- matrix(0,2,2) ## finite diference Hessian
for (i in 1:length(th0)) { ## loop over parameters
  th1 <- th0; th1[i] <- th1[i] + eps ## increase th0[i] by eps
  gll1 <- gll(th1,t=t80,y=y) ## compute resulting nll
  Hfd[i,] <- (gll1 - gll0)/eps ## approximate second derivs
}
Hfd;hll(th0,t=t80,y=y)

nll2 <- function(theta,t,y) {
## wrapper function for nll and its grad and Hessian,
## suitable for optimization by nlm 
  z <- nll(theta,t,y) ## the objective
  attr(z,"gradient") <- gll(theta,t,y)
  attr(z,"hessian") <- hll(theta,t,y)
  z
} ## nll2

nlm(nll2,th0,y=y,t=t80)

## positive parameters...

nll3 <- function(theta,ti,y) {
## -ve log likelihood for AIDS model y_i ~ Poi(alpha*exp(beta*t_i))
## theta = (log(alpha),beta)
  alpha <- exp(theta[1]) ## so theta[1] unconstrained, but alpha > 0
  beta <- theta[2]  
  mu <- alpha * exp(beta * ti) ## mu = E(y)
  -sum(dpois(y,mu,log=TRUE))  ## the negative log likelihood
} ## nll3

optim(c(log(10),.1),nll3,y=y,ti=t80,method="BFGS")

## deriv

rb <- deriv(expression(k*(z-x^2)^2 + (1-x)^2),
            c("x","z"), ## diff w.r.t. these
            function.arg=c("x","z","k"), 
            hessian=TRUE)

nlli <- deriv(expression(-y*(log(alpha)+beta*t)+alpha*exp(beta*t)+lgamma(y+1)),
            c("alpha","beta"), ## diff w.r.t. these
            function.arg=c("alpha","beta","t","y"), 
            hessian=TRUE)

nlli(10,.1,t80,y)

nll4 <- function(th,t,y) {
  nli <- nlli(th[1],th[2],t,y)
  nll <- sum(nli)
  attr(nll,"gradient") <- colSums(attr(nli,"gradient"))
  attr(nll,"hessian") <- apply(attr(nli,"hessian"),c(2,3),sum)
  nll
} ## nll4  

nll4(th0,t80,y)
