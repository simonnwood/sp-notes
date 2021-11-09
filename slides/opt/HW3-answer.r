## HW3 - a solution

fdH <- function(theta,f,g0,...) {
## compute FD approx Hessian, g0 is grad at theta.
  p <- length(theta);H <- matrix(0,p,p)
  eps <- 1e-7
  for (i in 1:p) { ## FD loop
    ## perturb ith element of theta...
    th1 <- theta; th1[i] <- th1[i] + eps
    ## difference grad vectors to get each Hess col...
    H[,i] <- (attr(f(th1,...),"gradient")-g0)/eps
  }
  0.5*(t(H)+H) ## return symmetric H
} ## fdH

pchol <- function(H) {
## Check if H is positive definite and modify if not.
## Check is by seeing if chol fails. An increasingly large multiple of the
## identity matrix is added to the Hessian until it is pos def if the original
## is not. Returns the Cholesky factor of the modified H.
  ok <- FALSE
  reg <- 0; I <- diag(ncol(H))
  k <- 0
  while (!ok) {
    k <- k + 1
    if (k>20) stop("can't make H +ve def")
    R <- try(chol(H + reg*I),silent=TRUE) ## succeeds only if +ve def
    if (inherits(R,"try-error")) { ## failed - not pos def.
      reg <- if (reg==0) max(abs(H)) * 1e-8 else reg <- reg * 10
    } else ok <- TRUE ## it is +ve def
  }
  attr(R,"Hpd") <- reg==0 ## was original H +ve def?
  R ## cholesky factor
} ## pchol

ffinite <- function(f) {
## check that objective and all derivs are finite.
  if (!is.finite(f)) return(FALSE)
  g <- attr(f,"gradient")
  if (!all(is.finite(g))) return(FALSE)
  H <- attr(f,"hessian");
  ## only check H if it is provided...
  if (!is.null(H)&&!all(is.finite(H))) return(FALSE)
  return(TRUE) ## if we got this far, all is fine
} ## ffinite

newton <- function(theta,f,...,tol=1e-8,fscale=1,maxit=100,max.half=20) {
## Newton method optimizer.
## * theta is vector of parameters to optimize
## * f is objective function, to be called as f(theta,...) - value returned
##   will have attributes "gradient" (vector) and "hessian" (matrix)
## * ... extra arguments of f.
## * tol convergence tolerance.
## * fscale a number within a few orders of magnitude of the typical value
##   of the objective near the minimum. Used for convergence testing.
## * maxit maximum number of Newton steps to take.
## * max.half how many time to try halving the Newton step if it is too long.
  f0 <- f(theta,...) ## initial value, run a couple of checks...
  if (is.null(attr(f0,"gradient"))) stop("no gradient supplied")
  if (!ffinite(f0)) stop("f is not finite at initial theta ")
  fd <- is.null(attr(f0,"hessian")) ## do we need to finite difference?
  for (i in 1:maxit) { ## main loop
    g <- attr(f0,"gradient")
    H <- if (fd) fdH(theta,f,g,...) else attr(f0,"hessian"); 
    if (max(abs(g))<(abs(f0)+fscale)*tol) break ## converged?
    R <- pchol(H) ## get chol factor of (perturbed?) Hessian
    s <- -backsolve(R,forwardsolve(t(R),g)) ## Newton step
    for (k in 1:max.half) { ## step check and halving loop
      f1 <- f(theta + s,...)
      if (ffinite(f1) && f1<f0) { ## accept step
        theta <- theta + s
	f0 <- f1
        break
      } else s <- s/2 ## need to halve step
    } ## end of step half loop
    if (k==max.half) {
      warning("step failed")
      break
    }
  }
  if (i==maxit) warning("iteration limit reached")
  if (!attr(R,"Hpd")) warning("Hessian indefinite")
  list(f=as.numeric(f0),theta=theta,iter=i,g=g,Hi=chol2inv(R))
} ## newton

## Two test examples

## auto generate Rosenbrock function with grad and Hess...
rb0 <- deriv(expression(k*(z-x^2)^2 + (1-x)^2),c("z","x"),
            function.arg=c("z","x","k"),hessian=TRUE)

rb <- function(theta,k) {
## wrapper in format expected by 'newton' returning results from
## rb0 in exact format required by 'newton'
  v <- rb0(theta[1],theta[2],k)
  attr(v,"hessian") <- attr(v,"hessian")[1,,]
  attr(v,"gradient") <- attr(v,"gradient")[1,]
  v
} ## rb

## Same again, but without Hessian...
rb1 <- deriv(expression(k*(z-x^2)^2 + (1-x)^2),c("z","x"),
            function.arg=c("z","x","k"))

rb2 <- function(theta,k) {
## wrapper in format expected by 'newton' returning results from
## rb1 in exact format required by 'newton'
  v <- rb1(theta[1],theta[2],k)
  attr(v,"gradient") <- attr(v,"gradient")[1,]
  v
} ## rb2


th0 <- c(-.5,2)
newton(th0,rb,k=10) ## full version
 
newton(th0,rb2,k=10) ## gradient only version

