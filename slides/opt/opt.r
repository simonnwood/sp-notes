## overflow...
set.seed(0)
x <- rnorm(400)

log(prod(dnorm(x,2,2)))
sum(dnorm(x,2,2,log=TRUE))



rb0 <- function(theta,k) {
## Rosenbrock's function
  z <- theta[1]; x <- theta[2]
  k*(z-x^2)^2 + (1-x)^2 
} ## rb0

th0 <- c(-.5,5)

optim(th0,rb0,k=10)

optim(th0,rb0,method="BFGS",k=10)

nlm(rb0,th0,k=10)

grb0 <- function(theta,k) {
## Grad vecor for Rosenbrock's function
  z <- theta[1]; x <- theta[2]
  # k*(z-x^2)^2 + (1-x)^2
  c(2*k*(z-x^2),-4*k*(z-x^2)*x - 2*(1-x))
} ## grb0

## testing gradients by FD
fd.rb <- th0*0
rb.lo <- rb0(th0,10)
eps <- 1e-7
for (i in 1:length(th0)) {
  th1 <- th0;th1[i] <- th1[i] + eps
  rb.hi <- rb0(th1,10)
  fd.rb[i] <- (rb.hi-rb.lo)/eps
}
fd.rb
grb0(th0,10)

optim(th0,rb0,gr=grb0,method="BFGS",k=10)

## now use auto-differentation...

rb <- deriv(expression(k*(z-x^2)^2 + (1-x)^2),c("z","x"),
            function.arg=c("z","x","k"),hessian=TRUE)

rb1 <- function(theta,k) {
  rb(theta[1],theta[2],k)
}
q

nlm(rb1,th0,k=10)
