

## Functions

g <- function(a,b=20) {
  a[a<0] <- 0
  c <- a^b
  c
}

a <- cat("hello\n")

b <- 20

fbad <- function(a) {
  a[a <= 0] <- 1e-6
  d <- log(a) + b 
}

a <- fbad(rnorm(10))

m <- 3; B <- crossprod(matrix(runif(m*m),m,m))
eb <- eigen(B)
eb$vectors %*% diag(eb$values) %*% t(eb$vectors)


mat.fun <- function(A,fun=I) {
  ea <- eigen(A,symmetric=TRUE)
  ea$vectors %*% (fun(ea$values)*t(ea$vectors)) ## note use of re-cycling rule!
}

set.seed(1)

range(B - mat.fun(B))


## ...


foo <- function(x,a,b) x/a + b

mat.fun <- function(A,fun=I,...) {
  ea <- eigen(A,symmetric=TRUE)
  ea$vectors %*% (fun(ea$values,...)*t(ea$vectors)) ## note use of re-cycling rule!
}

mat.fun(B,fun=foo,a=2,b=3)

ff <- function(res=1,...) res
ff(r=2)

ff <- function(...,res=1) res
ff(r=2)
