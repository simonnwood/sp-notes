mat.fun <- function(A,fun=I) {
  if (!is.matrix(A)) A <- matrix(A) ## coerce to matrix if not already a matrix
  if (ncol(A)!=nrow(A)) stop("matrix not square") ## halt execution in this case
  if(max(abs(A-t(A)))>norm(A)*1e-14) stop("matrix not symmetric")
  ea <- eigen(A,symmetric=TRUE) ## get eigen decomposition
  ## generalize 'fun' to matrix case...
  ea$vectors %*% (fun(ea$values)*t(ea$vectors)) ## note use of re-cycling rule!
}

## test fun=I and inverse
n <- 10; A <- matrix(runif(n*n)-.5,n,n)
A <- A + t(A) ## make symmetric

B <- mat.fun(A) ## identity function case
if (max(abs(A-B))>norm(A)*1e-10) warning(" mat.fun I failure")

B <- mat.fun(A,function(x) 1/x) ## inverse function case
if (max(abs(solve(A)-B))>norm(A)*1e-10) warning("mat.fun inv failure")

A <- crossprod(A) ## ensure +ve def for sqrt case
B <- mat.fun(A,sqrt) ## sqrt function case
if (max(abs(A-B%*%B))>norm(A)*1e-10) warning("mat.fun sqrt failure")

## use try to check that an error is produced, as intended for non-square argument
if (!inherits(try(mat.fun(matrix(1:12,4,3)),silent=TRUE),"try-error")) 
              warning("error handling failure")
