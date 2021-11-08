n <- 2000; y <- runif(n) ## example vector
A <- matrix(runif(n*n),n,n) ## example matrix
B <- matrix(runif(n*n),n,n) ## example matrix

system.time(a1 <- A %*% B %*% y)

system.time(a2 <- A %*% (B %*% y))

range(a1-a2) ## results the same

A <- matrix(1:12,3,4)
A;t(A)

crossprod(A); t(A) %*% A

diag(crossprod(A))

A*A
A+A
A-A

b <- 5:8
A %*% b
drop(A%*%b)

A <- matrix(runif(16),4,4)
c <- runif(4)

## solving Ab = c for b

b <- solve(A,c)
drop(A%*%b);c

Ai <- solve(A)
round(Ai%*%A,digits=14)

