## Section 11, matrix computation solutions.

## 11 tr(AB) = sum_i sum_k A[i,k] * B[k,i] i.e. sum(A*t(B))

## 11.2
## let mx = E(X), Vx = E[(X-mx)(X-mx)^T]. E(Y) = AE(x) = Amx (= my, say).
## Now Vy =  E[(Y-my)(Y-my)^T] = E[A(X-mx)(X-mx)^TA^T] = AE[(X-mx)(X-mx)^T]A^T
## = AVxA^T (recall (AB)^T = B^TA^T)

rmvn <- function(n,m,S) {
## generate n multivariate normal vectors from MVN(m,S).
## Let p=length(m). Start with chol decomp R^TR=S, and
## p vector of N(0,1) random variables Z. R^TZ have cov
## matrix S, by preceding result. So R^TZ + m gives
## desired random vector.
  R <- chol(S)  ## Cholesky factor of cov matrix M
  p <- length(m)
  if (p!=nrow(S)&&p!=1) stop("m and S dimensions do not match")
  Z <- matrix(rnorm(n*p),p,n) ## n standard normal p-vectors
  t(R)%*%Z+m  ## n MVN(m,S) vectors
} ## rmvn

## check it is working as intended
S <- crossprod(matrix(runif(9),3,3))
m <- runif(3)
x <- rmvn(10000,m,S) 
rowMeans(x);m ## check mean 
cov(t(x));S   ## check covariance


## 11.3 inverse of R...
n <- 5;R <- qr.R(qr(matrix(runif(n*n),n,n))) ## get an R
Ri <- backsolve(R,diag(ncol(R))) ## solve R %*% Ri = I
range(R %*% Ri-diag(n))  ## check R %*% Ri = I

## 11.3 QQ^T = I and |A|=|A^T| so |Q|^2 = 1 => |Q|= +/-1

set.seed(0);n<-100; A <-matrix(runif(n*n),n,n)
R <- qr.R(qr(A))
prod(abs(diag(R)))
determinant(A,log=FALSE)

A <-A*1000
R <- qr.R(qr(A))
prod(abs(diag(R))) ## overflow
determinant(A,log=FALSE) ## overflow

sum(log(abs(diag(R)))) ## fine
determinant(A) ## fine

## 11.5.1

X <- sweep(as.matrix(iris[,1:4]),2,colMeans(iris[,1:4])) ## column centred data matrix
V <- t(X)%*%X/(nrow(X)-1) ## estimated covariance matrix
sd <- diag(V)^.5  ## standard deviation
C <- t(t(V/sd)/sd) ## form C=diag(1/sd)%*%V%*$diag(1/sd) efficiently
## Actually cor(iris[,1:4]) does the same!!
ec <- eigen(C)    ## eigen decompose the correlation matrix
U <- ec$vectors;lambda <- ec$values  ## exract eigenvectors and values
Z <- X %*% U;                        ## the principle co-ordinated 
plot(Z[,1],Z[,2],col=c(1,2,4)[as.numeric(iris$Species)],main="iris PCA",
     xlab="PCA 1",ylab="PCA 2")      ## plot first two components  
sum(lambda[1:2])/sum(lambda)
## ... first 2 components explain lower proportion than with cov based PCA

## 11.6 (X^TX)^{-1}^Ty = (VD^2V^T)^{-1}VDU^Ty=VD^{-2}V^TVDU^Ty = VD^{-1}U^Ty
X <- model.matrix(~group,PlantGrowth)
sx <- svd(X)
beta.hat <- sx$v%*%((t(sx$u)%*%PlantGrowth$weight)/sx$d)
beta.hat;coef(lm(weight~group,PlantGrowth))
