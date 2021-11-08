## Section 9.2. These are pencil and paper exercises, but the following R code
## generates the answers...
## 1.
x <- c(0.1, 1.3, 0.4, 1.4, 2.0, 1.6)
z = factor(c("a", "a", "fred", "a", "c", "c"))
model.matrix(~x+z) ## obviously any re-ordering of columns is fine

#  (Intercept)   x zc zfred
#1           1 0.1  0     0
#2           1 1.3  0     0
#3           1 0.4  0     1
#4           1 1.4  0     0
#5           1 2.0  1     0
#6           1 1.6  1     0

## 2.
x <- factor(c("a", "a", "b", "a", "b", "a"))
z <- factor(c("ctrl", "trt", "trt", "trt", "ctrl", "ctrl"))
model.matrix(~x*z)

#  (Intercept) xb ztrt xb:ztrt
#1           1  0    0       0
#2           1  0    1       0
#3           1  1    1       1
#4           1  0    1       0
#5           1  1    0       0
#6           1  0    0       0

## 3.
z <- c(1, 2, 1, 3, 4, 2)
model.matrix(~x*z)

#  (Intercept) xb z xb:z
#1           1  0 1    0
#2           1  0 2    0
#3           1  1 1    1
#4           1  0 3    0
#5           1  1 4    4
#6           1  0 2    0


## 9.3

pgm <- lm(weight~group,data=PlantGrowth)
plot(pgm)
## ... slight hint that variability might not be constant, but
## could also occurr by chance with a sample size of 30. Approximations
## see good enough here.
pg0 <- lm(weight~1,data=PlantGrowth) ## fit no group effect model
anova(pg0,pgm) ## p-value 1.6% => evidence for an effect

summary(pgm)
## the intercept is now the control group mean weight, and the other
## parameters the difference in mean weight between the other groups
## and the control. Although for the data al together we can detect that
## group has an effect, the p-values in the above table indicate that it
## is not clear which group differences are driving the overall effect.
## Probably it is the difference between trt1 and trt2 that drives the
## overall effect. Let's investigate...

PlantGrowth1 <- PlantGrowth
PlantGrowth1$group <-factor(PlantGrowth1$group,levels=c("trt1","trt2","ctrl"))
summary(lm(weight~group,data=PlantGrowth1))
##... yes!

## Remember, when we test hypotheses about parameters, we are testing hypotheses
## about the true population parameter values - not the estimates we obtained
## in the particular sample. 

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
