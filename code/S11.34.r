## 

X <- model.matrix(~group,PlantGrowth) ## model matrix (note: response not needed!)
qrx <- qr(X)              ## get QR decomposition of model matrix
y <- PlantGrowth$weight   ## response
p <- ncol(X)              ## number of parameters
Qty <- qr.qty(qrx,y)[1:p] ## get Q^T y
beta <- backsolve(qr.R(qrx),Qty) ## R^{-1} Q^T y
beta
coef(lm(weight~group,PlantGrowth))

## PCA

pairs(iris)

X <- sweep(as.matrix(iris[,1:4]),2,colMeans(iris[,1:4])) ## col centred data matrix
ec <- eigen(t(X)%*%X/(nrow(X)-1))    ## eigen decompose the covariance matrix
U <- ec$vectors;lambda <- ec$values  ## exract eigenvectors and values
Z <- X %*% U;                        ## the principle co-ordinated 
plot(Z[,1],Z[,2],col=c(1,2,4)[as.numeric(iris$Species)],main="iris PCA",
     xlab="PCA 1",ylab="PCA 2")      ## plot first two components  

apply(Z,2,var);lambda ## compare Z column variances to lambda
sum(lambda[1:2])/sum(lambda) ## proportion variance explained by components 1 and 2
