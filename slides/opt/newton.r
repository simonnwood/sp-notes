## (c) Simon Wood 2007
## source("c:/lnotes/APTS/R-code/optimization/newton.r")

ad.quad <- function(f0,g,H,x,z,col=2,trans=log10,lev=1:10/2,lty=1) {
## function to add a quadratic approximation to a contour
## plot of a function, based on a Taylor expansion
## at x,z
  qap <- function(x,z,f0,g,H) {
    X <- matrix(c(x,z),length(x),2)
    f0 + X%*%g + rowSums((X%*%H)*X)/2
  }
  n <- 50
  xx <- seq(-1.5,1.5,length=n)
  zz <- seq(-.5,1.5,length=n)
  q <- trans(outer(xx-x,zz-z,qap,f0=f0,g=g,H=H)) 
  contour(xx,zz,matrix(q,n,n),col=col,add=TRUE,levels=lev,lty=lty)
}



newton.2d <- function(fn,fn.g,fn.h,x,click=FALSE,trans=log10,lev=1:10/2,stub="newt2d") {
## function to illustrate Newton in
## 2d. x is a 3 by 2 matrix
  kk <- 2
  n <- 200
  xx <- seq(-1.5,1.5,length=n)
  zz <- seq(-.5,1.5,length=n)
  f <- outer(xx,zz,fn)
  if (TRUE){ 
    track.col <- 2;
    ap.col1 <- 4;ap.lty1=1
    ap.col2 <- 3;ap.lty2=1
    lwd.trial <- 2
    rb.col <- 1
  } else {
    track.col <- 1;
    ap.col1 <- 1;ap.lty1=1
    ap.col2 <- 1;ap.lty2=4
    lwd.trial <- 1
    rb.col <- "grey"
  }
  xl <- x[1]
  zl <- x[2]
  converged <- FALSE
  fi <- 0
  if (!click) { pdf(paste(stub,fi,".pdf",sep=""),width=8,height=6)
    par(mfrow=c(1,1),mar=c(1,1,1,1)); fi <- fi + 1}
  contour(xx,zz,matrix(trans(f),n,n),levels=lev,
            xaxt="n",yaxt="n",col=rb.col)
  if (!click) dev.off()
  g <- fn.g(x[1],x[2])
  while (!converged) {
    ## first get the quad approx.
    f0 <- fn(x[1],x[2])
   
    H <- fn.h(x[1],x[2])
    if (!click) { pdf(paste(stub,fi,".pdf",sep=""),width=8,height=6)
    par(mfrow=c(1,1),mar=c(1,1,1,1)); fi <- fi + 1}
    contour(xx,zz,matrix(trans(f),n,n),levels=lev,
            xaxt="n",yaxt="n",col=rb.col)
    if (length(xl)>1) lines(xl,zl,col=track.col,lwd=2)
    
    ## Now check for +ve definiteness
    eh <- eigen(H,symmetric=TRUE)
    ind <- eh$values<0
    if (sum(ind)) ad.quad(f0,g,H,x[1],x[2],trans=trans,lev=lev,
                  col=ap.col1,lty=ap.lty1) ## plot it
    eh$values[ind] <- -eh$values[ind]
    H <- eh$vectors%*%(eh$values*(t(eh$vectors)))
    ad.quad(f0,g,H,x[1],x[2],col=ap.col2,trans=trans,lev=lev,
            lty=ap.lty2) ## plot modified quad ap
    ## get search direction
    dx <- solve(H,-g)
    x1 <- x + dx 
    lines(c(x[1],x1[1]),c(x[2],x1[2]),col=ap.col2,lwd=lwd.trial)
    f1 <- fn(x1[1],x1[2])
    while (f1>f0) { ## step halving loop
      dx <- dx/2
      x1 <- x+dx
      f1 <- fn(x1[1],x1[2])
    }  
    lines(c(x[1],x1[1]),c(x[2],x1[2]),col=track.col,lwd=3)
    x <- x1
    xl <- c(xl,x[1])
    zl <- c(zl,x[2])
    kk <- kk+1
    g <- fn.g(x[1],x[2])
    converged <- (max(abs(g))<1e-7)
    if (converged) points(x[1],x[2],pch=20)
    if (click) dum <- locator(1) else dev.off()
  }
 # points(x[1],x[2],pch=20)
  x
}


rb <- function(x,z) {
## Rosenbrock's function
  100*(z-x^2)^2 + (1-x)^2 
}

rb.grad <- function(x,z) {
## gradient of rosenbrocks function at a single point
  g <- rep(NA,2)
  g[2] <- 200*(z-x^2)
  g[1] <- 400*(x^3-z*x) + 2*(x-1)
  g
}

rb.hess <- function(x,z) {
## Hessian of Rosenbrock's function, at a single point
  H <- matrix(NA,2,2)
  H[2,2] <- 200
  H[1,1] <- 1200*x^2 - 400*z + 2
  H[2,1] <- H[1,2] <- -400*x
  H
}


lev <- c(-10^(3:-3),10^(-3:3))
lev <- c(-1000,-300,-100,-30,-10,-3,-1,1,3,10,30,100,300,1000)
ps <- FALSE
if (ps) { postscript("c:/lnotes/APTS/newton.eps")
par(mfrow=c(5,4),mar=c(1,1,1,1))} else {
par(mfrow=c(1,1),mar=c(1,1,1,1))
}
xx <- c(-.5,1)
setwd("~sw283/lnotes/StatProg/opt/ani")
res <- newton.2d(rb,rb.grad,rb.hess,xx,click = FALSE,trans=function(x) 11*(exp(x*.02)/(1+exp(x*.02))-.5),lev=c(-(5:1),0.2,1:5))


print(res)
if (ps) dev.off()
lev <- c(-1000,-300,-100,-30,-10,-3,-1,1,3,10,30,100,300,1000)
res <- newton.2d(rb,rb.grad,rb.hess,xx,click = !ps,trans=I,lev=lev)

## co-ord descent...

cd.2d <- function(fn,fn.g,fn.h,x,click=FALSE,trans=log10,lev=1:10/2,stub="newt2d") {
## function to illustrate co-ord descent in
## 2d. x is a 3 by 2 matrix
  kk <- 2
  n <- 200
  xx <- seq(-1.5,1.5,length=n)
  zz <- seq(-.5,1.5,length=n)
  f <- outer(xx,zz,fn)
  if (TRUE){ 
    track.col <- 2;
    ap.col1 <- 4;ap.lty1=1
    ap.col2 <- 3;ap.lty2=1
    lwd.trial <- 2
    rb.col <- 1
  } else {
    track.col <- 1;
    ap.col1 <- 1;ap.lty1=1
    ap.col2 <- 1;ap.lty2=4
    lwd.trial <- 1
    rb.col <- "grey"
  }
  xl <- x[1]
  zl <- x[2]
  converged <- FALSE
  if (!click) { pdf(paste(stub,0,".pdf",sep=""),width=8,height=6)
    par(mfrow=c(1,1),mar=c(1,1,1,1)); }
  contour(xx,zz,matrix(trans(f),n,n),levels=lev,
            xaxt="n",yaxt="n",col=rb.col)
	    points(1,1,pch=3)
  if (!click) dev.off()
  g <- fn.g(x[1],x[2])
  xt <- zt <- rep(NA,1000000)
  k <- 1
  xt[k] <- x[1]
  zt[k] <- x[2]
  while (!converged&&k<41) {
    if (!click) { pdf(paste(stub,k,".pdf",sep=""),width=8,height=6)
    par(mfrow=c(1,1),mar=c(1,1,1,1));}
    contour(xx,zz,matrix(trans(f),n,n),levels=lev,
            xaxt="n",yaxt="n",col=rb.col)
    points(1,1,pch=3)
    ## optimize z
    x2 <- optimize(function(z,x) rb(x,z),c(-.5,1.5),x=x[1],tol=1e-8)$minimum
    k <- k + 1
    xt[k] <- xt[k-1]
    x[2] <- zt[k] <- x2
    lines(xt[1:k],zt[1:k],col=2,lwd=2)
    if (!click) dev.off()
    if (!click) { pdf(paste(stub,k,".pdf",sep=""),width=8,height=6)
    par(mfrow=c(1,1),mar=c(1,1,1,1));}
    contour(xx,zz,matrix(trans(f),n,n),levels=lev,
            xaxt="n",yaxt="n",col=rb.col)
	    points(1,1,pch=3)
    ## optimize x
    x1 <- optimize(fn,c(-1.5,1.5),z=x[2],tol=1e-8)$minimum
    k <- k + 1
    zt[k] <- zt[k-1]
    x[1] <- xt[k] <- x1
    lines(xt[1:k],zt[1:k],col=2,lwd=2)
    g <- fn.g(x[1],x[2])
    converged <- (max(abs(g))<1e-3)
    if (converged) points(x[1],x[2],pch=20)
    cat(k,"  ",max(abs(g)),"\n")
    if (click) dum <- locator(1) else dev.off()
  }
  k
}

cd.2d(rb,rb.grad,rb.hess,xx,click = FALSE,trans=function(x) 11*(exp(x*.02)/(1+exp(x*.02))-.5),
      lev=c(-(5:1),0.2,1:5),stub="cd")

## polytope

utope <- function(fn,x) {
## update the polytope in x
  ff <- fn(x[,1],x[,2])
  if (max(ff)-min(ff) < 1e-4*abs(mean(ff)+.1)) return(as.numeric(x[ff==min(ff),]))
  worst <- (1:3)[ff==max(ff)]
  best.centroid <- colMeans(x[-worst,])
  xn <- 2*best.centroid - x[worst,]
  f.new <- fn(xn[1],xn[2])
  if (f.new < min(ff)) { ## new best point -- try expansion
    xe <- best.centroid + 2*(xn - best.centroid)
    fe <- fn(xe[1],xe[2]) ## trying expanded step
    if (fe<f.new) x[worst,] <- xe else x[worst,] <- xn
  } else 
  if (f.new > max(ff[-worst])) { ## new worst step -- contract
     f.thresh <- min(f.new,max(ff))
     f.c <- f.thresh+1
     gamma <- .5
     if (f.new >= max(ff)) {
       xc <- best.centroid + gamma*(x[worst,]-best.centroid)
     } else {
       xc <- best.centroid + gamma*(xn-best.centroid)
     }
     gamma <- gamma/2
     f.c <- fn(xc[1],xc[2])
     if (f.c < f.thresh) x[worst,] <- xc else {
       ## shrink towards best
       best <- (1:3)[ff==min(ff)]
       x <- t(.5*t(x) + .5*x[best,])
     }
  } else { 
    x[worst,] <- xn
  }
  x
}


polytope.2d <- function(fn,x=c(-.5,1),click=FALSE,dx=.1,trans=function(x) 11*(exp(x*.02)/(1+exp(x*.02))-.5),lev=c(-(5:1),0.2,1:5),stub="nm2d",...) {
## function to illustrate nelder-mead polytope in
## 2d. x is a 3 by 2 matrix

  n <- 100
  xx <- seq(-1.5,1.5,length=n)
  zz <- seq(-.5,1.5,length=n)
  f <- outer(xx,zz,rb)
  
  if (click) contour(xx,zz,matrix(trans(f),n,n),levels=lev,xaxt="n",yaxt="n",...)

  z <- x
  x <- matrix(c(z[1],z[1]+dx,z[1],z[2],z[2],z[2]+dx),3,2)
  kk <- 2
  ff <- rep(0,3)
  jj <- 1
  if (!click) {
    ltype <- c(1,1,1)
    lcol <- 2:4
    ppch <- c(20,20,20)
  } else {
    ltype <- c(1,1,2)
    lcol <- c("black","grey","black")
    ppch <- c(20,20,21)
  }
  
  while (is.matrix(x)) {
    if (!click) {
    if (kk>2) dev.off()
    pdf(paste(stub,kk-2,".pdf",sep=""),width=8,height=6)
    par(mfrow=c(1,1),mar=c(1,1,1,1));
    contour(xx,zz,matrix(trans(f),n,n),levels=lev,xaxt="n",yaxt="n",...) }
    for (i in 1:3) ff[i] <- fn(x[i,1],x[i,2])
    k  <- (1:3)[ff==max(ff)]
    #kol = kk;lt <- 1;pc <- 20
    kol <- lcol[jj];lt <- ltype[jj];pc <- ppch[jj]
    if (jj==3) jj <- 1 else jj <- jj + 1 
    points(x[k,1],x[k,2],col=kol,pch=pc) ## plot worst
    lines(x[1:2,1],x[1:2,2],col=kol,lty=lt)
    lines(x[2:3,1],x[2:3,2],col=kol,lty=lt)
    lines(c(x[3,1],x[1,1]),c(x[3,2],x[1,2]),col=kol,lty=lt)
    x <- utope(fn,x)
    kk <- kk+1 
    if (click) dum <- locator(1) 
    #ani.record()
  }
  points(x[1],x[2],pch=20,cex=2)
  if (!click) dev.off()
  #ani.record()
  c(x,kk)
}


rb <- function(x,z) {
  100*(z-x^2)^2 + (1-x)^2 
}
setwd("~sw283/lnotes/StatProg/opt/ani")
res <- polytope.2d(rb,c(-.5,1),FALSE,cex.lab=1.8,
            cex.axis=1.6,cex=1.5)
	    
ps <- FALSE
if (ps) { postscript("c:/lnotes/APTS/polytope.eps")
par(mfrow=c(1,1),mar=c(6,6,1,1)) } else {
par(mfrow=c(1,1),mar=c(1,1,1,1))
}

xx <- c(-.5,1)
res <- polytope.2d(rb,xx,FALSE)
print(res)
if (ps) dev.off()



