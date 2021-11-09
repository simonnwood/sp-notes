## nodes are lists, with the following elements
## * x,y are node centres for plotting
## * child is a list of node children containing
##   - ind, indices of child nodes
##   - txt, any label to attach to the edge.
## * parent is a vector of parent node indices
## * par.lab is a label to go next to the incoming arrows from parents
## * lab is a label to go inside the node.
## * col is the node colour




link <- function(x0,y0,x1,y1,r,txt=NULL,txt2=NULL,col=1,lty=1) {
  rm <- sqrt((x1-x0)^2+(y1-y0)^2)
  if (rm < 2*r) stop("can't link nodes")
  dx <- (x1-x0)*r/rm;dy <- (y1-y0)*r/rm
  if (rm < 3) { 
    x <- c(x0+dx,x1-dx); y <- c(y0+dy,y1-dy) 
    xt <- mean(x);yt <- mean(y)
    ## get text rotation
    dx <- convertWidth(unit(x1-x0,"native"),"mm",valueOnly=TRUE)
    dy <- convertHeight(unit(y1-y0,"native"),"mm",valueOnly=TRUE)
    rot = atan((dy)/(dx)) 
    ## and offset from line
    rr <- sqrt(dx^2 + dy^2)
    dxt <- 5*dy/rr;dyt <- -5*dx/rr
    dxt <- convertWidth(unit(dxt,"mm"),"native",valueOnly=TRUE)
    dyt <- convertHeight(unit(dyt,"mm"),"native",valueOnly=TRUE)
  #  rr <- sqrt(dxt^2 + dyt^2)
  #  dxt <- dxt/rr; dyt <- dyt/rr
    xt2 <- xt - dxt
    yt2 <- yt - dyt
    xt <- xt + dxt 
    yt <- yt + dyt 
  } else { ## shameless hack
    x <- seq(x0+r,x1-r,length=200)
    delta <- 1.5*r
    dx <- (x1-x0)/2
    y <- x0 - delta*2*(x-x0)/dx + delta*(x-x0)^2/dx^2
    n <- round(length(x)/2)
    xt <- x[n];yt <-y[n]-r/4;rot=0
    xt2 <- x[n]
    yt2 <- y[n] +r/4

  }
  if (!is.null(txt)) grid.text(txt,xt,yt,default.units="native",
                     gp=gpar(col=col),rot=rot*180/pi)
  if (!is.null(txt2)) grid.text(txt2,xt2,yt2,default.units="native",
                     gp=gpar(col=col),rot=rot*180/pi)
  grid.lines(x,y,default.units="native",
             arrow=arrow(length=unit(.1,"native")),gp=gpar(col=col,lty=lty) )
  y[2]
}


graphit <- function(node,r=.05) {
## get the x/y range
  n.node <- length(node)
  x0 <- x1 <- node[[1]]$x
  y0 <- y1 <- node[[1]]$y
  for (k in 2:n.node) {
    x0 <- min(x0,node[[k]]$x)
    x1 <- max(x1,node[[k]]$x)
    y0 <- min(y0,node[[k]]$y)
    y1 <- max(y1,node[[k]]$y)
  } 
  x0 <- x0 - r; x1 <- x1 + r
  y0 <- y0 - 2*r; y1 <- y1 + r 
 # node <- convert.to.npc(node,r)
  grid.newpage()
  pushViewport(viewport(xscale=c(x0,x1),yscale=c(y0,y1),default.units="native"))
 # grid.text(expression(paste("f=(",x[1],x[2],sin~x[3],+e^{{x[1]~x[2]}},")/",x[3])),
 #           x=3,y=.5,default.units="native")
  for (i in 1:n.node) {
    node[[i]]$par.txt.y <- 0 ## to store par.txt heights
  }

  for (i in 1:n.node) { ## draw graph
    x <- node[[i]]$x
    y <- node[[i]]$y
    coli <- node[[i]]$col
    grid.circle(x,y,r,default.units="native",gp=gpar(col=coli))
    if (is.null(node[[i]]$lab2)) 
    grid.text(node[[i]]$lab,x,y,default.units="native",rot=0,gp=gpar(col=coli))
    else {
      grid.text(node[[i]]$lab,x,y+r/5,default.units="native",rot=0,gp=gpar(col=coli))
      grid.text(node[[i]]$lab2,x,y-r/9,default.units="native",rot=0,
      gp=gpar(col=node[[i]]$col2))
    }
    if (!is.null(node[[i]]$child))
    for (j in 1:length(node[[i]]$child$ind)) {
      colj <- node[[node[[i]]$child$ind[j]]]$col
      if (is.null(node[[i]]$child$lty)) lty <- 1 else
      lty <- node[[i]]$child$lty[j]
      node[[node[[i]]$child$ind[j]]]$par.txt.y <- 
           node[[node[[i]]$child$ind[j]]]$par.txt.y +
           link(x,y,
           node[[node[[i]]$child$ind[j]]]$x,
           node[[node[[i]]$child$ind[j]]]$y,r=r,
           txt=node[[i]]$child$txt[j],
           txt2=node[[i]]$child$etxt[j],col=colj,lty=lty)
    }
  }

  for (i in 1:n.node) {
    if (!is.null(node[[i]]$par.lab))
    grid.text(node[[i]]$par.lab,node[[i]]$x-r*1.2,
     node[[i]]$par.txt.y/2,default.units="native",rot=0,
     gp=gpar(col=node[[i]]$col))
  } 
  
}

library(grid)

node <- list()
node[[1]] <- list(x=1,y=1,child=list(ind=3),lab="c")
node[[2]] <- list(x=1,y=2,child=list(ind=3),lab="d")
node[[3]] <- list(x=2,y=1.5,child=list(ind=c(4,9,10)),parent=1:2,lab=expression(mu))
node[[4]] <- list(x=3,y=1.5,parent=c(3,5),lab=expression(x[i]))
node[[5]] <- list(x=4,y=1.5,child=list(ind=c(4,9,10)),parent=6,lab=expression(sigma^2))
node[[6]] <- list(x=5,y=1.5,child=list(ind=5,lty=2),parent=7:8,lab=expression(tau))
node[[7]] <- list(x=6,y=1,child=list(ind=6),lab="a")
node[[8]] <- list(x=6,y=2,child=list(ind=6),lab="b")

node[[9]] <- list(x=3,y=1.9,parent=c(3,5),lab=expression(x[i+1]))
node[[10]] <- list(x=3,y=1.1,parent=c(3,5),lab=expression(x[i-1]))

setwd("~sw283/lnotes/StatProg/Gibbs")
ps <- TRUE
if (ps) postscript("norm-dag.eps",width=4.5,height=3)
graphit(node,r=.3)
if (ps) dev.off()

node <- list()
node[[1]] <- list(x=1,y=1,child=list(ind=2),lab="parent")
node[[2]] <- list(x=2,y=1,parent=1,lab="child")

if (ps) postscript("pc-dag.eps",width=3,height=1.4)
graphit(node,r=.2)
if (ps) dev.off()

node <- list()
node[[1]] <- list(x=1,y=1,child=list(ind=2,lty=2),lab="parent")
node[[2]] <- list(x=2,y=1,parent=1,lab="child")
if (ps) postscript("stoch-dag.eps",width=3,height=1.4)
graphit(node,r=.2)
if (ps) dev.off()

## this one is the Ricker segment...

node <- list()
node[[1]] <- list(x=1,y=1,child=list(ind=7),lab=expression(alpha[1]))
node[[2]] <- list(x=1,y=1.5,child=list(ind=7),lab=expression(lambda[1]))
node[[3]] <- list(x=1,y=2,child=list(ind=8),lab=expression(alpha[2]))
node[[4]] <- list(x=1,y=2.5,child=list(ind=8),lab=expression(lambda[2]))
node[[5]] <- list(x=1,y=3,child=list(ind=9),lab=expression(alpha[3]))
node[[6]] <- list(x=1,y=3.5,child=list(ind=9),lab=expression(lambda[3]))

node[[7]] <- list(x=2,y=1.5,child=list(ind=11:12),parent=1:2,lab=expression(r))
node[[8]] <- list(x=2,y=2.25,child=list(ind=11:12),parent=3:4,lab=expression(K))
node[[9]] <- list(x=2,y=3.0,child=list(ind=11:12),parent=5:6,lab=expression(sigma[e]^2))

node[[10]] <- list(x=3,y=1.125,child=list(ind=11),lab=expression(n[t-2]))
node[[11]] <- list(x=3,y=1.825,child=list(ind=c(12,14),lty=c(1,2)),parent=c(7:9,10),lab=expression(n[t-1]))
node[[12]] <- list(x=3,y=2.625,child=list(ind=13:14,lty=c(1,2)),parent=c(7:9,11),lab=expression(n[t]))
node[[13]] <- list(x=3,y=3.375,parent=12,lab=expression(n[t+1]))

node[[14]] <- list(x=4,y=2.225,child=list(ind=15),parent=c(11:12,16),lab=expression(mu[tau]))
node[[15]] <- list(x=5,y=2.225,parent=c(14,16),lab=expression(y[tau]))

node[[16]] <- list(x=6,y=2.225,child=list(ind=15),parent=c(17,18),lab=expression(sigma^2))

node[[17]] <- list(x=7,y=1.975,child=list(ind=16),lab=expression(alpha[4]))
node[[18]] <- list(x=7,y=2.475,child=list(ind=16),lab=expression(lambda[4]))

setwd("~sw283/lnotes/StatProg/Gibbs")
ps <- FALSE
if (ps) postscript("ricker-graph.eps",width=7,height=3)
graphit(node,r=.2)
if (ps) dev.off()

#graphit(node,r=.2)
