library(ggplot2)
head(mpg)
mpg$transmission <- rep("manual",nrow(mpg))
mpg$transmission[grep("auto",mpg$trans)] <- "automatic"
setwd("~sw283/lnotes/StatProg/sp-notes")
ps <- FALSE
if (ps) postscript("mpg-base.eps",width=8,height=3)
par(mfrow=c(1,2),mar=c(4,4,1,1)) 
plot(mpg$displ,mpg$hwy,xlab="displacement",ylab="highway mpg",
     pch=19,col=(mpg$transmission=="automatic")+1)

plot(mpg$displ,mpg$hwy,type="n",xlab="displacement",ylab="highway mpg")
rect(0,0,8,50,col="lightgrey")
abline(v=2:7,h=seq(10,45,by=5),col="white")
points(mpg$displ,mpg$hwy,col=(mpg$transmission=="automatic")+1,pch=19,cex=.5)
if (ps) dev.off()

a <- ggplot(mpg,aes(x=displ,y=hwy)) + geom_point(aes(colour=transmission)) +
     geom_smooth(method=gam,formula=y~s(x))
#ggsave("mpg-gg.pdf",width=6,height=3)


## univariate

ps <- FALSE
if (ps) postscript("geyser-base.eps",width=8,height=3.8)
par(mfrow=c(1,2)) 
hist(faithful$eruptions) ## default plot
hist(faithful$eruptions,breaks=quantile(faithful$eruptions,seq(0,1,length=15)),
xlab="eruption time (mins)",main="Old Faithful geyser",col="purple")
if (ps) dev.off()

ps <- TRUE
if (ps) postscript("geyser-kernel.eps",width=8,height=3.8)
par(mfrow=c(1,2)) 
n <- nrow(faithful);set.seed(8)
erupt <- faithful$eruption + rnorm(n*1000,sd=.2)
hist(erupt,breaks=100,freq=FALSE)
plot(density(faithful$eruptions,adjust=.6),type="l",xlab="eruption time (mins)",
     main="kernel denisty estimate",col=2)
if (ps) dev.off()

hist(faithful$eruptions,breaks=30)
plot(density(faithful$eruptions,adjust=1),type="l",xlab="eruption time (mins)",
     main="kernel denisty estimate")

a <- ggplot(faithful,aes(x=eruptions,after_stat(density))) +
geom_histogram(bins=20,col="green",fill="purple") +
geom_density(adjust=.5,col="red")

#ggsave(a,file="geyser-gg.eps",width=6,height=3)

## exercise using functions seq, rowSums, matrix, rep and dnorm,
## create a kernel density estimation function... 
kernel <- function(x,h=1,xlim=range(x),n=100) {
  x0 <- seq(xlim[1],xlim[2],length=n);m <- length(x)
  y <- rowSums(matrix(dnorm(rep(x0,m),mean=rep(x,each=n),sd=h),n,m))
  list(x=x0,y=y)
}
plot(kernel(faithful$eruptions,h=.2,xlim=c(1,6)),type="l")

if (ps) postscript("geyser-acf.eps",width=8,height=3.8)
par(mfrow=c(1,2)) 
acf(faithful$eruptions); pacf(faithful$eruptions)
if (ps) dev.off()
## 1-D categorical
d <- c(3362,428,290,369,978,1723,2471,3731,5607,7926,12934,20688,29905,40456,53579,84784,106448,136269,150325,178684)

## covid.deaths
cd <- c(3,1,3,10,26,65,133,257,438,740,1418,2587,4250,6337,8528,13388,18622,25220,27718,30247)

names(cd) <- names(d) <- c("0","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+")

if (ps) postscript("deaths1.eps",width=11,height=4)
par(mfrow=c(1,2))
barplot(d,las=2,main="All deaths Mar 20-Aug 21")
barplot(cd,ylim=c(0,max(d)),las=2,main="Covid deaths Mar 20-Aug 21")
if (ps) dev.off()

if (ps) postscript("deaths2.eps",width=11,height=4)
## png("deaths2.png",width=1200,height=300)
par(mfrow=c(1,2))
D <- rbind(cd,d-cd) ## create matrix of counts to plot
rownames(D) <- c("covid","not covid")
## Add in a legend identifying colours
barplot(D,las=2,main="All deaths Mar 20-Aug 21",legend.text=TRUE,
args.legend=list(x=7,y=150000,xjust=.5)) 
barplot(D[,1:10],las=2,main="Age 0-44 deaths  Mar 20-Aug 21")
if (ps) dev.off()

## ggplot (insists on tidy data), geom_bar would expect to have to count up
## occurances of a discrete variable in a data frame. Clearly ridiculous to
## expand our 20 observations to .85 Million, so use geom_col

n <- length(d)
age <- factor(1:n);levels(age) <- names(d)
ggdat <- data.frame(age=c(age,age),deaths=c(cd,d-cd),type=c(rep("covid",n),rep("not covid",n))) ## tidy data frame
b <- ggplot(ggdat,aes(x=age,y=deaths,fill=type)) + geom_col() +
labs(title="All UK deaths Mar 2020 - Aug 2021")
b
#ggsave(b,file="deaths3.eps",width=11,height=4)

## other bivariate plots

head(ToothGrowth)
if (ps) postscript("boxplot1.eps",width=10,height=3)
par(mfrow=c(1,3),mar=c(5,5,1,1))
boxplot(len~supp,data=ToothGrowth,cex.lab=1.5)
boxplot(len~dose,data=ToothGrowth,cex.lab=1.5)
boxplot(len~dose+supp,data=ToothGrowth,cex.lab=1.5)
if (ps) dev.off()

ToothGrowth$comb <- with(ToothGrowth,paste(supp,dose))

## can also use x = comb, but that won't work if numeric
a <- ggplot(ToothGrowth,aes(y=len,x=comb)) + geom_boxplot()
a
b <- ggplot(ToothGrowth,aes(y=len,x=as.factor(dose))) + geom_violin()
b
# ggsave(a,file="boxplot2.eps",width=6,height=4)
# ggsave(b,file="violin.eps",width=4,height=4)

## Some 3D plotting...
foo <- function(x,y) {
  r <- sqrt(x^2+y^2)
  exp(-r*2)*cos(r*10)
} ## foo

n <- 50
x <- seq(-1,1,length=n)
y <- seq(-1,1,length=n)
xy = expand.grid(x=x,y=y)

xy$z <- foo(xy$x,xy$y)
z <- matrix(xy$z,n,n)

if (ps) postscript("3d.eps",width=11,height=4)
par(mfrow=c(1,3),mar=c(4,4,1,1))
persp(x,y,z,theta=30,phi=30,col="lightblue",shade=.3)
image(x,y,z)
contour(x,y,z,xlab="x",ylab="y")
if (ps) dev.off()

a <- ggplot(xy,aes(x=x,y=y)) + geom_raster(aes(fill=z)) + geom_contour(aes(z=z))
# ggsave(a,file="gg-contour.eps",width=5,height=4)