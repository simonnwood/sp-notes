## scatterplots...

library(ggplot2)
head(mpg)
mpg$transmission <- rep("manual",nrow(mpg)) ## simplified transmission variable
mpg$transmission[grep("auto",mpg$trans)] <- "automatic" ## note automatics

par(mfrow=c(1,2),mar=c(4,4,1,1)) ## two panel window

plot(mpg$displ,mpg$hwy,xlab="displacement",ylab="highway mpg",
     pch=19,col=(mpg$transmission=="automatic")+1)


plot(mpg$displ,mpg$hwy,xlab="displacement",ylab="highway mpg",type="n") # empty plot
rect(0,0,8,50,col="lightgrey") # add a grey rectangle over plot area 
abline(v=2:7,h=seq(10,45,by=5),col="white") # add grid lines
points(mpg$displ,mpg$hwy,col=(mpg$transmission=="automatic")+1,pch=19,cex=.5) # data


a <- ggplot(mpg,aes(x=displ,y=hwy)) 
a <- a + geom_point(aes(colour=transmission)) 
a <- a + geom_smooth(method="gam",formula=y~s(x)) ## add a smooth as it's easy!
a    ## 'print' plot object 'a' - that is plot it!

b1 <- a + coord_cartesian(xlim=c(0,8),ylim=c(0,50))
b1

b2 <- a + scale_x_continuous(limits=c(2,6),name="displacement (litres)")
b2

postscript("some-plot.eps",width=8,height=3)
## the code to produce the plot goes here - e.g. from previous section
dev.off()

ggsave("some-plot.eps",width=5,height=3)


## univariate - continuous

library(ggplot2)
par(mfrow=c(1,2)) ## set up to allow 2 histograms side by side
hist(faithful$eruptions) ## default
hist(faithful$eruptions,breaks=quantile(faithful$eruptions,seq(0,1,length=15)),
     xlab="eruption time (mins)",main="Old Faithful geyser",col="purple")


n <- nrow(faithful);set.seed(8)
erupt <- faithful$eruption + rnorm(n*1000,sd=.2) ## simulate much bigger sample
hist(erupt,breaks=100,freq=FALSE) ## plot it

plot(density(faithful$eruptions,adjust=0.6),type="l",
xlab="eruption time (mins)",main="kernel density estimate")


ggplot(faithful,aes(x=eruptions,after_stat(density))) + ## data, base mappings etc
geom_histogram(bins=20,col="green",fill="purple") +     ## garish histogram
geom_density(adjust=.5,col="red")                       ## added kernel density estimate


par(mfrow=c(1,2))
acf(faithful$eruptions); pacf(faithful$eruptions)


## univariate discrete

d <- c(3362,428,290,369,978,1723,2471,3731,5607,7926,12934,20688,29905,
       40456,53579,84784,106448,136269,150325,178684) ## all deaths
cd <- c(3,1,3,10,26,65,133,257,438,740,1418,2587,4250,6337,8528,13388,
        18622,25220,27718,30247)  ## Covid-19 deaths
names(cd) <- names(d) <- c("0","1-4","5-9","10-14","15-19","20-24","25-29",
           "30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69",
           "70-74","75-79","80-84","85-89","90+") ## ONS age classes

par(mfrow=c(1,2))
barplot(d,las=2,main="All deaths Mar 20-Aug 21")
barplot(cd,ylim=c(0,max(d)),las=2,main="Covid deaths Mar 20-Aug 21")

par(mfrow=c(1,2))
D <- rbind(cd,d-cd) ## create matrix of counts to plot
rownames(D) <- c("covid","not covid")
barplot(D,las=2,main="All deaths Mar 20-Aug 21",legend.text=TRUE,
args.legend=list(x=7,y=150000)) 
barplot(D[,1:10],las=2,main="Age 0-44 deaths  Mar 20-Aug 21")

library(ggplot2)

n <- length(d)
age <- factor(1:n);levels(age) <- names(d) ## age class as a factor
ggdat <- data.frame(age=c(age,age),deaths=c(cd,d-cd),
         type=c(rep("covid",n),rep("not covid",n))) ## tidy data frame
ggplot(ggdat,aes(x=age,y=deaths,fill=type)) + geom_col() +
       labs(title="All UK deaths Mar 2020 - Aug 2021")


## boxplots
head(ToothGrowth)
par(mfrow=c(1,3),mar=c(5,5,1,1))
boxplot(len~supp,data=ToothGrowth,cex.lab=1.5)
boxplot(len~dose,data=ToothGrowth,cex.lab=1.5)
boxplot(len~dose+supp,data=ToothGrowth,cex.lab=1.5)

library(ggplot2)

ToothGrowth$comb <- with(ToothGrowth,paste(supp,dose))

ggplot(ToothGrowth,aes(y=len,group=comb)) + geom_boxplot()

ggplot(ToothGrowth,aes(y=len,x=as.factor(dose))) + geom_violin()

## 3D plots

foo <- function(x,y) { ## function to be visualized
  r <- sqrt(x^2+y^2)
  exp(-r*2)*cos(r*10)
} ## foo
n <- 50; x <- seq(-1,1,length=n); y <- seq(-1,1,length=n)
xy = expand.grid(x=x,y=y) ## regular x,y grid over 
xy$z <- foo(xy$x,xy$y)    ## evaluate foo on the grid
z <- matrix(xy$z,n,n)     ## put evaluated function in a matrix
par(mfrow=c(1,3),mar=c(4,4,1,1)) ## divide plot devise into 3 columns
persp(x,y,z,theta=30,phi=30,col="lightblue",shade=.3) 
image(x,y,z)
contour(x,y,z,xlab="x",ylab="y")

ggplot(xy,aes(x=x,y=y)) + geom_raster(aes(fill=z)) + geom_contour(aes(z=z))
