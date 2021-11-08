set.seed(0) ## just so you can reproduce what I got
sample(1:100,10)

set.seed(3);runif(5)
runif(5)
set.seed(3);runif(5)

sample(1:100,10,replace=TRUE)

PlantGrowth
ii <- sample(1:nrow(PlantGrowth),10)
PlantGrowth[ii,]

sample(1:100,10,prob=1/1:100)
sum(1/1:25)/sum(1/100)

## Birthday problem...

n <- 1000000 ## number of classes
bd <- matrix(sample(1:366,30*n,replace=TRUE,prob=c(rep(4,365),1)),
             n,30) ## birthdays
mean(apply(bd,1,function(x) length(unique(x)))!=30)


## distributions...

?distributions
rnorm(3,mean=c(1,3.2,-0.5),sd=c(1,1,2.5))

## Covid infection to death distrbution...
n <- 100000
d <-  rlnorm(n,1.63,.5) + rgamma(n,shape=4.45,scale=4)
mean(d<7)
hist(d,breaks=0:100)