## basic vector examples

a3d <- c(TRUE,FALSE,FALSE) ## create an example logical vector
class(a3d)   ## its class
typeof(a3d)  ## its type
a3d[2:3]  ## print its 2nd and 3rd elements 
a3d[2] <- TRUE ## change the 2nd element
a3d ## print the resulting vector

bb <- 1:10 ## create numeric example
class(bb) ## check class
bb[c(1,4,9)] <- c(.1,-3,2.2) ## change selected elements
class(bb)   ## note automatic change of class 
typeof(bb)  ## how it is actually being stored
bb[3:8] ## print elements 3:8 

## vectorised operations
set.seed(2)
a <- runif(6);b <- runif(6)
a;b
c <- sin(a) * b
c
c <- a * b + 2
c

## recycling

a <- 1:4 # a 4-vector 
b <- 5:6 # a 2-vector
a*b      # multiplication with recycling
b <- 5:7 # a 3 vector
a*b      # multiplication with recycling

## array 
a <- array(1:24,c(3,2,4))
d <- dim(a)
a[3+1*d[1]+2*d[1]*d[2]]

## e.g. access elements (1,2,4), (1,1,3) and (3,1,1)
i <- c(1,1,3);j <- c(2,1,1);k <- c(4,3,1)
a[i,j,k] ## wrong
a[i+(j-1)*d[1]+(k-1)*d[1]*d[2]] ## right


B <- matrix(1:6,2,3); B ## create a matrix (filled by col)
B[1,2] <- -1            ## change elment 1,2
a <- c(.3,-1.2,2.3)    ## a 3-vector
B %*% a                 ## matrix multiplication
B*a                     ## element wise multiplication with recycling!!

## list

stuff <- list(a=1:6,txt="furin cleavage site",l2 = function(x) log(x^2),
         more = list(a="er",b=42))
stuff[[1]]
stuff[["l2"]]
stuff$a
stuff[c(1,2)]

## factors

fac <- factor(c("fred","sue","sue","bill","fred"))
class(fac)
fac         ## default printing for class factor 
levels(fac) ## extract the levels attribute
as.numeric(fac)  ## look at the underlying coding




## data.frame

dat <- data.frame(y = c(.3,.7,1.2),x = 1:3,fac = factor(c("a","b","a")))
dat      ## a data.frame
dim(dat) ## like a matrix
dat$fac  ## and lie a list!

## attributes

attributes(PlantGrowth)
A <- matrix(1:6,2,3)
attributes(A)
attr(A,"dim")
attr(A,"foo") <- list(1:3,"fred")

str(dat)

## operators...

a <- c(FALSE,TRUE,TRUE,FALSE,FALSE)
b <- c(TRUE,TRUE,FALSE,FALSE,TRUE)
a;b;a&b;a&&b
a;b;a|b;a||b
a;!a

A <- matrix(1:4,2,2);B <- matrix(1:6,3,2)
A;B;A%x%B ## Kronecker product

## if/loop

if (runif(1)>.5) {
  cat("heads")
} else {
  cat("tails")
}

if (runif(1)>.5) {
  cat("heads\n")
} else if (runif(1)>.7) {
  cat("tails\n")
} else cat("also tails\n")

if ((rnorm(1)>0||rnorm(1)>0) && runif(1)<2/3) {
  cat("heads")
} else {
  cat("tails")
}

coin <- if (runif(1)>.5) "heads" else "tails"
coin


vec <- c("I","am","bored")
for (a in vec) cat(a," ")

n <- 100;p <- rep(.5,n)
for (i in 2:n) p[i] <- 3.7*p[i-1]*(1-p[i-1]) 
plot(p,type="l")

p[1] <- .7
for (i in 2:n) {
  p[i] <- 3.7 * p[i-1] * (1 - p[i-1])
  if (p[i] > .924) break
}  
plot(p[1:i],type="l")

i <- 1
while(p[i] < .924) {
  i <- i + 1
  p[i] <- 3.7 * p[i-1] * (1 - p[i-1])
}

i <- 1
repeat {
  i <- i + 1
  p[i] <- 3.7 * p[i-1] * (1 - p[i-1])
  if (p[i] > .924 || i>99) break
}




## Functions

foo <- function(x,a,b) x/a + b

mat.fun <- function(A,fun=I,...) {
  ea <- eigen(A,symmetric=TRUE)
  ea$vectors %*% (fun(ea$values,...)*t(ea$vectors)) ## note use of re-cycling rule!
}

mat.fun(B,fun=foo,a=2,b=3)
