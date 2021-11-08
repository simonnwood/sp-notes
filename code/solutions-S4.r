## 1. Either the following, or similar on 'pow'...
poem1 <- gsub(".","",gsub(",","",poem,fixed=TRUE),fixed=TRUE)

## 2. Following, or more structured without nesting...
cat(gsub(".",".\n",gsub(", ",",\n",poem,fixed=TRUE),fixed=TRUE))

## 3.
set.seed(0);y <- rt(100,df=4)
hist(y)
my <- mean(y);sdy <- sd(y)
yt <- y[y > my-2*sdy & y < my+2*sdy] ## exclude far out points
mean(yt)

## 4.

trmean <- function(y,k=2) {
## Function to compute a trimmed mean after removal of points
## more that k standard deviations from mean...
  my <- mean(y);sdy <- sd(y)
  k <- abs(k) ## deal with silly input k
  yt <- y[y > my-k*sdy & y < my+k*sdy] ## exclude far out points
  if (length(yt)==0) return(NA) ## deal with no data left after trim 
  mean(yt) ## return trimmed mean
}

trmean(y)