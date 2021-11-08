getwd()
setwd("/home/sw283/lnotes/StatProg/sp-notes/code")

source("hello.r")

a <- "some stuff"
alice <- list(age=45,height=1.65,weight=60)
Ba <- matrix(1:16,4,4) 

dump(c("a","alice","Ba"),file="stuff.R")
rm(list=ls());ls()

source("stuff.R")
ls()

a <- scan("data.txt",what="c");a

b <- scan("data.txt",what=list("a",1,1),skip=1); b

names(b) <- scan("data.txt",what="c",n=3)
d <- data.frame(b); d

d <- read.table("data.txt",header=TRUE)

d$name[1] <- "susan"

write.table(d,file="data1.txt")

A <- matrix(1:6,3,2); y <- rnorm(100) ## create some objects
save(A,y,file="stuff.Rd") ## save them to a binary file
rm(y,A)
## delete the original objects
load("stuff.Rd") ## load them back in again from the file

raw <- readLines("https://michaelgastner.com/DAVisR_data/homicides.txt")


