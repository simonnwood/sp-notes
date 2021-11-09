setwd("~sw283/lnotes/StatProg/sp-notes")
a <- scan("data.txt",what="a")
b <- scan("data.txt",what=list("a",1,1),skip=1); b
names(b) <- scan("data.txt",what="c",n=3)
d <- data.frame(b);d

read.table("data.txt",header=TRUE)
read.table("data.txt",skip=1)



