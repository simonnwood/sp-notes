seir <- function(n=10000,ni=10,nt=100,gamma=1/3,delta=1/5,bmu=5e-5,bsc=1e-5) {
## SEIR stochastic simulation model.
## n = population size; ni = initially infective; nt = number of days
## gamma = daily prob E -> I; delta = daily prob I -> R;
## bmu = mean beta; bsc = var(beta) = bmu * bsc
  x <- rep(0,n) ## initialize to susceptible state
  beta <- rgamma(n,shape=bmu/bsc,scale=bsc) ## infectivity
  x[1:ni] <- 2 ## create some infectives
  S <- E <- I <- R <- rep(0,nt)
  S[1] <- n-ni;I[1] <- ni
  for (i in 2:nt) { ## loop over days
    u <- runif(n)
    x[x==2&u<delta] <- 3 ## I -> R with prob delta
    x[x==1&u<gamma] <- 2 ## E -> I with prob gamma
    x[x==0&u<beta*I[i-1]] <- 1 ## S -> E with prob beta*I[i-1]
    S[i] <- sum(x==0); E[i] <- sum(x==1)
    I[i] <- sum(x==2); R[i] <- sum(x==3)
  }
  list(S=S,E=E,I=I,R=R,beta=beta)
} ## seir

ps <- FALSE
if (ps) postscript("seir.eps",width=8,height=3.5)

par(mfcol=c(2,3),mar=c(4,4,1,1))
epi <- seir(bmu=7e-5,bsc=1e-7);hist(epi$beta,xlab="beta",main="")
plot(epi$S,ylim=c(0,max(epi$S)),xlab="day",ylab="N")
points(epi$E,col=4);points(epi$I,col=2)

epi <- seir(bmu=7e-5,bsc=7e-5);hist(epi$beta,xlab="beta",main="")
plot(epi$S,ylim=c(0,max(epi$S)),xlab="day",ylab="N")
points(epi$E,col=4);points(epi$I,col=2)

epi <- seir(bmu=4.6e-5,bsc=2e-6);hist(epi$beta,xlab="beta",main="")
plot(epi$S,ylim=c(0,max(epi$S)),xlab="day",ylab="N")
points(epi$E,col=4);points(epi$I,col=2)

if (ps) dev.off()

n.rep <- 10000; n.ci <- 0; n <- 20;p <- .2
for (i in 1:n.rep) {
  x <- rbinom(1,n,p)
  p.hat <- x/n
  sig.p <- sqrt(p.hat*(1-p.hat)/n)
  if (p.hat - 1.96*sig.p <p && p.hat+1.96*sig.p > p) n.ci <- n.ci + 1
}
n.ci/n.rep
