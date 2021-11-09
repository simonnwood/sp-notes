## 16.1.1...


n <- 60
nrep <- 1000
nb <- 1000
q.true <- diff(qnorm(c(.25,.75)))
pcp <- bcp <- 0
qb <- rep(0,nb)
for (j in 1:nrep) {
  y <- rnorm(n) ## simulated sample
  q <- as.numeric(diff(quantile(y,c(.25,.75)))) ## sample IQR
  for (i in 1:nb) { ## bootstrap loop, computing BS IQR
    qb[i] <- as.numeric(diff(quantile(sample(y,n,replace=TRUE),c(.25,.75))))
  }
  pci <- quantile(qb,c(.025,.975)) ## percentile interval
  b <- q - pci[1]; c <- pci[2] - q ## get upper and lower interval margins
  if (q-c < q.true && q+b > q.true) bcp <- bcp + 1 ## basic interval ok?
  if (pci[1] < q.true && pci[2] > q.true) pcp <- pcp + 1 ## percentile ok?
}

pcp/nrep;bcp/nrep ## coverage probabilities

## So neither is great here - the percentile interval exludes the truth only
## half as often as it should, and the basic interval excludes the truth twice
## as often as it should. But given that the intervals are the same length,
## the percentile version is clearly preferable in this case.


