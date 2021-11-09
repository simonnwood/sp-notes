## 15.3...

hist(faithful$eruptions,main="",freq=FALSE,breaks=25,xlab="eruption duration (min)")
lines(density(faithful$eruptions,adjust=.4))

