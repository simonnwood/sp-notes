m0 <- lm(weight~group,PlantGrowth)
print(m0)
summary(m0)
class(m0)

methods(summary)

print.foo <- function(x) {
  cat(x$a-1.96*x$sd,"\n")
  cat(x$a+1.96*x$sd,"\n")
}

a <- list(a = 1:3, sd = c(.1,.2,.1))
class(a) <- "foo"
print(a)

x <- runif(20); y <- rpois(20,exp(2*x))
m <- glm(y~x,family=poisson)
class(m)

inherits(m,"glm")

