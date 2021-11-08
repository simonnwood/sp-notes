X <- model.matrix(weight~group,PlantGrowth)

library(ggplot2)
mpg1 <- data.frame(mpg) ## convert to regular data frame
head(mpg1)

mpg1$trans <- factor(gsub("\\(.*\\)","",mpg1$trans)) ## convert to 2 level
head(model.matrix(cty~trans+displ,mpg1)) ## get model matrix

head(model.matrix(cty~trans*displ,mpg1))

m1 <- lm(cty ~ trans + displ, mpg1)
plot(m1)
plot(mpg1$displ,residuals(m1))

m2 <- lm(cty~trans+displ+I(displ^2),mpg1)
plot(m2)
summary(m2)

m3 <- lm(cty~(trans+displ+I(displ^2))^2,mpg1)
m0 <- lm(cty~displ+I(displ^2),mpg1)
anova(m0,m3) ## F ratio test of H_0: m0 is correct vs H_1: we need m3

summary(lm(cty~trans,mpg1))