# CHAPTER 6 ---------------------------------------------------------------
rm(list=ls())

gradeA <- c(84,98,65,83,77,62,95,58,90,86)
gradeB <- c(85,88,78,98,87,67,78)

## H0: E(A) = E(B) vs H1: E(A) not equal to E(B)
# normal or not / large sample or not / equal variance or not

# check normality
# reject H0 if p-value < 0.05 otherwise normal
shapiro.test(gradeA)
shapiro.test(gradeB)

# Normal distributed, variance not equal
test <- t.test(gradeA, gradeB, alternative = "two.sided", var.equal = FALSE)
# reject H0?
test$p.value < 0.05

# pooled variance (equal variance)
test <- t.test(gradeA, gradeB, alternative = "two.sided", var.equal = TRUE)
# reject H0?
test$p.value < 0.05

## H0: E(A) = E(B) vs H1: E(A) < E(B)
# variance not equal
t.test(gradeA, gradeB, alternative = "less", var.equal = FALSE)

#if not normal
wilcox.test(gradeA, gradeB, alternative = "two.sided")


# CHAPTER 7 ---------------------------------------------------------------

rm(list=ls())

gradeA <- c(84,98,65,83,77,62,95,58,90,86)
gradeB <- c(85,88,78,98,87,67,78)

## H0: E(A) = E(B) vs H1: E(A) not equal to E(B)
var.test(gradeA, gradeB, alternative = "two.sided")
t.test(gradeA, gradeB, alternative = "greater", var.equal = TRUE)

## H0: var(A) = 180 vs H1: var(A) > 180
#install.packages("EnvStats")
library(EnvStats)
varTest(gradeA, alternative = "greater", sigma.square = 180)


# CHAPTER 11 --------------------------------------------------------------
rm(list=ls())

x <- seq(1, 10, 0.1)
y <- 2*x + rnorm(length(x), 0, 0.1) # true model
x
y
plot(x,y)

# y (indep.) - x (dep.) / giving the linear model
l <- lm(y~x)
# str(l)
hist(l$residuals)
l$coefficients
summary(l)
anova(l)



