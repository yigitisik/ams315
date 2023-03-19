# Q1 ----------------------------------------------------------------------
rm(list=ls())

grade <- c(84,98,65,83,77,62,95,58,90,86)
n <- length(grade)
ybar <- mean(grade)
s <- sd(grade)

n > 30 # We use t distribution
df <- n-1

# (1) 95% confidence interval
c(ybar-qt(p=0.025, df=df, lower.tail=FALSE)*s/sqrt(n),
  ybar+qt(p=0.025, df=df, lower.tail=FALSE)*s/sqrt(n))

# (2) Hypothesis test - H0: mu >= 85 vs H1: mu < 85
alpha <- 0.05

## R.R. - Reject H0 if t < critical value
critical_value <- qt(alpha, df, lower.tail=TRUE)
# -t_alpha
critical_value
t <- (ybar - 85) / (s/sqrt(n))
# T.S.
t
# Reject H0?
t < critical_value

## p value - Reject H0 if p value < alpha
p_value <- pt(t, df, lower.tail=TRUE)
# p value
p_value
# Reject H0?
p_value < alpha

## R function
t_test <- t.test(grade, mu=85, alternative="less")
p_value <- t_test$p.value
# p value
p_value
# Reject H0?
p_value < alpha
