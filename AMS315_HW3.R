# QUESTION 7 --------------------------------------------------------------

# H0 is having less than or equal to 500
# H1 is having more than 500
n <- 36
s <- 124
ybar <- 553
testmean <- 500

#T.S
z <- ((ybar-testmean) / (s / sqrt (n)) )
z

#pvalue
p_value <- pnorm(z, mean=0, sd=1, lower.tail = FALSE)
p_value

# pvalue(0.0051) less than given 0.025 so we reject H0 --> greater than 500


# QUESTION 8 --------------------------------------------------------------

# PART A
set <- c(0.814, 0.208, 0.446, 0.591, 0.335, 0.476, 0.424, 0.229, 0.588, 0.514)
mean <- mean(set)
mean
sd <- sd(set)
n <- length(set)
df <- n-1

# PART B 
# %95 C.I.for mean
c(mean-qt(p=0.025, df=df, lower.tail=FALSE)*sd/sqrt(n),
  mean+qt(p=0.025, df=df, lower.tail=FALSE)*sd/sqrt(n))

# PART C

# H0: avg less than or equal to .3 
# H1: avg more than .3

#T.S.
testingmean <- .3
t <- ((mean - testingmean) / (sd / sqrt(n)))
t

t_alpha <- qt(p=0.01, df=df, lower.tail=FALSE) # given alpha 0.01
t_alpha

# we reject if t >= t_alpha and here we get 2.83 > 2.82 so we reject H0
### END