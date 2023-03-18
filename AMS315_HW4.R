# QUESTION 6 --------------------------------------------------------------
upstream <- c(5.2, 4.8, 5.1, 5.0, 4.9, 4.8, 5.0, 4.7, 4.7, 5.0, 4.6, 5.2, 5.0, 4.9, 4.7)
downstream <- c(3.2, 3.4, 3.7, 3.9, 3.6, 3.8, 3.9, 3.6, 4.1, 3.3, 4.5, 3.7, 3.9, 3.8, 3.7)

## part a to find differences of mean for the paired data
diff <- upstream - downstream
diff
meandiff <- mean(diff)
meandiff

## part b 
shapiro.test(upstream)
shapiro.test(downstream)
# p-values from shapiro provide us that both sets are normal

# H0: meandiff = 0 vs H1: meandiff not equal to 0 
# normal distribute, since no info given about variance, try FALSE
test <- t.test(upstream, downstream, alternative = "two.sided", var.equal = FALSE)
# reject H0?
test$p.value < 0.05
# test gives true, so we reject H0 and accept H1 which shows meandiff is not zero -> sufficient evidence to indicate difference

## part c

# estimate size of meandiff with C.I. 99%
tvalue <- 2.048
sp <- 0.262
n_up <- 15
n_down <- 15
estimate_low <- ( meandiff - tvalue*sp*sqrt((1/n_up)+(1/n_down)) )
estimate_up <- ( meandiff + tvalue*sp*sqrt((1/n_up)+(1/n_down)) )
estimate_low
estimate_up
# QUESTION 7 --------------------------------------------------------------
library(EnvStats)
p1<-c(130,135,135,131,129,135,126,136,127,132)
p2<-c(154,144,147,150,155,153,149,139,140,141)
var.test(p1, p2, alternative = "less", conf.level = 0.95)

