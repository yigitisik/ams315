# Linear Regression -------------------------------------------------------
install.packages(XQuartz)
rm(list=ls())
library(MASS) # This data is in the package MASS
fix(Boston) # 'fix' to fix the Boston data
names(Boston)
str(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)
lm.fit
summary(lm.fit)
names(lm.fit)

confint(lm.fit)
predict(lm.fit, data.frame(lstat = c(1,5,10)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(1,5,10)), interval = "prediction")
predict(lm.fit, data.frame(lstat = c(1,5,10)), interval = "none")

plot(Boston$lstat, Boston$medv)
abline(lm.fit, lwd = 3, col = "red")

plot(lm.fit)
par(mfrow = c(2,2)) # change the display into 2 x 2
plot(lm.fit)

# Multiple linear regression
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
str(Boston)
lm.fit = lm(medv ~. , data = Boston)
summary(lm.fit)
