
# QUESTION 5 --------------------------------------------------------------
read.table(file = "HW6 Data.csv", header = TRUE, sep = ",")

lm <- lm(formula = InfctRsk ~ Stay + Age + Xray)
lm
str(lm)
summary(lm)

par(mfrow = c(2,2)) 
plot(lm)

confint(lm)
predict(lm, data.frame(lstat = c(1,5,10)), interval = "prediction")

lm = lm(medv ~ lstat + Stay + Age + Xray, data = table)
summary(lm)
lm = lm(medv ~. , data = table)
summary(lm)

