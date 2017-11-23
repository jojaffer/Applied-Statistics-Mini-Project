library(readxl)
detroit <- read_excel("~/Documents/Graduate-Math-Projects/detroit.xlsx")

### Visual Analysis
plot(homicide ~ police, data = detroit)
abline(lm(homicide ~ police, data = detroit))
plot(homicide ~ umemp, data = detroit)
abline(lm(homicide ~ umemp, data = detroit))
plot(homicide ~ register, data = detroit)
abline(lm(homicide ~ register, data = detroit))
plot(homicide ~ weekly, data = detroit)
abline(lm(homicide ~ weekly, data = detroit))

### Linear Regression Models
reg.fit <- lm(homicide ~ police, data = detroit)
reg.fit1 <- lm(homicide ~ umemp, data = detroit)
reg.fit2 <- lm(homicide ~ register, data = detroit)
reg.fit3 <- lm(homicide ~ weekly, data = detroit)
summary(reg.fit)
summary(reg.fit1)
summary(reg.fit2)
summary(reg.fit3)

### Multiple Linear Regression Model
reg.fit4 <- lm(formula = homicide ~ police + umemp + register + weekly, data = detroit)
library(MASS)
step <- stepAIC(reg.fit4, direction = "both")
summary(lm(homicide ~ police + register, data = detroit))
predict(reg.fit4, newdata = detroit[10,], interval = "prediction")

### Regression Plots
plot(reg.fit4)
