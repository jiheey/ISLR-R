library(ISLR)

# Ex8

head(Auto)
fix(Auto)
dim(Auto) 
names(Auto)

cor(Auto$mpg, Auto$horsepower)
cor.test(Auto$mpg, Auto$horsepower)

lm.fit1 = lm(mpg~horsepower, data=Auto)
summary(lm.fit1)
predict(lm.fit1, data.frame(horsepower=98)) 
predict(lm.fit1, data.frame(horsepower=98), interval="confidence") #신뢰구간
predict(lm.fit1, data.frame(horsepower=98), interval="prediction") #예측구간
plot(horsepower, mpg) 
abline(lm.fit1, lwd=2, col="red")
par(mfrow=c(2,2))
plot(lm.fit1)
which.max(hatvalues(lm.fit1))  #117, 116





# Ex9

pairs(Auto)
library(corrplot)
par(mfrow=c(1,1))
x = cor(Auto[,1:8])
corrplot(x, method="number")

lm.fit2 = lm(mpg~.-name, data=Auto) 
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
which.max(hatvalues(lm.fit2))
summary(lm(mpg~.-name+cylinders:displacement, data=Auto))
lm.fit3 = lm(mpg~horsepower+I(horsepower^2))
lm.fit4 = lm(mpg~log(horsepower),data=Auto)
lm.fit5 = lm(mpg~sqrt(horsepower),data=Auto)
plot(lm.fit5)
summary(lm(mpg~.-name+sqrt(horsepower), data=Auto))





# Ex13

set.seed(1)
x = rnorm(100, 0, 1)
x
eps = rnorm(100, 0, 0.25)
eps
y = -1+0.5*x+eps
length(y)
plot(x, y)
model = lm(y~x)
summary(model)
plot(x, y)
abline(model, lwd=2, col="red")
legend("topleft",lty=1,col='red',legend='최소제곱선')
model2 = lm(y~x+I(x^2))
anova(model, model2)
confint(model, level=0.95)


set.seed(1)
x = rnorm(100, 0, 1)
x
eps = rnorm(100, 0, 1)
eps
y = -1+0.5*x+eps
length(y)
plot(x, y)
model = lm(y~x)
summary(model)
plot(x, y)
abline(model, lwd=2, col="red")
legend("topleft",lty=1,col='red',legend='최소제곱선')
model2 = lm(y~x+I(x^2))
anova(model, model2)
confint(model, level=0.95)

set.seed(1)
x = rnorm(100, 0, 1)
x
eps = rnorm(100, 0, 0.1)
eps
y = -1+0.5*x+eps
length(y)
plot(x, y)
model = lm(y~x)
summary(model)
plot(x, y)
abline(model, lwd=2, col="red")
legend("topleft",lty=1,col='red',legend='최소제곱선')
model2 = lm(y~x+I(x^2))
anova(model, model2)
confint(model, level=0.95)



# Ex14
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
plot(x1, x2)
cor(x1,x2)
model2 = lm(y~x1+x2)
summary(model2)
model3 = lm(y~x1)
summary(model3)
model4 = lm(y~x2)
summary(model4)

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
model5 = lm(y~x1+x2)
summary(model5)
model6 = lm(y~x1)
summary(model6)
model7 = lm(y~x2)
summary(model7)

par(mfrow=c(2,2))
plot(model5)
plot(model6)
plot(model7)
