install.packages(c("ISLR", "gam", "akima"))

library(ISLR)
library(gam)
library(akima)
library(splines)


data(Wage)
Wage

age.grid = 18:80

## wage ~ age
# Local Regression
# loess
fit1 = loess(wage ~age, data = Wage, span = .2)
fit2 = loess(wage ~age, data = Wage, span = .5)
fit3 = loess(wage ~age, data = Wage, span = .75)

plot(Wage$age, Wage$wage, col = "gray")
pred1 = predict(fit1, newdata = data.frame(age = age.grid))
pred2 = predict(fit2, newdata = data.frame(age = age.grid))
pred3 = predict(fit3, newdata = data.frame(age = age.grid))

lines(age.grid, pred1, col = "blue")
lines(age.grid, pred2, col = "orange")
lines(age.grid, pred3, col = "red")
legend("topright", legend = c("S = .2", "S = .5", "S = .75"), 
       col = c("blue", "orange", "red"), cex = .3, lty=1, lwd=2)

###############
# GAM's
fit1 = lm(wage ~ ns(year, df=4) + ns(age, df=5) + education, data = Wage)
coef(summary(fit1))

plot.Gam(fit1)
