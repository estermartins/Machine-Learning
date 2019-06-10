## Modelos não lineares
## Machine Learning
## Prof. Neylson Crepalde
## Ester Pereira Martins


install.packages("ISLR", "splines")
library(ISLR)
library(splines)

data(Wage)
head(Wage)


## Vamos estimar um modelo polinomial de grau 4 
## para identificar o efeito da idade sobre a renda
## age
## wage

fit1 = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
coef(summary(fit1))
# efeito da idade sobre  arenda, a medida que a idade cresce a renda cresce

# Extraindo os extremos da variável
age.range = range(Wage$age)
# Monta a sequência de idades entre os extremos
age.grid = seq(age.range[1], age.range[2])
# Fazer a predição para cada idade
pred = predict(fit1, newdata = list(age = age.grid), se = T)
pred

plot(Wage$age, Wage$wage, col = "gray")
lines (age.grid, pred$fit,
       lwd = 1, col = "red",
       lty = 1)


inter.conf = cbind(pred$fit + 2*pred$se.fit,
                   pred$fit - 2*pred$se.fit)

matlines(age.grid, inter.conf, col ="blue", lty = "dashed")

##
fit2 = lm(wage ~ poly(age, 4), data=Wage)
coef(summary(fit2))

pred2 = predict(fit2, newdata = list(age=age.grid), se = T)
pred2
plot(Wage$age, Wage$wage, col="gray")
lines(age.grid, pred2$fit, lwd=2, col="red")
inter.conf2 = cbind(pred2$fit + 2*pred2$se.fit, pred2$fit - 2*pred2$se.fit)
matlines(age.grid, inter.conf2, col="blue", lty="dashed")

## Step Functions
## COrta a variável em 4 partes
table(cut(Wage$age, breaks = c(18,35,45,60,80)))
fit3 = lm(wage ~ cut(age, breaks = c(18,35,45,60,80)), data=Wage)
coef(summary(fit3))

pred3 = predict(fit3, newdata = list(age=age.grid), se = T)
pred3
inter.conf3 = cbind(pred3$fit + 2*pred3$se.fit, pred3$fit - 2*pred3$se.fit)
plot(Wage$age, Wage$wage, col="gray")
lines(age.grid, pred3$fit, lwd=2, col="red")
matlines(age.grid, inter.conf3, col="blue", lty="dashed")
## modelo de polinominal dá mais aderência
