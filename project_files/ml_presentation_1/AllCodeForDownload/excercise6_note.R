####6.a

# we first install the package required and set.seed
set.seed(1)
library(ISLR)
library(boot)

# then we creat a loop to check the cv errors for age from the power from 1 to 10, ,then we add each corresponding
# cv error to the all.deltas, finds out the power shows the lowest cv error
# lowest cv error, 

all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(wage~poly(age, i), data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}

# we plot the all.deltas graph, here is the code that plots the graph for cv error versus degree(different powers
# from 1 to 10)
plot(1:10, all.deltas, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point = min(all.deltas)
sd.points = sd(all.deltas)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

# from the line graph we can see than 7th degree and 9 degree show very low cv errors

# we also want to find the best fitting degree using anova, first we creat 10 models using degree 1 to 10,
# then we apply anova function to see the p value. age cube shows very small p value

fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

# we plot the entire data set in gray dot and use the predict the model with power 3, and draw it in blue lines

plot(wage~age, data=Wage, col="darkgrey")
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.fit = lm(wage~poly(age, 3), data=Wage)
lm.pred = predict(lm.fit, data.frame(age=age.grid))
lines(age.grid, lm.pred, col="blue", lwd=2)

####6.b

# here we again use loop to test how many cut will performs the best using cross validation

all.cvs = rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut = cut(Wage$age, i)
  lm.fit = glm(wage~age.cut, data=Wage)
  all.cvs[i] = cv.glm(Wage, lm.fit, K=10)$delta[2]
}

# we plot the cv error for each cut from 2 to 10, everybody can see that 8 cut shows the lowest cv error
plot(2:10, all.cvs[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2)

# so we use cut 8 to fit a model
lm.fit = glm(wage~cut(age, 8), data=Wage)

#predict corresponding wage for every age range from lowest age to highest age
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.pred = predict(lm.fit, data.frame(age=age.grid))

# plot the value in red line, and entire data set in grey
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)
