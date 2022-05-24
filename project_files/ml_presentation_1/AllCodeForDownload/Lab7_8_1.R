rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################

needed  <-  c("ISLR")      
installIfAbsentAndLoad(needed)

attach(Wage)

#Using lm() to predict a fourth degree polynomial in the variable 'age'. 
# 
fit=lm(wage~poly(age ,4) ,data=Wage)
coef(summary (fit))


# We can obtain age. age^2, age^3 and age^4 directly if we add the argument raw=T to the poly() function
# The coefficients are affected but the fitted values are not
fit2=lm(wage~poly(age ,4, raw =T),data=Wage)
coef(summary (fit2))


# Yet another way to fit a fourth degree polynomial w/i the lm() function
fit2a=lm(wage~age+I(age ^2)+I(age ^3)+I(age ^4) ,data=Wage)
coef(fit2a)


#Another method
fit2b=lm(wage~cbind(age ,age ^2, age ^3, age ^4) ,data=Wage)


# We use our first model, 'fit', to make wage predictions at every age in the data, from 18 to 80 years old. 
# We also create standard error bands that are +/- 2 SE from the predictions
agelims =range(age)
age.grid=seq (from=agelims [1], to=agelims [2])
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)


#Graph the data points, the prediction line and the SE bars
par(mfrow =c(1,2) ,mar=c(4.5 ,4.5 ,1 ,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5, col =" darkgrey ")
title (" Degree -4 Polynomial ",outer =T)
lines(age.grid ,preds$fit ,lwd =2, col =" blue")
matlines (age.grid ,se.bands ,lwd =1, col =" blue",lty =3)


# We use our second model, 'fit2', to also make predictions of wage at every age, from 18 to 80 years old. 
preds2 =predict (fit2 ,newdata =list(age=age.grid),se=TRUE)
# Find the max difference between predictions made by the two different models, 'fit' and 'fit2'
max(abs(preds$fit - preds2$fit ))


# Hypothesis testing. We must determine what order polynomial to use.
# Try fitting a linear model with a linear model, as well as second, third, fourth and fifth order polynomials
# Which model is best?
# The ANOVA tests the hypothesis that the current model, 'fit.1' for example, is as sufficient to explain 
# the data the next most complex model, in this case, 'fit.2'
fit.1= lm(wage~age ,data=Wage)
fit.2= lm(wage~poly(age,2) ,data=Wage)
fit.3= lm(wage~poly(age,3) ,data=Wage)
fit.4= lm(wage~poly(age,4) ,data=Wage)
fit.5= lm(wage~poly(age,5) ,data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
# The output indicates that successive models improve the interpretation of the data up to ~ the third order polynomial
# The fourth order polynomial may be a slight improvement over the third order, but the fifth is not an improvement
# over the fourth order

# We can also use the coef() function to achieve the same thing. This only works because we used the poly() 
# function and poly() creates orthogonal polynomials. The P-values are the same from the ANOVA and the the 
# coef() function. 
coef(summary(fit.5))

( -11.983) ^2

#ANOVA works if the polynomials are not orthogonal and when we include other terms
fit.1= lm(wage~education +age ,data=Wage)
fit.2= lm(wage~education +poly(age ,2) ,data=Wage)
fit.3= lm(wage~education +poly(age ,3) ,data=Wage)
anova(fit.1, fit.2, fit.3)


#Polynomial logistic regression model
# Use glm() to predict whether an individual earns more than $250K, include family = binomial to make it logistic
# output of 1 is coerced to TRUE and output of 0 is coerced to FALSE
fit=glm(I(wage >250)~poly(age ,4) ,data=Wage ,family =binomial )


#Make predictions using the polynomial logistic regression model
preds=predict (fit ,newdata =list(age=age.grid),se=T)


#Calculating confidence intervals
pfit=exp(preds$fit )/(1+ exp( preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+ exp(se.bands.logit))


#Could have gotten the probabilities by including type='response' when making the predictions
preds=predict (fit ,newdata =list(age=age.grid),type="response",se=T)

#Plot the polynomial logistic regression
plot(age ,I(wage >250) ,xlim=agelims ,type ="n",ylim=c(0 ,.2) )
points (jitter (age), I((wage >250) /5) ,cex =.5, pch ="|",col =" darkgrey ")
lines(age.grid ,pfit ,lwd =2, col =" blue")
matlines (age.grid ,se.bands ,lwd =1, col =" blue",lty =3)

# Use cut function to fit a stepwise function. The function automatically chose the ages 33.5, 49 and 64.5
# We could manually select ages using the 'breaks' option
table(cut (age ,4))

# The intercept is left out so the estimate of 33.5 is the average salary. The other estimates are additional
# salary ontop of those in the previous age group
fit=lm(wage~cut (age ,4) ,data=Wage)
coef(summary (fit))

