# Multivariate Linear Regression

#  How strong is the relationship between advertising budget and sales?
#  Which media contribute to sales?

ad.lm <- lm(sales~., data=advertising)
summary(ad.lm)
Coef1=summary(ad.lm)$coefficients #Coefficient matrix
Coef1

#Confidence Interval
confint(ad.lm)

########  
# How accurately can we estimate the effect of each medium on sales?
# How accurately can we predict future sales?
# model accuracy: 
# R^2 (coefficient of determination): the proportion of the variance in the dependent variable that is predictable from the independent variable(s)
# mean squared error (MSE) 

yhat=ad.lm$fitted.values #predicted 
y=advertising$sales #observed 

# r-squared
rsq = var(yhat)/var(y)
#summary(ad.lm)$r.sq

# mse
mse1 = mean((y-yhat)^2)

#  Is the relationship linear?
#  Is there synergy (interaction) among the advertising media?
ad.lm2 <- lm(sales~.+I(TV^2), data=advertising)
summary(ad.lm2)
