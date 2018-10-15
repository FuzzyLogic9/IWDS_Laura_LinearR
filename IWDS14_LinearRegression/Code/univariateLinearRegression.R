# Documentation and datasets
# http://www-bcf.usc.edu/~gareth/ISL/data.html
# https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R

# Set your working directory
#setwd("~/IWDS14_LinearRegression")

### Libraries
# NA

# Get the Advertising data set
# Description: Sales in thousands of units, as a function of TV, 
# radio, and newspaper budgets, in thousands of dollars, for 200 
# different markets.
advertising <- read.csv("Data/Advertising.csv")
View(advertising)


# Drop the first column (number of rows)
advertising <- advertising[,-1]
head(advertising)

# Get the basic statistics
summary(advertising)
# TV             radio          newspaper          sales      
# Min.   :  0.70   Min.   : 0.000   Min.   :  0.30   Min.   : 1.60  
# 1st Qu.: 74.38   1st Qu.: 9.975   1st Qu.: 12.75   1st Qu.:10.38  
# Median :149.75   Median :22.900   Median : 25.75   Median :12.90  
# Mean   :147.04   Mean   :23.264   Mean   : 30.55   Mean   :14.02  
# 3rd Qu.:218.82   3rd Qu.:36.525   3rd Qu.: 45.10   3rd Qu.:17.40  
# Max.   :296.40   Max.   :49.600   Max.   :114.00   Max.   :27.00 

#  Q1: Is there a relationship between advertising budget and sales?
# See relationships by pairs
pairs(advertising)

# See relationships between budgets and sales
attach(advertising)
par(mfrow = c(1,3))
plot(TV, sales, cex.lab=2, cex.axis=1.2)
plot(radio,sales,cex.lab=2,cex.axis=1.2)
title("Advertising data",cex.main = 2,font.main= 4, col.main= "blue")
plot(newspaper,sales,cex.lab=2,cex.axis=1.2)


#  Q2: How strong is the relationship between advertising budget and sales?

lm.radio = lm(sales ~ radio)
summary(lm.radio)
# estimated_sales = 9.3116 + 0.2025*radio
lm.tv = lm(sales ~ TV)
summary(lm.tv)
# estimated_sales = 7.03259 + 0.04754*TV
lm.newspaper = lm(sales ~ newspaper)
summary(lm.newspaper)
# estimated_sales = 12.35141 + 0.05469*newspaper

par(mfrow = c(1,3))
plot(TV, sales, cex.lab = 2, cex.axis = 1.2)
abline(lm.tv, col = "blue", lty = 1, lwd = 2)
plot(radio,sales,cex.lab=2,cex.axis=1.2)
abline(lm.radio, col="blue", lty=1, lwd=2)
plot(newspaper,sales,cex.lab=2,cex.axis=1.2)
abline(lm.newspaper, col="blue", lty=1, lwd=2)

# Each blue line represents a simple model that 
# can be used to predict sales using TV, radio, and newspaper.








