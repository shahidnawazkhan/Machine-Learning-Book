#
# Template for Multiple Regression Project
#
# Set up with the dependent variable as the first column
# and the three independent variables in columns 2, 3, & 4
#
# The code will use column 1 as y, column 2 as x1, column 3 as x2
#   and column 4 as x3
#
# load library regclass for VIF function
library(regclass)
# load library lmtest for Breusch-Pagan test
library(lmtest)
# In RStudio, use File, Import Dataset, From Excel...
# to get Excel data file
##########
#
# NOTE: Replace RegProj with the name of your imported Excel file
#
str(Home_Price)
# To have most of our R code reuseable for future
# analyses, we will use a data object called dataobj
##########
# NOTE: Replace RegProj with the name of your imported Excel file
# 
dataobj <- as.data.frame(Home_Price)
str(dataobj)
par(mfrow = c(1, 1))

# Set column 1 as y, column 2 as x1, column 3 as x2, and column 4 as x3
dataobj$y <- dataobj[,1]
dataobj$x1 <- dataobj[,2]
dataobj$x2 <- dataobj[,3]
dataobj$x3 <- dataobj[,4]
summary(dataobj$y)
sd(dataobj$y)
summary(dataobj$x1)
sd(dataobj$x1)
summary(dataobj$x2)
sd(dataobj$x2)
summary(dataobj$x3)
sd(dataobj$x3)
# get number of observations
n <- length(dataobj$y)
n
str(dataobj)
# Scatterplot Matrix
pairs(~y+x1+x2+x3,pch=19,main="Scatterplot Matrix",data=dataobj)
# correlation test for each pair of variables
cor.test(dataobj$y,dataobj$x1)
cor.test(dataobj$y,dataobj$x2)
cor.test(dataobj$y,dataobj$x3)
cor.test(dataobj$x1,dataobj$x2)
cor.test(dataobj$x1,dataobj$x3)
cor.test(dataobj$x2,dataobj$x3)

par(mfrow = c(2, 2))
# Fit the model
model <- lm(y ~ x1 + x2 + x3, data=dataobj)
summary(model)
plot(model)
# Shapiro-Wilk test for normality of errors and use alpha=0.01
shapiro.test(resid(model))
# Breusch-Pagan Test for a common error variance and use alpha=0.01
bptest(model)
par(mfrow = c(1, 1))
# histogram of residuals
hist(resid(model), main="Histogram of Residuals",xlab="Residuals")
# boxplot of residuals
boxplot(resid(model), main="Boxplot of Residuals")
# Influential observations
influence.measures(model)
# Check for multicollinearity
VIF(model)

