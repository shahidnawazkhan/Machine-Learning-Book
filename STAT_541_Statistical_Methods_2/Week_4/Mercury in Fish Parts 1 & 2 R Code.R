#
# Homework Assignment 3 Multiple Regression Part 1
#  Mercury contamination of fish in various lakes
#


# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(Mercury_in_Fish)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(Mercury_in_Fish)
str(dataobj)

# load library regclass for VIF function
library(regclass)
# load library lmtest for Breusch-Pagan test
library(lmtest)

par(mfrow = c(1, 1)) 

# examine scatterplot matrix of dependent and independent variables
#  use columns 2,3,4,5,6 in dataset
plot(dataobj[,2:6],main="Mercury in Fish")


# correlation matrix with p-values
#  correlation matrix is first, then n, then p-values for each
#    pairwise correlation
library(ggplot2)
library(Hmisc)
mat <- as.matrix(dataobj[,2:6])
rcorr(mat, type="pearson")

# multiple regression model
model <- lm(EHg ~ Alk + pH + Ca + Chlo, data=dataobj)
summary(model)



############
## Part 2 ##
############

# confidence intervals on model parameters
confint(model,level=0.95)

# confidence interval on E(y) for specified values of all 
#   independent variables
forecastdata = data.frame(Alk=80,
                          pH=7,
                          Ca=40,
                          Chlo=25) 
predict(model,newdata=forecastdata,interval="confidence")
# prediction interval on y for specified values of all 
#   independent variables
predict(model,newdata=forecastdata,interval="prediction")

par(mfrow = c(2, 2))
plot(model, main="Mercury in Fish")
# Shapiro-Wilk test for normality of errors and use alpha=0.01
shapiro.test(resid(model))
# Breusch-Pagan Test for a common error variance and use alpha=0.01
bptest(model)

par(mfrow = c(1, 1))
# histogram of residuals
hist(resid(model), main="Mercury in Fish Histogram of Residuals",xlab="Residuals")
# boxplot of residuals
boxplot(resid(model), main="Mercury in Fish Boxplot of Residuals")

# Influential observations
influence.measures(model)

# Check for multicollinearity
# The VIF function is in R package regclass
VIF(model)







