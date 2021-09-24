#
# Multiple Regression Week 1 Ag Data R Code
# 

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(Ag_Data)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(Ag_Data)
str(dataobj)

# load library regclass for VIF function
library(regclass)
# load library lmtest for Breusch-Pagan test
library(lmtest)

par(mfrow = c(1, 1))

# Scatterplot Matrix (note that columns 7,10,12 correspond to x6,x9,y)
pairs(dataobj[,c(7,10,12)],pch=19)

# Correlation test for each pair of variables 
cor.test(dataobj$'y',dataobj$'x6')
cor.test(dataobj$'y',dataobj$'x9')
cor.test(dataobj$'x6',dataobj$'x9')

# Fit the proposed model
model <- lm(y ~ x6 + x9, data=dataobj)
summary(model)



############
## Part 2 ##
############

# confidence interval on model parameters
confint(model,level=0.95)

# confidence interval on E(y) for specified values of all 
#   independent variables
forecastdata = data.frame(x6=90, 
                          x9=70) 
predict(model,newdata=forecastdata,interval="confidence")
# prediction interval on y for specified values of all 
#   independent variables
predict(model,newdata=forecastdata,interval="prediction")

par(mfrow = c(2, 2))
plot(model, main="Ag Data Model with x6 and x9")

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
# The VIF function uses the R package regclass
VIF(model)


