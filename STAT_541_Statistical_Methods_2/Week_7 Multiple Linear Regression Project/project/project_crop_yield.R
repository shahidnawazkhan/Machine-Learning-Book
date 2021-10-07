#
# Homework Assignment 3 Multiple Regression Part 1
#  Mercury contamination of fish in various lakes
#


# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(crop_yield)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(crop_yield)
str(dataobj)

# load library regclass for VIF function
library(regclass)
# load library lmtest for Breusch-Pagan test
library(lmtest)

par(mfrow = c(1, 1)) 

# examine scatterplot matrix of dependent and independent variables
#  use columns 2,3,4,5,6 in dataset
#plot(dataobj[,2:7],main="Crop Yield Using temporal NDVI")
plot(dataobj[,2:7],main="Crop Yield Using temporal NDVI")

# correlation matrix with p-values
#  correlation matrix is first, then n, then p-values for each
#    pairwise correlation
library(ggplot2)
library(Hmisc)
mat <- as.matrix(dataobj[,2:7])
rcorr(mat, type="pearson")

# multiple regression model
model <- lm(yield_y ~ may_ndvi + june_ndvi + july_ndvi +september_ndvi, data=dataobj)
summary(model)
############
## Part 2 ##
############

# confidence intervals on model parameters
confint(model,level=0.95)

# confidence interval on E(y) for specified values of all 
#   independent variables
forecastdata = data.frame(may_ndvi=0.449006685,
                          june_ndvi= 	0.605696133,
                          july_ndvi=0.762091794,
                          august_ndvi=0.824389924,
                          september_ndvi=0.707260766) 
				

predict(model,newdata=forecastdata,interval="confidence")
# prediction interval on y for specified values of all 
#   independent variables
predict(model,newdata=forecastdata,interval="prediction")

par(mfrow = c(2, 2))
plot(model, main="Crop Yield Prediction")
# Shapiro-Wilk test for normality of errors and use alpha=0.01
shapiro.test(resid(model))
# Breusch-Pagan Test for a common error variance and use alpha=0.01
bptest(model)

par(mfrow = c(1, 1))
# histogram of residuals
hist(resid(model), main="Crop Yield Histogram of Residuals",xlab="Residuals")
# boxplot of residuals
boxplot(resid(model), main="Mercury in Fish Boxplot of Residuals")

# Influential observations
influence.measures(model)

# Check for multicollinearity
# The VIF function is in R package regclass
VIF(model)







