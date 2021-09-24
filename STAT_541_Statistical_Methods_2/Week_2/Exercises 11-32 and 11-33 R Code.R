#
# Chapter 11 Exercises 11.32 and 11.33
# 

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(ex11_32)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(ex11_32)
str(dataobj)

par(mfrow = c(1, 1))
plot(dataobj$ExposureTime,dataobj$WeightLoss,xlab="Exposure Time",
     ylab="Weight Loss",
     main="Exercises 11.32 and 11.33", pch=19,cex=1.5)

# Split the plotting panel into a 2 x 2 grid
#  this puts four graphs in one window
par(mfrow = c(2, 2))

# model the dependent variable as a function of 
#   the independent variable
model <- lm(WeightLoss ~ ExposureTime , data=dataobj)
summary(model)
plot(model)
# Shapiro-Wilk test for normality of errors and use alpha=0.01
shapiro.test(resid(model))

par(mfrow=c(1,1))

# Plot the regression line on the scatterplot
plot(dataobj$ExposureTime,dataobj$WeightLoss,xlab="Exposure time",
     ylab="Weight Loss",     main="Exercises 11.32 and 11.33",
     pch=19,cex=1.5)
abline(model$coefficients)

# This exercise asks for a right-tailed test of the
#   slope parameter
# The following code will extract the test statistic and 
#   determine the p-value for a right-tailed test
result <- summary(model)
T.S. <- result$coefficients[2,3]
pvalrt <- pt(T.S.,df=df.residual(model),lower.tail=FALSE)
print(pvalrt)

# Compute confidence intervals at specified values of Exposure Time
new.expotime <- data.frame(ExposureTime=c(4,5,6,7))
predict(model,newdata=new.expotime,interval="confidence")

# Compute prediction intervals at specified values of Exposure Time
predict(model,newdata=new.expotime,interval="prediction")

# use ggplot2 to get nice graphs
# R package ggplot2 must be installed first
library(ggplot2)

# 1. Add predictions 
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(dataobj, pred.int)
# 2. Regression line + confidence bands
p <- ggplot(mydata, aes(ExposureTime, WeightLoss)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction bands
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")



