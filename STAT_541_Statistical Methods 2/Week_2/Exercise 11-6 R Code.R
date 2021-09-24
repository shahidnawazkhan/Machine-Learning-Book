#
# Chapter 11 Exercise 11.6
# 

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(ex11_6)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(ex11_6)
str(dataobj)

par(mfrow=c(1,1))
plot(dataobj$x,dataobj$y,xlab="x", ylab="y",
     main="Chapter 11 Exercise 11.6", pch=19,cex=1.5)

# Split the plotting panel into a 2 x 2 grid
#  this puts four graphs in one window
par(mfrow = c(2, 2))

# model the dependent variable as a function of 
#   the independent variable
model <- lm(y ~ x , data=dataobj)
summary(model)
confint(model,level=0.95)
plot(model)

par(mfrow=c(1,1))
plot(dataobj$x,dataobj$y,xlab="x", ylab="y",
     main="Chapter 11 Exercise 11.6", pch=19,cex=1.5)
lines(sort(dataobj$x),fitted(model)[order(dataobj$x)], col="blue", type="l")

# Predict y for a given value of x
predict(model, data.frame(x = 77))

