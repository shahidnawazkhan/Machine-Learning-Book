#
# Chapter 11 Simple Linear Regression for height and weight data
# 

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(height_and_weight)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(height_and_weight)
str(dataobj)

# Scatterplot to examine relationship
par(mfrow=c(1,1))
plot(dataobj$height,dataobj$weight,xlab="Height,inches", ylab="Weight, pounds",
     ylim=c(110,220),xlim=c(62,76),main="Simple Linear Regression Example",
     pch=19,cex=1.5)

# Split the plotting panel into a 2 x 2 grid
#  this puts four graphs in one window
par(mfrow = c(2, 2))

# model weight as a function of height
model <- lm(weight ~ height , data=dataobj)
summary(model)
confint(model,level=0.95)
plot(model)
# Shapiro-Wilk test for normality of errors
shapiro.test(resid(model))

# plot the regression line on the scatterplot
par(mfrow=c(1,1))
plot(dataobj$height,dataobj$weight,xlab="Height,inches", ylab="Weight, pounds",
   ylim=c(110,220),xlim=c(62,76),main="Simple Linear Regression Example",
   pch=19,cex=1.5)
lines(sort(dataobj$height),fitted(model)[order(dataobj$height)], col="blue", type="l")

# confidence interval for mean weight given height=68 inches
newdatamu <- data.frame(height=68)
predict(model,newdatamu,interval="confidence")

# prediction interval of weight for height=68 inches
newdatay <- data.frame(height=68)
predict(model,newdatay,interval="predict")

# Investigate correlation between height and weight
par(mfrow=c(1,1))
plot(dataobj$height,dataobj$weight,xlab="Height,inches", ylab="Weight, pounds",
     ylim=c(110,220),xlim=c(62,76),main="Simple Linear Regression Example",
     pch=19,cex=1.5)
cor(dataobj$height, dataobj$weight, method="pearson")
# Hypothesis test for population correlation equal to zero
cor.test(dataobj$height, dataobj$weight, method="pearson",
         alternative="two.sided",conf.level=0.95)

