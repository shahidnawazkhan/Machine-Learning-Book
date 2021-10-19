#
# RF Power Setting and Etching Rate
#

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(Etching_Rate)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(Etching_Rate)
str(dataobj)
summary(dataobj)
colnames(dataobj)

# Create factor variable to use in analysis
dataobj$Power.Level <- as.factor(dataobj$Power)
dataobj$Power.Level
class(dataobj$Power.Level)

# Look at boxplot of observed etching rates
boxplot(EtchRate~Power.Level,data=dataobj,main="Boxplot of Observed Etching Rates")

# average of all etching rates
mean(dataobj$EtchRate)

# Use aggregate function to compute summary statistics
# Use table function to get number of observations
ResponseMean <- aggregate(EtchRate ~ Power, data=dataobj, mean)
ResponseVar <- aggregate(EtchRate ~ Power, data=dataobj, var)
ResponseObs <- matrix(table(dataobj$Power))
# Build table and define column names
ResponseAgg <- cbind(ResponseMean, ResponseVar[,2], ResponseObs)
names(ResponseAgg) <- c("Power Level", "Mean", "Variance", "#Obs")
ResponseAgg

par(mfrow = c(2, 2))
# Use lm function to analyze the data
result <- lm(EtchRate~Power.Level,data=dataobj)
anova(result)  
plot(result)
#
# Check assumptions using residuals
#
par(mfrow = c(1, 1))
model.residuals <- resid(result)

hist(model.residuals,main="Histogram of Etching Rate Residuals")
boxplot(model.residuals~as.factor(Power),data=dataobj,
        main="Boxplot of Etching Rate Residuals")


# Shapiro-Wilk Normality Test of residuals
shapiro.test(model.residuals)

# Need R package car for Levene's Test for Homogeneity of Variances
# Install R package car before running the following code
library(car)
leveneTest(EtchRate~Power.Level,data=dataobj)



