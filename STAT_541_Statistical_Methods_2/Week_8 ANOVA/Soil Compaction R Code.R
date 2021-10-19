#
# Cattle Grazing and Soil Compaction
#

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(Soil_Compaction)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(Soil_Compaction)
str(dataobj)

# Create factor variable to designate populations used in analysis
dataobj$grazing.factor <- as.factor(dataobj$Grazing)
str(dataobj)

# boxplot of observed data
boxplot(Density ~ Grazing, data=dataobj, main="Boxplot of Observed Soil Density")

# average of all soil density observations
mean(dataobj$Density)

# Use aggregate function to compute summary statistics
# Use table function to get number of observations
ResponseMean <- aggregate(Density ~ Grazing, data=dataobj, mean)
ResponseVar <- aggregate(Density ~ Grazing, data=dataobj, var)
ResponseObs <- matrix(table(dataobj$grazing.factor))
# Build table and define column names
ResponseAgg <- cbind(ResponseMean, ResponseVar[,2], ResponseObs)
names(ResponseAgg) <- c("Grazing", "Mean", "Variance", "#Obs")
ResponseAgg

# Split the plotting panel into a 2 x 2 grid
#  this puts four graphs in one window
par(mfrow = c(2, 2))

# Use lm function for ANOVA analysis 
#  Note: Variable for treatments must be a factor
result <- lm(Density ~ grazing.factor,data=dataobj)
anova(result)  
plot(result)
#
# Check assumptions using residuals
#
par(mfrow = c(1, 1))
# Examine residuals
dataobj$residuals <- resid(result)
hist(resid(result),main="Histogram of Soil Compaction Residuals")
boxplot(residuals~Grazing,data=dataobj,main="Boxplot of Soil Compaction Residuals")
shapiro.test(resid(result))

# Need R package car for Levene's Test for Homogeneity of Variance
# Install R package car before running the following code
library(car)
leveneTest(Density~grazing.factor,data=dataobj)

# Need R package inferr for Levene's Test for Homogeneity of Variance
#  Install R package inferr before running the following code
library(inferr)
infer_levene_test(data=dataobj, Density, group_var=grazing.factor)
# use the line for Levene



