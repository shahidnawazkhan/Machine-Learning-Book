#
# ANOVA Handout Silt Content of Soils
#

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(Silt_Content_of_Soils)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(Silt_Content_of_Soils)
str(dataobj)

# boxplot of observed data
boxplot(Silt ~ Site, data=dataobj, main="Silt Content of Soils")

# Create factor variable for ANOVA
dataobj$site.factor <- as.factor(dataobj$Site)
str(dataobj)

# average silt content of all observations
mean(dataobj$Silt)

# Use aggregate function to compute summary statistics
# Use table function to get number of observations
SiteMean <- aggregate(Silt ~ Site, data=dataobj, mean)
SiteVar <- aggregate(Silt ~ Site, data=dataobj, var)
SiteObs <- matrix(table(dataobj$site))
# Build table and define column names
SiltAgg <- cbind(SiteMean, SiteVar[,2], SiteObs)
names(SiltAgg) <- c("Site", "Mean", "Variance", "#Obs")
SiltAgg

# Split the plotting panel into a 2 x 2 grid
#  this puts four graphs in one window
par(mfrow = c(2, 2))

# Use lm function for ANOVA analysis 
#  Note: Variable for populations must be a factor
result <- lm(Silt~site.factor,data=dataobj)
anova(result)  
plot(result)

par(mfrow = c(1, 1))
# Examine residuals
dataobj$residuals <- resid(result)
hist(resid(result),main="Histogram of Silt Content of Soils Residuals")
boxplot(residuals~Site,data=dataobj,main="Boxplots of Silt Content of Soils Residuals")
shapiro.test(resid(result))

# Need R package car for Levene's Test for Homogeneity of Variances
# Install R package car before running the following code
library(car)
leveneTest(Silt~site.factor,data=dataobj)

# Need R package inferr for Levene's Test for Homogeneity of Variance
#  Install R package inferr before running the following code
library(inferr)
infer_levene_test(data=dataobj, Silt, group_var=site.factor)
# use the line for Levene


