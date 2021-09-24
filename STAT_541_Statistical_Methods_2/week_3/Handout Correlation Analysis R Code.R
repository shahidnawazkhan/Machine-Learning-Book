#
# Chapter 11 Handout Correlaton Analysis
# 

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(HomeInfo)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(HomeInfo)
str(dataobj)

par(mfrow = c(1, 1))

# Scatterplot Matrix for the four Gender/Type variables
pairs(dataobj[,2:6],pch=19)

# correlation matrix with p-values
#  correlation matrix is first, then n, then p-values for each
#    pairwise correlation
# Need two R packages that must be installed first: ggplot2 and Hmisc
library(ggplot2)
library(Hmisc)
mat <- as.matrix(dataobj[,2:6])
rcorr(mat, type="pearson")

# An alernative method is to do a scatterplot and correlation test
#  for each pair of variables 
# For example, Male/Verbal and Female/Verbal
plot(dataobj$size,dataobj$bath,
     xlab="Home Size, thousands of square feet",
     ylab="Number of Bathrooms")
cor.test(dataobj$size,dataobj$bath)

# Repeat plot and cor.test for all other pairs of variables

