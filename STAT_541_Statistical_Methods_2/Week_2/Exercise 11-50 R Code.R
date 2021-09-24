#
# Chapter 11 Exercise 11.50
# 

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(ex11_50)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(ex11_50)
str(dataobj)

par(mfrow = c(1, 1))

# Scatterplot Matrix for the four Gender/Type variables
pairs(dataobj[,2:5],pch=19)

# correlation matrix with p-values
#  correlation matrix is first, then n, then p-values for each
#    pairwise correlation
# Need two R packages that must be installed first: ggplot2 and Hmisc
library(ggplot2)
library(Hmisc)
mat <- as.matrix(dataobj[,2:5])
rcorr(mat, type="pearson")

# An alernative method is to do a scatterplot and correlation test
#  for each pair of variables 
# For example, Male/Verbal and Female/Verbal
plot(dataobj$'Male/Verbal',dataobj$'Female/Verbal',
     xlab="Male/Verbal",ylab="Female/Verbal")
cor.test(dataobj$'Male/Verbal',dataobj$'Female/Verbal')

# Repeat plot and cor.test for the other five pairs of variables

