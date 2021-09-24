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

par(mfrow = c(1, 1))

# Scatterplot Matrix (note that columns 7,10,12 correspond to x6,x9,y)
pairs(dataobj[,c(7,10,12)],pch=19)

# correlation matrix with p-values
#  correlation matrix is first, then n, then p-values for each
#    pairwise correlation
# Need R package Hmisc
# library(Hmisc)
# mat <- as.matrix(dataobj[,c(7,10,12)])
# rcorr(mat, type="pearson")

# Correlation test for each pair of variables 
cor.test(dataobj$'y',dataobj$'x6')
cor.test(dataobj$'y',dataobj$'x9')
cor.test(dataobj$'x6',dataobj$'x9')

# Fit the proposed model
model <- lm(y ~ x6 + x9, data=dataobj)
summary(model)



