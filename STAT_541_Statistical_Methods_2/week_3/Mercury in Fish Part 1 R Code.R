#
# Homework Assignment 3 Multiple Regression Part 1
#  Mercury contamination of fish in various lakes
#


# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(Mercury_in_Fish)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj
dataobj <- as.data.frame(Mercury_in_Fish)
str(dataobj)

par(mfrow = c(1, 1)) 

# examine scatterplot matrix of dependent and independent variables
#  use columns 2,3,4,5,6 in dataset
plot(dataobj[,2:6],main="Mercury in Fish")


# correlation matrix with p-values
#  correlation matrix is first, then n, then p-values for each
#    pairwise correlation
library(ggplot2)
library(Hmisc)
mat <- as.matrix(dataobj[,2:6])
rcorr(mat, type="pearson")

# multiple regression model
model <- lm(EHg ~ Alk + pH + Ca + Chlo, data=dataobj)
summary(model)







