#
# Exercise 3.34
#

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel file

# Note that the import function defaulted to the file name ex3_34
str(ex3_34)

# Store  data in y and examine data structure
y <- ex3_34$rates
str(y)
summary(y)

# Histogram of data
hist(y,main="Frequency Histogram for Resting Pulse Rates",
     breaks=5,freq=TRUE)

# Examine boxplot
boxplot(y,  main="Boxplot of Resting Pulse Rates")



