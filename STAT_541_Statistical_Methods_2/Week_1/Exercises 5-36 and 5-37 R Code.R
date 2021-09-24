#
# Exercises 5.36 and 5.37
#

# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

# Note the name for the imported Excel file
str(ex5_36)


# Analysis for Reading Time
ReadTime <- ex5_36$ReadTime
summary(ReadTime)
sd(ReadTime)

# Two sided confidence interval
t.test(ReadTime,alternative="two.sided",conf.level=0.95)

# Test assumption of normality
# Examine Normal Q-Q plot
qqnorm(ReadTime,main="Normal Q-Q plot of Reading Time")
qqline(ReadTime)

# Examine boxplot
boxplot(ReadTime,  main="Boxplot of Reading Time")

# See if data is from a normal distribution using Shapiro-Wilk test
# Reject the assumption of normality if p-value is small (<0.01)
shapiro.test(ReadTime)



# Analysis for Reading Comprehension
Comprehension <- ex5_36$Comprehension
summary(Comprehension)
sd(Comprehension)

# Hypothesis test 
t.test(Comprehension,alternative="greater",mu=80,conf.level=0.95)

# Test assumption of normality
# Examine Normal Q-Q plot
qqnorm(Comprehension,main="Normal Q-Q plot of Comprehension")
qqline(Comprehension)

# Examine boxplot
boxplot(Comprehension,  main="Boxplot of Comprehension")

# See if data is from a normal distribution using Shapiro-Wilk test
# Reject the assumption of normality if p-value is small (<0.01)
shapiro.test(Comprehension)

