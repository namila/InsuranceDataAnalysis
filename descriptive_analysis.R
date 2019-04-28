# importing libraries
library(ggplot2)


# imporing the cvs file

setwd("/Users/namilap/Documents/Msc/DataAnalysis/CourseWork")
insuranceData = read.csv("insuranceData.csv", header = TRUE)
head(insuranceData)

# Overall summery of the dataset
summary(insuranceData)

# Analyzing age 

# summery of the variable
summary(insuranceData$age)

# Histogram for age
hist(insuranceData$age, main = "Histogram of Age", xlab = "Age", ylab = "Number of People", col = "blue")

# Standard Deiviation 
age_sd=sd(insuranceData$age)
age_sd

# box plot and outliers
outliers = boxplot(insuranceData$age, main = "Box plot for Age", col = "blue", ylab = "Age")$out
cat ("Outliers", "\n", outliers)


# Analyzing gender 

# summery of the variable
gender_summery = summary(insuranceData$gender)
gender_summery

# Pie chat of the gender distribution
gender_percentages = round(100*gender_summery/sum(gender_summery), 1)
pie(gender_summery, col = rainbow(length(gender_summery)), labels = gender_percentages)
legend("topright", c("female","male"), cex = 0.8, fill = rainbow(length(gender_summery)))





