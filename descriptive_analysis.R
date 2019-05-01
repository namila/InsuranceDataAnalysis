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
hist(insuranceData$age, main = "Histogram of Age", xlab = "Age", ylab = "Number of People",labels = TRUE, col = "blue")
#ggplot(insuranceData, aes(x=age)) + 
 # geom_histogram(color= "black", fill= "blue", binwidth = 5)+
  #ggtitle("Histogram of Age") 

# Histograms for age according each gender

genders = levels(insuranceData$gender)

for(current_gender in genders){
  hist(insuranceData[insuranceData$gender == current_gender,]$age, main = paste("Histogram of Age - ", current_gender), xlab = "Age", ylab = "Number of People",labels = TRUE, col = sample(colors(),1))
}




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
pie(gender_summery, main= "Gender distribution", col = rainbow(length(gender_summery)), labels = gender_percentages)
legend("topright", names(gender_summery), cex = 0.8, fill = rainbow(length(gender_summery)))

# Analyzing BMI

# Summery of the BMI
summary(insuranceData$bmi)

# Histogram for BMI
hist(insuranceData$bmi, main = "Histogram of BMI", xlab = "BMI", ylab = "Number of People", col = "blue")

# Standard Deiviation 
bmi_sd=sd(insuranceData$bmi)
bmi_sd

# box plot and outliers
outliers = boxplot(insuranceData$bmi, main = "Box plot for BMI", col = "blue", ylab = "BMI")$out
cat ("Outliers", "\n", outliers)


# Analyzing number of kids

# Summery of the num_kids
summary(insuranceData$num_kids)

# Histogram for num_kids
hist(insuranceData$num_kids, main = "Histogram of number of kids", xlab = "num_kids", ylab = "Number of People", col = "blue")

# Standard Deiviation 
num_kids_sd=sd(insuranceData$num_kids)
num_kids_sd

# box plot and outliers
outliers = boxplot(insuranceData$num_kids, main = "Box plot for number of kids", col = "blue", ylab = "num_kids")$out
cat ("Outliers", "\n", outliers)



# Analyzing Smoking Status

# summery of the variable
smoking_status_summery = summary(insuranceData$smoking_status)
smoking_status_summery

# Pie chat of the smoking status distribution
smoking_status_percentages = round(100*smoking_status_summery/sum(smoking_status_summery), 1)
pie(smoking_status_summery, main = "Smoking status distribution", col = rainbow(length(smoking_status_summery)), labels = smoking_status_percentages)
legend("topright", names(smoking_status_summery), cex = 0.8, fill = rainbow(length(smoking_status_summery)))


# Analyzing District

# summery of the variable
district_summery = summary(insuranceData$district)
district_summery

# Pie chat of the district distribution
district_percentages = round(100*district_summery/sum(district_summery), 1)
pie(district_summery, main = "District distribution", col = rainbow(length(district_summery)), labels = district_percentages)
legend("topright", names(district_summery), cex = 0.8, fill = rainbow(length(district_summery)))


# Analysing premium

# Summery of the Premium
premium_summary = summary(insuranceData$premium)
premium_summary

# Histogram for premium
hist(insuranceData$premium, main = "Histogram of Premium", xlab = "Premium", ylab = "Number of People", col = "blue")

# Standard Deiviation 
histogram_sd=sd(insuranceData$premium)
histogram_sd

# box plot and outliers
outliers = boxplot(insuranceData$premium, main = "Box plot for Premium", col = "blue", ylab = "Premium")$out
cat ("Outliers", "\n", outliers)










