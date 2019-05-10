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
# Standard Deiviation 
age_sd=sd(insuranceData$age)
age_sd
#ggplot(insuranceData, aes(x=age)) + 
 # geom_histogram(color= "black", fill= "blue", binwidth = 5)+
  #ggtitle("Histogram of Age") 

# Histograms for age according each gender

genders = levels(insuranceData$gender)
par(mfcol=c(1,2))
for(current_gender in genders){
  hist(insuranceData[insuranceData$gender == current_gender,]$age, main = paste("Histogram of Age - ", current_gender), xlab = "Age", ylab = "Number of People",labels = TRUE, col = sample(colors(),1))
}
par(mfcol=c(1,1))

# Standard deviation of age for each gender
male_sd = sd(insuranceData[insuranceData$gender == "male",]$age)
male_sd

female_sd = sd(insuranceData[insuranceData$gender == "female",]$age)
female_sd



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

# Density curve
hist(insuranceData$bmi, prob=TRUE,main = "Density Histogram", xlab = "BMI", col="blue")
lines(density(insuranceData$bmi), col="red", lwd=2)

# Probability of being underweight
probability_under_weight = pnorm(18.5, 30.92, 6.212359)
probability_under_weight

# Probability of being healthy weight
probability_healthy_weight = pnorm(24.9, 30.92, 6.212359) - pnorm(18.5, 30.92, 6.212359)
probability_healthy_weight

# Probability of being over wight
probability_over_weight = pnorm(29.9, 30.92, 6.212359) - pnorm(25, 30.92, 6.212359)
probability_over_weight

# Probability of being obese wight
probability_obese_weight = 1- pnorm(30, 30.92, 6.212359)
probability_obese_weight


# Pie chat for BMI category distribution
category_vector =  c("under", "healthy", "over", "obese")
percentage_vector = c(probability_under_weight * 100, probability_healthy_weight * 100, probability_over_weight * 100, probability_obese_weight * 100)
bmi_percentage_data = data.frame(category = category_vector, percentage=percentage_vector)
pie(x = bmi_percentage_data$percentage, main= "BMI Category Distribution", labels = round(bmi_percentage_data$percentage,1), col = rainbow(length(bmi_percentage_data$category)))
legend("topright", category_vector, cex = 0.9, fill = rainbow(length(bmi_percentage_data$category)))


# Gender wise Density curve and BMI category distribution
genders = levels(insuranceData$gender)
par(mfcol=c(1,2))
for(current_gender in genders){
  gender_data = insuranceData[insuranceData$gender == current_gender,]
  
  hist(gender_data$bmi, prob=TRUE,main = paste("Density Histogram for", current_gender), xlab = "BMI", col="blue")
  lines(density(gender_data$bmi), col="red", lwd=2)
  
  mean_bmi = mean(gender_data$bmi)
  standard_deviation = sd(gender_data$bmi)
  
  # Probability of being underweight
  probability_under_weight = pnorm(18.5, mean = mean_bmi, sd = standard_deviation)
  probability_under_weight
  
  # Probability of being healthy weight
  probability_healthy_weight = pnorm(24.9, mean = mean_bmi, sd = standard_deviation) - pnorm(18.5,  mean = mean_bmi, sd = standard_deviation)
  probability_healthy_weight
  
  # Probability of being over wight
  probability_over_weight = pnorm(29.9, mean = mean_bmi, sd = standard_deviation) - pnorm(25,  mean = mean_bmi, sd = standard_deviation)
  probability_over_weight
  
  # Probability of being obese wight
  probability_obese_weight = 1- pnorm(30, mean = mean_bmi, sd = standard_deviation)
  probability_obese_weight
  
  
  # Pie chat for weight category distribution
  category_vector =  c("under", "healthy", "over", "obese")
  percentage_vector = c(probability_under_weight * 100, probability_healthy_weight * 100, probability_over_weight * 100, probability_obese_weight * 100)
  bmi_percentage_data = data.frame(category = category_vector, percentage=percentage_vector)
  pie(x = bmi_percentage_data$percentage, main = paste("BMI Category Distribution for", current_gender), labels = round(bmi_percentage_data$percentage,1), col = rainbow(length(bmi_percentage_data$category)))
  legend("bottomright", category_vector, cex = 0.5, fill = rainbow(length(bmi_percentage_data$category)))
  
}

par(mfcol=c(1,1))


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




# Looking at pattern between variables.


# looking at the relationship between age and premuim
ggplot(insuranceData, aes(x = age, y = premium, color = gender)) + geom_point()

# Calculating pearson correlation_coefficient 
pearson_correlation_coefficient = cor(insuranceData$age, insuranceData$premium, method = c("pearson"))
pearson_correlation_coefficient

# looking at the relationship between gender and premuim


coded_gender = factor(insuranceData$gender, levels = levels(insuranceData$gender), labels = c(0,1))
head(coded_gender)
class(coded_gender)
spearman_correlation_coefficient = cor(insuranceData$gender, insuranceData$premium, method = c("spearman"))
spearman_correlation_coefficient



premium_summary_male=summary(insuranceData[insuranceData$gender == "male",]$premium)
premium_summary_male

premium_summary_female=summary(insuranceData[insuranceData$gender == "female",]$premium)
premium_summary_female

# Looking at the relationship between BMI and Premium
ggplot(insuranceData, aes(x = bmi, y = premium, color = gender)) + geom_point()

# Calculating pearson correlation_coefficient betwen bmi and premium
pearson_correlation_coefficient = cor(insuranceData$bmi, insuranceData$premium, method = c("pearson"))
pearson_correlation_coefficient

# Relationship between number of kids and premium
mean_premium_wth_num_kids = aggregate(insuranceData$premium, list(num_kids = insuranceData$num_kids), mean)
ggplot(mean_premium_wth_num_kids, aes(x = num_kids, y = x)) + 
  geom_point(col = "blue") +
  xlab("Number of Kids") +
  ylab("Average Premium") +
  ggtitle("Average premium against number of kids")

correlation_num_kids_and_avg_premuim = cor(mean_premium_wth_num_kids$num_kids, mean_premium_wth_num_kids$x, method = c("pearson"))
correlation_num_kids_and_avg_premuim

# Relationship between smoking status and premuim
mean_premium_for_smoking_status = aggregate(insuranceData$premium, list(smoking_status = insuranceData$smoking_status), mean)

ggplot(data = mean_premium_for_smoking_status, aes(x = smoking_status, y = x)) +
geom_bar(stat = "identity", fill = "blue") +
geom_text(aes(label = round(x,2)), vjust = -0.3, size = 3.5) +
xlab("Smoking Status") +
ylab("Average Premium") +
ggtitle("Average premium against smoking status")

# Relationship between district and premium
mean_premium_for_each_district = aggregate(insuranceData$premium, list(district = insuranceData$district), mean)
mean_premium_for_each_district

ggplot(data = mean_premium_for_each_district, aes(x = district, y = x) ) +
geom_bar(stat = "identity", fill = "blue") +
geom_text(aes(label = round(x,2)), vjust = -0.3, size = 3.5) +
xlab("District") +
ylab("Average Premium") +
ggtitle("Average premium for each district")


# Finding the suitable model

insuranceData$district
as.factor(insuranceData$district)
 a= factor(insuranceData$district)
# Spliting the data to train and test sets

 insuranceData$coded_gender =  factor(insuranceData$gender,levels = levels(insuranceData$gender),labels = 1:length(levels(insuranceData$gender)))
 row_count = nrow(insuranceData)

train_ids = sample(1:row_count, row_count * 0.7, replace = FALSE)
test_ids = setdiff(1:row_count, train_ids)

train_data = insuranceData[train_ids,]
test_data = insuranceData[test_ids,]

  
min_model = lm(premium ~ 1, data = train_data)
summary(min_model)

forward_model = step(min_model, direction = "forward", scope = (~age + gender + bmi + num_kids + smoking_status + district))
summary(forward_model)


modified_forward_model = lm(premium ~ ., data = train_data)
summary(modified_forward_model)
?step()
contrasts(train_data$district)



