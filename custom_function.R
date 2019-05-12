get_mode = function(dataSet){
  uniqueItems <- unique(dataSet)
  uniqueItems[which.max(tabulate(match(dataSet, uniqueItems)))]
}

my_function = function(dataSet, parameter){
  
  # Iterating through each column
  for(column_name in colnames(dataSet)){
    
    if(column_name == "X1"){
      next    
    }
    data_column = dataSet[[column_name]]
    
    # Calculating number of missing values for the current column
    no_of_missing_values = sum(is.na(data_column))
   
    # Checking for quantitative variables
    if(is.numeric(data_column) == TRUE){
      cat(column_name," is quantitative \n")
      cat("Number of missing values", no_of_missing_values, "\n")
      
      if(no_of_missing_values > 0){
        
        # Calculating the mean in order to replace missing values
        mean_value = mean(data_column, na.rm = TRUE)
        dataSet[[column_name]][is.na(dataSet[[column_name]])] =  mean_value
      }
    
     outLiers =  boxplot(dataSet[[column_name]], ylab = column_name, main = paste("BoxPlot for ", column_name))$out
    
     hist(dataSet[[column_name]], main=paste("Histogram of ",column_name), xlab = column_name)
     cat("Outliers", outLiers, "\n\n\n")
     
    } 
    
    # Qualititative variables
    else{
      cat(column_name," is qualititative \n")
      cat("Number of missing values", no_of_missing_values, "\n\n\n")
      
      if(no_of_missing_values > 0){
        
        # Calculating mode inorder to replace missing values
        mode_value = get_mode(data_column)
        dataSet[[column_name]][is.na(dataSet[[column_name]])] =  mode_value
      }
      
    
      variable_summery = summary(dataSet[[column_name]])
    
      # plotting a bar chart  
      barplot(variable_summery, main= paste(column_name, " distribution") )
      
      # plotting a pie chart
      percentages = round(100*variable_summery/sum(variable_summery), 1)
      print(head(dataSet))
      pie(variable_summery, main= paste(column_name, " distribution") , col = rainbow(length(variable_summery)), labels = percentages)
      legend("topright", names(variable_summery), cex = 0.8, fill = rainbow(length(variable_summery)))
      
    }
    
  }
  
}

setwd("/Users/namilap/Documents/Msc/DataAnalysis/CourseWork")
dataSet = read.csv("insuranceData.csv", header = TRUE)
  #read.csv("population_by_district_in_census_years.csv", header = TRUE)
#head(SLPopulation)
my_function(dataSet, "")

column_name = "district"
  #"District"
variable_summery = summary(dataSet[[column_name]])
head(variable_summery)
barplot(variable_summery, main = )
?barplot
ggplot(data = mean_premium_for_each_district, aes(x = district, y = x) ) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(x,2)), vjust = -0.3, size = 3.5) +
  xlab("District") +
  ylab("Average Premium") +
  ggtitle("Average premium for each district")


