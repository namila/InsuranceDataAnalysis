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
    }
    
  }
  
   min_model = lm(premium ~ 1, data = dataSet)
   my_model = step(min_model, direction = "forward", scope = (~age + gender + bmi + num_kids + smoking_status + district))
   summary(my_model)
}

setwd("/Users/namilap/Documents/Msc/DataAnalysis/CourseWork")
insuranceDataDup = read.csv("insuranceDataDup.csv", header = TRUE)
#head(SLPopulation)
my_function(insuranceDataDup, "")



