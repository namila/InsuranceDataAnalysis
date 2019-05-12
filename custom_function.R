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
  
  if(is.numeric(dataSet[[parameter]])){
    min_model_fomula = as.formula(paste(parameter, paste(c(1)), sep = "~"))
    column_names = names(dataSet)
    column_names = column_names[column_names != "X" & column_names != paste(parameter)]
    full_model_fomula = as.formula(paste("~", paste(column_names, collapse = "+"),sep = ""))
    
    min_model = lm(min_model_fomula, data = dataSet)
    forward_model = step(min_model, direction = "forward", scope = full_model_fomula)
    print(summary(forward_model))
    
    # Checking for model assumptions
    standardized_residules = rstandard(forward_model)
    
    # Comparing the distribution of the residuals with the standard distribution
    qqnorm(standardized_residules, ylab = "Quantiles of Standardized Residuals", xlab = "Quantiles of Standard Distribution", main = "Q - Q plot for Residuals and Normal Distribution")
    qqline(standardized_residules)
    
    # Testing normality using shapiro test
    print(shapiro.test(forward_model$residuals))
    
    # looking for variance and linearity
    for(column in column_names){
      
      if(class(dataSet[[column]]) != "factor"){
        plot( y = forward_model$residuals , x = dataSet[[column]], xlab = column, ylab = "Rediduals", main = paste(column," and Residuals"))
        abline(h = 0, col = "red")      
      }
      
    }
    
  }
  
  if(is.factor(dataSet[[parameter]])){
    
    if(length(levels(dataSet[[parameter]])) == 2){
      
      min_model_fomula = as.formula(paste(parameter, paste(c(1)), sep = "~"))
      column_names = names(dataSet)
      column_names = column_names[column_names != "X" & column_names != paste(parameter)]
      full_model_fomula = as.formula(paste("~", paste(column_names, collapse = "+"),sep = ""))
      
      min_binary_model = glm(formula = min_model_fomula, data = dataSet, family=binomial(link = logit))
      summary(min_binary_model)
      
      forward_binary_model = step(min_binary_model, direction = "forward", scope = full_model_fomula)
      summary(forward_binary_model)
    }
      
  }
  
  
  
  
  
  
}

setwd("/Users/namilap/Documents/Msc/DataAnalysis/CourseWork")
dataSet = read.csv("insuranceData.csv", header = TRUE)
  #read.csv("population_by_district_in_census_years.csv", header = TRUE)
#head(SLPopulation)
my_function(dataSet, "smoking_status")

parameter = "smoking_status"




if(is.numeric(dataSet[[parameter]])){
  min_model_fomula = as.formula(paste(parameter, paste(c(1)), sep = "~"))
  column_names = names(dataSet)
  column_names = column_names[column_names != "X" & column_names != paste(parameter)]
  full_model_fomula = as.formula(paste("~", paste(column_names, collapse = "+"),sep = ""))
  
  min_model = lm(min_model_fomula, data = dataSet)
  forward_model = step(min_model, direction = "forward", scope = full_model_fomula)
  summary(forward_model)
  
  
  standardized_residules = rstandard(forward_model)
  
  # Comparing the distribution of the residuals with the standard distribution
  qqnorm(standardized_residules, ylab = "Quantiles of Standardized Residuals", xlab = "Quantiles of Standard Distribution", main = "Q - Q plot for Residuals and Normal Distribution")
  qqline(standardized_residules)
  
  # Testing normality using shapiro test
  shapiro.test(forward_model$residuals)
  #p-value < 2.2e-16
  
  # looking for variance and linearity
  for(column in column_names){
    
    if(class(dataSet[[column]]) != "factor"){
      plot( y = forward_model$residuals , x = dataSet[[column]], xlab = column, ylab = "Rediduals")
      abline(h = 0, col = "red")      
    }

  }
  
  
} 

if(is.factor(dataSet[[parameter]])){

  if(length(levels(dataSet[[parameter]])) == 2){
    
    min_model_fomula = as.formula(paste(parameter, paste(c(1)), sep = "~"))
    column_names = names(dataSet)
    column_names = column_names[column_names != "X" & column_names != paste(parameter)]
    full_model_fomula = as.formula(paste("~", paste(column_names, collapse = "+"),sep = ""))
    
    min_binary_model = glm(formula = min_model_fomula, data = dataSet, family=binomial(link = logit))
    summary(min_binary_model)
    
    forward_binary_model = step(min_binary_model, direction = "forward", scope = full_model_fomula)
    summary(forward_binary_model)
  }
  
  
}

  #"District"



