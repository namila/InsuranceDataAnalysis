my_function = function(dataSet, parameter){
  
  for(column_name in colnames(dataSet)){
    data_column = dataSet[[column_name]]
    no_of_missing_values = sum(is.na(data_column))
   
    if(is.numeric(data_column) == TRUE){
      cat(column_name," is quantitative \n")
      
      if(no_of_missing_values > 0){
        mean_value = mean(data_column, na.rm = TRUE)
        data_column[is.na(data_column)] = mean_value
        
      }
    } else{
      cat(column_name," is qualititative \n")
      
      if(no_of_missing_values > 0){
        mode_value = mode(data_column)
        data_column[is.na(data_column)] = mode_value
      }
    }
    cat("Number of missing values", no_of_missing_values, "\n")
  }
  
  head(dataSet)
}

head(SLPopulation)
my_function(SLPopulation, "")
is.na(insuranceData$district)
insuranceData$gender[is.na(insuranceData$gender)]
sum(is.na(insuranceData[["age"]]))
cat("asd","ss")

