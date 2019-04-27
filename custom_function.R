my_function = function(dataSet, parameter){
  
  for(column_name in colnames(dataSet)){
    data_column = dataSet[[column_name]]
    no_of_missing_values = sum(is.na(data_column))
   
    if(is.numeric(data_column) == TRUE){
      cat(column_name," is quantitative \n")
      
      if(no_of_missing_values > 0){
        mean_value = mean(data_column, na.rm = TRUE)
        dataSet[[column_name]][is.na(dataSet[[column_name]])] =  mean_value
      }
    } else{
      cat(column_name," is qualititative \n")
      
      if(no_of_missing_values > 0){
        mode_value = mode(data_column)
        cat("Mode value ", mode_value)
        dataSet[[column_name]][is.na(dataSet[[column_name]])] =  mode_value
      }
    }
    cat("Number of missing values", no_of_missing_values, "\n")
  }
  
  head(dataSet)
}

#head(SLPopulation)
my_function(insuranceDataDup, "")

getmode(insuranceDataDup)



