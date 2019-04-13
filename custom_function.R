

my_function = function(dataSet, parameter){
  
  for(column_name in names(dataSet)){
    
    if(is.numeric(dataSet[column_name])){
      paste(column_name," is quantitative")
    } else{
      paste(column_name," is qualititative")
    }  
  }
  
}

my_function(insuranceData, "")



