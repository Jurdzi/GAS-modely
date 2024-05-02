test_VaR <- function(data = NULL, quantile_func = NULL, percentile = NULL) {
  
  if(identical(quantile_func, qnorm)){
    
    quantile <- quantile_func(percentile, mean = data[2,], sd = exp(data[3,]))
    
  }else if (identical(quantile_func, qt)){
    
    quantile <- data[2,] + exp(data[3,])*qt(percentile, df = exp(data[4,]))
    
  }else if (identical(quantile_func, qlaplace)){
    
    quantile <- quantile_func(percentile, data[2,], sqrt(2)*exp(data[3,]))
    
  }else if (identical(quantile_func, qalaplace)){
    
    quantile <- quantile_func(percentile, data[2,], sqrt(2)*exp(data[3,]))
    
  }else if(identical(quantile_func, qSST)){
    
    quantile <- quantile_func(percentile, data)
    
  }else{
    
    quantile <- quantile_func(percentile, data)
    
  }
  
  if(percentile <= 0.5){
    test <- BacktestVaR(data = data[1,], VaR = quantile, alpha = percentile)
  }else{
    test <- BacktestVaR(data = -data[1,], VaR = -quantile, alpha = 1 - percentile)
  }
  
  return(list(quantile = quantile, test = test))
  
}


