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
    
    x = data[1,]
    mu = data[2,]
    sigma = exp(data[3,])
    alfa = exp(data[4,])
    nu = exp(data[5,])
    
    quantile <- rep(0,length(x))
    
    for(i in 1:length(x)){
      if(x[i] <= mu[i]){
        quantile[i] <- mu[i] + 2*alfa[i]*sigma[i]*K_func(nu[i])*qt(percentile/(2*alfa[i]), df = nu[i])
      }else{
        quantile[i] <- mu[i] + 2*(1-alfa[i])*sigma[i]*K_func(nu[i])*qt( (percentile+1-2*alfa[i]) / (2*(1-alfa[i])), df = nu[i] )
      }
    }
      
  }
  
  if(percentile <= 0.5){
    test <- BacktestVaR(data = data[1,], VaR = quantile, alpha = percentile)
  }else{
    test <- BacktestVaR(data = -data[1,], VaR = -quantile, alpha = 1 - percentile)
  }
  
  return(list(quantile = quantile, test = test))
  
}
