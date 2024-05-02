qSST <- function(percentile, data) {
  
  mu = data[2,]
  sigma = exp(data[3,])
  alfa = exp(data[4,])/(1+exp(data[4,]))
  nu = exp(data[5,])
  
  quantile <- rep(0,length(mu))
  
  for(i in 1:length(mu)){
    if(percentile <= alfa[i]){
      quantile[i] <- mu[i] + 2*alfa[i]*sigma[i]*K_func(nu[i])*qt(percentile/(2*alfa[i]), df = nu[i])
    }else{
      quantile[i] <- mu[i] + 2*(1-alfa[i])*sigma[i]*K_func(nu[i])*qt( (percentile+1-2*alfa[i]) / (2*(1-alfa[i])), df = nu[i] )
    }
  }
  
  return(quantile)
}


qAST <- function(percentile, data) {
  
  mu = data[2,]
  sigma = exp(data[3,])
  alfa = exp(data[4,])/(1+exp(data[4,]))
  nu_1 = exp(data[5,])
  nu_2 = exp(data[6,])
  
  quantile <- rep(0,length(mu))
  
  for(i in 1:length(mu)){
    if(percentile <= alfa[i]){
      quantile[i] <- mu[i] + 2*alfa[i]*sigma[i]*K_func(nu_1[i])*qt(percentile/(2*alfa[i]), df = nu_1[i])
    }else{
      quantile[i] <- mu[i] + 2*(1-alfa[i])*sigma[i]*K_func(nu_2[i])*qt( (percentile+1-2*alfa[i]) / (2*(1-alfa[i])), df = nu_2[i] )
    }
  }
  
  return(quantile)
}
