#norm 
test_ES_norm <- function(data = NULL, percentile = NULL) {
  
  if(percentile <= 0.5){
    
    ES_X <- data[2,] - exp(data[3,])/(percentile*sqrt(2*pi))*exp(-qnorm(percentile)^2/2)
    I <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(I)
    P <- pnorm(ES_X, data[2,], exp(data[3,]), lower.tail = TRUE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    
  }else{
    
    ES_X <- data[2,] + exp(data[3,])/((1-percentile)*sqrt(2*pi))*exp(-qnorm(percentile)^2/2)
    I <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(I)
    P <- pnorm(ES_X, data[2,], exp(data[3,]), lower.tail = FALSE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
  }
  
  if(k <= mu - 0.5){
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = TRUE)
  }else{
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = FALSE)
  }
  
  LR_ind <- test_LR_ind(I)
  
  return(list(LR_uc = LR_uc, LR_ind = LR_ind))
}

#t
test_ES_t <- function(data = NULL, percentile = NULL) {
  
  if(percentile <= 0.5){
    
    ES_X <- data[2,] - exp(data[3,])*exp(data[4,])*K_func(exp(data[4,])) / (percentile*(exp(data[4,])-1)) * ( 1+qt(percentile, df = exp(data[4,]))^2/exp(data[4,]) )^( -(exp(data[4,])-1)/2 )
    I <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(I)
    P <- pt(ES_X, df = exp(data[4,]))
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    
    }else{
      
    ES_X <- data[2,] + exp(data[3,])*exp(data[4,])*K_func(exp(data[4,])) / ((1-percentile)*(exp(data[4,])-1)) * ( 1+qt(percentile, df = exp(data[4,]))^2/exp(data[4,]) )^( (exp(data[4,])-1)/2 )
    I <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(I)
    P <- pt(ES_X, data[2,], df = exp(data[4,]))
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    }
  
  if(k <= mu - 0.5){
      LR_uc <- 2*pt((k + 0.5 - mu)/sigma, 0, 1, lower.tail = TRUE)
    }else{
      LR_uc <- 2*pt((k + 0.5 - mu)/sigma, 0, 1, lower.tail = FALSE)
    }
  
  LR_ind <- test_LR_ind(I)
  
  return(list(LR_uc = LR_uc, LR_ind = LR_ind))
}
