lossES <- function(data = NULL, ES = NULL, percentile = NULL){
  
  S <- rep(0, length(data))
  if(percentile <= 0.5){
    alfa <- percentile
  }else{
    alfa <- 1 - percentile
  }
  
  for (i in 1:length(data)) {
    if(data[i] <= ES[i]){
      S[i] <- alfa*abs(ES[i]-data[i])
    }else{
      S[i] <- (1-alfa)*abs(ES[i]-data[i])
    }
  }
  loss <- mean(S)
  return(loss)
}

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
  
  if(LR_uc >= 0.05 & LR_ind >= 0.05){
    loss <- lossES(data[1,], ES_X, percentile)
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, loss = loss, k = k))
  }else{
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, k = k))
  }
}

#t
test_ES_t <- function(data = NULL, percentile = NULL) {
  
  if(percentile <= 0.5){
    
    ES_X <- data[2,] - exp(data[3,]) * (exp(data[4,]) + qt(percentile, df = exp(data[4,])) ^2) / (percentile*(exp(data[4,])-1)) * dt(qt(percentile, df = exp(data[4,])),df = exp(data[4,])) 
    I <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(I)
    P <- pt( (ES_X-data[2,])/exp(data[3,]), df = exp(data[4,]), lower.tail = TRUE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    
    }else{
      
    ES_X <- data[2,] + exp(data[3,]) * (exp(data[4,]) + qt(percentile, df = exp(data[4,])) ^2) / ((1-percentile)*(exp(data[4,])-1)) * dt(qt(percentile, df = exp(data[4,])),df = exp(data[4,])) 
    I <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(I)
    P <- pt( (ES_X-data[2,])/exp(data[3,]), df = exp(data[4,]), lower.tail = FALSE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    }
  
  if(k <= mu - 0.5){
      LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = TRUE)
    }else{
      LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = FALSE)
    }
  
  LR_ind <- test_LR_ind(I)
  
  if(LR_uc >= 0.05 & LR_ind >= 0.05){
    loss <- lossES(data[1,], ES_X, percentile)
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, loss = loss, k = k))
  }else{
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, k = k))
  }
}

#laplace
test_ES_laplace <- function(data = NULL, percentile = NULL) {
  
  if(percentile <= 0.5){
      
    ES_X <- data[2,] - exp(data[3,]) * (1-log(2*percentile))
    I <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(I)
    P <- extraDistr::plaplace(ES_X, data[2,], exp(data[3,]), lower.tail = TRUE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    
  }else{
    
    ES_X <- data[2,] + exp(data[3,]) * (1-log(2*(1-percentile)))
    I <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(I)
    P <- extraDistr::plaplace(ES_X, data[2,], exp(data[3,]), lower.tail = FALSE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
  }
  
  if(k <= mu - 0.5){
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = TRUE)
  }else{
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = FALSE)
  }
  
  LR_ind <- test_LR_ind(I)
  
  if(LR_uc >= 0.05 & LR_ind >= 0.05){
    loss <- lossES(data[1,], ES_X, percentile)
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, loss = loss, k = k))
  }else{
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, k = k))
  }
}

#A laplace 
test_ES_alaplace <- function(data = NULL, percentile = NULL) {
  
  ES_X <- rep(0,length(data[1,]))
  P <- rep(0,length(data[1,]))
  
  if(percentile <= 0.5){
    for (i in 1:length(data[1,])) {
      if(percentile < exp(data[4,i])^2/(1+exp(data[4,i])^2)){
        ES_X[i] <- data[2,i] - exp(data[3,i]) * exp(data[4,i]) * ( 1 - log( (1+exp(data[4,i])^2)*percentile/exp(data[4,i])^2 )) / sqrt(2)
      }else{
        ES_X[i] <- data[2,i] - exp(data[3,i])/(sqrt(2)*percentile*exp(data[4,i])) * ( (1-percentile)*( 1 - log( (1+exp(data[4,i])^2)*(1-percentile))) - 1 + exp(data[4,i])^2 )   
      }
      P[i] <- LaplacesDemon::palaplace(ES_X[i], location=data[2,i], scale=exp(data[3,i]), kappa=exp(data[4,i]))
    }
    
    I <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(I)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    
  }else{
    
    for (i in 1:length(data[1,])) {
      if(percentile < exp(data[4,i])^2/(1+exp(data[4,i])^2)){
        ES_X[i] <- data[2,i] + exp(data[3,i])/(sqrt(2)*(1-percentile)*exp(data[4,i])) * ( percentile*( 1 - log( (1+exp(data[4,i])^2)*percentile)) - 1 + exp(data[4,i])^2 )
      }else{
        ES_X[i] <- data[2,i] + exp(data[3,i]) * exp(data[4,i]) * ( 1 - log( (1+exp(data[4,i])^2)*(1-percentile)/exp(data[4,i])^2 )) / sqrt(2)
      }
      P[i] <- 1 - LaplacesDemon::palaplace(ES_X[i], location=data[2,i], scale=exp(data[3,i]), kappa=exp(data[4,i]))
    }
    
    I <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(I)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
  }
  
  if(k <= mu - 0.5){
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = TRUE)
  }else{
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = FALSE)
  }
  
  LR_ind <- test_LR_ind(I)
  
  if(LR_uc >= 0.05 & LR_ind >= 0.05){
    loss <- lossES(data[1,], ES_X, percentile)
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, loss = loss, k = k))
  }else{
    return(list(LR_uc = LR_uc, LR_ind = LR_ind, k = k))
  }
}


#AST
test_ES_AST <- function(data = NULL, percentile = NULL) {
  
  ES_X <- rep(0,length(data[1,]))
  
  if(percentile <= 0.5){
    
    for (i in 1:length(data[1,])) {
      if(percentile <= exp(data[4,i])){
        ES_X[i] <- data[2,i] - ( 2*exp(data[4,i]) )^2 * K_func(exp(data[5,i])) * exp(data[3,i]) * (exp(data[5,i]) + qt(percentile/(2*exp(data[4,i])), df = exp(data[5,i])) ^2) * dt(qt(percentile/(2*exp(data[4,i])), df = exp(data[5,i])),df = exp(data[5,i])) / (percentile*(exp(data[5,i])-1))
      }else{
        ES_X[i] <- data[2,i] - ( 2*(1-exp(data[4,i])) )^2 * K_func(exp(data[6,i])) * exp(data[3,i]) * (exp(data[6,i]) + qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[6,i])) ^2) * dt(qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[6,i])),df = exp(data[6,i])) / (percentile*(exp(data[6,i])-1)) - 4*exp(data[3,i])/percentile * ( (exp(data[4,i])*K_func(exp(data[5,i]))^2*exp(data[5,i])) / ( exp(data[5,i])-1) - ( (1-exp(data[4,i]))*K_func(exp(data[6,i]))^2*exp(data[6,i]) / ( exp(data[6,i])-1) ))  
      }
    }
    
    I <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(I)
    P <- pAST(ES_X, mu = data[2,], sigma = exp(data[3,]), alfa = exp(data[4,]), nu_1 = exp(data[5,]), nu_2 = exp(data[6,]))
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    
  }else{
    
    for (i in 1:length(data[1,])) {
      if(percentile > exp(data[4,i])){
        ES_X[i] <- data[2,i] + ( 2*exp(data[4,i]) )^2 * K_func(exp(data[6,i])) * exp(data[3,i]) * (exp(data[6,i]) + qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[6,i])) ^2) * dt(qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[6,i])),df = exp(data[6,i])) / ((1-percentile)*(exp(data[6,i])-1))
      }else{
        ES_X[i] <- data[2,i] + ( 2*(1-exp(data[4,i])) )^2 * K_func(exp(data[6,i])) * exp(data[3,i]) * (exp(data[6,i]) + qt(percentile/(2*exp(data[4,i])), df = exp(data[6,i])) ^2) * dt(qt(percentile/(2*exp(data[4,i])), df = exp(data[6,i])),df = exp(data[6,i])) / (percentile*(exp(data[6,i])-1)) + 4*exp(data[3,i])/percentile * ( (exp(data[4,i])*K_func(exp(data[5,i]))^2*exp(data[5,i])) / ( exp(data[5,i])-1) - ( (1-exp(data[4,i]))*K_func(exp(data[6,i]))^2*exp(data[6,i]) / ( exp(data[6,i])-1) ))  
      }
    }
    
    I <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(I)
    P <- 1 - pAST(ES_X, mu = data[2,], sigma = exp(data[3,]), alfa = exp(data[4,]), nu_1 = exp(data[5,]), nu_2 = exp(data[6,]))
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
  }
  
  if(k <= mu - 0.5){
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = TRUE)
  }else{
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = FALSE)
  }
  
  LR_ind <- test_LR_ind(I)
  
  return(list(LR_uc = LR_uc, LR_ind = LR_ind, k = k))
}

#SST
test_ES_SST <- function(data = NULL, percentile = NULL) {
  
  ES_X <- rep(0,length(data[1,]))
  
  if(percentile <= 0.5){
    
    for (i in 1:length(data[1,])) {
      if(percentile <= exp(data[4,i])){
        ES_X[i] <- data[2,i] - ( 2*exp(data[4,i]) )^2 * K_func(exp(data[5,i])) * exp(data[3,i]) * (exp(data[5,i]) + qt(percentile/(2*exp(data[4,i])), df = exp(data[5,i])) ^2) * dt(qt(percentile/(2*exp(data[4,i])), df = exp(data[5,i])),df = exp(data[5,i])) / (percentile*(exp(data[5,i])-1))
      }else{
        ES_X[i] <- data[2,i] - ( 2*(1-exp(data[4,i])) )^2 * K_func(exp(data[5,i])) * exp(data[3,i]) * (exp(data[5,i]) + qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[5,i])) ^2) * dt(qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[5,i])),df = exp(data[5,i])) / (percentile*(exp(data[5,i])-1)) + 4*exp(data[3,i])/percentile * ( (exp(data[4,i])*K_func(exp(data[5,i]))^2*exp(data[5,i])) / ( exp(data[5,i])-1) - ( (1-exp(data[4,i]))*K_func(exp(data[5,i]))^2*exp(data[5,i]) / ( exp(data[5,i])-1) ))  
      }
    }
    
    I <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(I)
    P <- pSST(ES_X, mu = data[2,], sigma = exp(data[3,]), alfa = exp(data[4,]), nu = exp(data[5,]))
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    
  }else{
    
    for (i in 1:length(data[1,])) {
      if(percentile > exp(data[4,i])){
        ES_X[i] <- data[2,i] + ( 2*exp(data[4,i]) )^2 * K_func(exp(data[5,i])) * exp(data[3,i]) * (exp(data[5,i]) + qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[5,i])) ^2) * dt(qt((percentile+1-2*exp(data[4,i]))/(2*(1-exp(data[4,i]))), df = exp(data[5,i])),df = exp(data[5,i])) / ((1-percentile)*(exp(data[5,i])-1))
      }else{
        ES_X[i] <- data[2,i] + ( 2*(1-exp(data[4,i])) )^2 * K_func(exp(data[5,i])) * exp(data[3,i]) * (exp(data[5,i]) + qt(percentile/(2*exp(data[4,i])), df = exp(data[5,i])) ^2) * dt(qt(percentile/(2*exp(data[4,i])), df = exp(data[5,i])),df = exp(data[5,i])) / (percentile*(exp(data[5,i])-1)) + 4*exp(data[3,i])/percentile * ( (exp(data[4,i])*K_func(exp(data[5,i]))^2*exp(data[5,i])) / ( exp(data[5,i])-1) - ( (1-exp(data[4,i]))*K_func(exp(data[5,i]))^2*exp(data[5,i]) / ( exp(data[5,i])-1) ))  
      }
    }
    
    I <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(I)
    P <- 1 - pSST(ES_X, mu = data[2,], sigma = exp(data[3,]), alfa = exp(data[4,]), nu = exp(data[5,]))
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
  }
  
  if(k <= mu - 0.5){
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = TRUE)
  }else{
    LR_uc <- 2*pnorm((k + 0.5 - mu)/sigma, 0, 1, lower.tail = FALSE)
  }
  
  LR_ind <- test_LR_ind(I)
  
  return(list(LR_uc = LR_uc, LR_ind = LR_ind, k = k))
}
