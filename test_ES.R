test_ES_norm <- function(data = NULL, percentile = NULL) {
  
  if(percentile <= 0.5){
    
    ES_X <- data[2,] - exp(data[3,])/(percentile*sqrt(2*pi))*exp(-qnorm(percentile)^2/2)
    v <- ifelse(ES_X>data[1,], 1, 0)
    k <- sum(v)
    P <- pnorm(ES_X, lower.tail = TRUE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    LR_uc <- pnorm(k, mu, sigma, lower.tail = TRUE)
    
    }else{
      
    ES_X <- data[2,] + exp(data[3,])/((1-percentile)*sqrt(2*pi))*exp(-qnorm(percentile)^2/2)
    v <- ifelse(ES_X<data[1,], 1, 0)
    k <- sum(v)
    P <- pnorm(ES_X, lower.tail = FALSE)
    mu <- sum(P)
    sigma <- sqrt(sum(P*(1-P)))
    LR_uc <- pnorm(k, mu, sigma, lower.tail = FALSE)
    
  }
  return(LR_uc)
}
