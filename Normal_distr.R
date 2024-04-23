score_norm <- function(x = NULL, pars = NULL, h = NULL){

  mu <- pars[1]

  sigma <- exp(pars[2]) 
  
  new_grad <- c((x-mu)/sigma,(-1+(x-mu)^2/sigma)/2)
  
  new_inf_mat <- diag(c(1/sigma,1/2))
  
  inv_inf_mat <- ginv_2(new_inf_mat, h = h)

  score <- inv_inf_mat%*%new_grad

  ll <- dnorm(x = x, mean = mu, sd = sqrt(sigma), log = TRUE)
  
  return(list(score = score, ll = ll))

}

