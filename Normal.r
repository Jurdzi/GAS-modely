score_norm <- function(x, pars, h ){
  
  mu = pars[1]
  
  sigma = exp(pars[2])
 
  d_mu <- (x - mu)/sigma^2

  d_sigma <- (x - mu)^2/sigma^2 - 1

  gradient <- c(d_mu, d_sigma)

  inf_mat <- diag(1/sigma^2 , 2)

  inv_inf_mat <- ginv_2(inf_mat, h = h)
  
  score <- inv_inf_mat%*%gradient
  
  ll <- dnorm(x, mean = mu, sd = sigma, log = TRUE)
  
  return(list(score = score, ll = ll))
}
