score_laplace <- function(x,pars,h){
  
  mu = pars[1]
  sigma = exp(pars[2])
  
  novy_gradient <- c(sign(x-mu)/sigma,-1+abs(x-mu)/sigma)
  
  nova_inf_matica <- diag(c(1/sigma^2,1))
  
  inv_inf_mat <- ginv_2(nova_inf_matica,h=h)
  
  score <- inv_inf_mat%*%novy_gradient
  
  ll <- extraDistr::dlaplace(x=x, mu = mu, sigma = sigma, log = TRUE)
  
  return(list(score = score, ll = ll))
}