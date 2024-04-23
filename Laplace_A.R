score_laplace_A <- function(x,pars,h){
  
  mu = pars[1]

  sigma = exp(pars[2])

  kappa = exp(pars[3])

  if( x >= mu){

    d_mu <- sqrt(2)*kappa/sigma

    d_sigma <- sqrt(2)*kappa*(x - mu)/sigma - 1

    d_kappa <- 1 -2*kappa^2/(kappa^2 + 1) - sqrt(2)*kappa*(x - mu)/sigma

  }else{

    d_mu <- -sqrt(2)/(kappa*sigma)
    
    d_sigma <- sqrt(2)*(mu - x)/(kappa*sigma) - 1

    d_kappa <- 1 - 2*kappa^2/(kappa^2 + 1) + sqrt(2)*(mu - x)/(sigma*kappa)
  }
  
  novy_gradient <- c(d_mu, d_kappa, d_sigma)

  nova_inf_matica <- matrix(c(2/sigma^2, -sqrt(2)*2*kappa/(sigma * (1 + kappa^2) ),  0,
                           -sqrt(2)*2*kappa/(sigma * (1 + kappa^2) ), 1 + 4*kappa^2/(1+kappa^2)^2, -(1-kappa^2)/(1+kappa^2),
                           0, -(1-kappa^2)/(1+kappa^2), 1 ), 3, 3)
  

  inv_inf_mat <- ginv_2(nova_inf_matica,h=h)
  
  score <- inv_inf_mat%*%novy_gradient
  
  ll <- LaplacesDemon::dalaplace(x=x, location=mu, scale=sigma, kappa=kappa, log=TRUE)
  
  return(list(score = score, ll = ll))
}
