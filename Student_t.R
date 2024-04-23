score_t <- function(x, pars, h ){
  
  mu = pars[1]
  
  sigma = exp(pars[2])
  
  nu = exp(pars[3])
  
  k_1 <- digamma((nu+1)/2)-digamma(nu/2)-1/nu-log(1+(x-mu)^2/(nu*sigma))+((nu+1)*(x-mu)^2)/(sigma*(nu^2+((nu*(x-mu)^2)/sigma)))
  k_2 <- trigamma((nu+1)/2)/2-trigamma(nu/2)/2+1/(nu*(nu+1))-1/(nu+1)+(nu+2)/(nu*(nu+3))
  
  novy_gradient <- c( ((nu+1)*(x-mu))/(nu*sigma+(x-mu)^2),
                      -(1-((nu+1)*(x-mu)^2)/(nu*sigma+(x-mu)^2))/2,
                      nu*k_1/2)
  
  nova_inf_matica <- matrix(c((nu+1)/((nu+3)*sigma),0,0,
                              0,nu/(2*nu + 6), -nu/((nu+3)*(nu+1)),
                              0,-nu/((nu+3)*(nu+1)), -(k_2*nu^2)/2),3,3)
  
  inv_inf_mat <- ginv_2(nova_inf_matica, h = h)
  
  score <- inv_inf_mat%*%novy_gradient
  
  ll <- log(dt(x = (x-mu)/sqrt(sigma), df = nu, log = FALSE)/sqrt(sigma))
  
  return(list(score = score, ll = ll))
}
