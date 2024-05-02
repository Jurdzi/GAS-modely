score_t <- function(x, pars, h ){
  
  mu = pars[1]
  
  sigma = exp(pars[2])
  
  nu = exp(pars[3])
  
  z <- 1 + ( x - mu )^2/( sigma^2 * nu  ) 
  #
  d_mu <- (nu + 1) * ( x - mu )/( z * nu * sigma^2)

  d_sigma <- (nu + 1) * ( x - mu )^2 /( z * nu * sigma^2)  - 1

  d_nu <- ((nu + 1) * ( x - mu )^2 /( z * nu * sigma^2) - 1 + nu*( digamma((nu+1)/2) - digamma(nu/2) - log(z)))/2
  
  gradient <- c(d_mu, d_sigma, d_nu)

  # inf mat
  inf_mat <- matrix(0,3,3)

  inf_mat[1,] <- c( (nu + 1)/(( nu + 3 ) * sigma^2)  , 0 , 0)  

  inf_mat[2,] <- c(0, 2 * nu/ (nu + 3) ,  2 * nu /( (nu + 1)*( nu+3 )))

  inf_mat[3,] <- c( 0, 2 * nu /( (nu + 1)*( nu+3 )), -nu^2/2 *( trigamma((nu+1)/2)/2 - trigamma(nu/2)/2 -1/((nu+1)*nu) -1/(nu+1) + (nu+2)/( (nu+3)*nu)   ))

  inv_inf_mat <- ginv_2(inf_mat, h = h)
  
  score <- inv_inf_mat%*%gradient
  
  ll <- log(dt(x = (x-mu)/sigma, df = nu , log = FALSE)/sigma)
  
  return(list(score = score, ll = ll))
}

