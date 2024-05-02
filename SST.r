score_SST <- function(x,pars,h){
  
  # MU parametere bez1 zmeny
  mu = pars[1]
  
  # EXPONENCIALNA FUNKCIA
  sigma = exp(pars[2]) 
  
  # Logisticka funckai
  alfa = exp(pars[3])/(1+exp(pars[3])) 

  nu = exp(pars[4]) 

  # zatvorka 1
  z1 <- 1 + ( ( x - mu )/( 2*alfa*sigma*K_func(nu) ) )^2 /nu

  # zatvorka 2
  z2 <- 1 + ( ( x - mu )/( 2*(1-alfa)*sigma*K_func(nu) ) )^2 /nu


  if(x <= mu){

    d_mu <- (nu + 1)*( x - mu)/( nu * z1 * (2 * alfa * sigma * K_func(nu) )^2 )

    # uz transformovane
    d_sigma <- (nu + 1)*(x - mu)^2 / (nu * z1 * (2 * alfa * sigma * K_func(nu) )^2 ) - 1

    # uz transformovane derivacia alfy >> alfa(1-alfa)
    d_alfa <- ( 1 - alfa ) * ( nu + 1 ) * ( x - mu )^2 / (nu * z1 * (2 * alfa * sigma * K_func(nu) )^2 )

    # uz transformovane 
    d_nu <- nu* ((nu + 1)*(x - mu)^2 * (digamma( (nu + 1)/2 ) - digamma(nu/2) ) / ( nu * z1 * (2 * alfa * sigma * K_func(nu) )^2 ) - log(z1)  )/2

  }else{

    d_mu <- (nu + 1)*( x - mu)/( nu * z2 * (2 * (1-alfa) * sigma * K_func(nu) )^2 )

    # uz transformovane
    d_sigma <- (nu + 1)*(x - mu)^2 / (nu * z2 * (2 * (1-alfa) * sigma * K_func(nu) )^2 ) - 1

    # uz transformovane
    d_alfa <- -alfa * ( nu + 1 ) * ( x - mu )^2 / (nu * z2 * (2 * (1 - alfa) * sigma * K_func(nu) )^2 )

    #
    d_nu <- nu * ((nu + 1)*(x - mu)^2 * (digamma( (nu + 1)/2 ) - digamma(nu/2) ) / ( nu * z2 * (2 * (1 - alfa) * sigma * K_func(nu) )^2 ) - log(z2)  )/2
  }

  grad <- c(d_mu, d_sigma, d_alfa, d_nu)

  # poradie mu,sigma,alfa,nu
  inf_mat <- matrix(0,4,4)

  # prvy riadok matice 
  inf_mat[1,] <- c( 1/(4*sigma^2) *( (nu+1)/(alfa*(1-alfa)*(nu+3)* K_func(nu)^2 ) ), 0, -2/sigma * ( (nu+1)/(nu+3) ), 0 )

  # druhy riadok
  inf_mat[2,] <- c(0,2*nu/(nu+3), 0, nu*( nu*D_func(nu)/(nu+3) -1/(nu+1) ) )

  # treti riadok
  inf_mat[3,] <- c(-2/sigma * ( (nu+1)/(nu+3) ),0, 3*alfa*(1-alfa)*(nu+1)/(nu+3), 0)

  # stvrty riadok
  inf_mat[4,] <- c(0, nu*( nu*D_func(nu)/(nu+3) -1/(nu+1) ), 0, nu^2/2 * ( nu*D_func(nu)^2/(nu+3) - 2*D_func(nu)/(nu+1) - D_der(nu)))
  
  inv_inf_mat <- ginv_2(inf_mat, h = h)
  
  score <- inv_inf_mat%*%grad

  ll <- dens_SST(x = x, mu = mu, sigma = sigma, alfa = alfa, nu = nu,  logartihm = TRUE)
  
  return(list(score = score, ll = ll))
}
