score_AST <- function(x,pars,h){
  
  mu = pars[1]
  sigma = exp(pars[2])
  alfa = exp(pars[3])/(1+exp(pars[3])) 
  nu_1 = exp(pars[4]) 
  nu_2 = exp(pars[5])

  z1 <- 1 + ( ( x - mu )/( 2*alfa*sigma*K_func(nu_1) ) )^2/nu_1

  z2 <- 1 + ( ( x - mu )/( 2*(1-alfa)*sigma*K_func(nu_2) ) )^2/nu_2

  if(x <= mu){
    
    d_mu <- ( nu_1 + 1)/( nu_1*z1 )*( x - mu )/( 2*alfa*sigma*K_func(nu_1) )^2

    d_sigma <- ( nu_1 + 1)/( nu_1*z1 )*( ( x - mu )/( 2*alfa*sigma*K_func(nu_1) ) )^2 - 1

    d_alfa <- ( 1 - alfa )*( nu_1 + 1)/( nu_1*z1 )*( ( x - mu )/( 2*alfa*sigma*K_func(nu_1) ) )^2

    d_nu_1 <- nu_1* ( ( nu_1 + 1)/( nu_1*z1 )*( ( x - mu )/( 2*alfa*sigma*K_func(nu_1) ) )^2*(digamma( (nu_1 + 1)/2) - digamma(nu_1/2) ) - log(z1)  )/2

    d_nu_2 <- 0

  }else{

    d_mu <- ( nu_2 + 1)/( nu_2*z2 )*( x - mu )/( 2*(1-alfa)*sigma*K_func(nu_2) )^2

    d_sigma <- ( nu_2 + 1)/( nu_2*z2 )*( ( x - mu )/( 2*(1-alfa)*sigma*K_func(nu_2) ) )^2 - 1

    d_alfa <- -alfa*( nu_2 + 1)/( nu_2*z2 )*( ( x - mu )/( 2*(1-alfa)*sigma*K_func(nu_2) ) )^2

    d_nu_1 <- 0
    
    d_nu_2 <- nu_2* ( ( nu_2 + 1)/( nu_2*z2 )*( ( x - mu )/( 2*(1-alfa)*sigma*K_func(nu_2) ) )^2*(digamma( (nu_2 + 1)/2) - digamma(nu_2/2) ) - log(z2)  )/2
  }

  grad <- c(d_mu, d_sigma, d_alfa, d_nu_1, d_nu_2)

  inf_mat <- matrix(0,5,5)

  inf_mat[1,] <- c( ( (nu_1 + 1)/(alfa*(nu_1+3)*K_func(nu_1)^2) + (nu_2 + 1)/((1-alfa)*(nu_2+3)*K_func(nu_2)^2))/(4*sigma^2), 
                      -4*(nu_1/(nu_1+3) - nu_2/(nu_2+3))/(3*sigma),
                      -2*((1-alfa)*(nu_1+1)/(nu_1+3) + alfa*(nu_2+1)/(nu_2 + 3))/sigma,
                      nu_1/sigma * ( 1/(nu_1+1) - (nu_1+1)/(nu_1 + 3)*D_func(nu_1) ),
                      -nu_2/sigma * ( 1/(nu_2+1) - (nu_2+1)/(nu_2 + 3)*D_func(nu_2) ))

  inf_mat[2,] <- c( -4*(nu_1/(nu_1+3) - nu_2/(nu_2+3))/(3*sigma),
                    2*(alfa*nu_1/(nu_1 +3) + (1-alfa)*nu_2/(nu_2 + 3)),
                    2*alfa*(1-alfa)*( nu_1/(nu_1+3) - nu_2/(nu_2 + 3)),
                    alfa*nu_1*( nu_1*D_func(nu_1)/(nu_1 + 3) -1/(nu_1 + 1) ),
                    (1-alfa)*nu_2*( nu_2*D_func(nu_2)/(nu_2 + 3) -1/(nu_2 + 1)) )

  inf_mat[3,] <- c( -2*((1-alfa)*(nu_1+1)/(nu_1+3) + alfa*(nu_2+1)/(nu_2 + 3))/sigma,
                     2*alfa*(1-alfa)*( nu_1/(nu_1+3) - nu_2/(nu_2 + 3)),
                     3*alfa^2*(1-alfa)^2*( (nu_1 + 1)/(alfa*(nu_2 + 3) ) + (nu_2 + 1)/( (1-alfa)*(nu_2 + 3) ) ),
                     nu_1*alfa*(1-alfa)*( nu_1*D_func(nu_1)/(nu_1+3) - 1/(nu_1 + 1) ),
                     nu_2*alfa*(1-alfa)*(1/(nu_2 + 1) - nu_2*D_func(nu_2)/(nu_2+3) ))

  inf_mat[4,] <- c( nu_1/sigma * ( 1/(nu_1+1) - (nu_1+1)/(nu_1 + 3)*D_func(nu_1) ),
                     alfa*nu_1*( nu_1*D_func(nu_1)/(nu_1 + 3) -1/(nu_1 + 1) ),
                     nu_1*alfa*(1-alfa)*( nu_1*D_func(nu_1)/(nu_1+3) - 1/(nu_1 + 1) ),
                     nu_1^2*alfa/2 *(nu_1*D_func(nu_1)^2/(nu_1 + 3) - 2*D_func(nu_1)/(nu_1 + 1) - D_der(nu_1)),
                     0)
  inf_mat[5,] <- c( -nu_2/sigma * ( 1/(nu_2+1) - (nu_2+1)/(nu_2 + 3)*D_func(nu_2) ),
                     (1-alfa)*nu_2*( nu_2*D_func(nu_2)/(nu_2 + 3) -1/(nu_2 + 1)),
                     nu_2*alfa*(1-alfa)*(1/(nu_2 + 1) - nu_2*D_func(nu_2)/(nu_2+3) ),
                     0,
                     nu_2^2*(1-alfa)/2*(nu_2*D_func(nu_2)^2/(nu_2 + 3) - 2*D_func(nu_2)/(nu_2 + 1) - D_der(nu_2)) )

  inv_inf_mat <- ginv_2(inf_mat,h=h)
  
  score <- inv_inf_mat%*%grad
  
  ll <- dens_AST(x = x, mu = mu, sigma = sigma, alfa = alfa, nu_1 = nu_1, nu_2 = nu_2, log = TRUE)
  
  return(list(score = score, ll = ll))
}