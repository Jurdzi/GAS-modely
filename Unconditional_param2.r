library(extraDistr)
library(LaplacesDemon)


uncond_Laplace <- function(x, data){

    # mean
    mu = x[1]

    # scale
    sigma = exp(x[2])

    lh <- extraDistr::dlaplace(x=data, mu = mu, sigma = sigma, log = TRUE)

    return(-sum(lh))
}


uncond_Laplace_A <- function(x, data){

    # mean
    mu = x[1]

    # scale
    sigma = exp(x[2])

    # kappa
    kappa = exp(x[3])

    lh <- LaplacesDemon::dalaplace(x=data, location=mu, scale=sigma, kappa=kappa, log=TRUE)

    return(-sum(lh))
}


#########
# Hustota sikmeho studentovho rozdelenia


K_func <- function(x){
  ret <- gamma((x+1)/2)/(sqrt(pi*x) *gamma(x/2))
  return(ret)
}

D_func <- function(x){
  ret <- digamma((x+1)/2) - digamma(x/2)
  return(ret)
}

D_der <- function(x){
  ret <- trigamma((x+1)/2)/2 - trigamma(x/2)/2
  return(ret)
}

dens_SST <- function(x, mu = null, sigma = NULL, alpha = NULL, nu = NULL, logartihm = FALSE){

    # hustota
    d <- numeric(length(x))

    # rozdelenie hustoty
    if( all(x <= mu) ){

        d <- ( 1 + ( (x-mu)/(2*alpha*sigma*K_func(nu)) )^2 / nu )^(-(nu + 1)/2)/sigma

    }else if ( all(x > mu )  ) {

        d <- ( 1 + ( (x-mu)/(2*(1-alpha)*sigma*K_func(nu)) )^2 / nu )^(-(nu + 1)/2)/sigma

    }else{

        d[ x <= mu ] <- ( 1 + ( (x[ x <= mu ] - mu)/(2*alpha*sigma*K_func(nu)) )^2 / nu )^(-(nu + 1)/2)/sigma
        d[ x > mu] <- ( 1 + ( (x[ x > mu] - mu)/(2*(1-alpha)*sigma*K_func(nu)) )^2 / nu )^(-(nu + 1)/2)/sigma

    }

    if(logartihm){
        d <- log(d)
    }

    return(d)
}


dens_AST <- function(x, mu = NULL, sigma = NULL, alpha = NULL, nu1 = NULL, nu2= NULL, logartihm = FALSE){

    # hustota
    d <- numeric(length(x))

    # rozdelenie hustoty
    if( all(x <= mu) ){

        d <- ( 1 + ( (x-mu)/(2*alpha*sigma*K_func(nu1)) )^2 / nu1 )^(-(nu1 + 1)/2)/sigma

    }else if ( all(x > mu )  ) {

        d <- ( 1 + ( (x-mu)/(2*(1-alpha)*sigma*K_func(nu2)) )^2 / nu2 )^(-(nu2 + 1)/2)/sigma

    }else{

        d[ x <= mu ] <- ( 1 + ( (x[ x <= mu ] - mu)/(2*alpha*sigma*K_func(nu1)) )^2 / nu1 )^(-(nu1 + 1)/2)/sigma
        d[ x > mu] <- ( 1 + ( (x[ x > mu] - mu)/(2*(1-alpha)*sigma*K_func(nu2)) )^2 / nu2 )^(-(nu2 + 1)/2)/sigma

    }
    

    if(logartihm){
        d <- log(d)
    }

    return(d)
}

uncond_SST <- function(x, data){

    # mean
    mu = x[1]

    # scale
    sigma = exp(x[2])

    # kappa
    alpha = exp(x[3])/(1 + exp(x[3]))

    # nu
    #nu = exp(x[4]) + 2
    nu = exp(x[4]) 

    lh <- dens_SST(x = data, mu = mu, sigma = sigma, alpha = alpha, nu = nu, logartihm = TRUE)

    return(-sum(lh))
}


uncond_AST <- function(x, data){

     # mean
    mu = x[1]

    # scale
    sigma = exp(x[2])

    # kappa
    alpha = exp(x[3])/(1 + exp(x[3]))

    # nu
    nu1 = exp(x[4]) + 2

    nu2 = exp(x[5]) + 2

    lh <- dens_AST(x = data, mu = mu, sigma = sigma, alpha = alpha, nu1 = nu1, nu2 = nu2, logartihm = TRUE)

    return(-sum(lh))
}

uncond_t <- function(x, data){
  
  # mean
  mu = x[1]
  
  # scale
  sigma = exp(x[2])
  
  # degree of freedom
  nu = exp(x[3]) + 2
  
  std_data <- (data-mu)/sqrt(sigma)
  
  lh <- dt(x = std_data, df = nu, log = FALSE)/sqrt(sigma)
  
  return(-sum(log(lh)))
}
