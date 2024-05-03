# Optimalizacia

okno <- 84 

pocet_dat <- length(SP500_data)

par_mat_SP500 <- c()

for (i in 0:11) {
  print(i)
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_SST(x = x,
                 data = SP500_data[opt_data])
      },
      error=function(cond) {
        return(.Machine$double.xmax)
      },
      warning=function(cond) {
        return(.Machine$double.xmax)
      })   
    if(is.na(out) || is.nan(out)){
      out <- .Machine$double.xmax
    }   
    return(out)
  }
  


  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20), 
                         upper =  c(1,1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_SST","SP500_data","K_func","dens_SST")))
   
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
    {
        gas_model(static_param = x,               
                  data = SP500_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_SST,
                  start_par = start_p,
                  h = -1)
      },
      error=function(cond) {
        return(sqrt(.Machine$double.xmax))
      },
      warning=function(cond) {
        return(sqrt(.Machine$double.xmax))
      })   
    if(is.na(out) || is.nan(out) || is.infinite(out)){
      out <- sqrt(.Machine$double.xmax)
    }   
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(-1,4), rep(-1,4)), 
                         upper =  c(rep(1,4), rep(0.999999999,4)), control = DEoptim.control(
                           itermax = 300, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_SST","SP500_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_SST","uncond_SST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(-1,4),rep(-1,4)),
                    UB = c(rep(Inf,4),rep(0.99999,4)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(SP500_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_SST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_SP500 <- cbind(par_mat_SP500,a[,pred_ind])
  
}

# PRIDAT AKO VYZERAJU QUANTILOVE FUNKCIE
# 
x <- par_mat_SP500[1,]
mu <- par_mat_SP500[2,]
sigma <- exp(par_mat_SP500[3,])
alfa <- exp(par_mat_SP500[4,])/(1+exp(par_mat_SP500[4,]))
nu <- exp(par_mat_SP500[5,]) + 2

# existuje iba ak je nu > 1
stred <- mu + sigma*(2*sqrt(nu)*(1-2*alfa)*gamma((nu-1)/2)/(sqrt(pi)*gamma(nu/2)))

# existuje iba ak je nu > 2
variacia <- sigma^2*(4*nu*(alfa^3 + (1-alfa)^3 ) * gamma(3/2) * gamma((nu-2)/2) / (sqrt(pi) *gamma(nu/2)) )

# plot
plot(par_mat_SP500[1,] , type = "l")
points(stred, type = "l", col = "green")
points(qSST(0.95,par_mat_SP500), type = "l", col = "red")
points(qSST(0.05,par_mat_SP500), type = "l", col = "red")

###############################################################################

okno <- 84

pocet_dat <- length(gold_data)

par_mat_gold <- c()

for (i in 0:11) {
  print(i)
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_SST(x = x,
                   data = gold_data[opt_data])
      },
      error=function(cond) {
        return(.Machine$double.xmax)
      },
      warning=function(cond) {
        return(.Machine$double.xmax)
      })   
    if(is.na(out) || is.nan(out)){
      out <- .Machine$double.xmax
    }   
    return(out)
  }
  
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20), 
                         upper =  c(1,1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_SST","gold_data","K_func","dens_SST")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = gold_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_SST,
                  start_par = start_p,
                  h = -1)
      },
      error=function(cond) {
        return(sqrt(.Machine$double.xmax))
      },
      warning=function(cond) {
        return(sqrt(.Machine$double.xmax))
      })   
    if(is.na(out) || is.nan(out) || is.infinite(out)){
      out <- sqrt(.Machine$double.xmax)
    }   
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(-1,4), rep(-1,4)), 
                         upper =  c(rep(1,4), rep(0.999999999,4)), control = DEoptim.control(
                           itermax = 300, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_SST","gold_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_SST","uncond_SST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(-Inf,4),rep(-1,4)),
                    UB = c(rep(Inf,4),rep(0.99999,4)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(SP500_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_SST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_gold <- cbind(par_mat_gold,a[,pred_ind])
  
}

x <- par_mat_gold[1,]
mu <- par_mat_gold[2,]
sigma <- exp(par_mat_gold[3,])
alfa <- exp(par_mat_gold[4,])/(1+exp(par_mat_gold[4,]))
nu <- exp(par_mat_gold[5,]) 

# existuje iba ak je nu > 1
stred <- mu + sigma*(2*sqrt(nu)*(1-2*alfa)*gamma((nu-1)/2)/(sqrt(pi)*gamma(nu/2)))

# plot
plot(par_mat_gold[1,] , type = "l")
points(stred, type = "l", col = "green")
points(qSST(0.95,par_mat_gold), type = "l", col = "red")
points(qSST(0.05,par_mat_gold), type = "l", col = "red")

##############################################################################

pocet_dat <- length(btc_data)

par_mat_btc <- c()

for (i in 0:20) {
  print(i)
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_SST(x = x,
                   data = btc_data[opt_data])
      },
      error=function(cond) {
        return(.Machine$double.xmax)
      },
      warning=function(cond) {
        return(.Machine$double.xmax)
      })   
    if(is.na(out) || is.nan(out)){
      out <- .Machine$double.xmax
    }   
    return(out)
  }
  
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20), 
                         upper =  c(1,1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_SST","btc_data","K_func","dens_SST","D_func","D_der")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = btc_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_SST,
                  start_par = start_p,
                  h = -1)
      },
      error=function(cond) {
        return(sqrt(.Machine$double.xmax))
      },
      warning=function(cond) {
        return(sqrt(.Machine$double.xmax))
      })   
    if(is.na(out) || is.nan(out) || is.infinite(out)){
      out <- sqrt(.Machine$double.xmax)
    }   
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,4), rep(0,4)), 
                         upper =  c(rep(1,4), rep(0.999999999,4)), control = DEoptim.control(
                           itermax = 300, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_SST","btc_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_SST","uncond_SST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,4),rep(0,4)),
                    UB = c(rep(Inf,4),rep(0.99999,4)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(btc_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_SST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_btc <- cbind(par_mat_btc,a[,pred_ind])
  
}

x <- par_mat_btc[1,]
mu <- par_mat_btc[2,]
sigma <- exp(par_mat_btc[3,])
alfa <- exp(par_mat_btc[4,])/(1+exp(par_mat_btc[4,]))
nu <- exp(par_mat_btc[5,]) + 2

# existuje iba ak je nu > 1
stred <- mu + sigma*(2*sqrt(nu)*(1-2*alfa)*gamma((nu-1)/2)/(sqrt(pi)*gamma(nu/2)))

# plot
plot(par_mat_btc[1,] , type = "l")
points(stred, type = "l", col = "green")
points(qSST(0.95,par_mat_btc), type = "l", col = "red")
points(qSST(0.05,par_mat_btc), type = "l", col = "red")

#################################################################################

pocet_dat <- length(eurusd_data)

par_mat_eurusd <- c()

for (i in 0:12) {
  print(i)
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_SST(x = x,
                   data = eurusd_data[opt_data])
      },
      error=function(cond) {
        return(.Machine$double.xmax)
      },
      warning=function(cond) {
        return(.Machine$double.xmax)
      })   
    if(is.na(out) || is.nan(out)){
      out <- .Machine$double.xmax
    }   
    return(out)
  }
  
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20), 
                         upper =  c(1,1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_SST","eurusd_data","K_func","dens_SST")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = eurusd_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_SST,
                  start_par = start_p,
                  h = -1)
      },
      error=function(cond) {
        return(sqrt(.Machine$double.xmax))
      },
      warning=function(cond) {
        return(sqrt(.Machine$double.xmax))
      })   
    if(is.na(out) || is.nan(out) || is.infinite(out)){
      out <- sqrt(.Machine$double.xmax)
    }   
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,4), rep(0,4)), 
                         upper =  c(rep(1,4), rep(0.999999999,4)), control = DEoptim.control(
                           itermax = 300, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_SST","eurusd_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_SST","uncond_SST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,4),rep(0,4)),
                    UB = c(rep(Inf,4),rep(0.99999,4)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(eurusd_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_SST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_eurusd <- cbind(par_mat_eurusd,a[,pred_ind])
}
  
x <- par_mat_eurusd[1,]
mu <- par_mat_eurusd[2,]
sigma <- exp(par_mat_eurusd[3,])
alfa <- exp(par_mat_eurusd[4,])/(1+exp(par_mat_eurusd[4,]))
nu <- exp(par_mat_eurusd[5,]) + 2

# existuje iba ak je nu > 1
stred <- mu + sigma*(2*sqrt(nu)*(1-2*alfa)*gamma((nu-1)/2)/(sqrt(pi)*gamma(nu/2)))

# plot
plot(par_mat_eurusd[1,] , type = "l")
points(stred, type = "l", col = "green")
points(qSST(0.95,par_mat_eurusd), type = "l", col = "red")
points(qSST(0.05,par_mat_eurusd), type = "l", col = "red")

###############################################################################

pocet_dat <- length(bonds_data)

par_mat_bonds <- c()

for (i in 0:11) {
  print(i)
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_SST(x = x,
                   data = bonds_data[opt_data])
      },
      error=function(cond) {
        return(.Machine$double.xmax)
      },
      warning=function(cond) {
        return(.Machine$double.xmax)
      })   
    if(is.na(out) || is.nan(out)){
      out <- .Machine$double.xmax
    }   
    return(out)
  }
  
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20), 
                         upper =  c(1,1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_SST","bonds_data","K_func","dens_SST")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = bonds_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_SST,
                  start_par = start_p,
                  h = -1)
      },
      error=function(cond) {
        return(sqrt(.Machine$double.xmax))
      },
      warning=function(cond) {
        return(sqrt(.Machine$double.xmax))
      })   
    if(is.na(out) || is.nan(out) || is.infinite(out)){
      out <- sqrt(.Machine$double.xmax)
    }   
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(-1,4), rep(-1,4)), 
                         upper =  c(rep(1,4), rep(0.999999999,4)), control = DEoptim.control(
                           itermax = 300, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_SST","bonds_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_SST","uncond_SST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(-Inf,4),rep(-1,4)),
                    UB = c(rep(Inf,4),rep(0.99999,4)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(bonds_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_SST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_bonds <- cbind(par_mat_bonds,a[,pred_ind])
  
}

x <- par_mat_bonds[1,]
mu <- par_mat_bonds[2,]
sigma <- exp(par_mat_bonds[3,])
alfa <- exp(par_mat_bonds[4,])/(1+exp(par_mat_bonds[4,]))
nu <- exp(par_mat_bonds[5,]) + 2

# existuje iba ak je nu > 1
stred <- mu + sigma*(2*sqrt(nu)*(1-2*alfa)*gamma((nu-1)/2)/(sqrt(pi)*gamma(nu/2)))

# plot
plot(par_mat_bonds[1,] , type = "l")
points(stred, type = "l", col = "green")
points(qSST(0.95,par_mat_bonds), type = "l", col = "red")
points(qSST(0.05,par_mat_bonds), type = "l", col = "red")

################################################################################

save(par_mat_SP500, par_mat_gold, par_mat_btc, par_mat_eurusd, par_mat_bonds, file = "opt_SST.RData")

load("opt_SST.RData")
