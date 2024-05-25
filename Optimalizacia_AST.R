 # Optimalizacia
load("data.RData")

okno <- 84 

pocet_dat <- length(SP500_data)

par_mat_SP500 <- c()

for (i in 0:11) {
  
  print(i)
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_AST(x = x,
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
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20,-20), 
                         upper =  c(1,1,20,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_AST","SP500_data","K_func","dens_AST","D_func", "D_der")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = SP500_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_AST,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,5), rep(0,5)), 
                         upper =  c(rep(0.3,5), rep(0.99999,5)), control = DEoptim.control(
                           itermax = 500, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1, NP = 200,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_AST","SP500_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_AST","uncond_AST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,5),rep(0,5)),
                    UB = c(rep(Inf,5),rep(0.99999,5)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(SP500_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_AST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_SP500 <- cbind(par_mat_SP500,a[,pred_ind])
  
}

plot(par_mat_SP500[1,],type = "l")
points(qAST(0.05,par_mat_SP500), type = "l", col = "red")
points(qAST(0.95,par_mat_SP500), type = "l", col = "red")

###############################################################################

pocet_dat <- length(gold_data)

par_mat_gold <- c()

for (i in 0:11) {
  
  print(i)
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_AST(x = x,
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
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20,-20), 
                         upper =  c(1,1,20,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_AST","gold_data","K_func","dens_AST","D_func", "D_der")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = gold_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_AST,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,5), rep(0,5)), 
                         upper =  c(rep(0.3,5), rep(0.99999,5)), control = DEoptim.control(
                           itermax = 500, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1, NP = 200,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_AST","gold_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_AST","uncond_AST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,5),rep(0,5)),
                    UB = c(rep(Inf,5),rep(0.99999,5)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(gold_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_AST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_gold <- cbind(par_mat_gold,a[,pred_ind])
  
}

plot(par_mat_gold[1,],type = "l")
points(qAST(0.05,par_mat_gold), type = "l", col = "red")
points(qAST(0.95,par_mat_gold), type = "l", col = "red")

###############################################################################

pocet_dat <- length(btc_data)

par_mat_btc <- c()

for (i in 0:20) {
  
  print(i)
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_AST(x = x,
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
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20,-20), 
                         upper =  c(1,1,20,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_AST","btc_data","K_func","dens_AST","D_func", "D_der")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = btc_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_AST,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,5), rep(0,5)), 
                         upper =  c(rep(0.3,5), rep(0.99999,5)), control = DEoptim.control(
                           itermax = 500, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1, NP = 200,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_AST","btc_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_AST","uncond_AST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,5),rep(0,5)),
                    UB = c(rep(Inf,5),rep(0.99999,5)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(btc_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_AST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_btc <- cbind(par_mat_btc,a[,pred_ind])
  
}

plot(par_mat_btc[1,], type = "l", col = "black")
points(qAST(0.05,par_mat_btc), type = "l", col = "red")
points(qAST(0.95,par_mat_btc), type = "l", col = "red")

###############################################################################

pocet_dat <- length(eurusd_data)

par_mat_eurusd <- c()

for (i in 0:12) {
  
  print(i)
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_AST(x = x,
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
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20,-20), 
                         upper =  c(1,1,20,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_AST","eurusd_data","K_func","dens_AST","D_func", "D_der")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = eurusd_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_AST,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,5), rep(0,5)), 
                         upper =  c(rep(0.3,5), rep(0.99999,5)), control = DEoptim.control(
                           itermax = 500, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1, NP = 200,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_AST","eurusd_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_AST","uncond_AST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,5),rep(0,5)),
                    UB = c(rep(Inf,5),rep(0.99999,5)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(eurusd_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_AST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_eurusd <- cbind(par_mat_eurusd,a[,pred_ind])
  
}

plot(par_mat_eurusd[1,], type = "l", col = "black")
points(qAST(0.05,par_mat_eurusd), type = "l", col = "red")
points(qAST(0.95,par_mat_eurusd), type = "l", col = "red")

###############################################################################

pocet_dat <- length(bonds_data)

par_mat_bonds <- c()

for (i in 0:11) {
  
  print(i)
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_AST(x = x,
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
    
    print(as.numeric(out))
    return(out)
  }
  
  
  nCores <- detectCores() - 2
  cl <- makeCluster(nCores)
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20,-20,-20), 
                         upper =  c(1,1,20,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 500, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("extraDistr"),
                           parVar = c("opt_data","uncond_AST","bonds_data","K_func","dens_AST","D_func", "D_der")))
  
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = bonds_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_AST,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,5), rep(0,5)), 
                         upper =  c(rep(0.3,5), rep(0.99999,5)), control = DEoptim.control(
                           itermax = 500, strategy = 2, c = 0.2, p = 0.25, CR= 0.8, F = 1.2,
                           parallelType = 1, NP = 200,
                           cluster = cl,
                           packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2","MASS","extraDistr"),
                           parVar = c("gas_model","score_AST","bonds_data", "ginv_2","opt_data","start_p",
                                      "K_func","dens_AST","uncond_AST","D_func", "D_der")))
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,5),rep(0,5)),
                    UB = c(rep(Inf,5),rep(0.99999,5)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(bonds_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_AST,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_bonds <- cbind(par_mat_bonds,a[,pred_ind])
}

plot(par_mat_bonds[1,], type = "l", col = "black")
points(qAST(0.01,par_mat_bonds), type = "l", col = "red")
points(qAST(0.95,par_mat_bonds), type = "l", col = "red")

################################################################################
save(par_mat_SP500, par_mat_gold, par_mat_btc, par_mat_eurusd, par_mat_bonds, file = "opt_AST.RData")


