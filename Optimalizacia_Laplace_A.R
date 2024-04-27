# Optimalizacia

okno <- 84 

pocet_dat <- length(SP500_data)

par_mat_SP500 <- c()

for (i in 0:11) {
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_Laplace_A(x = x,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20), 
                         upper =  c(1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 300, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon"),
                           parVar = c("opt_data","uncond_Laplace_A", "SP500_data","dalaplace")))
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = SP500_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_laplace_A,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,3), rep(0,3)), 
                         upper =  c(rep(1,3), rep(0.99999,3)), control = DEoptim.control(
                           itermax = 200, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon","rlist","tidyr","tidyverse","reshape2","MASS"),
                           parVar = c("gas_model","score_laplace_A","SP500_data", "ginv_2","opt_data","start_p",
                                    "dalaplace")))
  

  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,3),rep(0,3)),
                    UB = c(rep(Inf,3),rep(0.99999,3)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(SP500_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_laplace_A,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_SP500 <- cbind(par_mat_SP500,a[,pred_ind])
  
}

plot(par_mat_SP500[1,],type = "l")
points(par_mat_SP500[2,] + exp(par_mat_SP500[3,])/sqrt(2)*(1/exp(par_mat_SP500[4,]) - exp(par_mat_SP500[4,])),type = "l", col = "green")
points(exp(par_mat_SP500[3,])/sqrt(2)*sqrt(1/exp(par_mat_SP500[4,])^2 + exp(par_mat_SP500[4,])^2),type = "l", col = "red")
points(par_mat_SP500[2,] + exp(par_mat_SP500[3,])/sqrt(2)*(1/exp(par_mat_SP500[4,]) - exp(par_mat_SP500[4,])) + 
         (exp(par_mat_SP500[3,])/sqrt(2)*sqrt(1/exp(par_mat_SP500[4,])^2 + exp(par_mat_SP500[4,])^2))*qalaplace(0.05),type = "l", col = "red")


################################################################################

pocet_dat <- length(gold_data)

par_mat_gold <- c()

for (i in 0:11) {
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_Laplace_A(x = x,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20), 
                         upper =  c(1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 300, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon"),
                           parVar = c("opt_data","uncond_Laplace_A", "gold_data","dalaplace")))
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = gold_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_laplace_A,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,3), rep(0,3)), 
                         upper =  c(rep(1,3), rep(0.99999,3)), control = DEoptim.control(
                           itermax = 200, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon","rlist","tidyr","tidyverse","reshape2","MASS"),
                           parVar = c("gas_model","score_laplace_A","gold_data", "ginv_2","opt_data","start_p",
                                      "dalaplace")))
  
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,3),rep(0,3)),
                    UB = c(rep(Inf,3),rep(0.99999,3)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(gold_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_laplace_A,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_gold <- cbind(par_mat_gold,a[,pred_ind])
  
}

plot(par_mat_gold[1,],type = "l")
points(par_mat_gold[2,] + exp(par_mat_gold[3,])/sqrt(2)*(1/exp(par_mat_gold[4,]) - exp(par_mat_gold[4,])),type = "l", col = "green")
points(exp(par_mat_gold[3,])/2*sqrt(1/exp(par_mat_gold[4,])^2 + exp(par_mat_gold[4,])^2),type = "l", col = "red")
points(par_mat_gold[2,] + exp(par_mat_gold[3,])/sqrt(2)*(1/exp(par_mat_gold[4,]) - exp(par_mat_gold[4,]))+
         (exp(par_mat_gold[3,])/2*sqrt(1/exp(par_mat_gold[4,])^2 + exp(par_mat_gold[4,])^2))*qalaplace(0.05),type = "l", col = "red")

################################################################################

pocet_dat <- length(btc_data)

par_mat_btc <- c()

for (i in 0:20) {
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_Laplace_A(x = x,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20), 
                         upper =  c(1,20,1), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 300, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon"),
                           parVar = c("opt_data","uncond_Laplace_A", "btc_data","dalaplace")))
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = btc_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_laplace_A,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,3), rep(0,3)), 
                         upper =  c(rep(1,3), rep(0.99999,3)), control = DEoptim.control(
                           itermax = 200, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon","rlist","tidyr","tidyverse","reshape2","MASS"),
                           parVar = c("gas_model","score_laplace_A","btc_data", "ginv_2","opt_data","start_p",
                                      "dalaplace")))
  
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,3),rep(0,3)),
                    UB = c(rep(Inf,3),rep(0.99999,3)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(btc_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_laplace_A,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_btc <- cbind(par_mat_btc,a[,pred_ind])
  
}

plot(par_mat_btc[1,],type = "l")
points(par_mat_btc[2,] + exp(par_mat_btc[3,])/sqrt(2)*(1/exp(par_mat_btc[4,]) - exp(par_mat_btc[4,])),type = "l", col = "green")
points(exp(par_mat_btc[3,])/2*sqrt(1/exp(par_mat_btc[4,])^2 + exp(par_mat_btc[4,])^2),type = "l", col = "red")
points(par_mat_btc[2,] + exp(par_mat_btc[3,])/sqrt(2)*(1/exp(par_mat_btc[4,]) - exp(par_mat_btc[4,]))+
         (exp(par_mat_btc[3,])/2*sqrt(1/exp(par_mat_btc[4,])^2 + exp(par_mat_btc[4,])^2))*qalaplace(0.05),type = "l", col = "red")

################################################################################

pocet_dat <- length(eurusd_data)

par_mat_eurusd <- c()

for (i in 0:12) {
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_Laplace_A(x = x,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20), 
                         upper =  c(1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 300, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon"),
                           parVar = c("opt_data","uncond_Laplace_A", "eurusd_data","dalaplace")))
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = eurusd_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_laplace_A,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,3), rep(0,3)), 
                         upper =  c(rep(1,3), rep(0.99999,3)), control = DEoptim.control(
                           itermax = 200, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon","rlist","tidyr","tidyverse","reshape2","MASS"),
                           parVar = c("gas_model","score_laplace_A","eurusd_data", "ginv_2","opt_data","start_p",
                                      "dalaplace")))
  
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,3),rep(0,3)),
                    UB = c(rep(Inf,3),rep(0.99999,3)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(eurusd_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_laplace_A,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_eurusd <- cbind(par_mat_eurusd,a[,pred_ind])
  
}

plot(par_mat_eurusd[1,],type = "l")
points(par_mat_eurusd[2,] + exp(par_mat_eurusd[3,])/sqrt(2)*(1/exp(par_mat_eurusd[4,]) - exp(par_mat_eurusd[4,])),type = "l", col = "green")
points(exp(par_mat_eurusd[3,])/2*sqrt(1/exp(par_mat_eurusd[4,])^2 + exp(par_mat_eurusd[4,])^2),type = "l", col = "red")
points(par_mat_eurusd[2,] + exp(par_mat_eurusd[3,])/sqrt(2)*(1/exp(par_mat_eurusd[4,]) - exp(par_mat_eurusd[4,]))+
         (exp(par_mat_eurusd[3,])/2*sqrt(1/exp(par_mat_eurusd[4,])^2 + exp(par_mat_eurusd[4,])^2))*qalaplace(0.05),type = "l", col = "red")

###############################################################################

pocet_dat <- length(bonds_data)

par_mat_bonds <- c()

for (i in 0:11) {
  
  opt_data <- 1:504 + i * okno
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        uncond_Laplace_A(x = x,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(-1,-20,-20), 
                         upper =  c(1,20,20), control = DEoptim.control(
                           CR = 0.8,F = 1.2, 
                           itermax = 300, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon"),
                           parVar = c("opt_data","uncond_Laplace_A", "bonds_data","dalaplace")))
  
  start_p <- de_opt_norm$optim$bestmem
  
  opt_fun <- function(x){
    
    out <- tryCatch(
      {
        gas_model(static_param = x,               
                  data = bonds_data[opt_data], 
                  p = 1, 
                  q = 1, 
                  score_fun = score_laplace_A,
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
  
  de_opt_norm <- DEoptim(fn = opt_fun, lower = c(rep(0,3), rep(0,3)), 
                         upper =  c(rep(1,3), rep(0.99999,3)), control = DEoptim.control(
                           itermax = 200, strategy = 1, c = 0.2,
                           parallelType = 1,
                           cluster = cl,
                           packages = c("LaplacesDemon","rlist","tidyr","tidyverse","reshape2","MASS"),
                           parVar = c("gas_model","score_laplace_A","bonds_data", "ginv_2","opt_data","start_p",
                                      "dalaplace")))
  
  
  opt_norm <- solnp(pars = de_opt_norm$optim$bestmem, 
                    fun = opt_fun,
                    LB = c(rep(0,3),rep(0,3)),
                    UB = c(rep(Inf,3),rep(0.99999,3)))
  
  pred_data <- 1:(504+okno) + i * okno
  
  if(((504+okno) + i * okno) > pocet_dat){
    pred_data <- pred_data[1]:pocet_dat
  }
  
  a <- gas_model(static_param = opt_norm$pars,               
                 data = as.vector(bonds_data[pred_data]), 
                 p = 1, 
                 q = 1, 
                 score_fun = score_laplace_A,
                 h = -1,
                 start_par = start_p,
                 out = TRUE)
  
  pred_ind <- 505:ncol(a)
  
  par_mat_bonds <- cbind(par_mat_bonds,a[,pred_ind])
  
}

plot(par_mat_bonds[1,],type = "l")
points(par_mat_bonds[2,] + exp(par_mat_bonds[3,])/sqrt(2)*(1/exp(par_mat_bonds[4,]) - exp(par_mat_bonds[4,])),type = "l", col = "green")
points(exp(par_mat_bonds[3,])/2*sqrt(1/exp(par_mat_bonds[4,])^2 + exp(par_mat_bonds[4,])^2),type = "l", col = "red")
points(par_mat_bonds[2,] + exp(par_mat_bonds[3,])/sqrt(2)*(1/exp(par_mat_bonds[4,]) - exp(par_mat_bonds[4,]))+
         (exp(par_mat_bonds[3,])/2*sqrt(1/exp(par_mat_bonds[4,])^2 + exp(par_mat_bonds[4,])^2))*qalaplace(0.05),type = "l", col = "red")

################################################################################

save(par_mat_SP500, par_mat_gold, par_mat_btc, par_mat_eurusd, par_mat_bonds, file = "opt_Laplace_A.RData")

load("opt_Laplace_A.RData")
