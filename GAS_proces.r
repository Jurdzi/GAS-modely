# VSEOBECNY GAS PROCES
gas_model <- function(static_param = NULL, 
                      data =NULL, 
                      p=NULL, 
                      q=NULL, 
                      score_fun = NULL, 
                      start_par = NULL, 
                      h = NULL,
                      out = FALSE){

  n <- length(static_param)/(p+q)

  A <- matrix(static_param[1:(n*p) ],n,p)

  B <- matrix(static_param[1:(q*n) + n*p],n,q)
  
  omega <- start_par*(1 - rowSums(B))

  if(is.null(start_par)){
    f_t <- matrix(rep(start_par,q),n,q)
  }else{
    f_t <- matrix(rep(start_par,q),n,q)
  }
  
  s_t <- A*0

  for(i in  1:p){
    S <- score_fun(x = data[i],pars = f_t[,i], h = h )
    s_t[,i] <- S$score
  
  }
  
  s_t <- s_t[,p:1]
  print(s_t)
  llh <- 0
  
  if(out){
    pars_out <- rbind(data[1:p],f_t)
  }



  for(i in (p+1):length(data)){

  f <- omega + rowSums(A*s_t) + rowSums(B*f_t)

  s <- score_fun(x = data[i], pars = f, h = h)
    
  f_t <- cbind(f,f_t)

  f_t <- f_t[,-(q+1)]

  s_t <- cbind(s$score,s_t)

  s_t <- s_t[,-(p+1)]

  llh <- llh + s$ll

  if(out){
    pars_out <- cbind(pars_out,c(data[i],f_t))
  }

  }

  if(out){
    return(pars_out)
  }else{
    return(as.numeric(-llh))
  }  
  

}
