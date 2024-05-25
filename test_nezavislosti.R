test_LR_ind <- function(I) {
  
  N_00 <- 0
  N_01 <- 0
  N_10 <- 0
  N_11 <- 0
  
  for (i in 2:length(I)) {
    if(I[i] == 0 & I[i-1] == 0){
      N_00 <- N_00 + 1
    }else if(I[i] == 0 & I[i-1] == 1){
      N_10 <- N_10 + 1
    }else if(I[i] == 1 & I[i-1] == 0){
      N_01 <- N_01 + 1
    }else{
      N_11 <- N_11 + 1
    }
  }
  
  N_0 <- N_00 + N_10
  N_1 <- N_11 + N_01
  pi_01 <- N_01/(N_00+N_01)
  pi_11 <- N_11/(N_10+N_11)
  pi_s <- (N_01 + N_11)/(N_00 + N_10 + N_01 + N_11)
  
  LR_ind <- pchisq( -2*log( (1-pi_s)^N_0 * pi_s^N_1 / ( (1-pi_01)^N_00 * pi_01^N_01 * (1-pi_11)^N_10 * pi_11^N_11 )  ), df = 1, lower.tail = FALSE)
  
}
