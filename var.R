load("opt_norm.RData")

var5 <- qnorm(0.05, mean = par_mat_SP500[2,], sd = exp(par_mat_SP500[3,]))
plot(par_mat_SP500[1,],type = "l")
points(var5,type = "l", col ='red')

test1 <- BacktestVaR(data = par_mat_SP500[1,], VaR = var5, alpha = 0.05)

sum(par_mat_SP500[1,] < var5)/length(par_mat_SP500[1,])

test_stat <- test1$LRcc["Test"] - test1$LRuc["Test"]

pchisq(test_stat, df = 1, lower.tail = FALSE)

#lossvar
l1 <- LossVaR(realized = par_mat_SP500[1,], evaluated = var5, tau = 0.05)
mean(l1)


################################################

load("opt_Laplace.RData")

var <- qlaplace(0.05, par_mat_SP500[2,], sqrt(2)*exp(par_mat_SP500[3,]))
plot(par_mat_SP500[1,],type = "l")
points(var,type = "l", col ='red')

test <- BacktestVaR(data = par_mat_SP500[1,], VaR = var, alpha = 0.05)

plot(data[1,],type = "l")
points(quantile,type = "l", col ='red')

###############################################
load("opt_t.RData")

var <- qt(0.95, exp(par_mat_SP500[4,]))
plot(par_mat_SP500[1,],type = "l")
points(par_mat_SP500[2,]+exp(par_mat_SP500[3,])*var,type = "l", col ='red')

test <- BacktestVaR(data = par_mat_SP500[1,], VaR = var, alpha = 0.05)

plot(data[1,],type = "l")
points(quantile,type = "l", col ='red')