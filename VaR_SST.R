#SST
load("opt_SST.RData")

#SP500
#5%
SP500_5 <- test_VaR(data = par_mat_SP500, quantile_func = qSST, percentile = 0.05)

SP500_5$test$LRuc
SP500_5$test$LRcc

sum(par_mat_SP500[1,] < SP500_5$quantile)/length(par_mat_SP500[1,])

test_stat_SP500_5 <- SP500_5$test$LRcc["Test"] - SP500_5$test$LRuc["Test"]

pchisq(test_stat_SP500_5, df = 1, lower.tail = FALSE)

#test presiel
loss_SP500_5 <- LossVaR(realized = par_mat_SP500[1,], evaluated = SP500_5$quantile, tau = 0.05)
mean(loss_SP500_5)

#2.5%

SP500_2.5 <- test_VaR(data = par_mat_SP500, quantile_func = qSST, percentile = 0.025)

SP500_2.5$test$LRuc
SP500_2.5$test$LRcc

sum(par_mat_SP500[1,] < SP500_2.5$quantile)/length(par_mat_SP500[1,])

test_stat_SP500_2.5 <- SP500_2.5$test$LRcc["Test"] - SP500_2.5$test$LRuc["Test"]

pchisq(test_stat_SP500_2.5, df = 1, lower.tail = FALSE)

#test presiel
loss_SP500_2.5 <- LossVaR(realized = par_mat_SP500[1,], evaluated = SP500_2.5$quantile, tau = 0.025)
mean(loss_SP500_2.5)

#1%

SP500_1 <- test_VaR(data = par_mat_SP500, quantile_func = qSST, percentile = 0.01)

SP500_1$test$LRuc
SP500_1$test$LRcc

sum(par_mat_SP500[1,] < SP500_1$quantile)/length(par_mat_SP500[1,])

test_stat_SP500_1 <- SP500_1$test$LRcc["Test"] - SP500_1$test$LRuc["Test"]

pchisq(test_stat_SP500_1, df = 1, lower.tail = FALSE)

#test presiel
loss_SP500_1 <- LossVaR(realized = par_mat_SP500[1,], evaluated = SP500_1$quantile, tau = 0.01)
mean(loss_SP500_1)

#95%
SP500_95 <- test_VaR(data = par_mat_SP500, quantile_func = qSST, percentile = 0.95)

SP500_95$test$LRuc
SP500_95$test$LRcc

sum(par_mat_SP500[1,] > SP500_95$quantile)/length(par_mat_SP500[1,])

test_stat_SP500_95 <- SP500_95$test$LRcc["Test"] - SP500_95$test$LRuc["Test"]

pchisq(test_stat_SP500_95, df = 1, lower.tail = FALSE)

#test presiel
loss_SP500_95 <- LossVaR(realized = par_mat_SP500[1,], evaluated = SP500_95$quantile, tau = 0.05)
mean(loss_SP500_95)

#97.5%
SP500_97.5 <- test_VaR(data = par_mat_SP500, quantile_func = qSST, percentile = 0.975)

SP500_97.5$test$LRuc
SP500_97.5$test$LRcc

sum(par_mat_SP500[1,] > SP500_97.5$quantile)/length(par_mat_SP500[1,])

test_stat_SP500_97.5 <- SP500_97.5$test$LRcc["Test"] - SP500_97.5$test$LRuc["Test"]

pchisq(test_stat_SP500_97.5, df = 1, lower.tail = FALSE)

#test presiel
loss_SP500_97.5 <- LossVaR(realized = par_mat_SP500[1,], evaluated = SP500_97.5$quantile, tau = 0.025)
mean(loss_SP500_97.5)

#99%
SP500_99 <- test_VaR(data = par_mat_SP500, quantile_func = qSST, percentile = 0.99)

SP500_99$test$LRuc
SP500_99$test$LRcc

sum(par_mat_SP500[1,] > SP500_99$quantile)/length(par_mat_SP500[1,])

test_stat_SP500_99 <- SP500_99$test$LRcc["Test"] - SP500_99$test$LRuc["Test"]

pchisq(test_stat_SP500_99, df = 1, lower.tail = FALSE)

################################################################################
#gold
#5%
gold_5 <- test_VaR(data = par_mat_gold, quantile_func = qSST, percentile = 0.05)

gold_5$test$LRuc
gold_5$test$LRcc

sum(par_mat_gold[1,] < gold_5$quantile)/length(par_mat_gold[1,])

test_stat_gold_5 <- gold_5$test$LRcc["Test"] - gold_5$test$LRuc["Test"]

pchisq(test_stat_gold_5, df = 1, lower.tail = FALSE)

#test presiel
loss_gold_5 <- LossVaR(realized = par_mat_gold[1,], evaluated = gold_5$quantile, tau = 0.05)
mean(loss_gold_5)

#2.5%

gold_2.5 <- test_VaR(data = par_mat_gold, quantile_func = qSST, percentile = 0.025)

gold_2.5$test$LRuc
gold_2.5$test$LRcc

sum(par_mat_gold[1,] < gold_2.5$quantile)/length(par_mat_gold[1,])

test_stat_gold_2.5 <- gold_2.5$test$LRcc["Test"] - gold_2.5$test$LRuc["Test"]

pchisq(test_stat_gold_2.5, df = 1, lower.tail = FALSE)

#test presiel
loss_gold_2.5 <- LossVaR(realized = par_mat_gold[1,], evaluated = gold_2.5$quantile, tau = 0.025)
mean(loss_gold_2.5)

#1%

gold_1 <- test_VaR(data = par_mat_gold, quantile_func = qSST, percentile = 0.01)

gold_1$test$LRuc
gold_1$test$LRcc

sum(par_mat_gold[1,] < gold_1$quantile)/length(par_mat_gold[1,])

test_stat_gold_1 <- gold_1$test$LRcc["Test"] - gold_1$test$LRuc["Test"]

pchisq(test_stat_gold_1, df = 1, lower.tail = FALSE)

#test presiel
loss_gold_1 <- LossVaR(realized = par_mat_gold[1,], evaluated = gold_1$quantile, tau = 0.01)
mean(loss_gold_1)

#95%
gold_95 <- test_VaR(data = par_mat_gold, quantile_func = qSST, percentile = 0.95)

gold_95$test$LRuc
gold_95$test$LRcc

sum(par_mat_gold[1,] > gold_95$quantile)/length(par_mat_gold[1,])

test_stat_gold_95 <- gold_95$test$LRcc["Test"] - gold_95$test$LRuc["Test"]

pchisq(test_stat_gold_95, df = 1, lower.tail = FALSE)

#test presiel
loss_gold_95 <- LossVaR(realized = par_mat_gold[1,], evaluated = gold_95$quantile, tau = 0.05)
mean(loss_gold_95)

#97.5%
gold_97.5 <- test_VaR(data = par_mat_gold, quantile_func = qSST, percentile = 0.975)

gold_97.5$test$LRuc
gold_97.5$test$LRcc

sum(par_mat_gold[1,] > gold_97.5$quantile)/length(par_mat_gold[1,])

test_stat_gold_97.5 <- gold_97.5$test$LRcc["Test"] - gold_97.5$test$LRuc["Test"]

pchisq(test_stat_gold_97.5, df = 1, lower.tail = FALSE)

#test presiel
loss_gold_97.5 <- LossVaR(realized = par_mat_gold[1,], evaluated = gold_97.5$quantile, tau = 0.025)
mean(loss_gold_97.5)

#99%
gold_99 <- test_VaR(data = par_mat_gold, quantile_func = qSST, percentile = 0.99)

gold_99$test$LRuc
gold_99$test$LRcc

sum(par_mat_gold[1,] > gold_99$quantile)/length(par_mat_gold[1,])

test_stat_gold_99 <- gold_99$test$LRcc["Test"] - gold_99$test$LRuc["Test"]

pchisq(test_stat_gold_99, df = 1, lower.tail = FALSE)

#test presiel
loss_gold_99 <- LossVaR(realized = par_mat_gold[1,], evaluated = gold_99$quantile, tau = 0.01)
mean(loss_gold_99)

################################################################################
#btc
#5%
btc_5 <- test_VaR(data = par_mat_btc, quantile_func = qSST, percentile = 0.05)

btc_5$test$LRuc
btc_5$test$LRcc

sum(par_mat_btc[1,] < btc_5$quantile)/length(par_mat_btc[1,])

test_stat_btc_5 <- btc_5$test$LRcc["Test"] - btc_5$test$LRuc["Test"]

pchisq(test_stat_btc_5, df = 1, lower.tail = FALSE)

#test presiel
loss_btc_5 <- LossVaR(realized = par_mat_btc[1,], evaluated = btc_5$quantile, tau = 0.05)
mean(loss_btc_5)

#2.5%

btc_2.5 <- test_VaR(data = par_mat_btc, quantile_func = qSST, percentile = 0.025)

btc_2.5$test$LRuc
btc_2.5$test$LRcc

sum(par_mat_btc[1,] < btc_2.5$quantile)/length(par_mat_btc[1,])

test_stat_btc_2.5 <- btc_2.5$test$LRcc["Test"] - btc_2.5$test$LRuc["Test"]

pchisq(test_stat_btc_2.5, df = 1, lower.tail = FALSE)

#test presiel
loss_btc_2.5 <- LossVaR(realized = par_mat_btc[1,], evaluated = btc_2.5$quantile, tau = 0.025)
mean(loss_btc_2.5)

#1%

btc_1 <- test_VaR(data = par_mat_btc, quantile_func = qSST, percentile = 0.01)

btc_1$test$LRuc
btc_1$test$LRcc

sum(par_mat_btc[1,] < btc_1$quantile)/length(par_mat_btc[1,])

test_stat_btc_1 <- btc_1$test$LRcc["Test"] - btc_1$test$LRuc["Test"]

pchisq(test_stat_btc_1, df = 1, lower.tail = FALSE)

#test presiel
loss_btc_1 <- LossVaR(realized = par_mat_btc[1,], evaluated = btc_1$quantile, tau = 0.01)
mean(loss_btc_1)

#95%
btc_95 <- test_VaR(data = par_mat_btc, quantile_func = qSST, percentile = 0.95)

btc_95$test$LRuc
btc_95$test$LRcc

sum(par_mat_btc[1,] > btc_95$quantile)/length(par_mat_btc[1,])

test_stat_btc_95 <- btc_95$test$LRcc["Test"] - btc_95$test$LRuc["Test"]

pchisq(test_stat_btc_95, df = 1, lower.tail = FALSE)

#test presiel
loss_btc_95 <- LossVaR(realized = par_mat_btc[1,], evaluated = btc_95$quantile, tau = 0.05)
mean(loss_btc_95)

#97.5%
btc_97.5 <- test_VaR(data = par_mat_btc, quantile_func = qSST, percentile = 0.975)

btc_97.5$test$LRuc
btc_97.5$test$LRcc

sum(par_mat_btc[1,] > btc_97.5$quantile)/length(par_mat_btc[1,])

test_stat_btc_97.5 <- btc_97.5$test$LRcc["Test"] - btc_97.5$test$LRuc["Test"]

pchisq(test_stat_btc_97.5, df = 1, lower.tail = FALSE)

#99%
btc_99 <- test_VaR(data = par_mat_btc, quantile_func = qSST, percentile = 0.99)

btc_99$test$LRuc
btc_99$test$LRcc

sum(par_mat_btc[1,] > btc_99$quantile)/length(par_mat_btc[1,])

test_stat_btc_99 <- btc_99$test$LRcc["Test"] - btc_99$test$LRuc["Test"]

pchisq(test_stat_btc_99, df = 1, lower.tail = FALSE)

#test presiel
loss_btc_99 <- LossVaR(realized = par_mat_btc[1,], evaluated = btc_99$quantile, tau = 0.05)
mean(loss_btc_99)

################################################################################
#eurusd
#5%
eurusd_5 <- test_VaR(data = par_mat_eurusd, quantile_func = qSST, percentile = 0.05)

eurusd_5$test$LRuc
eurusd_5$test$LRcc

sum(par_mat_eurusd[1,] < eurusd_5$quantile)/length(par_mat_eurusd[1,])

test_stat_eurusd_5 <- eurusd_5$test$LRcc["Test"] - eurusd_5$test$LRuc["Test"]

pchisq(test_stat_eurusd_5, df = 1, lower.tail = FALSE)

#test presiel
loss_eurusd_5 <- LossVaR(realized = par_mat_eurusd[1,], evaluated = eurusd_5$quantile, tau = 0.05)
mean(loss_eurusd_5)

#2.5%

eurusd_2.5 <- test_VaR(data = par_mat_eurusd, quantile_func = qSST, percentile = 0.025)

eurusd_2.5$test$LRuc
eurusd_2.5$test$LRcc

sum(par_mat_eurusd[1,] < eurusd_2.5$quantile)/length(par_mat_eurusd[1,])

test_stat_eurusd_2.5 <- eurusd_2.5$test$LRcc["Test"] - eurusd_2.5$test$LRuc["Test"]

pchisq(test_stat_eurusd_2.5, df = 1, lower.tail = FALSE)

#test presiel
loss_eurusd_2.5 <- LossVaR(realized = par_mat_eurusd[1,], evaluated = eurusd_2.5$quantile, tau = 0.025)
mean(loss_eurusd_2.5)

#1%

eurusd_1 <- test_VaR(data = par_mat_eurusd, quantile_func = qSST, percentile = 0.01)

eurusd_1$test$LRuc
eurusd_1$test$LRcc

sum(par_mat_eurusd[1,] < eurusd_1$quantile)/length(par_mat_eurusd[1,])

test_stat_eurusd_1 <- eurusd_1$test$LRcc["Test"] - eurusd_1$test$LRuc["Test"]

pchisq(test_stat_eurusd_1, df = 1, lower.tail = FALSE)

#test presiel
loss_eurusd_1 <- LossVaR(realized = par_mat_eurusd[1,], evaluated = eurusd_1$quantile, tau = 0.01)
mean(loss_eurusd_1)

#95%
eurusd_95 <- test_VaR(data = par_mat_eurusd, quantile_func = qSST, percentile = 0.95)

eurusd_95$test$LRuc
eurusd_95$test$LRcc

sum(par_mat_eurusd[1,] > eurusd_95$quantile)/length(par_mat_eurusd[1,])

test_stat_eurusd_95 <- eurusd_95$test$LRcc["Test"] - eurusd_95$test$LRuc["Test"]

pchisq(test_stat_eurusd_95, df = 1, lower.tail = FALSE)

#97.5%
eurusd_97.5 <- test_VaR(data = par_mat_eurusd, quantile_func = qSST, percentile = 0.975)

eurusd_97.5$test$LRuc
eurusd_97.5$test$LRcc

sum(par_mat_eurusd[1,] > eurusd_97.5$quantile)/length(par_mat_eurusd[1,])

test_stat_eurusd_97.5 <- eurusd_97.5$test$LRcc["Test"] - eurusd_97.5$test$LRuc["Test"]

pchisq(test_stat_eurusd_97.5, df = 1, lower.tail = FALSE)

#99%
eurusd_99 <- test_VaR(data = par_mat_eurusd, quantile_func = qSST, percentile = 0.99)

eurusd_99$test$LRuc
eurusd_99$test$LRcc

sum(par_mat_eurusd[1,] > eurusd_99$quantile)/length(par_mat_eurusd[1,])

test_stat_eurusd_99 <- eurusd_99$test$LRcc["Test"] - eurusd_99$test$LRuc["Test"]

pchisq(test_stat_eurusd_99, df = 1, lower.tail = FALSE)

#test presiel
loss_eurusd_99 <- LossVaR(realized = par_mat_eurusd[1,], evaluated = eurusd_99$quantile, tau = 0.01)
mean(loss_eurusd_99)

################################################################################
#bonds
#5%
bonds_5 <- test_VaR(data = par_mat_bonds, quantile_func = qSST, percentile = 0.05)

bonds_5$test$LRuc
bonds_5$test$LRcc

sum(par_mat_bonds[1,] < bonds_5$quantile)/length(par_mat_bonds[1,])

test_stat_bonds_5 <- bonds_5$test$LRcc["Test"] - bonds_5$test$LRuc["Test"]

pchisq(test_stat_bonds_5, df = 1, lower.tail = FALSE)

#2.5%

bonds_2.5 <- test_VaR(data = par_mat_bonds, quantile_func = qSST, percentile = 0.025)

bonds_2.5$test$LRuc
bonds_2.5$test$LRcc

sum(par_mat_bonds[1,] < bonds_2.5$quantile)/length(par_mat_bonds[1,])

test_stat_bonds_2.5 <- bonds_2.5$test$LRcc["Test"] - bonds_2.5$test$LRuc["Test"]

pchisq(test_stat_bonds_2.5, df = 1, lower.tail = FALSE)

#1%

bonds_1 <- test_VaR(data = par_mat_bonds, quantile_func = qSST, percentile = 0.01)

bonds_1$test$LRuc
bonds_1$test$LRcc

sum(par_mat_bonds[1,] < bonds_1$quantile)/length(par_mat_bonds[1,])

test_stat_bonds_1 <- bonds_1$test$LRcc["Test"] - bonds_1$test$LRuc["Test"]

pchisq(test_stat_bonds_1, df = 1, lower.tail = FALSE)

#test presiel
loss_bonds_1 <- LossVaR(realized = par_mat_bonds[1,], evaluated = bonds_1$quantile, tau = 0.01)
mean(loss_bonds_1)

#95%
bonds_95 <- test_VaR(data = par_mat_bonds, quantile_func = qSST, percentile = 0.95)

bonds_95$test$LRuc
bonds_95$test$LRcc

sum(par_mat_bonds[1,] > bonds_95$quantile)/length(par_mat_bonds[1,])

test_stat_bonds_95 <- bonds_95$test$LRcc["Test"] - bonds_95$test$LRuc["Test"]

pchisq(test_stat_bonds_95, df = 1, lower.tail = FALSE)

#test presiel
loss_bonds_95 <- LossVaR(realized = par_mat_bonds[1,], evaluated = bonds_95$quantile, tau = 0.05)
mean(loss_bonds_95)

#97.5%
bonds_97.5 <- test_VaR(data = par_mat_bonds, quantile_func = qSST, percentile = 0.975)

bonds_97.5$test$LRuc
bonds_97.5$test$LRcc

sum(par_mat_bonds[1,] > bonds_97.5$quantile)/length(par_mat_bonds[1,])

test_stat_bonds_97.5 <- bonds_97.5$test$LRcc["Test"] - bonds_97.5$test$LRuc["Test"]

pchisq(test_stat_bonds_97.5, df = 1, lower.tail = FALSE)

#test presiel
loss_bonds_97.5 <- LossVaR(realized = par_mat_bonds[1,], evaluated = bonds_97.5$quantile, tau = 0.025)
mean(loss_bonds_97.5)

#99%
bonds_99 <- test_VaR(data = par_mat_bonds, quantile_func = qSST, percentile = 0.99)

bonds_99$test$LRuc
bonds_99$test$LRcc

sum(par_mat_bonds[1,] > bonds_99$quantile)/length(par_mat_bonds[1,])

test_stat_bonds_99 <- bonds_99$test$LRcc["Test"] - bonds_99$test$LRuc["Test"]

pchisq(test_stat_bonds_99, df = 1, lower.tail = FALSE)

