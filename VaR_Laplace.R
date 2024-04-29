#Laplace
load("opt_Laplace.RData")
source("test_VaR.r")

#SP500
#5%
SP500_5 <- test_VaR(data = par_mat_SP500, quantile_func = qlaplace, percentile = 0.05)

SP500_5$test$LRuc
SP500_5$test$LRcc

sum(par_mat_SP500[1,] < SP500_5$quantile)/length(par_mat_SP500[1,])

#2.5%

SP500_2.5 <- test_VaR(data = par_mat_SP500, quantile_func = qlaplace, percentile = 0.025)

SP500_2.5$test$LRuc
SP500_2.5$test$LRcc

sum(par_mat_SP500[1,] < SP500_2.5$quantile)/length(par_mat_SP500[1,])

#1%

SP500_1 <- test_VaR(data = par_mat_SP500, quantile_func = qlaplace, percentile = 0.01)

SP500_1$test$LRuc
SP500_1$test$LRcc

sum(par_mat_SP500[1,] < SP500_1$quantile)/length(par_mat_SP500[1,])

#95%
SP500_95 <- test_VaR(data = par_mat_SP500, quantile_func = qlaplace, percentile = 0.95)

SP500_95$test$LRuc
SP500_95$test$LRcc
sum(-par_mat_SP500[1,] < -SP500_95$quantile)/length(par_mat_SP500[1,])

#97.5%
SP500_97.5 <- test_VaR(data = par_mat_SP500, quantile_func = qlaplace, percentile = 0.975)

SP500_97.5$test$LRuc
SP500_97.5$test$LRcc
sum(par_mat_SP500[1,] < SP500_97.5$quantile)/length(par_mat_SP500[1,])

#99%
SP500_99 <- test_VaR(data = par_mat_SP500, quantile_func = qlaplace, percentile = 0.99)

SP500_99$test$LRuc
SP500_99$test$LRcc
sum(-par_mat_SP500[1,] < -SP500_99$quantile)/length(par_mat_SP500[1,])

################################################################################
#gold
#5%
gold_5 <- test_VaR(data = par_mat_gold, quantile_func = qlaplace, percentile = 0.05)

gold_5$test$LRuc
gold_5$test$LRcc

#2.5%

gold_2.5 <- test_VaR(data = par_mat_gold, quantile_func = qlaplace, percentile = 0.025)

gold_2.5$test$LRuc
gold_2.5$test$LRcc

#1%

gold_1 <- test_VaR(data = par_mat_gold, quantile_func = qlaplace, percentile = 0.01)

gold_1$test$LRuc
gold_1$test$LRcc
sum(par_mat_gold[1,] < gold_1$quantile)/length(par_mat_gold[1,])

#95%
gold_95 <- test_VaR(data = par_mat_gold, quantile_func = qlaplace, percentile = 0.95)

gold_95$test$LRuc
gold_95$test$LRcc

#97.5%
gold_97.5 <- test_VaR(data = par_mat_gold, quantile_func = qlaplace, percentile = 0.975)

gold_97.5$test$LRuc
gold_97.5$test$LRcc


#99%
gold_99 <- test_VaR(data = par_mat_gold, quantile_func = qlaplace, percentile = 0.99)

gold_99$test$LRuc
gold_99$test$LRcc

################################################################################
#btc
#5%
btc_5 <- test_VaR(data = par_mat_btc, quantile_func = qlaplace, percentile = 0.05)

btc_5$test$LRuc
btc_5$test$LRcc

#2.5%

btc_2.5 <- test_VaR(data = par_mat_btc, quantile_func = qlaplace, percentile = 0.025)

btc_2.5$test$LRuc
btc_2.5$test$LRcc

#1%

btc_1 <- test_VaR(data = par_mat_btc, quantile_func = qlaplace, percentile = 0.01)

btc_1$test$LRuc
btc_1$test$LRcc

#95%
btc_95 <- test_VaR(data = par_mat_btc, quantile_func = qlaplace, percentile = 0.95)

btc_95$test$LRuc
btc_95$test$LRcc

#97.5%
btc_97.5 <- test_VaR(data = par_mat_btc, quantile_func = qlaplace, percentile = 0.975)

btc_97.5$test$LRuc
btc_97.5$test$LRcc

#99%
btc_99 <- test_VaR(data = par_mat_btc, quantile_func = qlaplace, percentile = 0.99)

btc_99$test$LRuc
btc_99$test$LRcc

################################################################################
#eurusd
#5%
eurusd_5 <- test_VaR(data = par_mat_eurusd, quantile_func = qlaplace, percentile = 0.05)

eurusd_5$test$LRuc
eurusd_5$test$LRcc

#2.5%

eurusd_2.5 <- test_VaR(data = par_mat_eurusd, quantile_func = qlaplace, percentile = 0.025)

eurusd_2.5$test$LRuc
eurusd_2.5$test$LRcc

#1%

eurusd_1 <- test_VaR(data = par_mat_eurusd, quantile_func = qlaplace, percentile = 0.01)

eurusd_1$test$LRuc
eurusd_1$test$LRcc

#95%
eurusd_95 <- test_VaR(data = par_mat_eurusd, quantile_func = qlaplace, percentile = 0.95)

eurusd_95$test$LRuc
eurusd_95$test$LRcc

#97.5%
eurusd_97.5 <- test_VaR(data = par_mat_eurusd, quantile_func = qlaplace, percentile = 0.975)

eurusd_97.5$test$LRuc
eurusd_97.5$test$LRcc

#99%
eurusd_99 <- test_VaR(data = par_mat_eurusd, quantile_func = qlaplace, percentile = 0.99)

eurusd_99$test$LRuc
eurusd_99$test$LRcc

################################################################################
#bonds
#5%
bonds_5 <- test_VaR(data = par_mat_bonds, quantile_func = qlaplace, percentile = 0.05)

bonds_5$test$LRuc
bonds_5$test$LRcc

#2.5%

bonds_2.5 <- test_VaR(data = par_mat_bonds, quantile_func = qlaplace, percentile = 0.025)

bonds_2.5$test$LRuc
bonds_2.5$test$LRcc

#1%

bonds_1 <- test_VaR(data = par_mat_bonds, quantile_func = qlaplace, percentile = 0.01)

bonds_1$test$LRuc
bonds_1$test$LRcc

#95%
bonds_95 <- test_VaR(data = par_mat_bonds, quantile_func = qlaplace, percentile = 0.95)

bonds_95$test$LRuc
bonds_95$test$LRcc

#97.5%
bonds_97.5 <- test_VaR(data = par_mat_bonds, quantile_func = qlaplace, percentile = 0.975)

bonds_97.5$test$LRuc
bonds_97.5$test$LRcc

#99%
bonds_99 <- test_VaR(data = par_mat_bonds, quantile_func = qlaplace, percentile = 0.99)

bonds_99$test$LRuc
bonds_99$test$LRcc
