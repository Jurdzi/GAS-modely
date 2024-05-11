#t
load("opt_t.RData")

#SP500
#5%
SP500_5 <- test_ES_t(data = par_mat_SP500, percentile = 0.05)

SP500_5

#2.5%
SP500_2.5 <- test_ES_t(data = par_mat_SP500, percentile = 0.025)

SP500_2.5

#1%
SP500_1 <- test_ES_t(data = par_mat_SP500, percentile = 0.01)

SP500_1

#95%
SP500_95 <- test_ES_t(data = par_mat_SP500, percentile = 0.95)

SP500_95

#97.5%
SP500_97.5 <- test_ES_t(data = par_mat_SP500, percentile = 0.975)

SP500_97.5

#99%
SP500_99 <- test_ES_t(data = par_mat_SP500, percentile = 0.99)

SP500_99

##############################################################################
#gold
#5%
gold_5 <- test_ES_t(data = par_mat_gold, percentile = 0.05)

gold_5

#2.5%
gold_2.5 <- test_ES_t(data = par_mat_gold, percentile = 0.025)

gold_2.5

#1%
gold_1 <- test_ES_t(data = par_mat_gold, percentile = 0.01)

gold_1

#95%
gold_95 <- test_ES_t(data = par_mat_gold, percentile = 0.95)

gold_95

#97.5%
gold_97.5 <- test_ES_t(data = par_mat_gold, percentile = 0.975)

gold_97.5

#99%
gold_99 <- test_ES_t(data = par_mat_gold, percentile = 0.99)

gold_99

##############################################################################
#btc
#5%
btc_5 <- test_ES_t(data = par_mat_btc, percentile = 0.05)

btc_5

#2.5%
btc_2.5 <- test_ES_t(data = par_mat_btc, percentile = 0.025)

btc_2.5

#1%
btc_1 <- test_ES_t(data = par_mat_btc, percentile = 0.01)

btc_1

#95%
btc_95 <- test_ES_t(data = par_mat_btc, percentile = 0.95)

btc_95

#97.5%
btc_97.5 <- test_ES_t(data = par_mat_btc, percentile = 0.975)

btc_97.5

#99%
btc_99 <- test_ES_t(data = par_mat_btc, percentile = 0.99)

btc_99

##############################################################################
#eurusd
#5%
eurusd_5 <- test_ES_t(data = par_mat_eurusd, percentile = 0.05)

eurusd_5

#2.5%
eurusd_2.5 <- test_ES_t(data = par_mat_eurusd, percentile = 0.025)

eurusd_2.5

#1%
eurusd_1 <- test_ES_t(data = par_mat_eurusd, percentile = 0.01)

eurusd_1

#95%
eurusd_95 <- test_ES_t(data = par_mat_eurusd, percentile = 0.95)

eurusd_95

#97.5%
eurusd_97.5 <- test_ES_t(data = par_mat_eurusd, percentile = 0.975)

eurusd_97.5

#99%
eurusd_99 <- test_ES_t(data = par_mat_eurusd, percentile = 0.99)

eurusd_99

##############################################################################
#bonds
#5%
bonds_5 <- test_ES_t(data = par_mat_bonds, percentile = 0.05)

bonds_5

#2.5%
bonds_2.5 <- test_ES_t(data = par_mat_bonds, percentile = 0.025)

bonds_2.5

#1%
bonds_1 <- test_ES_t(data = par_mat_bonds, percentile = 0.01)

bonds_1

#95%
bonds_95 <- test_ES_t(data = par_mat_bonds, percentile = 0.95)

bonds_95

#97.5%
bonds_97.5 <- test_ES_t(data = par_mat_bonds, percentile = 0.975)

bonds_97.5

#99%
bonds_99 <- test_ES_t(data = par_mat_bonds, percentile = 0.99)

bonds_99

