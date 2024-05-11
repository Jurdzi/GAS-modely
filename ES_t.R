#t
load("opt_t.RData")

#SP500
#5%
SP500_5 <- test_ES_t(data = par_mat_SP500, percentile = 0.05)

SP500_5$LR_uc
SP500_5$LR_ind

#2.5%
SP500_2.5 <- test_ES_t(data = par_mat_SP500, percentile = 0.025)

SP500_2.5$LR_uc
SP500_2.5$LR_ind

#1%
SP500_1 <- test_ES_t(data = par_mat_SP500, percentile = 0.01)

SP500_1$LR_uc
SP500_1$LR_ind

#95%
SP500_95 <- test_ES_t(data = par_mat_SP500, percentile = 0.95)

SP500_95$LR_uc
SP500_95$LR_ind

#97.5%
SP500_97.5 <- test_ES_t(data = par_mat_SP500, percentile = 0.975)

SP500_97.5$LR_uc
SP500_97.5$LR_ind

#99%
SP500_99 <- test_ES_t(data = par_mat_SP500, percentile = 0.99)

SP500_99$LR_uc
SP500_99$LR_ind

##############################################################################
#gold
#5%
gold_5 <- test_ES_t(data = par_mat_gold, percentile = 0.05)

gold_5$LR_uc
gold_5$LR_ind

#2.5%
gold_2.5 <- test_ES_t(data = par_mat_gold, percentile = 0.025)

gold_2.5$LR_uc
gold_2.5$LR_ind

#1%
gold_1 <- test_ES_t(data = par_mat_gold, percentile = 0.01)

gold_1$LR_uc
gold_1$LR_ind

#95%
gold_95 <- test_ES_t(data = par_mat_gold, percentile = 0.95)

gold_95$LR_uc
gold_95$LR_ind

#97.5%
gold_97.5 <- test_ES_t(data = par_mat_gold, percentile = 0.975)

gold_97.5$LR_uc
gold_97.5$LR_ind

#99%
gold_99 <- test_ES_t(data = par_mat_gold, percentile = 0.99)

gold_99$LR_uc
gold_99$LR_ind

##############################################################################
#btc
#5%
btc_5 <- test_ES_t(data = par_mat_btc, percentile = 0.05)

btc_5$LR_uc
btc_5$LR_ind

#2.5%
btc_2.5 <- test_ES_t(data = par_mat_btc, percentile = 0.025)

btc_2.5$LR_uc
btc_2.5$LR_ind

#1%
btc_1 <- test_ES_t(data = par_mat_btc, percentile = 0.01)

btc_1$LR_uc
btc_1$LR_ind

#95%
btc_95 <- test_ES_t(data = par_mat_btc, percentile = 0.95)

btc_95$LR_uc
btc_95$LR_ind

#97.5%
btc_97.5 <- test_ES_t(data = par_mat_btc, percentile = 0.975)

btc_97.5$LR_uc
btc_97.5$LR_ind

#99%
btc_99 <- test_ES_t(data = par_mat_btc, percentile = 0.99)

btc_99$LR_uc
btc_99$LR_ind

##############################################################################
#eurusd
#5%
eurusd_5 <- test_ES_t(data = par_mat_eurusd, percentile = 0.05)

eurusd_5$LR_uc
eurusd_5$LR_ind

#2.5%
eurusd_2.5 <- test_ES_t(data = par_mat_eurusd, percentile = 0.025)

eurusd_2.5$LR_uc
eurusd_2.5$LR_ind

#1%
eurusd_1 <- test_ES_t(data = par_mat_eurusd, percentile = 0.01)

eurusd_1$LR_uc
eurusd_1$LR_ind

#95%
eurusd_95 <- test_ES_t(data = par_mat_eurusd, percentile = 0.95)

eurusd_95$LR_uc
eurusd_95$LR_ind

#97.5%
eurusd_97.5 <- test_ES_t(data = par_mat_eurusd, percentile = 0.975)

eurusd_97.5$LR_uc
eurusd_97.5$LR_ind

#99%
eurusd_99 <- test_ES_t(data = par_mat_eurusd, percentile = 0.99)

eurusd_99$LR_uc
eurusd_99$LR_ind

##############################################################################
#bonds
#5%
bonds_5 <- test_ES_t(data = par_mat_bonds, percentile = 0.05)

bonds_5$LR_uc
bonds_5$LR_ind

#2.5%
bonds_2.5 <- test_ES_t(data = par_mat_bonds, percentile = 0.025)

bonds_2.5$LR_uc
bonds_2.5$LR_ind

#1%
bonds_1 <- test_ES_t(data = par_mat_bonds, percentile = 0.01)

bonds_1$LR_uc
bonds_1$LR_ind

#95%
bonds_95 <- test_ES_t(data = par_mat_bonds, percentile = 0.95)

bonds_95$LR_uc
bonds_95$LR_ind

#97.5%
bonds_97.5 <- test_ES_t(data = par_mat_bonds, percentile = 0.975)

bonds_97.5$LR_uc
bonds_97.5$LR_ind

#99%
bonds_99 <- test_ES_t(data = par_mat_bonds, percentile = 0.99)

bonds_99$LR_uc
bonds_99$LR_ind
