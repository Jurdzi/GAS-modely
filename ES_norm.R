#Norm
load("opt_norm.RData")

#SP500
#5%
SP500_5 <- test_ES_norm(data = par_mat_SP500, percentile = 0.05)

#2.5%
SP500_2.5 <- test_ES_norm(data = par_mat_SP500, percentile = 0.025)

#1%
SP500_1 <- test_ES_norm(data = par_mat_SP500, percentile = 0.01)

#5%
SP500_95 <- test_ES_norm(data = par_mat_SP500, percentile = 0.95)

#97.5%
SP500_97.5 <- test_ES_norm(data = par_mat_SP500, percentile = 0.975)

#99%
SP500_99 <- test_ES_norm(data = par_mat_SP500, percentile = 0.99)

##############################################################################
#gold
#5%
gold_5 <- test_ES_norm(data = par_mat_gold, percentile = 0.05)

#2.5%
gold_2.5 <- test_ES_norm(data = par_mat_gold, percentile = 0.025)

#1%
gold_1 <- test_ES_norm(data = par_mat_gold, percentile = 0.01)

#5%
gold_95 <- test_ES_norm(data = par_mat_gold, percentile = 0.95)

#97.5%
gold_97.5 <- test_ES_norm(data = par_mat_gold, percentile = 0.975)

#99%
gold_99 <- test_ES_norm(data = par_mat_gold, percentile = 0.99)
