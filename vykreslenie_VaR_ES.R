load("opt_norm.RData")
plot(par_mat_eurusd[1,],type = "l", col = "black", ylab = "Hodnoty")

points(qnorm(0.05, mean = par_mat_eurusd[2,], sd = exp(par_mat_eurusd[3,])),type= "l", col = "red")
points(qnorm(0.025, mean = par_mat_eurusd[2,], sd = exp(par_mat_eurusd[3,])),type= "l", col = "blue")
points(qnorm(0.01, mean = par_mat_eurusd[2,], sd = exp(par_mat_eurusd[3,])),type= "l", col = "green")

points(par_mat_eurusd[2,] - exp(par_mat_eurusd[3,])/(0.05*sqrt(2*pi))*exp(-qnorm(0.05)^2/2), type = "l", col = "coral")
points(par_mat_eurusd[2,] - exp(par_mat_eurusd[3,])/(0.025*sqrt(2*pi))*exp(-qnorm(0.025)^2/2), type = "l", col = "purple")
points(par_mat_eurusd[2,] - exp(par_mat_eurusd[3,])/(0.01*sqrt(2*pi))*exp(-qnorm(0.01)^2/2), type = "l", col = "orange")

legend("top", legend = c("VaR(0.05)", "VaR(0.025)", "VaR(0.01)", "ES(0.05)", "ES(0.025)", "ES(0.01)"), col = c("red", "blue", "green", "coral", "purple", "orange"), lty = 1, horiz = TRUE, inset = -0.1, xpd = TRUE, cex = 0.8)
