symbols <- c("^GSPC", "GC=F", "BTC-USD", "EURUSD=X","^TNX")
getSymbols(symbols, from = "2018-01-01", to = "2024-01-01", auto.assign = TRUE)


###########################
#SP500
SP500_data <- as.matrix(GSPC$GSPC.Adjusted)
date_SP500 <- as.Date(rownames(SP500_data))
plot(x = as.numeric(date_SP500), y = SP500_data,col = "red", type = "l", ylim = c(min(SP500_data),max(SP500_data)), xlab = "Rok", ylab = "Hodnota", xaxt='n', main = "S&P 500")
years <- unique(as.integer(format(date_SP500, "%Y")))
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#log-transformacia
SP500_data_log <- log(SP500_data)
plot(x = as.numeric(date_SP500), y = SP500_data_log,col = "red", type = "l", ylim = c(min(SP500_data_log),max(SP500_data_log)), xlab = "Rok", ylab = "Hodnota", xaxt='n', main = "S&P 500")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#zlogaritmovane sa zdaju mat viac homogennu variaciu 
SP500_data <- SP500_data_log

mean(SP500_data)
var(SP500_data)
skewness(SP500_data)
#left side
quantile(SP500_data, probs = c(0.05, 0.25))
quantile(SP500_data, probs = c(0.75, 0.95))
qqnorm(SP500_data, main = "QQ Plot for S&P 500 Data")
kurtosis(SP500_data)
#"lahke" chvosty
ks.test(SP500_data, "pnorm", mean=mean(SP500_data), sd=sqrt(var(SP500_data)))
#data nie su z normalneho rozdelenia

#histogram
hist(SP500_data, nclass=100, col="springgreen", probability=TRUE)
osX <- seq( min(SP500_data), max(SP500_data), length.out=1000)
lines(osX, dnorm(osX, mean=mean(SP500_data), sd=sqrt(var(SP500_data))))

#jednotkovy koren
#na grafe je vidiet linearny trend tak rovno diferencujem
SP500_data <- diff(SP500_data)
plot(x = as.numeric(date_SP500[-length(date_SP500)]), y = SP500_data,col = "red", type = "l", ylim = c(min(SP500_data),max(SP500_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "S&P 500")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)
summary(ur.df(SP500_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je mensia ako kriticka hodnota  => netreba diferencovat
SP500_data <- as.numeric(SP500_data)


###########################
#GOLD
gold_data <- as.matrix(`GC=F`$`GC=F.Adjusted`)
gold_data <- na.omit(gold_data)
date_gold <- as.Date(rownames(gold_data))
years <- unique(as.integer(format(date_gold, "%Y")))
plot(x = as.numeric(date_gold), y = gold_data, col = "red", type = "l", ylim = c(min(gold_data),max(gold_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Zlato")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#log-transformacia
gold_data_log <- log(gold_data)
plot(x = as.numeric(date_gold), y = gold_data_log,col = "red", type = "l", ylim = c(min(gold_data_log),max(gold_data_log)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Zlato")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#zlogaritmovane sa zdaju mat viac homogennu variaciu 
gold_data <- gold_data_log

mean(gold_data)
var(gold_data)
skewness(gold_data)
#left side
quantile(gold_data, probs = c(0.05, 0.25))
quantile(gold_data, probs = c(0.75, 0.95))
qqnorm(gold_data, main = "QQ Plot for Gold Data")
kurtosis(gold_data)
#"lahke" chvosty, celkom blyzko k normalnemu
ks.test(gold_data, "pnorm", mean=mean(gold_data), sd=sqrt(var(gold_data)))
#data nie su z normalneho rozdelenia

#histogram
hist(gold_data, nclass=100, col="springgreen", probability=TRUE)
osX <- seq( min(gold_data), max(gold_data), length.out=1000)
lines(osX, dnorm(osX, mean=mean(gold_data), sd=sqrt(var(gold_data))))

#jednotkovy koren
summary(ur.df(gold_data, lags = 5, selectlags = "BIC", type = "drift"))
#testova statistika je vacsia ako kriticka hodnota  => treba diferencovat
gold_data <- diff(gold_data)
plot(x = as.numeric(date_gold[-length(date_gold)]), y = gold_data,col = "red", type = "l", ylim = c(min(gold_data),max(gold_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Zlato")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)
summary(ur.df(gold_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je mensia ako kriticka hodnota  => netreba diferencovat
gold_data <- as.numeric(gold_data)


###########################
#BITCOIN
btc_data <- as.matrix(`BTC-USD`$`BTC-USD.Adjusted`)
date_btc <- as.Date(rownames(btc_data))
plot(x = as.numeric(date_btc), y = btc_data,col = "red", type = "l", ylim = c(min(btc_data),max(btc_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Bitcoin")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#log-transformacia
btc_data_log <- log(btc_data)
plot(x = as.numeric(date_btc), y = btc_data_log,col = "red", type = "l", ylim = c(min(btc_data_log),max(btc_data_log)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Bitcoin")
years <- unique(as.integer(format(date_SP500, "%Y")))
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#zlogaritmovane sa zdaju mat viac homogennu variaciu 
btc_data <- btc_data_log

mean(btc_data)
var(btc_data)
skewness(btc_data)
#right side
quantile(btc_data, probs = c(0.05, 0.25))
quantile(btc_data, probs = c(0.75, 0.95))
qqnorm(btc_data, main = "QQ Plot for Bitcoin Data")
kurtosis(btc_data)
#"lahke" chvosty
ks.test(btc_data, "pnorm", mean=mean(btc_data), sd=sqrt(var(btc_data)))
#data nie su z normalneho rozdelenia

#histogram
hist(btc_data, nclass=100, col="springgreen", probability=TRUE)
osX <- seq( min(btc_data), max(btc_data), length.out=1000)
lines(osX, dnorm(osX, mean=mean(btc_data), sd=sqrt(var(btc_data))))

#jednotkovy koren
summary(ur.df(btc_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je vacsia ako kriticka hodnota  => treba diferencovat
btc_data <- diff(btc_data)
plot(x = as.numeric(date_btc[-length(date_btc)]), y = btc_data,col = "red", type = "l", ylim = c(min(btc_data),max(btc_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Bitcoin")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)
summary(ur.df(btc_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je mensia ako kriticka hodnota  => netreba diferencovat
btc_data <- as.numeric(btc_data)


###########################
#EUR-USD
eurusd_data <- as.matrix(na.omit(`EURUSD=X`$`EURUSD=X.Adjusted`))
date_eurusd <- as.Date(rownames(eurusd_data))
plot(x = as.numeric(date_eurusd), y = eurusd_data,col = "red", type = "l", ylim = c(min(eurusd_data),max(eurusd_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "EUR/USD")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#log-transformacia
eurusd_data_log <- log(eurusd_data)
plot(x = as.numeric(date_eurusd), y = eurusd_data_log,col = "red", type = "l", ylim = c(min(eurusd_data_log),max(eurusd_data_log)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "EUR/USD")
years <- unique(as.integer(format(date_SP500, "%Y")))
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#zlogaritmovane sa nezdaju mat viac homogennu variaciu 

mean(eurusd_data)
var(eurusd_data)
skewness(eurusd_data)
#left side
quantile(eurusd_data, probs = c(0.05, 0.25))
quantile(eurusd_data, probs = c(0.75, 0.95))
qqnorm(eurusd_data, main = "QQ Plot for EUR/USD Data")
kurtosis(eurusd_data)
#"tazke" chvosty, celkom blyzko k normalnemu
ks.test(eurusd_data, "pnorm", mean=mean(eurusd_data), sd=sqrt(var(eurusd_data)))
#data nie su z normalneho rozdelenia
#histogram
hist(eurusd_data, nclass=100, col="springgreen", probability=TRUE)
osX <- seq( min(eurusd_data), max(eurusd_data), length.out=1000)
lines(osX, dnorm(osX, mean=mean(eurusd_data), sd=sqrt(var(eurusd_data))))

#jednotkovy koren
summary(ur.df(eurusd_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je vacsia ako kriticka hodnota  => treba diferencovat
eurusd_data <- diff(eurusd_data)
plot(x = as.numeric(date_eurusd[-length(date_eurusd)]), y = eurusd_data,col = "red", type = "l", ylim = c(min(eurusd_data),max(eurusd_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "EUR/USD")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)
summary(ur.df(eurusd_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je mensia ako kriticka hodnota  => netreba diferencovat
eurusd_data <- as.numeric(eurusd_data)


###########################
#10Y treasury bonds
bonds_data <- as.matrix(na.omit(TNX$TNX.Adjusted))
date_bonds <- as.Date(rownames(bonds_data))
plot(x = as.numeric(date_bonds), y = bonds_data,col = "red", type = "l", ylim = c(min(bonds_data),max(bonds_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Dlhopisy")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#log-transformacia
bonds_data_log <- log(bonds_data)
plot(x = as.numeric(date_bonds), y = bonds_data_log,col = "red", type = "l", ylim = c(min(bonds_data_log),max(bonds_data_log)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Dlhopisy")
years <- unique(as.integer(format(date_bonds, "%Y")))
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)

#zlogaritmovane sa nezdaju mat viac homogennu variaciu


mean(bonds_data)
var(bonds_data)
skewness(bonds_data)
#right side
quantile(bonds_data, probs = c(0.05, 0.25))
quantile(bonds_data, probs = c(0.75, 0.95))
qqnorm(bonds_data, main = "QQ Plot for Bonds Data")
kurtosis(bonds_data)
#"tazke" chvosty
ks.test(bonds_data, "pnorm", mean=mean(bonds_data), sd=sqrt(var(bonds_data)))
#data nie su z normalneho rozdelenia

#histogram
hist(bonds_data, nclass=100, col="springgreen", probability=TRUE)
osX <- seq( min(bonds_data), max(bonds_data), length.out=1000)
lines(osX, dnorm(osX, mean=mean(bonds_data), sd=sqrt(var(bonds_data))))

#jednotkovy koren
summary(ur.df(bonds_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je vacsia ako kriticka hodnota  => treba diferencovat
bonds_data <- diff(bonds_data)
plot(x = as.numeric(date_bonds[-length(date_bonds)]), y = bonds_data,col = "red", type = "l", ylim = c(min(bonds_data),max(bonds_data)), xlab = "Čas", ylab = "Hodnota", xaxt='n', main = "Dlhopisy")
axis(1, at = as.Date(paste0(years, "-01-01")), labels = years, cex.axis = 0.85, las = 0)
summary(ur.df(bonds_data, lags = 5, selectlags = "BIC", type = "none"))
#testova statistika je mensia ako kriticka hodnota  => netreba diferencovat
bonds_data <- as.numeric(bonds_data)

save(SP500_data, gold_data, btc_data, eurusd_data, bonds_data, file = "data.RData")

