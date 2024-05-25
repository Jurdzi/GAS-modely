# vykreslenie casovych radov do jedneho grafu
symbols <- c("^GSPC", "GC=F", "BTC-USD", "EURUSD=X","^TNX")
getSymbols(symbols, from = "2018-01-01", to = "2024-01-01", auto.assign = TRUE)

X <- merge(Cl(get("GSPC")),
           Cl(get("GC=F")),
           Cl(get("BTC-USD")),
           Cl(get("EURUSD=X")),
           Cl(get("TNX")),
           all = FALSE)

colnames(X) <- c("GSPC.Adjusted", "GC.Adjusted", "BTCUSD.Adjusted", "EURUSD.Adjusted", "TNX.Adjusted")
date <- index(X)
years <- unique(as.integer(format(date, "%Y")))

set.seed(123)
data <- data.frame(
  x = as.Date(date),
  y1 = as.vector(log(X$GSPC.Adjusted)),
  y2 = as.vector(log(X$GC.Adjusted)),
  y3 = as.vector(log(X$BTCUSD.Adjusted)),
  y4 = as.vector(X$EURUSD.Adjusted),
  y5 = as.vector(X$TNX.Adjusted)
)


max_left_y <- max(data$y1, data$y2, data$y3, na.rm = TRUE)
max_right_y <- max(data$y4, data$y5, na.rm = TRUE)

scaling_factor <- max_left_y / max_right_y

plot <- ggplot(data, aes(x = x))

plot <- plot +
  geom_line(aes(y = y1, color = "y1"), show.legend = TRUE) +
  geom_line(aes(y = y2, color = "y2"), show.legend = TRUE) +
  geom_line(aes(y = y3, color = "y3"), show.legend = TRUE) +
  geom_line(aes(y = y4 * scaling_factor, color = "y4"), show.legend = TRUE) + # Scale y4
  geom_line(aes(y = y5 * scaling_factor, color = "y5"), show.legend = TRUE) + # Scale y5
  scale_color_manual(
    values = c("y1" = "blue", "y2" = "red", "y3" = "green", "y4" = "purple", "y5" = "orange"),
    labels = c("S&P 500", "Zlato", "Bitcoin", "EUR/USD", "Dlhopisy")
  ) +
  scale_y_continuous(
    name = "Hodnoty zlogaritmovaných dát z S&P 500, zlata a bitcoinu",
    limits = c(0, max_left_y),
    sec.axis = sec_axis(~./scaling_factor, name = "Hodnoty dát z EUR/USD a dhopisov", breaks = scales::pretty_breaks(n = 10))
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme(axis.title.y.right = element_text(color = "black", margin = margin(l = 20)))

plot <- plot +
  labs(
    x = "Rok",
    y = "Hodnoty zlogaritmovaných dát z S&P 500, zlata a bitcoinu",
    color = "Data Sets"
  )

print(plot + theme(legend.position = "top"))
