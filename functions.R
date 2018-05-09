library(rstan)
library(dplyr)
library(ggplot2)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(max.print=100000)

insts <- function(s) {
  if (s == "CA2Y") { 
    return(1)
  } else if (s == "CA5Y") {
    return(2)
  } else if (s == "CA10Y") {
    return(3)
  } else return(4)
}

plot_curves <- function(xs, df) {
  clr <- rgb(0,0,0,alpha = 0.03)
  clr1 <- rgb(1,0,0)
  plot(xs, df[1, ], "l", ylim = c(-1, 5), col=clr1)
  for (i in seq(1, nrow(df))) {lines(xs, df[i, ], col=clr)}
}

plot_series <- function(df) {
  dt <- strptime(df$DateTime, format="%d/%m/%Y %H:%M:%OS")
  plot(dt, df$z, xaxt="n", cex=0.2)
}

quantSmall <- function(x) {
  quantile(x, probs = c(0.05))
}

quantBig <- function(x) {
  quantile(x, probs = c(0.95))
}





train_and_plot <- function(n) {
  trades <- read.csv("trades.csv")
  trades["DateTime"] <- paste(trades$date, trades$TimeStamp)
  trades <- trades %>% filter(date != '04/04/2018')
  trades <- trades %>% mutate(Inst = if_else(alias=="CA2Y", 1,
                                           if_else(alias=="CA5Y", 2,
                                                   if_else(alias=="CA10Y", 3, 4))))

  trades <- trades %>% filter(date %in% c('21/03/2018'))

  trades <- head(trades, n)

  options(digits.secs=3)
  dt <- strptime(trades$DateTime, format="%d/%m/%Y %H:%M:%OS")

  timeDiff <- c(0)
  for (i in 2:nrow(trades)) {
    timeDiff[i] <- difftime(dt[i], dt[i-1], units = "secs")
  }

  trades["TimeDiff"] <- timeDiff

  N <- nrow(trades)
  M <- 4
  D <- 3
  y <- trades$Column3
  inst <- trades$Inst
  t <- trades$TimeDiff
  qty <- trades$QtyNominal/1e6
  vol_init <- 1e-8
  a_vol <- 0.001
  b_vol <- 0.001
  noise <- rep(100, M)
  beta <- 0.001
  a_alpha <- 0.001
  b_alpha <- 0.001
  vol <- 0.01
  W <- matrix(c(1, 1, 1,
              1, 1, -1,
              1, -1, -1,
              1, -1, 1), nrow=4, ncol=3)
  data_list <- list(N = N, M = M, D = D, y = y, inst = inst, t = t, qty = qty,
                  vol_init = vol_init, a_vol = a_vol, b_vol = b_vol, noise = noise,
                  beta = beta, a_alpha = a_alpha, b_alpha = b_alpha, vol = vol, W = W)
  # m <- stan_model(file = "alltoall.stan")
  # fit <- vb(m, data = data_list, algorithm="meanfield", iter=30000, seed=123456)
  fit <- stan(file = "alltoall.stan", data = data_list, chains = 1, iter = 2000, 
              control = list(max_treedepth = 12, adapt_delta = 0.8, stepsize = 1), seed = 123456)

  par2y <- sapply(seq(1:N), function(i) {paste("y_sim[", i, ", 1]", sep="")})
  par5y <- sapply(seq(1:N), function(i) {paste("y_sim[", i, ", 2]", sep="")})
  par10y <- sapply(seq(1:N), function(i) {paste("y_sim[", i, ", 3]", sep="")})
  par30y <- sapply(seq(1:N), function(i) {paste("y_sim[", i, ", 4]", sep="")})

  s <- summary(fit, par2y, probs = c(0.25, 0.75))
  ss <- s$summary
  trades$sim2y25 <- ss[, "25%"]
  trades$sim2y75 <- ss[, "75%"]

  s <- summary(fit, par5y, probs = c(0.25, 0.75))
  ss <- s$summary
  trades$sim5y25 <- ss[, "25%"]
  trades$sim5y75 <- ss[, "75%"]

  s <- summary(fit, par10y, probs = c(0.25, 0.75))
  ss <- s$summary
  trades$sim10y25 <- ss[, "25%"]
  trades$sim10y75 <- ss[, "75%"]

  s <- summary(fit, par30y, probs = c(0.25, 0.75))
  ss <- s$summary
  trades$sim30y25 <- ss[, "25%"]
  trades$sim30y75 <- ss[, "75%"]

  plot2y <- if ("CA2Y" %in% unique(trades$alias)) geom_ribbon(aes(ymin = sim2y25, ymax = sim2y75), alpha=0.3)
  plot5y <- if ("CA5Y" %in% unique(trades$alias)) geom_ribbon(aes(ymin = sim5y25, ymax = sim5y75), alpha = 0.3)
  plot10y <- if ("CA10Y" %in% unique(trades$alias)) geom_ribbon(aes(ymin = sim10y25, ymax = sim10y75), alpha = 0.3)
  plot30y <- if ("CA30Y" %in% unique(trades$alias)) geom_ribbon(aes(ymin = sim30y25, ymax = sim30y75), alpha = 0.3)
  trades %>% 
    ggplot(aes(x = dt)) +
    geom_point(aes(y = Column3, size = QtyNominal, color = alias)) +
    plot2y +
    plot5y +
    plot10y +
    plot30y +
    labs(x = "Time", y = "Trade level (yield)", size = "Qty", color="Inst")
  ggsave(filename = paste("~/ml/gp/graphs/all/latentpca", n, ".png", sep=""))
  
  trades %>% tail(30) %>% 
    ggplot(aes(x = tail(dt, 30))) +
    geom_point(aes(y = Column3, size = QtyNominal, color = alias)) +
    plot2y +
    plot5y +
    plot10y +
    plot30y +
    labs(x = "Time", y = "Trade level (yield)", size = "Qty", color="Inst")
  ggsave(filename = paste("~/ml/gp/graphs/rolling/latentpcarolling", n, ".png", sep=""))
  
  trades %>% mutate(Y = if_else(alias == "CA2Y", Column3, NULL)) %>%
    ggplot(aes(x = dt)) +
    geom_point(aes(y = Y, size = QtyNominal, color=alias)) +
    plot2y
  ggsave(filename = paste("~/ml/gp/graphs/2y/latentpca", n, "_2y.png", sep=""))
  
  trades %>% mutate(Y = if_else(alias == "CA5Y", Column3, NULL)) %>%
    ggplot(aes(x = dt)) +
    geom_point(aes(y = Y, size = QtyNominal, color=alias)) +
    plot5y
  ggsave(filename = paste("~/ml/gp/graphs/5y/latentpca", n, "_5y.png", sep=""))
  
  trades %>% mutate(Y = if_else(alias == "CA10Y", Column3, NULL)) %>%
    ggplot(aes(x = dt)) +
    geom_point(aes(y = Y, size = QtyNominal, color=alias)) +
    plot10y
  ggsave(filename = paste("~/ml/gp/graphs/10y/latentpca", n, "_10y.png", sep=""))
  
  trades %>% mutate(Y = if_else(alias == "CA30Y", Column3, NULL)) %>%
    ggplot(aes(x = dt)) +
    geom_point(aes(y = Y, size = QtyNominal, color=alias)) +
    plot30y
  ggsave(filename = paste("~/ml/gp/graphs/30y/latentpca", n, "_30y.png", sep=""))
}
