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
