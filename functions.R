library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

plot_curves <- function(xs, df) {
  clr <- rgb(0,0,0,alpha = 0.03)
  clr1 <- rgb(1,0,0)
  plot(xs, df[1, ], "l", ylim = c(-1, 5), col=clr1)
  for (i in seq(1, nrow(df))) {lines(xs, df[i, ], col=clr)}
}
