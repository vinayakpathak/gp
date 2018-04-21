w_values <- c(1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,-1,-1,-1,-1,
              1,1,1,-1,-1,-1,1,1,1)

D <- 1
M <- 9
mu <- rep(0, M)
N <- 308
sigma <- 0.1
#W <- matrix(w_values, M, D)
W <- matrix(rnorm(M * D), M, D)

data_list <- list(D = D, M = M, N = N, sigma = sigma, W = W, mu = mu)
fit <- stan(file = "simulation.stan", data = data_list, cores = 1, chains = 2, iter = 2000)
fit_data <- as.data.frame(fit)
ys <- c('y[1]', 'y[2]', 'y[3]', 'y[4]', 'y[5]', 'y[6]', 'y[7]', 'y[8]', 'y[9]')




xs <- c(0, 3, 6, 9, 12, 15, 18, 21, 24)
clr <- rgb(0,0,0,alpha = 0.03)
clr1 <- rgb(1,0,0)
plot(xs, fit_data[1, ys], "l", ylim = c(-10, 10), col=clr1)
for (i in seq(1, nrow(fit_data))) {lines(xs, fit_data[i, ys], col=clr)}


