source("functions.R")
N <- 10
vol <- 0.001
vol_intercept <- 0.0000001
noise <- 0.01
z1 <- 1
lambda <- 0.005

data_list <- list(N = N, vol = vol, vol_intercept = vol_intercept, noise = noise, z1 = z1,
                 lambda = lambda)

fit <- stan(file = "simulate_time_series.stan", data = data_list, chains = 2, iter = 3000, 
            seed = 123456)

print(fit)
pairs(fit)
fit_data <- as.data.frame(fit)
fit_data

launch_shinystan(as.shinystan(fit))