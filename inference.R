source("functions.R")
df <- read.csv("pca.csv")

xs <- c(0, 3, 6, 9, 12, 15, 18, 21, 24)
plot_curves(xs, df)

D <- 3
M <- 9
N <- nrow(df)
beta <- 0.001
a_alpha <- 0.001 
b_alpha <- 0.001
a_tau <- 0.001
b_tau <- 0.001

data_list <- list(D = D, M = M, N = N, y = df, beta = beta, a_alpha = a_alpha,
                  b_alpha = b_alpha, a_tau = a_tau, b_tau = b_tau);
fit <- stan(file = "inference.stan", data = data_list, chains = 2, iter = 2000)
fit_data <- as.data.frame(fit)

write.csv(fit_data, "fit_data.csv")


summary(fit_data)
ys <- c('y_sim[1]', 'y_sim[2]', 'y_sim[3]', 'y_sim[4]', 'y_sim[5]', 'y_sim[6]', 
        'y_sim[7]', 'y_sim[8]', 'y_sim[9]')

xs <- c(0, 3, 6, 9, 12, 15, 18, 21, 24)
plot_curves(xs, fit_data[, ys])
clr1 <- rgb(0,0,1, alpha=0.1)
for (i in seq(1, nrow(df))) {lines(xs, df[i, ], col=clr1)}


alphas <- fit_data[, c('alpha[1]', 'alpha[2]', 'alpha[3]')]
colMeans(alphas)
