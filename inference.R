df <- read.csv("pca.csv")


xs <- c(0, 3, 6, 9, 12, 15, 18, 21, 24)
clr <- rgb(0,0,0,alpha = 0.03)
clr1 <- rgb(1,0,0)
plot(xs, df[1, ], "l", ylim = c(-1, 5), col=clr1)
for (i in seq(1, nrow(df))) {lines(xs, df[i, ], col=clr)}



D <- 3
M <- 9
N <- nrow(df)
sigma <- 1
vector[M] y[N];

data_list <- list(D = D, M = M, N = N, sigma = sigma, y = df)
fit <- stan(file = "inference.stan", data = data_list, cores = 1, chains = 2, iter = 2000)
fit_data <- as.data.frame(fit)

summary(fit_data)

