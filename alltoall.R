source("functions.R")


train_and_plot(10)
n <- 10
trades <- read.csv("trades.csv")
trades["DateTime"] <- paste(trades$date, trades$TimeStamp)
trades <- trades %>% filter(date != '04/04/2018')
trades <- trades %>% mutate(Inst = if_else(alias=="CA2Y", 1,
                                 if_else(alias=="CA5Y", 2,
                                         if_else(alias=="CA10Y", 3, 4))))

# trades <- trades %>% filter(date %in% c('21/03/2018', '20/03/2018', '19/03/2018'))
trades <- trades %>% filter(date %in% c('21/03/2018'))

trades <- head(trades, n)
nrow(trades)

options(digits.secs=3)
dt <- strptime(trades$DateTime, format="%d/%m/%Y %H:%M:%OS")

timeDiff <- c(0)
for (i in 2:nrow(trades)) {
  timeDiff[i] <- difftime(dt[i], dt[i-1], units = "secs")
}

trades["TimeDiff"] <- timeDiff
trades
nrow(trades[trades$TimeDiff <= 0,])

colnames(trades)

trades %>% 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = Column3, size = QtyNominal, color = alias))

N <- nrow(trades)
N
#M <- length(unique(trades$alias))
M <- 4
M
D <- 3
y <- trades$Column3
y
inst <- trades$Inst
inst
t <- trades$TimeDiff
t
qty <- trades$QtyNominal/1e6
qty
vol_init <- 1e-8
a_vol <- 0.001
b_vol <- 0.001
noise <- rep(100, M)
noise
beta <- 0.001
a_alpha <- 0.001
b_alpha <- 0.001
# x = seq(0.1, 20, by = 0.001)
# plot(x, dgamma(x, shape = a_alpha, rate = b_alpha))
samples <- rgamma(1000000, shape = a_alpha, rate = b_alpha)
mean(samples)
sd(samples)
vol <- 0.01
W <- matrix(c(1, 1, 1,
         1, 1, -1,
         1, -1, -1,
         1, -1, 1), nrow=4, ncol=3)
W
data_list <- list(N = N, M = M, D = D, y = y, inst = inst, t = t, qty = qty,
                  vol_init = vol_init, a_vol = a_vol, b_vol = b_vol, noise = noise,
                  beta = beta, a_alpha = a_alpha, b_alpha = b_alpha, vol = vol, W = W)
#View(trades)
fit <- stan(file = "alltoall.stan", data = data_list, chains = 1, iter = 2000, 
            control = list(max_treedepth = 12, adapt_delta = 0.8, stepsize = 1), seed = 123456)
m <- stan_model(file = "alltoall.stan")
fit <- vb(m, data = data_list, algorithm="meanfield", iter=30000, seed=123456)
print(fit)
summary(fit)
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

trades <- trades %>% mutate(alpha2y = if_else(sim2y75 - sim2y25 < 1, 0.3, 0))
trades <- trades %>% mutate(alpha5y = if_else(sim5y75 - sim5y25 < 1, 0.3, 0))
trades <- trades %>% mutate(alpha10y = if_else(sim10y75 - sim10y25 < 1, 0.3, 0))
trades <- trades %>% mutate(alpha30y = if_else(sim30y75 - sim30y25 < 1, 0.3, 0))


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

m2y <- get_posterior_mean(fit, pars = par2y)
m5y <- get_posterior_mean(fit, pars = par5y)
m10y <- get_posterior_mean(fit, pars = par10y)
m30y <- get_posterior_mean(fit, pars = par30y)

trades$sim2y <- m2y[,"mean-all chains"]
trades$sim5y <- m5y[,"mean-all chains"]
trades$sim10y <- m10y[,"mean-all chains"]
trades$sim30y <- m30y[,"mean-all chains"]



trades %>% 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = Column3, size = QtyNominal, color = alias)) +
  geom_point(aes(y = sim2y, alpha = 0.1)) +
  geom_point(aes(y = sim5y, alpha = 0.1)) +
  geom_point(aes(y = sim10y, alpha = 0.1)) +
  geom_point(aes(y = sim30y, alpha = 0.1))


trades

launch_shinystan(fit)

fit_data <- as.data.frame(fit)
W_post_mean <- matrix(c(mean(fit_data$`w[1,1]`),
                        mean(fit_data$`w[1,2]`),
                        mean(fit_data$`w[1,3]`),
                        mean(fit_data$`w[1,4]`),
                        mean(fit_data$`w[2,1]`),
                        mean(fit_data$`w[2,2]`),
                        mean(fit_data$`w[2,3]`),
                        mean(fit_data$`w[2,4]`),
                        mean(fit_data$`w[3,1]`),
                        mean(fit_data$`w[3,2]`),
                        mean(fit_data$`w[3,3]`),
                        mean(fit_data$`w[3,4]`)),
                      nrow=4, ncol=3)
W_post_mean
sd(fit_data$`w[1,1]`)
sd(fit_data$`w[1,2]`)
sd(fit_data$`w[1,3]`)
sd(fit_data$`w[1,4]`)
sd(fit_data$`w[2,1]`)
sd(fit_data$`w[2,2]`)
sd(fit_data$`w[2,3]`)
sd(fit_data$`w[2,4]`)
sd(fit_data$`w[3,1]`)
sd(fit_data$`w[3,2]`)
sd(fit_data$`w[3,3]`)
sd(fit_data$`w[3,4]`)


mean(fit_data$`alpha[1]`)
mean(fit_data$`alpha[2]`)
mean(fit_data$`alpha[3]`)

sd(fit_data$`alpha[1]`)
sd(fit_data$`alpha[2]`)
sd(fit_data$`alpha[3]`)

mean(fit_data$vol)
sd(fit_data$vol)

params <- as.data.frame(extract(fit, permuted=FALSE))
divergent <- get_sampler_params(fit, inc_warmup=FALSE)[[1]][, 'divergent__']
params$divergent <- divergent
div_params <- params[params$divergent == 1,]
nondiv_params <- params[params$divergent == 0,]
plot(nondiv_params$`chain:1.alpha[1]`, nondiv_params$`chain:1.w[1,2]`)
points(div_params$`chain:1.alpha[1]`, div_params$`chain:1.w[1,2]`, col="green")
?gsub
params$iter <- 1:700
plot(params$`chain:1.vol`)
