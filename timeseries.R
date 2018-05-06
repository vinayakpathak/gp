source("functions.R")

trades <- read.csv("trades.csv")
trades["DateTime"] <- paste(trades$date, trades$TimeStamp)
trades <- trades %>% filter(date != '04/04/2018')
trades <- trades %>% filter(date %in% c('21/03/2018', '20/03/2018', '19/03/2018'))
# trades <- trades %>% filter(date %in% c('21/03/2018'))
trades
trades2y <- trades %>% 
  filter(alias == "CA2Y") %>%
  select('DateTime', 'date', 'Column3', 'alias', 'QtyNominal')

options(digits.secs=3)
dt <- strptime(trades2y$DateTime, format="%d/%m/%Y %H:%M:%OS")

timeDiff <- c(0)
for (i in 2:nrow(trades2y)) {
  timeDiff[i] <- difftime(dt[i], dt[i-1], units = "secs")
}

trades2y["TimeDiff"] <- timeDiff
trades2y

plot(dt, trades2y$Column3, xaxt="n", cex=0.2)



N <- nrow(trades2y)
N
y <- trades2y$Column3
y
t <- trades2y$TimeDiff
t
qty <- trades2y$QtyNominal/1e6
qty
vol_init <- 0.0000001
# a_vol <- 1
# b_vol <- 1e-5
a_vol <- 0.0001
b_vol <- 0.0001
# x = seq(0.1, 20, by = 0.001)
# plot(x, dgamma(x, shape = a_vol, rate = b_vol))
samples <- rgamma(1000000, shape = a_vol, rate = b_vol)
mean(samples)
sd(samples)
sd(samples) * sd(samples)

a_noise <- 0.25
b_noise <- 0.0025
# a_noise <- 0.0001
# b_noise <- 0.0001
samples <- rgamma(1000000, shape = a_noise, rate = b_noise)
mean(samples)
sd(samples)
sd(samples) * sd(samples)
# x = seq(0.001, 0.1, by = 0.0001)
# plot(x, dgamma(x, shape = a_noise, rate = b_noise))
N_Days <- length(unique(trades2y$date))
N_Days
trades2y["Date"] <- as.Date(trades2y$date, "%d/%m/%Y")
k <- trades2y %>%
  group_by(Date) %>%
  summarise(n()) %>%
  arrange(Date)
k
k$`n()`
#array(k$`n()`, dim = 1)
vol <- 1e10
vol
noise <- 100
noise
# data_list <- list(N = N, y = y, t = t, vol_init = vol_init, K = array(k$`n()`, dim = 1),
                  # N_Days = N_Days, qty = qty,
                  # a_vol = a_vol, b_vol = b_vol, a_noise = a_noise, b_noise = b_noise)
data_list <- list(N = N, y = y, t = t, vol_init = vol_init, K = k$`n()`,
                  N_Days = N_Days, qty = qty,
                  a_vol = a_vol, b_vol = b_vol, noise = noise)
# data_list <- list(N = N, y = y, t = t, vol_init = vol_init, K = array(k$`n()`, dim=1),
                  # N_Days = N_Days, vol = vol, noise = noise)
fit <- stan(file = "simple.stan", data = data_list, chains = 2, iter = 3000, 
            control = list(max_treedepth = 13, adapt_delta = 0.9), seed = 123456)
print(fit)
check_treedepth(fit)
check_energy(fit)
check_divergences(fit)
fit_data <- as.data.frame(fit)
mean(fit_data$vol)
colnames(fit_data)
length(colnames(fit_data))


zs <- fit_data %>% summarise_all(mean)
z5 <- fit_data %>% summarise_all(quantSmall)
z95 <- fit_data %>% summarise_all(quantBig)
zss <- zs %>% select(starts_with('z['))
z5s <- z5 %>% select(starts_with('z['))
z95s <- z95 %>% select(starts_with('z['))

ncol(zss)
nrow(trades2y)
trades2y["z"] <- as.numeric(t(zss))
trades2y["z5"] <- as.numeric(t(z5s))
trades2y["z95"] <- as.numeric(t(z95s))

trades2y %>% 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = Column3)) +
  geom_point(aes(y = z, color = "red"))

trades2y %>% 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = Column3, size = QtyNominal)) +
  geom_ribbon(aes(ymin = z5, ymax = z95, alpha = 0.3))
View(trades2y)

View(trades2y)
trades2y$z
x <- trades2y %>% filter(grepl("21/03/2018", DateTime))
plot_series(trades2y)

dt <- strptime(trades2y$DateTime, format="%d/%m/%Y %H:%M:%OS")
lines(dt, trades2y$Column3)
lines(dt, trades2y$z5)
lines(dt, trades2y$z95)
trades2y %>% select('Column3', 'z', 'z5', 'z95')
colnames(trades2y)
head(x)

hist(fit_data$vol)
summary(fit_data$vol)

hist(sqrt(1/fit_data$vol))
summary(sqrt(1/fit_data$vol))

hist(fit_data$noise)
summary(fit_data$noise)

hist(sqrt(1/fit_data$noise))
summary(sqrt(1/fit_data$noise))

colnames(x)


options(max.print=100000)
library(shinystan)
install.packages("shinystan")
