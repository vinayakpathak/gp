source("functions.R")

trades <- read.csv("trades.csv")
trades["DateTime"] <- paste(trades$date, trades$TimeStamp)
trades <- trades %>% filter(date != '04/04/2018')
trades <- trades %>% filter(date %in% c('21/03/2018', '20/03/2018'))
trades
trades2y <- trades %>% 
  filter(alias == "CA2Y") %>%
  select('DateTime', 'Column3', 'alias')

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
y <- trades2y$Column3
t <- trades2y$TimeDiff
vol_init <- 0.0000001
a_vol <- 0.0001
b_vol <- 0.0001
a_noise <- 0.0001
b_noise <- 0.0001
data_list <- list(N = N, y = y, t = t, vol_init = vol_init, 
                  a_vol = a_vol, b_vol = b_vol, a_noise = a_noise, b_noise = b_noise)
fit <- stan(file = "simple.stan", data = data_list, chains = 2, iter = 3000)
fit_data <- as.data.frame(fit)

colnames(fit_data)
length(colnames(fit_data))

quant5 <- function(x) {
  quantile(x, probs = c(0.01))
}

quant95 <- function(x) {
  quantile(x, probs = c(0.99))
}

zs <- fit_data %>% summarise_all(mean)
z5 <- fit_data %>% summarise_all(quant5)
z95 <- fit_data %>% summarise_all(quant95)
zss <- zs %>% select(starts_with('z'))
z5s <- z5 %>% select(starts_with('z'))
z95s <- z95 %>% select(starts_with('z'))

ncol(zss)
nrow(trades2y)
trades2y["z"] <- as.numeric(t(zss))
trades2y["z5"] <- as.numeric(t(z5s))
trades2y["z95"] <- as.numeric(t(z95s))

trades2y$z
x <- trades2y %>% filter(grepl("21/03/2018", DateTime))
plot_series(x)

dt <- strptime(trades2y$DateTime, format="%d/%m/%Y %H:%M:%OS")
lines(dt, trades2y$Column3)
lines(dt, trades2y$z5)
lines(dt, trades2y$z95)
trades2y %>% select('Column3', 'z', 'z5', 'z95')
colnames(trades2y)
head(x)
hist(fit_data$vol)
hist(fit_data$noise)

colnames(x)

