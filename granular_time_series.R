source("functions.R")

trades <- read.csv("trades.csv")
trades["DateTime"] <- paste(trades$date, trades$TimeStamp)
trades <- trades %>% filter(date != '04/04/2018')
# trades <- trades %>% filter(date %in% c('21/03/2018', '20/03/2018', '19/03/2018'))
trades <- trades %>% filter(date %in% c('21/03/2018'))
trades
trades2y <- trades %>% 
  filter(alias == "CA2Y") %>%
  select('DateTime', 'date', 'Column3', 'alias', 'QtyNominal')

options(digits.secs=0)
dt <- strptime(trades2y$DateTime, format="%d/%m/%Y %H:%M:%OS")
dt

timeDiff <- c(0)
for (i in 2:nrow(trades2y)) {
  timeDiff[i] <- floor(difftime(dt[i], dt[1], units = "secs"))
}

trades2y["TimeDiff"] <- timeDiff
trades2y

trades2y %>% 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = Column3, size = QtyNominal))

M <- trades2y$TimeDiff[length(trades2y$TimeDiff)] + 1
M

N <- nrow(trades2y)
N

t <- trades2y$TimeDiff
t

y <- trades2y$Column3
y

qty <- trades2y$QtyNominal/1e5
qty

# real<lower=0> vol_init;
a_vol <- 0.001
b_vol <- 0.001
noise <- 0.01

data_list = list(N = N, M = M, t = t, qty = qty, a_vol = a_vol, b_vol = b_vol,
                 noise = noise)
fit <- stan(file = "granular_time_series.stan", data = data_list, chains = 2, 
            iter = 3000, 
            control = list(max_treedepth = 13, adapt_delta = 0.9), seed = 123456)

fit_data <- as.data.frame(fit)

