source("functions.R")

trades <- read.csv("trades.csv")
trades["DateTime"] <- paste(trades$date, trades$TimeStamp)
trades <- trades %>% filter(date != '04/04/2018')
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
a_vol <- 0.001
b_vol <- 0.001
a_noise <- 0.001
b_noise <- 0.001
data_list <- list(N = N, y = y, t = t, vol_init = vol_init, 
                  a_vol = a_vol, b_vol = b_vol, a_noise = a_noise, b_noise = b_noise)
fit <- stan(file = "simple.stan", data = data_list, chains = 2, iter = 3000)
fit_data <- as.data.frame(fit)

colnames(fit_data)
length(colnames(fit_data))

zs <- fit_data %>% summarise_all(mean)

zss <- zs %>% select(starts_with('z'))
ncol(zss)
nrow(trades2y)
trades2y["z"] <- as.numeric(t(zss))


trades2y$z
x <- trades2y %>% filter(grepl("21/03/2018", DateTime))
plot_series(x)
dt <- strptime(trades2y$DateTime, format="%d/%m/%Y %H:%M:%OS")
lines(dt, trades2y$Column3)

head(trades2y)
