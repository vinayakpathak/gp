source("functions.R")

trades <- read.csv("trades.csv")
trades["DateTime"] <- paste(trades$date, trades$TimeStamp)
trades2y <- trades[trades$alias == "CA2Y", c('DateTime', 'Column3', 'alias')]

options(digits.secs=3)
dt <- strptime(trades2y$DateTime, format="%d/%m/%Y %H:%M:%OS")

difftime(x[2], x[1], units = "secs")

plot(dt, trades2y$Column3, xaxt="n", cex=0.2)

trades2y$DateTime
?plot
