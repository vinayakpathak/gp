x <- seq(-10, 10, 0.01)
y <- dnorm(x, 0, 1000)

data.frame(x, y) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  ylim(0, 0.001) +
  labs(x = "x", y = "P(x)")
ggsave(filename = "~/ml/gp/gauss1.png", scale = 0.35)

posterior <- function(y, prior_mean, prior_sd) {
  mean <- (y + prior_mean/prior_sd/prior_sd)/(1 + 1/prior_sd/prior_sd)
  variance <- 1/(1+1/prior_sd/prior_sd)
  return(list(mean = mean, sd = sqrt(variance)))
}

data <- 5
p <- posterior(data, 0, 1000)
y <- dnorm(x, p$mean, p$sd + 2)
data.frame(x, y) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  ylim(0, 0.4) +
  # xlim(-10, 10) +
  labs(x = "x", y = "P(x|y)")+
  geom_point(aes(x = data, y = 0), color="red")
  # geom_point(aes(x = 4.1, y = 0), color="red")
ggsave(filename = "~/ml/gp/evolve.png", scale = 0.35)

data <- 4.1
p <- posterior(data, p$mean, p$sd + 2)
y <- dnorm(x, p$mean, p$sd)
data.frame(x, y) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  labs(x = "x", y = "P(x|y)")+
  geom_point(aes(x = 5, y = 0), color="red") +
  geom_point(aes(x = 4.1, y = 0), color="red")
ggsave(filename = "~/ml/gp/evolve1.png", scale = 0.35)

data <- 3.1
p <- posterior(data, p$mean, p$sd)
y <- dnorm(x, p$mean, p$sd)
data.frame(x, y) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  labs(x = "x", y = "P(x|y)")+
  geom_point(aes(x = 5, y = 0), color="red") +
  geom_point(aes(x = 4.1, y = 0), color="red") + 
  geom_point(aes(x = 3.1, y = 0), color="red")
ggsave(filename = "~/ml/gp/gauss4.png", scale = 0.35)

data <- 5.2
p <- posterior(data, p$mean, p$sd)
y <- dnorm(x, p$mean, p$sd)
data.frame(x, y) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  labs(x = "x", y = "P(x|y)")+
  geom_point(aes(x = 5, y = 0), color="red") +
  geom_point(aes(x = 4.1, y = 0), color="red") + 
  geom_point(aes(x = 3.1, y = 0), color="red") +
  geom_point(aes(x = 5.2, y = 0), color="red")
ggsave(filename = "~/ml/gp/gauss5.png", scale = 0.35)

data <- -1
p <- posterior(data, p$mean, p$sd)
y <- dnorm(x, p$mean, p$sd)
data.frame(x, y) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  labs(x = "x", y = "P(x|y)")+
  geom_point(aes(x = 5, y = 0), color="red") +
  geom_point(aes(x = 4.1, y = 0), color="red") + 
  geom_point(aes(x = 3.1, y = 0), color="red") +
  geom_point(aes(x = 5.2, y = 0), color="red") +
  geom_point(aes(x = -1, y = 0), color="red")
ggsave(filename = "~/ml/gp/gauss6.png", scale = 0.35)
