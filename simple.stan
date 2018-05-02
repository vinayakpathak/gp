data {
  int<lower=0> N;
  
  vector[N] y;
  vector[N] t;  //t[1] = garbage, t[i] = time gap between i and i-1
  
  real<lower=0> vol_init;
  real a_vol;
  real b_vol;
  real a_noise;
  real b_noise;
}

parameters {
  vector[N] z;
  real<lower=0> vol;
  real<lower=0> noise;
}

model {
  z[1] ~ normal(1, 1000);
  vol ~ gamma(a_vol, b_vol);
  noise ~ gamma(a_noise, b_noise);
  //for (i in 2:N)
  //  z[i] ~ normal(z[i-1], t[i] / vol + vol_init);
  
  z[2:N] ~ normal(z[1:N-1], t[2:N]/vol + vol_init);
  
  y ~ normal(z, 1/noise);
}
