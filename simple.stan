data {
  int<lower=0> N;
  int<lower=0> N_Days;  //number of days
  vector[N_Days] K;  //number of observations in each day
  
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
  vol ~ gamma(a_vol, b_vol);
  noise ~ gamma(a_noise, b_noise);
  //for (i in 2:N)
  //  z[i] ~ normal(z[i-1], t[i] / vol + vol_init);
  
  int n = 0;
  for (i in 1:N_Days) {
    z[n+1] ~ normal(1, 1000);
    z[n+2:n+K[i]] ~ normal(z[n+1:n+K[i]-1], t[n+2:n+K[i]]/vol + vol_init);
    n = n + K[i];
  }
  
  y ~ normal(z, 1/noise);
}
