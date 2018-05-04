data {
  int<lower=0> N;
  int<lower=0> N_Days;  //number of days
  int K[N_Days];  //number of observations in each day
  
  vector[N] y;
  vector[N] t;  //t[1] = garbage, t[i] = time gap between i and i-1
  
  real<lower=0> vol_init;
  real a_vol;
  real b_vol;
  real a_noise;
  real b_noise;

  // real<lower=0> vol;
  // real<lower=0> noise;
}

transformed data {
  int I[N_Days];
  I[1] = 1;
  for (i in 2:N_Days) {
    I[i] = I[i-1] + K[i-1];
  }
  print(K, I)
}

parameters {
  vector[N] z_tilde;
  // vector[N] z;
  real<lower=0> vol;
  real<lower=0> noise;
}

transformed parameters {
  vector[N] z;
  z[1] = 1000*z_tilde[1];
  for (i in 2:N) {
    z[i] = z[i-1] + (vol_init + t[i]/vol) * z_tilde[i];
  }
  // z[2:N] = z[1:N-1] + (vol_init + t[2:N]/vol) * z_tilde[2:N];
}

model {
  vol ~ gamma(a_vol, b_vol);
  noise ~ gamma(a_noise, b_noise);
  z_tilde ~ normal(0, 1);
  
  // for (i in 2:N)
  //   z[i] ~ normal(z[i-1], t[i] / vol + vol_init);
  
//  for (i in 1:N_Days) {
//    z[I[i]] ~ normal(1, 1000);
//    z[I[i]+1:I[i]+K[i]-1] ~ normal(z[I[i]:I[i]+K[i]-2], t[I[i]+1:I[i]+K[i]-1]/vol + vol_init);
//  }

  y ~ normal(z, 1/noise);
}
