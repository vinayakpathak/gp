data {
  int<lower=0> M;  //number of seconds
  int<lower=0> N;  //number of events
  int<lower=0> t[N];  //time of events in seconds
  
  vector[N] y;
  real<lower=0> qty[N];
  
  real<lower=0> vol_init;
  real<lower=0> a_vol;
  real<lower=0> b_vol;
  real<lower=0> noise;
}

parameters {
  vector[M] z_tilde;
  real<lower=0> vol;
}

transformed parameters {
  vector[M] z;
  z[1] = y[1];
  for (i in 2:M) {
    z[i] = z[i-1] + 1/vol * z_tilde[i];
  }
}

model {
  vol ~ gamma(a_vol, b_vol);
  z_tilde ~ normal(0, 1);

  for (i in 1:N) {
    y[i] ~ normal(z[t[i]+1], noise/sqrt(qty[i]));
  }
}
