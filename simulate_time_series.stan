data {
  int<lower=0> N;
  real<lower=0> vol;
  real<lower=0> vol_intercept;
  real<lower=0> noise;
  real z1;
  real<lower=0> lambda;
}

parameters {
  vector[N] z;
  vector[N] t;
  vector[N] y;
}

model {
  z[1] ~ normal(z1, 0.00000001);
  t ~ exponential(lambda);
  
  z[2:N] ~ normal(z[1:N-1], vol*t[2:N]+vol_intercept);
  y ~ normal(z, noise);
}
