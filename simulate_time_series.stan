data {
  int<lower=0> N;
  real<lower=0> vol;
  real<lower=0> vol_intercept;
  real<lower=0> noise;
  real z1;
  real<lower=0> lambda;
}

parameters {
  vector[N] z_std;
  real<lower=0> t[N];
  vector[N] y_std;
}

transformed parameters {
  vector[N] z;
  vector[N] y;
  z[1] = z1 + 0.00000001 * z_std[1];
  for (i in 2:N) {
    z[i] = z[i-1] + sqrt(vol*vol*t[i]+vol_intercept)*z_std[i];
  }
  y = z + noise*y_std;
}

model {
  // z[1] ~ normal(z1, 0.00000001);
  t ~ exponential(lambda);
  
  z_std ~ normal(0, 1);
  y_std ~ normal(0, 1);
  // z[2:N] ~ normal(z[1:N-1], vol*t[2:N]+vol_intercept);
  // y ~ normal(z, noise);
}
