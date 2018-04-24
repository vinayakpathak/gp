data {
  int<lower=0> N;
  
  vector<N> y;
  vector<N> t;  //t[1] = garbage, t[i] = time gap between i and i-1
  
  real<lower=0> vol;
}

parameters {
  vector<N> z;
  
}

model {
  z[1] ~ normal(1, 1000);
  for (i in 2:N)
    z[i] ~ normal(z[i-1], t[i] * vol);
  
  y ~ normal(z, 0.1);
}