data {
  int<lower=0> N;  //number of data points
  int<lower=0> M;  //number of instruments
  int<lower=0> D;  //number of dimensions in the latent space
  
  vector[N] y;  //traded level
  int inst[N];  //instrument that was observed
  real<lower=0> t[N];  //t[1] = garbage, t[i] = time gap between i and i-1
  real<lower=0> qty[N];  //quantitiy traded
  
  real<lower=0> vol_init;  //base vol to add when time gap is close to 0
  real a_vol;  //parameters for gamma prior on vol
  real b_vol;  //parameters for gamma prior on vol
  real noise[M];  //how noisy is measurement on each instrument
  real beta;  //precision for mu
  real a_alpha;  //parameter for gamma prior for precision of elements of W
  real b_alpha;  //parameter for gamma prior for precision of elements of W
}

parameters {
  vector[D] z_std[N];
  real<lower=0> vol;
  vector[M] mu;
  vector[M] w_std[D];
  vector<lower=0>[D] alpha;
}

transformed parameters {
  matrix[M, D] W;
  vector[D] z[N];
  vector[M] w[D];
  
  for (i in 1:D)
    w[i] = 1/alpha[i] * w_std[i];
  
  for (i in 1:D)
    for (j in 1:M)
      W[j, i] = w[i][j];
      
  z[1] = z_std[1];
  for (i in 2:N) {
    z[i] = z[i-1] + sqrt(t[i]*vol*vol + vol_init) * z_std[i];
  }
}

model {
  vector[M] yi;
  
  vol ~ gamma(a_vol, b_vol);
  
  // z[1] ~ normal(0, 1);
  // for (i in 2:N)
  //   z[i] ~ normal(z[i-1], t[i] / vol + vol_init);
  
  for (i in 1:N) 
    z_std[i] ~ normal(0, 1);
  
  mu ~ normal(0, 1/(beta*beta));
  for (i in 1:D) {
    alpha[i] ~ gamma(a_alpha, b_alpha);
  }
  
  // for (i in 1:D)
  //   w[i] ~ normal(0, 1/alpha[i]);
  
  for (i in 1:D)
    w_std[i] ~ normal(0, 1);
  
  for (i in 1:N) {
    yi = W*z[i]+mu;
    y[i] ~ normal(yi[inst[i]], 1/(noise[inst[i]]*sqrt(qty[i])));
  }
}

generated quantities {
  vector[M] y_sim[N];
  for (i in 1:N)
    y_sim[i] = W*z[i]+mu;
}
