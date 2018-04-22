data {
  int<lower=0> D; //dimension of latent space
  int<lower=0> M; //dimension of data space
  int<lower=0> N; //number of data points
  
  vector[M] y[N];
  
  real beta;
  real a_alpha;
  real b_alpha;
  real a_tau;
  real b_tau;
}

transformed data {
  print(y[1]); //for debugging
  print(gamma_lpdf(1 | a_alpha, b_alpha));
}

parameters {
  vector[M] mu;
  vector[D] z[N];
  vector[M] w[D];
  real<lower=0> tau;
  vector<lower=0>[D] alpha;
}

transformed parameters {
  matrix[M, D] W;
  for (i in 1:D)
    for (j in 1:M)
      W[j, i] = w[i][j];
}

model {
  vector[D] z_mean[N];
  
  z_mean = rep_array(rep_vector(0, D), N);
  z ~ multi_normal(z_mean, diag_matrix(rep_vector(1, D)));
  
  mu ~ normal(0, 1/beta);
  for (i in 1:D) {
    alpha[i] ~ gamma(a_alpha, b_alpha);
    //print(alpha[i]);
  }
  
  for (i in 1:D)
    w[i] ~ normal(0, 1/alpha[i]);

  tau ~ gamma(a_tau, b_tau);
  
  for (i in 1:N)
    y[i] ~ multi_normal(W*z[i]+mu, diag_matrix(rep_vector(1/tau, M)));
}

generated quantities {
  vector[D] z_sim;
  vector[M] y_sim;
  z_sim = multi_normal_rng(rep_vector(0, D), diag_matrix(rep_vector(1, D)));
  y_sim = multi_normal_rng(W*z_sim+mu, diag_matrix(rep_vector(1/tau, M)));
}
