data {
  int<lower=0> D; //dimension of latent space
  int<lower=0> M; //dimension of data space
  int<lower=0> N; //number of data points
  
  real sigma; //sd of noise
  vector[M] y[N];
}

parameters {
  matrix[M, D] W;
  vector[M] mu;

  vector[D] z[N];
}

model {
  for (i in 1:N) {
    z[i] ~ multi_normal(rep_vector(0, D), diag_matrix(rep_vector(1, D)));
    y[i] ~ multi_normal(W*z[i]+mu, diag_matrix(rep_vector(sigma*sigma, M)));
  }
}

generated quantities {
  vector[D] z_sim;
  vector[M] y_sim;
  z_sim = multi_normal_rng(rep_vector(0, D), 
                                 diag_matrix(rep_vector(1, D)));
  y_sim = multi_normal_rng(W*z+mu, 
                                 diag_matrix(rep_vector(sigma*sigma, M)));
  }
}
