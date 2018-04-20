data {
  int<lower=0> D; //dimension of latent space
  int<lower=0> M; //dimension of data space
  int<lower=0> N; //number of data points
  
  real sigma; //sd of noise
  matrix[M, D] W;
  vector[M] mu;
}

parameters {
  vector[M] y;
  vector[D] z;
}

model {
  z ~ multi_normal(rep_vector(0, D), diag_matrix(rep_vector(1, D)));
  y ~ multi_normal(W*z+mu, diag_matrix(rep_vector(sigma*sigma, M)));
}
