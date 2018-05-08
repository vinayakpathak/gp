data {
  int<lower=0> N;  //number of data points
  int<lower=0> M;  //number of instruments
  
  vector[N] y;  //traded level
  int inst[N];  //instrument that was observed
  real<lower=0> t[N];  //t[1] = garbage, t[i] = time gap between i and i-1
  real<lower=0> qty[N];  //quantitiy traded
}

parameters {
  
}

model {
  
}