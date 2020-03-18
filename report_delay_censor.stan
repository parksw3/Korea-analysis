// generated with brms 2.10.0
functions {
}

data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int absmax[N];  // censor variable
  // data for splines
  int Ks;  // number of linear effects
  matrix[N, Ks] Xs;  // design matrix for the linear effects
  // data for spline s(day)
  int nb_1;  // number of bases
  int knots_1[nb_1];  // number of knots
  // basis function matrices
  matrix[N, knots_1[1]] Zs_1_1;
  int prior_only;  // should the likelihood be ignored?
}

transformed data {
}

parameters {
  // temporary intercept for centered predictors
  real Intercept;
  // spline coefficients
  vector[Ks] bs;
  // parameters for spline s(day)
  // standarized spline coefficients
  vector[knots_1[1]] zs_1_1;
  // standard deviations of the coefficients
  real<lower=0> sds_1_1;
  real<lower=0> shape;  // shape parameter
}

transformed parameters {
  // actual spline coefficients
  vector[knots_1[1]] s_1_1 = sds_1_1 * zs_1_1;
  
  // initialize linear predictor term
  vector[N] mu = exp(Intercept + rep_vector(0, N) + Xs * bs + Zs_1_1 * s_1_1);
}

model {
  bs ~ normal(0, 2);
  Intercept ~ normal(0, 2);
  // priors including all constants
  target += normal_lpdf(zs_1_1 | 0, 1);
  target += student_t_lpdf(sds_1_1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += gamma_lpdf(shape | 0.01, 0.01);
  // likelihood including all constants
  if (!prior_only) {
    target += neg_binomial_2_lpmf(Y | mu, shape)-neg_binomial_2_lcdf(absmax | mu, shape);
  }
}

generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
