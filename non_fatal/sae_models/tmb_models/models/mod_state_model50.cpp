// see fit_mod_state_model50.R for model description

#include <TMB.hpp>
#include <mkl.h>
using namespace density;
using Eigen::SparseMatrix;
#include "lcar_strmat.hpp"
#include "prior_param_struct.hpp"
#include "prior_penalty.hpp"

template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}

// Objective function
template<class Type>
Type objective_function<Type>::operator() () {
  
  printf("MKL threads (mkl_get_max_threads): %d\n", mkl_get_max_threads());
  
  // Model family
  DATA_STRING(family);
  
  // Define data and inputs
  DATA_SPARSE_MATRIX(graph_a);
  DATA_SPARSE_MATRIX(graph_t);
  
  
  // Define data and inputs for state-mcnty observations
  DATA_VECTOR(Yx_k);     // mean (Gaussian)
  DATA_IVECTOR(A_k);    // age indicator
  DATA_IVECTOR(T_k);    // time indicator
  DATA_IVECTOR(R_k);    // race indicator
  DATA_IVECTOR(U2_k);    // under-20 indicator
  DATA_IVECTOR(U4_k);    // under-5 indicator
  DATA_IVECTOR(U6_k);    // under-1 indicator
  
  DATA_MATRIX(X_k);     // covariates (at minimum, a column for the intercept)
  DATA_MATRIX(X1_k);    // covariates that vary by age (random slope)
  DATA_MATRIX(X2_k);    // 20+ restricted covariates that vary by age (random slope)
  DATA_MATRIX(X4_k);    // 5+ restricted covariates that vary by age (random slope)
  DATA_MATRIX(X6_k);    // 1+ restricted covariates that vary by age (random slope)
  
  DATA_IVECTOR(dt_agg_rows_k);    // agg indicator
  DATA_VECTOR(agg_wt_xwalk);    // aggregation weights indicator
  
  // Define parameters
  
  // standard deviation
  PARAMETER(logSigma);
  ADREPORT(exp(2*logSigma));
  
  // Global Intercept
  PARAMETER_VECTOR(B);
  
  // Other intercepts
  PARAMETER_VECTOR(B1); // covars_subpop
  PARAMETER_VECTOR(B2); // covars_subpop_20plus
  PARAMETER_VECTOR(B4); // covars_subpop_5plus
  PARAMETER_VECTOR(B6); // covars_subpop_1plus
  
  // RE1: year-age-level random intercept (LCAR:LCAR)
  PARAMETER_ARRAY(re1);
  PARAMETER(re1_log_sigma);
  Type sigma_1 = exp(re1_log_sigma);
  PARAMETER(logit_rho_1a);
  Type rho_1a = invlogit(logit_rho_1a);
  PARAMETER(logit_rho_1t);
  Type rho_1t = invlogit(logit_rho_1t);
  DATA_STRUCT(re1_prior_param, prior_type_sigma);
  
  // Age-varying covariates
  PARAMETER_MATRIX(covar_subpop_re_matrix);
  PARAMETER_VECTOR(covar_subpop_log_sigmas);
  DATA_STRUCT(covar_subpop_hyperpriors_parameters, prior_type_sigma);
  
  // 20+ and age-varying covariates
  PARAMETER_MATRIX(covar_subpop_re_matrix_20plus);
  PARAMETER_VECTOR(covar_subpop_log_sigmas_20plus);
  DATA_STRUCT(covar_subpop_hyperpriors_parameters_20plus, prior_type_sigma); // all have same hyperpriors
  
  // 5 plus age-varying covariates
  PARAMETER_MATRIX(covar_subpop_re_matrix_5plus);
  PARAMETER_VECTOR(covar_subpop_log_sigmas_5plus);
  DATA_STRUCT(covar_subpop_hyperpriors_parameters_5plus, prior_type_sigma); // all have same hyperpriors
  
  // 1 plus age-varying covariates
  PARAMETER_MATRIX(covar_subpop_re_matrix_1plus);
  PARAMETER_VECTOR(covar_subpop_log_sigmas_1plus);
  DATA_STRUCT(covar_subpop_hyperpriors_parameters_1plus, prior_type_sigma); // all have same hyperpriors
  
  // NLL contribution from random effects
  Type nll = 0;
  max_parallel_regions = omp_get_max_threads();
  
  // RE1: year-age-level random intercept (LCAR)
  SparseMatrix<Type> K_1t = lcar_strmat(graph_t, rho_1t);
  SparseMatrix<Type> K_1a = lcar_strmat(graph_a, rho_1a);
  PARALLEL_REGION nll += SCALE(SEPARABLE(GMRF(K_1a), GMRF(K_1t)), sigma_1)(re1); 
  
  // Random effect on the age specific covariates
  for(size_t i = 0; i < covar_subpop_re_matrix.cols(); i++) {
    Type temp_sigma = exp(covar_subpop_log_sigmas[i]);
    PARALLEL_REGION nll -= dnorm(vector<Type>(covar_subpop_re_matrix.col(i)), Type(0), temp_sigma, true).sum();
  }
  
  // Random effect on the 20+ age specific covariates
  for(size_t i = 0; i < covar_subpop_re_matrix_20plus.cols(); i++) {
    Type temp_sigma_20plus = exp(covar_subpop_log_sigmas_20plus[i]);
    PARALLEL_REGION nll -= dnorm(vector<Type>(covar_subpop_re_matrix_20plus.col(i)), Type(0), temp_sigma_20plus, true).sum();
  }
  
  // Random effect on age specific covariates
  for(size_t i = 0; i < covar_subpop_re_matrix_5plus.cols(); i++) {
    Type temp_sigma_5plus = exp(covar_subpop_log_sigmas_5plus[i]);
    PARALLEL_REGION nll -= dnorm(vector<Type>(covar_subpop_re_matrix_5plus.col(i)), Type(0), temp_sigma_5plus, true).sum();
  }
  
  // Random effect on age specific covariates
  for(size_t i = 0; i < covar_subpop_re_matrix_1plus.cols(); i++) {
    Type temp_sigma_1plus = exp(covar_subpop_log_sigmas_1plus[i]);
    PARALLEL_REGION nll -= dnorm(vector<Type>(covar_subpop_re_matrix_1plus.col(i)), Type(0), temp_sigma_1plus, true).sum();
  }
  
  Type pen1 = eval_prior_sigma(re1_prior_param, re1_log_sigma);
  
  PARALLEL_REGION nll -= pen1;
  
  REPORT(pen1);
  
  // loop over entries in covar_subpop_hyperpriors_parameters, evaluate, and subtract that from nll
  for(size_t i = 0; i < covar_subpop_re_matrix.cols(); i++) {
    Type temp_pen = eval_prior_sigma(covar_subpop_hyperpriors_parameters, covar_subpop_log_sigmas[i]);
    REPORT(temp_pen);
    PARALLEL_REGION nll -= temp_pen;
    
  }
  
  for(size_t i = 0; i < covar_subpop_re_matrix_20plus.cols(); i++) {
    // loop over entries in covar_subpop_hyperpriors_parameters for 20+ covariates, evaluate, and subtract that from nll
    // for now, all hyperpriors same for all covariates
    Type temp_pen_20plus = eval_prior_sigma(covar_subpop_hyperpriors_parameters_20plus, covar_subpop_log_sigmas_20plus[i]);
    REPORT(temp_pen_20plus);
    PARALLEL_REGION nll -= temp_pen_20plus;
  }
  
  for(size_t i = 0; i < covar_subpop_re_matrix_5plus.cols(); i++) {
    // loop over entries in covar_subpop_hyperpriors_parameters for covars_subpop_5plus, evaluate, and subtract that from nll
    // for now, all hyperpriors same for all covariates
    Type temp_pen_5plus = eval_prior_sigma(covar_subpop_hyperpriors_parameters_5plus, covar_subpop_log_sigmas_5plus[i]);
    REPORT(temp_pen_5plus);
    PARALLEL_REGION nll -= temp_pen_5plus;
  }
  
  for(size_t i = 0; i < covar_subpop_re_matrix_1plus.cols(); i++) {
    // loop over entries in covar_subpop_hyperpriors_parameters for covars_subpop_1plus, evaluate, and subtract that from nll
    // for now, all hyperpriors same for all covariates
    Type temp_pen_1plus = eval_prior_sigma(covar_subpop_hyperpriors_parameters_1plus, covar_subpop_log_sigmas_1plus[i]);
    REPORT(temp_pen_1plus);
    PARALLEL_REGION nll -= temp_pen_1plus;
  }
  
  PARALLEL_REGION nll -= dnorm(logit_rho_1t, Type(0), Type(1.5), true);
  PARALLEL_REGION nll -= dnorm(logit_rho_1a, Type(0), Type(1.5), true);
  
  if (family == "g") {
    // NLL contribution from data
    // predictions
    // Implement spatial aggregation
    Type mu_k = 0;
    vector<Type> logit_m_k = X_k * B;
    for(size_t k = 0; k < Yx_k.size(); k++) {
      
      // add in the covariates w/ random effects
      for (size_t f = 0; f < covar_subpop_re_matrix.cols(); f++) {
        logit_m_k[k] += (covar_subpop_re_matrix(A_k[k], f) + B1[f]) * X1_k(k, f); // add the fth covariate-RE product
        
      } // end f
      
      // add in the 20+ covariates w/ random effects
      for (size_t f = 0; f < covar_subpop_re_matrix_20plus.cols(); f++) {
        // add covariates restricted to 20+
        if(U2_k[k] == 0) {
          logit_m_k[k] += (covar_subpop_re_matrix_20plus(A_k[k], f) + B2[f]) * X2_k(k, f);
        }
      } // end f
      
      // add in the 5+ covariates w/ random effects
      for (size_t f = 0; f < covar_subpop_re_matrix_5plus.cols(); f++) {
        if(U4_k[k] == 0) {
          logit_m_k[k] += (covar_subpop_re_matrix_5plus(A_k[k], f) + B4[f]) * X4_k(k, f);
        }
      } // end f
      
      // add in the 1+ covariates w/ random effects
      for (size_t f = 0; f < covar_subpop_re_matrix_1plus.cols(); f++) {
        if(U6_k[k] == 0) {
          logit_m_k[k] += (covar_subpop_re_matrix_1plus(A_k[k], f) + B6[f]) * X6_k(k, f);
        }
      } // end 
      
      // add other random effects
      logit_m_k[k] += re1(T_k[k], A_k[k]);
      
      // add to linear predictor for the state
      mu_k += invlogit(logit_m_k[k]) * agg_wt_xwalk[k];
      
      // data likelihood
      if(k != Yx_k.size() - 1) {
        if(dt_agg_rows_k[k] != dt_agg_rows_k[k + 1]) {
          PARALLEL_REGION nll -= dnorm(logit(Yx_k[k]), logit(mu_k), exp(logSigma), true);
          mu_k = 0;
        }
      } else { // at the end of all IDS/observations
        PARALLEL_REGION nll -= dnorm(logit(Yx_k[k]), logit(mu_k), exp(logSigma), true);
        mu_k = 0;
      } // end eval likelihood
    } // end k (finished all rows of data)
    
    
    REPORT(logit_m_k);
    
    return nll;
  }}
