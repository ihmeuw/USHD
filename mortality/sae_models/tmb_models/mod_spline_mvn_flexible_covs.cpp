
#include <TMB.hpp>
#include <mkl.h>
using namespace density;
using Eigen::SparseMatrix;
#include "lcar_strmat.hpp"
#include "prior_param_struct.hpp"
#include "prior_penalty.hpp"

// Objective function
template<class Type>
  Type objective_function<Type>::operator() () {
    
    printf("MKL threads (mkl_get_max_threads): %d\n", mkl_get_max_threads());
    
    // Define data and inputs
    DATA_VECTOR(Y);     // deaths
    DATA_VECTOR(N);     // population
    DATA_MATRIX(X);     // covariates (at minimum, a column for the intercept)
    DATA_IVECTOR(J);    // area indicator
    DATA_IVECTOR(T);    // year indicator
    DATA_IVECTOR(A);    // age indicator
    DATA_IVECTOR(R);    // race indicator
    DATA_MATRIX(X1);    // race-ethnicity specific covariates
    
    DATA_SPARSE_MATRIX(graph_j);  // neighborhood structure
    DATA_SPARSE_MATRIX(graph_t);  // year matrix
    DATA_SPARSE_MATRIX(graph_a);  // age matrix
    DATA_SPARSE_MATRIX(graph_ts); // time spline matrix
    DATA_SPARSE_MATRIX(graph_as); // age spline matrix
    DATA_INTEGER(num_subpop); // number of groups in the subpopulation
    
    // spline bases
    DATA_MATRIX(s_age);
    DATA_MATRIX(s_year);
    
    // Define parameters
    // fixed effects (intercept, covariate effects)
    PARAMETER_VECTOR(B);
    
    // RE1: area-race-age spline-time spline-level random intercept (LCAR:MVN:LCAR:LCAR)
    PARAMETER_ARRAY(re1);
    PARAMETER_VECTOR(re1_log_sigma);
    vector<Type> sigma_1 = exp(re1_log_sigma);
    PARAMETER(logit_rho_1j);
    Type rho_1j = invlogit(logit_rho_1j);
    // year spline parameters
    PARAMETER(logit_rho_1t);
    Type rho_1t = invlogit(logit_rho_1t);
    // age spline parameters
    PARAMETER(logit_rho_1a);
    Type rho_1a = invlogit(logit_rho_1a);
    PARAMETER_VECTOR(cor_1);
    DATA_STRUCT(re1_prior_param, prior_type_sigma);
    
    
    // RE2: area-level random intercept (LCAR)
    PARAMETER_VECTOR(re2); 
    PARAMETER(re2_log_sigma);
    Type sigma_2 = exp(re2_log_sigma);
    PARAMETER(logit_rho_2);
    Type rho_2 = invlogit(logit_rho_2);
    DATA_STRUCT(re2_prior_param, prior_type_sigma);
    
    // RE3: age-year-race-level random intercept (LCAR:LCAR:MVNORM)
    PARAMETER_ARRAY(re3);
    PARAMETER(re3_log_sigma);
    Type sigma_3 = exp(re3_log_sigma);
    PARAMETER(logit_rho_3t);
    Type rho_3t = invlogit(logit_rho_3t);
    PARAMETER(logit_rho_3a);
    Type rho_3a = invlogit(logit_rho_3a);
    DATA_STRUCT(re3_prior_param, prior_type_sigma);
    
    // Subpopulation covariates (e.g. covariates by race/ethnicity)
    PARAMETER_ARRAY(covar_subpop_re_matrix);
    
    PARAMETER_VECTOR(covar_subpop_log_sigmas);
    
    DATA_STRUCT(covar_subpop_hyperpriors_parameters, prior_type_sigma);
    
    // NLL contribution from random effects
    Type nll = 0;
    max_parallel_regions = omp_get_max_threads();
    
    // RE1: area-race-age spline-time spline-level random intercept (LCAR:MVN:LCAR:LCAR)
    SparseMatrix<Type> K_1j = lcar_strmat(graph_j, rho_1j);
    SparseMatrix<Type> K_1t = lcar_strmat(graph_ts, rho_1t);
    SparseMatrix<Type> K_1a = lcar_strmat(graph_as, rho_1a);
    matrix<Type> cov_1 = UNSTRUCTURED_CORR(cor_1).cov();
    REPORT(cor_1);
    REPORT(cov_1);
    // race(area(age_spline,year_spline))
    PARALLEL_REGION nll += SEPARABLE(VECSCALE(MVNORM(cov_1), sigma_1), SEPARABLE(GMRF(K_1j), SEPARABLE(GMRF(K_1a), GMRF(K_1t))))(re1);
    
    
    // RE2: area-level random intercept
    SparseMatrix<Type> K_2 = lcar_strmat(graph_j, rho_2); 
    PARALLEL_REGION nll += SCALE(GMRF(K_2), sigma_2)(re2);
    
    // RE3: age-year-race-level random intercept
    SparseMatrix<Type> K_3t = lcar_strmat(graph_t, rho_3t);
    SparseMatrix<Type> K_3a = lcar_strmat(graph_a, rho_3a);
    
    // Create sparse identity matrix
    matrix<Type> Sigma(num_subpop, num_subpop);
    Sigma.setIdentity();
    Eigen::SparseMatrix<Type> Sigma_Sparse=asSparseMatrix(Sigma);
    REPORT(Sigma_Sparse);
    
    PARALLEL_REGION nll += SCALE(SEPARABLE(GMRF(Sigma_Sparse), SEPARABLE(GMRF(K_3a), GMRF(K_3t))), sigma_3)(re3);
    
    // Random effect on the subpopulation specific covariates
    for(size_t i = 0; i < covar_subpop_re_matrix.cols(); i++) {
      Type temp_sigma = exp(covar_subpop_log_sigmas[i]);
      PARALLEL_REGION nll -= dnorm(vector<Type>(covar_subpop_re_matrix.col(i)), Type(0), temp_sigma, true).sum();
    }
    
    vector<Type> pen1(sigma_1.size());
    pen1.setZero();
    
    for(int j = 0; j < sigma_1.size(); j++){
      pen1[j] = eval_prior_sigma(re1_prior_param, re1_log_sigma[j]);
      PARALLEL_REGION nll -= pen1[j];
    }
    
    Type pen2 = eval_prior_sigma(re2_prior_param, re2_log_sigma);
    Type pen3 = eval_prior_sigma(re3_prior_param, re3_log_sigma);
    
    PARALLEL_REGION nll -= pen2;
    PARALLEL_REGION nll -= pen3;
    
    // loop over entries in covar_subpop_hyperpriors_parameters, evaluate, and subtract that from nll
    for(size_t i = 0; i < covar_subpop_re_matrix.cols(); i++) {
      Type temp_pen = eval_prior_sigma(covar_subpop_hyperpriors_parameters, covar_subpop_log_sigmas[i]);
      PARALLEL_REGION nll -= temp_pen;
    }
    
    PARALLEL_REGION nll -= dnorm(logit_rho_1j, Type(0), Type(1.5), true);
    PARALLEL_REGION nll -= dnorm(logit_rho_1t, Type(0), Type(1.5), true);
    PARALLEL_REGION nll -= dnorm(logit_rho_1a, Type(0), Type(1.5), true);
    PARALLEL_REGION nll -= dnorm(logit_rho_2, Type(0), Type(1.5), true);
    PARALLEL_REGION nll -= dnorm(logit_rho_3t, Type(0), Type(1.5), true);
    PARALLEL_REGION nll -= dnorm(logit_rho_3a, Type(0), Type(1.5), true);
    
    // NLL contribution from data
    // predictions
    vector<Type> log_m = X * B;
    
    for(size_t i = 0; i < Y.size(); i++) {
      
      // add in the covariates w/ random effects
      for (size_t k = 0; k < covar_subpop_re_matrix.cols(); k++) {
        log_m[i] += covar_subpop_re_matrix(R[i], k) * X1(i, k);
      } // end k
      
      log_m[i] += re2[J[i]] + re3(T[i], A[i], R[i]);
      
      for (size_t tp = 0; tp < s_year.cols(); tp++) {
        for (size_t ap = 0; ap < s_age.cols(); ap++) {
          log_m[i] += re1(tp, ap, J[i], R[i]) * s_year(T[i],tp) * s_age(A[i],ap);
        }
      }
    }
    
    
    vector<Type> m = exp(log_m);
    
    
    // data likelihood
    for(size_t i = 0; i < Y.size(); i++)
      PARALLEL_REGION nll -= dpois(Y[i], N[i]*m[i], true);
    
    return nll;
  }
