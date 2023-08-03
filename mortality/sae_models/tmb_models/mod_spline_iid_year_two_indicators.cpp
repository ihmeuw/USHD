// see fit_mod_spline_iid_year_two_indicators.r for model description

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
    DATA_IVECTOR(I_first);    // indicator of if a year is 2001 or not
    DATA_IVECTOR(I_second);    // indicator of if a year is 2001 or not
    
    DATA_SPARSE_MATRIX(graph_j);  // neighborhood structure
    DATA_SPARSE_MATRIX(graph_t); // year matrix
    DATA_SPARSE_MATRIX(graph_a); // age matrix
    DATA_SPARSE_MATRIX(graph_ts); // time spline matrix
    DATA_SPARSE_MATRIX(graph_as); // age spline matrix
    DATA_INTEGER(num_r); // number of race/ethnicity groups
    
    // spline bases
    DATA_MATRIX(s_age);
    DATA_MATRIX(s_year);
    
    // Define parameters
    // fixed effects (intercept, covariate effects)
    PARAMETER_VECTOR(B);
    
    // RE1: area/race/age spline/time spline effect (LCAR:IID:LCAR:LCAR)
    PARAMETER_ARRAY(re1);
    PARAMETER(re1_log_sigma);
    Type sigma_1 = exp(re1_log_sigma);
    PARAMETER(logit_rho_1j);
    Type rho_1j = invlogit(logit_rho_1j);
    // year spline parameters
    PARAMETER(logit_rho_1t);
    Type rho_1t = invlogit(logit_rho_1t);
    // age spline parameters
    PARAMETER(logit_rho_1a);
    Type rho_1a = invlogit(logit_rho_1a);
    DATA_STRUCT(re1_prior_param, prior_type_sigma);
    
    
    // RE2: area-level random intercept (LCAR)
    PARAMETER_VECTOR(re2); 
    PARAMETER(re2_log_sigma);
    Type sigma_2 = exp(re2_log_sigma);
    PARAMETER(logit_rho_2);
    Type rho_2 = invlogit(logit_rho_2);
    DATA_STRUCT(re2_prior_param, prior_type_sigma);
    
    // RE3: age-year-race-level random intercept (LCAR:LCAR:IID)
    PARAMETER_ARRAY(re3);
    PARAMETER(re3_log_sigma);
    Type sigma_3 = exp(re3_log_sigma);
    PARAMETER(logit_rho_3t);
    Type rho_3t = invlogit(logit_rho_3t);
    PARAMETER(logit_rho_3a);
    Type rho_3a = invlogit(logit_rho_3a);
    DATA_STRUCT(re3_prior_param, prior_type_sigma);
    
    // RE4: age/area-level random intercept (IID)
    PARAMETER_VECTOR(re4);
    PARAMETER(re4_log_sigma);
    Type sigma_4 = exp(re4_log_sigma);
    DATA_STRUCT(re4_prior_param, prior_type_sigma);
    
    // RE5: age/area effect
    PARAMETER_VECTOR(re5);
    PARAMETER(re5_log_sigma);
    Type sigma_5 = exp(re5_log_sigma);
    DATA_STRUCT(re5_prior_param, prior_type_sigma);
    
    // age effect
    PARAMETER_VECTOR(re6);
    PARAMETER(re6_log_sigma);
    Type sigma_6 = exp(re6_log_sigma);
    DATA_STRUCT(re6_prior_param, prior_type_sigma);
    
    // another age effect
    PARAMETER_VECTOR(re7);
    PARAMETER(re7_log_sigma);
    Type sigma_7 = exp(re7_log_sigma);
    DATA_STRUCT(re7_prior_param, prior_type_sigma);
    
    
    // NLL contribution from random effects
    Type nll = 0;
    max_parallel_regions = omp_get_max_threads();
    
    // RE1: area/race/age spline/time spline effect random effect
    SparseMatrix<Type> K_1j = lcar_strmat(graph_j, rho_1j);
    SparseMatrix<Type> K_1t = lcar_strmat(graph_ts, rho_1t);
    SparseMatrix<Type> K_1a = lcar_strmat(graph_as, rho_1a);
    
    // Create sparse identity matrix
    matrix<Type> Sigma(num_r,num_r); // cannot use sigma1.size() because sigma 1 here only has one dimension
    Sigma.setIdentity();
    Eigen::SparseMatrix<Type> Sigma_Sparse=asSparseMatrix(Sigma);
    REPORT(Sigma_Sparse);
    
    PARALLEL_REGION nll += SCALE(SEPARABLE(GMRF(Sigma_Sparse), SEPARABLE(GMRF(K_1j), SEPARABLE(GMRF(K_1a), GMRF(K_1t)))),sigma_1)(re1);
    
    
    // RE2: area-level random intercept
    SparseMatrix<Type> K_2 = lcar_strmat(graph_j, rho_2); 
    PARALLEL_REGION nll += SCALE(GMRF(K_2), sigma_2)(re2);
    
    // RE3: age-year-race-level random intercept
    SparseMatrix<Type> K_3t = lcar_strmat(graph_t, rho_3t);
    SparseMatrix<Type> K_3a = lcar_strmat(graph_a, rho_3a);
    
    // identity matrix for race
    matrix<Type> Sigma1(num_r,num_r); // cannot use sigma1.size() because sigma 1 here only has one dimension
    Sigma1.setIdentity();
    Eigen::SparseMatrix<Type> Sigma1_Sparse=asSparseMatrix(Sigma1);
    REPORT(Sigma1_Sparse);
    
    PARALLEL_REGION nll += SCALE(SEPARABLE(GMRF(Sigma1_Sparse), SEPARABLE(GMRF(K_3a), GMRF(K_3t))), sigma_3)(re3);
    
    // Area/age-level IID random intercepts
    PARALLEL_REGION nll -= dnorm(vector<Type>(re4), Type(0), sigma_4, true).sum();
    PARALLEL_REGION nll -= dnorm(vector<Type>(re5), Type(0), sigma_5, true).sum(); 
    
    PARALLEL_REGION nll -= dnorm(vector<Type>(re6), Type(0), sigma_6, true).sum();
    PARALLEL_REGION nll -= dnorm(vector<Type>(re7), Type(0), sigma_7, true).sum(); 
    
    Type pen1 = eval_prior_sigma(re1_prior_param, re1_log_sigma);
    Type pen2 = eval_prior_sigma(re2_prior_param, re2_log_sigma);
    Type pen3 = eval_prior_sigma(re3_prior_param, re3_log_sigma);
    Type pen4 = eval_prior_sigma(re4_prior_param, re4_log_sigma);
    Type pen5 = eval_prior_sigma(re5_prior_param, re5_log_sigma);
    Type pen6 = eval_prior_sigma(re6_prior_param, re6_log_sigma);
    Type pen7 = eval_prior_sigma(re7_prior_param, re7_log_sigma);
    
    PARALLEL_REGION nll -= pen1;
    PARALLEL_REGION nll -= pen2;
    PARALLEL_REGION nll -= pen3;
    PARALLEL_REGION nll -= pen4;
    PARALLEL_REGION nll -= pen5;
    PARALLEL_REGION nll -= pen6;
    PARALLEL_REGION nll -= pen7;
    
    
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
      
      log_m[i] += re2[J[i]] + re3(T[i], A[i], R[i]) + 
        re4[J[i]]*I_first[i] +
        re6[A[i]]*I_first[i]+
        re5[J[i]]*I_second[i] +
        re7[A[i]]*I_second[i];
      
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
