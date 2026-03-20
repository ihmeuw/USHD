// see fit_mod_model154.R for model description

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
  DATA_VECTOR(Y);     // counts (Poisson) or cases (binomial)
  DATA_VECTOR(N);     // exposure (Poisson) or population (binomial)
  DATA_IVECTOR(J);    // area indicator
  DATA_IVECTOR(T);    // year indicator
  DATA_IVECTOR(A);    // age indicator
  DATA_IVECTOR(R);    // race indicator
  DATA_IVECTOR(E);    // education indicator
  DATA_IVECTOR(M);    // marital indicator
  DATA_IVECTOR(U);    // post-stratification indicator
  DATA_IVECTOR(D_V2);    // data source indicator
  DATA_IVECTOR(D_V2_no_gs);    // data source indicator excluding gold-standard
  DATA_IVECTOR(DR_V2);    // data source-race indicator
  DATA_MATRIX(X);     // covariates (at minimum, a column for the intercept)
  DATA_MATRIX(X1);     // covariates (at minimum, a column for the intercept)
  
  DATA_IVECTOR(GOLD_STANDARD);    // Gold-standard source indicator
  
  DATA_SPARSE_MATRIX(graph_j);  // neighborhood structure
  DATA_SPARSE_MATRIX(graph_a);
  DATA_SPARSE_MATRIX(graph_e);
  DATA_SPARSE_MATRIX(graph_as); // LCAR graph for linear age spline
  DATA_SPARSE_MATRIX(graph_ts); // LCAR graph for linear time spline
  // don't need LCAR graph for the natural spline b/c using an IID effect

  DATA_INTEGER(num_r);
  DATA_INTEGER(num_e);
  DATA_INTEGER(num_m);
  DATA_INTEGER(num_d_v2);
  DATA_INTEGER(num_dr_v2);
  DATA_INTEGER(num_year_spline_ns);
  DATA_INTEGER(num_age_spline_ns);
  
  // spline bases
  DATA_MATRIX(s_age);
  DATA_MATRIX(s_year);
  DATA_MATRIX(s_year_ns);
  DATA_MATRIX(s_age_ns);

  // Define data and inputs for state-mcnty observations
  DATA_VECTOR(Y_k);     // counts (Poisson) or cases (binomial)
  DATA_VECTOR(N_k);     // exposure (Poisson) or population (binomial)
  DATA_IVECTOR(J_k);    // area indicator
  DATA_IVECTOR(T_k);    // year indicator
  DATA_IVECTOR(A_k);    // age indicator
  DATA_IVECTOR(R_k);    // race indicator
  DATA_IVECTOR(E_k);    // education indicator
  DATA_IVECTOR(M_k);    // marital indicator
  DATA_IVECTOR(U_k);    // post-stratification indicator
  DATA_IVECTOR(D_V2_k);    // data source indicator
  DATA_IVECTOR(D_V2_no_gs_k);    // data source indicator excluding gold-standard
  DATA_IVECTOR(DR_V2_k);    // data source-race indicator
  DATA_MATRIX(X_k);     // covariates (at minimum, a column for the intercept)
  DATA_MATRIX(X1_k);     // covariates (at minimum, a column for the intercept)
  DATA_IVECTOR(dt_agg_rows_k);    // agg indicator
  DATA_VECTOR(agg_wt_xwalk);    // aggregation weights indicator
  
  DATA_IVECTOR(GOLD_STANDARD_k);    // Gold-standard source indicator
  
  // Define parameters
  // fixed effects (intercept, covariate effects)
  PARAMETER_VECTOR(B);
  PARAMETER_VECTOR(B1);
  
  // RE1: area/race/age spline effect -- linear spline basis
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
  
  // RE2: area-level random intercept
  PARAMETER_VECTOR(re2); 
  PARAMETER(re2_log_sigma);
  Type sigma_2 = exp(re2_log_sigma);
  PARAMETER(logit_rho_2);
  Type rho_2 = invlogit(logit_rho_2);
  DATA_STRUCT(re2_prior_param, prior_type_sigma);
  
  // RE3: age-race-edu-marital-level random intercept (LCAR:IID:LCAR:IID:IID)
  PARAMETER_ARRAY(re3);
  PARAMETER(re3_log_sigma);
  Type sigma_3 = exp(re3_log_sigma);
  PARAMETER(logit_rho_3a);
  Type rho_3a = invlogit(logit_rho_3a);
  PARAMETER(logit_rho_3e);
  Type rho_3e = invlogit(logit_rho_3e);
  DATA_STRUCT(re3_prior_param, prior_type_sigma);
    
  // RE5: race-source-level random intercept
  PARAMETER_VECTOR(re5);
  PARAMETER(re5_log_sigma);
  Type sigma_5 = exp(re5_log_sigma);
  DATA_STRUCT(re5_prior_param, prior_type_sigma);

  // RE8: age-source-level random intercept
  PARAMETER_ARRAY(re8);
  PARAMETER(re8_log_sigma);
  Type sigma_8 = exp(re8_log_sigma);
  PARAMETER(logit_rho_8a);
  Type rho_8a = invlogit(logit_rho_8a);
  DATA_STRUCT(re8_prior_param, prior_type_sigma);
  
  // B6: year spline
  PARAMETER_VECTOR(B6);

  // B7: age spline
  PARAMETER_VECTOR(B7);
  
  // NLL contribution from random effects
  Type nll = 0;
  max_parallel_regions = omp_get_max_threads();
  
  // RE1: area/race/age spline random effect
  SparseMatrix<Type> K_1j = lcar_strmat(graph_j, rho_1j);
  SparseMatrix<Type> K_1t = lcar_strmat(graph_ts, rho_1t);
  SparseMatrix<Type> K_1a = lcar_strmat(graph_as, rho_1a);
  
  // Create sparse identity matrix
  matrix<Type> Sigma(num_r, num_r); // cannot use sigma1.size() because sigma 1 here only has one dimension
  Sigma.setIdentity();
  Eigen::SparseMatrix<Type> Sigma_Sparse=asSparseMatrix(Sigma);
  REPORT(Sigma_Sparse);
  
  // race(area(age_spline,year_spline))
  PARALLEL_REGION nll += SCALE(SEPARABLE(GMRF(Sigma_Sparse), SEPARABLE(GMRF(K_1j), SEPARABLE(GMRF(K_1a), GMRF(K_1t)))), sigma_1)(re1);
    
  // RE2: area-level random intercept
  SparseMatrix<Type> K_2 = lcar_strmat(graph_j, rho_2); 
  PARALLEL_REGION nll += SCALE(GMRF(K_2), sigma_2)(re2);
  
  // RE3: year-age-race-edu-marital-level random intercept
  SparseMatrix<Type> K_3a = lcar_strmat(graph_a, rho_3a);
  SparseMatrix<Type> K_3e = lcar_strmat(graph_e, rho_3e);
  
  // identity matrix for race
  matrix<Type> Sigma1(num_r, num_r); // cannot use sigma1.size() because sigma 1 here only has one dimension
  Sigma1.setIdentity();
  Eigen::SparseMatrix<Type> Sigma1_Sparse=asSparseMatrix(Sigma1);
  REPORT(Sigma1_Sparse);
  
  // Create sparse identity matrix for IID marital status effects
  matrix<Type> Sigma_m(num_m, num_m);
  Sigma_m.setIdentity();
  Eigen::SparseMatrix<Type> Sigma_Sparse_m = asSparseMatrix(Sigma_m);
  REPORT(Sigma_Sparse_m);
  
  PARALLEL_REGION nll += SCALE(SEPARABLE(GMRF(Sigma_Sparse_m), SEPARABLE(GMRF(K_3e), SEPARABLE(GMRF(Sigma1_Sparse), GMRF(K_3a)))), sigma_3)(re3);
   
  // RE5: race-data random intercept
  // Create sparse identity matrix for source-race 1D vector
  matrix<Type> Sigma5(num_dr_v2, num_dr_v2);
  Sigma5.setIdentity();
  Eigen::SparseMatrix<Type> Sigma5_Sparse=asSparseMatrix(Sigma5);
  REPORT(Sigma5_Sparse);

  PARALLEL_REGION nll += SCALE(GMRF(Sigma5_Sparse), sigma_5)(re5);
  
  // RE8: age-source random intercept
  // Sparse identify matrix for sources that are not gold-standard (i.e., dimensions
  // num_d_v2 - 1 because we don't have a RE8 for the gold-standard source)
  matrix<Type> Sigma8(num_d_v2 - 1, num_d_v2 - 1);
  Sigma8.setIdentity();
  Eigen::SparseMatrix<Type> Sigma8_Sparse=asSparseMatrix(Sigma8);
  REPORT(Sigma8_Sparse);
  // Sparse matrix for the LCAR graph for age
  SparseMatrix<Type> K_8a = lcar_strmat(graph_a, rho_8a);
  PARALLEL_REGION nll += SCALE(SEPARABLE(GMRF(Sigma8_Sparse), GMRF(K_8a)), sigma_8)(re8);

  Type pen1 = eval_prior_sigma(re1_prior_param, re1_log_sigma);
  Type pen2 = eval_prior_sigma(re2_prior_param, re2_log_sigma);
  Type pen3 = eval_prior_sigma(re3_prior_param, re3_log_sigma);
  Type pen5 = eval_prior_sigma(re5_prior_param, re5_log_sigma);
  Type pen8 = eval_prior_sigma(re8_prior_param, re8_log_sigma);
    
  PARALLEL_REGION nll -= pen1;
  PARALLEL_REGION nll -= pen2;
  PARALLEL_REGION nll -= pen3;
  PARALLEL_REGION nll -= pen5;
  PARALLEL_REGION nll -= pen8;
    
  REPORT(pen1);
  REPORT(pen2);
  REPORT(pen3);
  REPORT(pen5);
  REPORT(pen8);
    
  PARALLEL_REGION nll -= dnorm(logit_rho_1j, Type(0), Type(1.5), true);
  PARALLEL_REGION nll -= dnorm(logit_rho_1t, Type(0), Type(1.5), true);
  PARALLEL_REGION nll -= dnorm(logit_rho_1a, Type(0), Type(1.5), true);
  PARALLEL_REGION nll -= dnorm(logit_rho_2, Type(0), Type(1.5), true);
  PARALLEL_REGION nll -= dnorm(logit_rho_3a, Type(0), Type(1.5), true);
  PARALLEL_REGION nll -= dnorm(logit_rho_3e, Type(0), Type(1.5), true);
  PARALLEL_REGION nll -= dnorm(logit_rho_8a, Type(0), Type(1.5), true);
    
  if (family == "b") {
    // NLL contribution from data
    // predictions
    vector<Type> logit_p = X * B;
    for(size_t i = 0; i < Y.size(); i++) {
      logit_p[i] += re2[J[i]];
      
      if (U[i] == 1) {
        logit_p[i] += re3(A[i], R[i], E[i], M[i]);
      } 

      if(GOLD_STANDARD[i] == 0){ // don't have RE5 & RE8 for gold-standard
        logit_p[i] += re5(DR_V2[i]) + re8(A[i], D_V2_no_gs[i]); // index RE8 by the version of source_v2 that excludes the gold-standard
      }
      
      for (size_t tp = 0; tp < s_year.cols(); tp++) {
        for (size_t ap = 0; ap < s_age.cols(); ap++) {
          logit_p[i] += re1(tp, ap, J[i], R[i]) * s_year(T[i], tp) * s_age(A[i], ap);
        }
      }

      // add year spline (B6)
      for (size_t tp = 0; tp < B6.size(); tp++) {
        logit_p[i] += B6[tp] * s_year_ns(T[i], tp);
      }

      // add age spline (B7)
      for (size_t ap = 0; ap < B7.size(); ap++) {
        logit_p[i] += B7[ap] * s_age_ns(A[i], ap);
      }
      
      // add in the covariates
      for (size_t f = 0; f < B1.size(); f++) {
        logit_p[i] += B1[f] * X1(i, f); // add the fth covariate-RE product
      }

    }
        
    vector<Type> p = invlogit(logit_p);
    
    REPORT(p);
    
    // // data likelihood
    for (size_t i = 0; i < Y.size(); i++) {
      if (!isNA(Y[i])) { // If the outcome is not NA
        PARALLEL_REGION nll -= dbinom(Y[i], N[i], p[i], true);
      }
    }
    
    Type prob_k = 0;
    vector<Type> logit_p_k = X_k * B;
    for (size_t k = 0; k < Y_k.size(); k++) {
      logit_p_k[k] += re2[J_k[k]];
      if (U_k[k] == 1) {
        logit_p_k[k] += re3(A_k[k], R_k[k], E_k[k], M_k[k]);
      }
      if(GOLD_STANDARD_k[k] == 0){ // don't have RE5 and RE8 for gold-standard
        logit_p_k[k] += re5(DR_V2_k[k]) + re8(A_k[k], D_V2_no_gs_k[k]); 
      }
      
      for (size_t tp = 0; tp < s_year.cols(); tp++) {
        for (size_t ap = 0; ap < s_age.cols(); ap++) {
          logit_p_k[k] += re1(tp, ap, J_k[k], R_k[k]) * s_year(T_k[k], tp) * s_age(A_k[k], ap);
        }
      }

      // add year spline (B6)
      for (size_t tp = 0; tp < B6.size(); tp++) {
        logit_p_k[k] += B6[tp] * s_year_ns(T_k[k], tp);
      }

      // add age spline (B7)
      for (size_t ap = 0; ap < B7.size(); ap++) {
        logit_p_k[k] += B7[ap] * s_age_ns(A_k[k], ap);
      }
      
      // add in the covariates
      for (size_t f = 0; f < B1.size(); f++) {
        logit_p_k[k] += B1[f] * X1_k(k, f); // add the fth covariate-RE product
      }

      prob_k += invlogit(logit_p_k[k]) * agg_wt_xwalk[k];
      
      if (k != Y_k.size() - 1) {
        if(dt_agg_rows_k[k] != dt_agg_rows_k[k + 1]) {
          if (!isNA(Y_k[k])) { // If the outcome is not NA
            PARALLEL_REGION nll -= dbinom(Y_k[k], N_k[k], prob_k, true);
          }
          prob_k = 0;
        }
      } else { // at the end of all IDS/observations
        if (!isNA(Y_k[k])) { // If the outcome is not NA
          PARALLEL_REGION nll -= dbinom(Y_k[k], N_k[k], prob_k, true);
        }
        prob_k = 0;
      }
    }
    
    REPORT(logit_p_k);
    
    return nll;
  } else {
    // give error if family is not "b"
    error("Family must be 'b' (binomial)");
  }
}
