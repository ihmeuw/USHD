// Function for preparing the log density of a prior on the log standard deviation
template<class Type>
Type eval_prior_sigma(prior_type_sigma<Type> prior, Type log_sigma){
  Type penalty;
  // calculate the precision and log precision
  Type tau = pow(exp(log_sigma), -2.);
  Type logtau = log(tau);
  
  // The following documentation is helpful in understanding these derivations: https://becarioprecario.bitbucket.io/inla-gitbook/ch-priors.html#sec:priors
  
  if(prior.name == "pc") {
    
    Type lambda = - log(prior.par2) / prior.par1;
    penalty = -lambda * exp(-logtau/Type(2.0)) - logtau/Type(2.0);
  } 
  else if(prior.name == "half_normal") {
    
    penalty = dnorm(exp(log_sigma), prior.par1, prior.par2, true) + log_sigma;
    
  }
  else if(prior.name == "loggamma") {
    
    penalty = dgamma(tau, prior.par1, prior.par2, true) - 2*log_sigma;
    
  }
  
  return penalty;
}

