####################################################################################################
## Description: Define a function for extracting model parameters
##
## Arguments:   model [character] -- file path to the saved model object (output from sdreport(),
##                expected to be named "out")
##              fixed_age_time [logical] -- does this model use a fixed age-time effect?
##
## Output:      a data.table with four columns:
##              - param: parameter name
##              - part:  part of the model ("fixed" or "random")
##              - est:   point estimate
##              - se:    standard error
####################################################################################################

get_params <- function(model, fixed_age_time = F) {

  # load the model
  out <- readRDS(model)

  # pull out the point estimates and SEs for the fixed effects
  par.fixed <- out$par.fixed
  se.fixed <- sqrt(diag(out$cov.fixed))

  # pull out the point estimates and SEs for the random effects
  par.random <- out$par.random
  se.random <- sqrt(out$diag.cov.random)

  # if using a fixed age-time effect, add in the fixed estimates for the intercept and age-time random effect
  if (fixed_age_time) {
    # add intercept
    B0 <- attr(out$env$parameters$B, "shape")[1]
    par.random <- c(B = B0, par.random)
    se.random <- c(0, se.random)

    # add re1
    re1 <- as.numeric(attr(out$env$parameters$re1, "shape"))
    names(re1) <- rep("re1", length(re1))
    par.random <- c(par.random, re1)
    se.random <- c(se.random, rep(0, length(re1)))
  }

  # combine all elements into a data.table and return
  data.table(id = 1:length(c(par.fixed, par.random)),
             param = c(names(par.fixed), names(par.random)),
             part = c(rep("fixed", length(par.fixed)), rep("random", length(par.random))),
             est = c(par.fixed, par.random),
             se = c(se.fixed, se.random))
}
