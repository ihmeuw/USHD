####################################################################################################
## Description: This script is sourced by risk_factors/2_exposure/high_bmi/correction_models/model_based_cw/INLA_based_bmi_correction_binned_quantiles.R
##  to load the INLA formulas for the models we're fitting. Will create an object called
##  formula which is a list of formulas
##         
####################################################################################################

formulas <- list(
  form1 = log_ratio ~
          1 + # check for warning about multiple global intercepts
          f(age20, model = 'iid', constr = FALSE) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code))) + # replicate means that race has separate hyperparameters here and in main effect
          # can copy the column/replicate so that there are separate hyperparameters on the race effect for the main effects & in interaction -- using the replicate arg in f()
            # if doing age replicated by race, we'd need a copy of the age column, but not the race column (check)
          # if we want races to inform each other across terms, use group -- then they share hyperparaemters -- using group arg in f()
          # # add a smooth effect of bmi_report_percentile_bin (natural spline)
          # splines::ns(bmi_report_percentile_bin, knots = bmi_report_percentile_bin_knots)
          # Info on specifications of RW2: https://inla.r-inla-download.org/r-inla.org/doc/latent/rw2.pdf
          # see https://becarioprecario.bitbucket.io/inla-gitbook/ch-priors.html#sec:pcpriorson setting pc priors
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          ns(year_center, df = 4, intercept = FALSE, Boundary.knots = range(year_values_index)), # natural spline with 4 degrees of freedom. Intentionally using only two internal knots b/c I expect SR bias to change slowly over time
  form2 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code))) + # replicate means that race has separate hyperparameters here and in main effect
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          f(year_center, 
            model = 'rw2', 
            constr = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))), # this means P(sigma > 0.1) = 0.001. This is forcing the variance to be relatively small, i.e., steps between levels are small
  form3 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code))) + # replicate means that race has separate hyperparameters here and in main effect
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          f(decade, model = "iid", constr = FALSE),
  # use age10 instead of age20. Use the continuous time effect
  form4 = log_ratio ~
          1 + 
          f(age10, model = 'rw2', constr = FALSE, scale.model = TRUE, hyper = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(age10_2, model = "rw2", replicate = as.integer(as.factor(race_eth_code))) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          f(year_center, 
            model = 'rw2', 
            constr = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))), # this means P(sigma > 0.1) = 0.001. This is forcing the variance to be relatively small, i.e., steps between levels are small
  # use age10. All add a source_v2 effect (source_v2 --> splits BRFSS based on change in survey)
  form5 = log_ratio ~
          1 + 
          f(age10, model = 'rw2', constr = FALSE, scale.model = TRUE, hyper = list(prec = list(prior = "pc.prec", param = c(5, 0.001)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(age10_2, model = "iid", replicate = as.integer(as.factor(race_eth_code))) +
          f(source_v2, model = "iid", constr = TRUE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          f(year_center, 
            model = 'rw2', 
            constr = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) ,# this means P(sigma > 0.5) = 0.001. This is forcing the variance to be relatively small, i.e., steps between levels are small
  # form6 is like form2, but I've added a source_v2 effect
  form6 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code))) + # replicate means that race has separate hyperparameters here and in main effect
          f(source_v2, model = "iid", constr = TRUE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          f(year_center, 
            model = 'rw2', 
            constr = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))),
  # form7 is like form1, but I use a linear B-spline for year_center instead of a natural spline
  form7 = log_ratio ~
          1 +
          f(age20, model = 'iid', constr = FALSE) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code))) + 
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          bs(year_center, df = 4, degree = 1, intercept = FALSE, Boundary.knots = range(year_values_index)),
  # form5 without the age*race interactions
  form8 = log_ratio ~
          1 + 
          f(age10, model = 'rw2', constr = FALSE, scale.model = TRUE, hyper = list(prec = list(prior = "pc.prec", param = c(5, 0.001)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE) +
          f(source_v2, model = "iid", constr = TRUE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))) + 
          f(year_center, 
            model = 'rw2', 
            constr = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))),
  # form 2 without the age*race interaction. Make age and race fixed effects rather than random.
  # Use weaker priors
  form9 = log_ratio ~
          1 + 
          age20 + race_eth_code +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.01)))) +
          f(year_center, 
            model = 'rw2', 
            constr = TRUE,
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))),
   # model with age20, source, and race as fixed effects.  Use RW2 on year and bmi_report_percentile_bin
   form10 = log_ratio ~
          1 + 
          age20 + race_eth_code + source_v2 +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01)))) +
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))),#c(0.5, 0.001)))),
  # Similar to model 10 (in that uses mostly fixed effects. Except use splines on year 
  # and bmi_report_percentile_bin for additional smoothing. Use num_quantiles/3.5 degrees 
  # of freedom for bmi_report_percentile_bin, and year_values/3 degrees of freedom for year_center
  # NOTE -- formula 11 does work b/c INLA has issues with names when we use two ns() terms
  # Circumvent the issue using RW2 https://groups.google.com/g/r-inla-discussion-group/c/ur2t7wNz5tQ
  form11 = log_ratio ~
          1 + 
          age20 + race_eth_code + source_v2 +
          ns(bmi_report_percentile_bin, df = bmi_report_percentile_bin_df, intercept = FALSE, Boundary.knots = bmi_report_percentile_bin_boundary) +
          ns(year_center, df = year_center_df, intercept = FALSE, Boundary.knots = year_center_boundary),
  # Form1 2 is based on model 10, but use continuous random walk instead of descrete random walk
  form12 = log_ratio ~
          1 + 
          age20 + race_eth_code + source_v2 +
          f(bmi_report_percentile_bin, 
            model = "crw2", 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01)))) +
          f(year_center, 
            model = 'crw2', 
            values = as.integer(year_values_index), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  # based on form10, but adds age*race interaction and source*race interactions (as random effects)
  form13 = log_ratio ~
          1 + 
          age20 + race_eth_code + source_v2 +
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01)))) + 
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  # model 14 is like model 13, but make the main effects random
  form14 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.05)))) + 
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))),
  # form 15 is like model 14, except remove source terms
  form15 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
           f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01)))) + 
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  # form16 like form 14, but remove the age*race interaction (hoping to 
  # increase identifiability of the source*race term, and increase smoothness of
  # the time term)
  # Additionally, make the prior on the time & quantile terms stronger (i.e., for a 
  # smaller variance)
  form16 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.001)))) + 
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))),
  # form 17 = form14 without the time term
  form17 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) + 
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  # model 18, based on form 14, but add an age20 x year interaction
  # Drop main effects on year and age20
  form18 = log_ratio ~
          1 + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.05)))) +
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            replicate = as.integer(as.factor(age20)),
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))),
  # form 19 -- remove the race main effect. Add an interaction between
  # race and bmi_quantile_bin
  form19 = log_ratio ~
          1 + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.05)))) +
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            replicate = as.integer(as.factor(age20)),
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))),
  # form 20 is like form 19, but remove source*race interaction
  form20 = log_ratio ~
          1 + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.05)))) +
          # year x age20
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            replicate = as.integer(as.factor(age20)),
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))),
  # form 21 based on models 19 and 20. source + age*race + source*race + bmi_percenitle_bin + year*race.
  # Trying to capture race-specific time trends better.
  form21 = log_ratio ~
          1 + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.05)))) +
          # year x age20
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))),
  # form22 is model 21, but remove the source*race interaction, which is not very
  # identifiable given that we have race*time trends.
  form22 = log_ratio ~
          1 + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.05)))) + 
          # year x age20
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))),
  # form 23 is like 21 and 22, but also add a main effect on year. Tighen prior
  # on year*race interaction
  form23 = log_ratio ~
          1 + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(1,0.05)))) + 
          # year
          f(year_center, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))) +
          # year x age20
          f(year2, 
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.005)))),
  # form 24 is based on form 17, but add a decade term (fixed effect)
  form24 = log_ratio ~
          1 +
          decade +
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  # form 25 is like form 24, but use a linear spline on year center (with 3 DF)
  # instead of a decade effect to avoid discontinuity in the correction
  form25 = log_ratio ~
        1 +
        bs(year_center, df = 3, degree = 1, intercept = FALSE, Boundary.knots = range(year_values_index)) +  # not that rows don't sum to 1 in this basis in all cases. That's intentional b/c we don't want an intercept in this spline basis. We would want an intercept if it's interacted with something
        f(age20, model = 'iid', constr = FALSE, 
          hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
        f(race_eth_code, model = 'iid', constr = FALSE, 
          hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
        f(source_v2, model = 'iid', constr = FALSE, 
          hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
        f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
          hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
          constr = FALSE) +
        # source_v2 * race
        f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
          hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
          constr = FALSE) +
        f(bmi_report_percentile_bin, 
          model = "rw2", 
          scale.model = TRUE,
          values = sort(as.integer(bmi_report_percentile_bin_vals)), 
          hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  # form 26 is like 24 and 25, but include a linear effect on year (fixed effect) 
  # and a RW2 effect on year -- so we get an overall time trend, plus some flexability
  form26 = log_ratio ~
          1 +
          # linear effect on time
          year_center + 
          # year-to-year variation
          f(year2,  
            model = 'rw2', 
            scale.model = TRUE,
            values = sort(as.integer(year_values_index)), # define year values b/c the years are not equally spaced
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.005)))) + # set a strong prior (little variation)
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
    # form 27 based on form 24, but use a natural spline on year (but set the boundary
    # knots to be 2002 & 2016 to avoid fitting spline to more problematic NHANES
    # in beginning/end years
    form27 = log_ratio ~
          1 +
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))) +
          ns(year_center, df = time_spline_df, intercept = FALSE, Boundary.knots = year_boundaries_restricted), # natural spline with 3 degrees of freedom. Use restricted boundary knots to force trends before/after 2002 and 2016, respectively, to be linear
    # model 28 is like model 24, but remove the source effects
    form28 = log_ratio ~
          1 +
          decade +
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  # model 29 is model 24, but use 10-year age groups instead of 20-year age groups,
  #  and use a continuous random walk on the BMI percentile bin. Set RW2 on age now that
  #   we use 10-year age groups
  form29 = log_ratio ~
          1 +
          decade +
          f(age10, model = 'rw2', constr = FALSE, 
            values = c("20", "25_34", "35_44", "45_54", "55_64", "65_74", "75+"),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(quantile_scaled, 
            model = "crw2", 
            values = cont_quantile_vals,
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  # model 30 is model 24, but using 10 year age groups
  form30 = log_ratio ~
          1 +
          decade +
          f(age10, model = 'rw2', constr = FALSE, 
            values = c("20", "25_34", "35_44", "45_54", "55_64", "65_74", "75+"),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(source_v2_copy2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  # just like model 17, but flip order of source*race interaction, and constrain to sume to zero
  form31 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(race_eth_code_copy, model = "iid", replicate = as.integer(as.factor(source_v2)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = TRUE) + 
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
# form 31, but remove source*race interaction
form32 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  # model 31, but add a age20*bmi_report_percentile_bin interaction
  form34 = log_ratio ~
          1 + 
          f(age20, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age20_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(race_eth_code_copy, model = "iid", replicate = as.integer(as.factor(source_v2)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = TRUE) + 
          # interaction between age20 and bmi_report_percentile_bin
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            replicate = as.integer(as.factor(age20)),
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
   # model 34, but using age 10 instead of age 20
   form35 = log_ratio ~
          1 + 
          f(age10, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age10_2, model = "iid", replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = FALSE) +
          # source_v2 * race
          f(race_eth_code_copy, model = "iid", replicate = as.integer(as.factor(source_v2)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = TRUE) + 
          # interaction between age10 and bmi_report_percentile_bin
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            replicate = as.integer(as.factor(age10)),
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05)))),
  form36 = log_ratio ~
          1 + 
          f(age10, model = 'rw1', scale.model = TRUE, constr = TRUE,  
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(race_eth_code, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(source_v2, model = 'iid', constr = FALSE, 
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) + 
          f(age10_2, model = "iid", 
            replicate = as.integer(as.factor(race_eth_code)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05)))) +
          # source_v2 * race
          f(race_eth_code_copy, model = "iid", replicate = as.integer(as.factor(source_v2)),
            hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
            constr = TRUE) + 
          # interaction between age10 and bmi_report_percentile_bin
          f(bmi_report_percentile_bin, 
            model = "rw2", 
            scale.model = TRUE,
            values = sort(as.integer(bmi_report_percentile_bin_vals)), 
            replicate = as.integer(as.factor(age10)),
            hyper = list(prec = list(prior = "pc.prec", param = c(0.5,0.05))))
  
  
)