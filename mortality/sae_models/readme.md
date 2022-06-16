## Settings

The code in this folder is intended to be generic; different datasets and model options are provided via a settings .csv file called "settings.csv" or via .yaml file called "settings.yaml". This settings file should contain the following arguments:


| Argument | Type | Description | Required/Optional |
| :------ | :------ | :--------------------------------------------------------------| :-------------------- |
| `model_class` | String | The "type" of model to run. The is a bit of a misnomer, since it's really the program used to the fit the model rather than the model class. In any case, the only current option is "TMB". | Required |
| `model` | String | The SAE model to run. This matches the code in the `models` folder. | Required |
| `area_var` | String | The name of the variable in all input files denoting the areas to be modeled. | Required |
| `years` | Integer vector | The range of years to model. | Required |
| `ages` | Integer vector | The age groups to model. | Required |
| `sexes` | Integer vector | The sexes to model. | Required |
| `by_race` | Logical | Indicates whether or not race-specific models are to be run. | Optional, defaults to `FALSE` |
| `races` | Integer vector | The races to model. | Required if `by_races == T` |
| `race_labels` | String vector | The labels corresponding to the groups in `races`. | Optional, must be same length as `races` and the order must correspond to the order of values in `races`|
| `by_edu` | Logical | Indicates whether or not education-specific models are to be run. Analogous to `by_race`. | Optional, defaults to `FALSE` |
| `edu_groups` | Integer vector | The educational attainment groups to model. Analogous to `races`. | Required if `by_edu == T` |
| `edu_labels` | String vector | The labels corresponding to the education groups in `edu_groups`. Analogous to `race_labels` | Optional, must be same length as `edu_groups` and the order must correspond to the order of values in `edu_groups`
| `covars` | String vector | Area- and year-level covariates to include in the model. | Optional |
| `covars_as` | String vector | Area-, year-, age-, and sex-level covariates to include in the model. | Optional |
| `covars_subpop` | String vector | Area-, year-, race- and sex-level covariates to include in the model (even if it is not sex specific, the data must be replicated for each sex). | Optional |
| `covars_trans` | String vector (named) | Transformations to apply to the covariates in `covars` and/or `covars_as`. | Optional |
| `n.sims` | Integer | The number of posterior draws to generate and use. | Required |
| `adjmat_file` | String | File path to the adjacency matrix. | Required |
| `deaths_file` | String | File path to the deaths data. | Required |
| `pop_file` | String | File path to the population data. | Required |
| `covar_file` | String | File path to the area- and year-level covariates data. | Required if `covars` is specified |
| `covar_as_file` | String | File path to the area-, year-, age-, and sex-level covariates data. | Required if `covars_as` is specified |
| `covar_subpop_file` | String | File path to the area-, year-, subpop-level covariates data. Subpop can be either race/ethnicity of education. | Required if `covars_subpop` is specified |
| `geoagg_files` | String vector (named) | File path(s) to geographic crosswalk files relating the modeled areas to geographic aggregates. | Optional |
| `shape_file` | String | File path to the area-level shape file. | Required |
| `age_std_file` | String | File path to the weights used for generating age-standardized estimates. | Required |
| `ref_lt_file` | String | File path to the reference life table to be used for estimating YLLs. | Required |
| `fixed_age_time` | Logical | Is a fixed (pre-specified) age-time effect being utilized? | Optional, defaults to `FALSE` |
| `ref_dir` | String | File path to the directory containing the reference results used for a fixed age-time effect. | Required if `ref_dir == T` |
| `ref_raked` | Logical | Are the reference results used for a fixed age-time effect raked? | Required if `ref_dir == T` |
| `ref_level` | String | What is the geographic level being used for reference for a fixed age-time effect? | Required if `ref_dir == T` |
| `val_sizes` | Integer vector | Population sizes to test during model validation. | Required to run validation | 
| `val_iter` | Integer | Number of iterations to repeat the validation exercise at each population size. | Required to run validation | 
| `val_types` | String | String vector of the types of validation sets. Probably should be c("sampled_down_val_set_only", "sampled_down_all"). | Required to run validation. | 
| `val_dir` | String | Directory where validation input datasets are stored. | Required to run validation | 
| `gs_file` | String | File path to the gold standard dataset. | Required to run validation | 
| `rake_to_gbd` | String | Should estimates be raked to GBD estimates acquired via central functions? | Required to run raking |
| `raking_area_var` | String | Geographic level to use for raking. | Required to run raking |
| `rake_to_gbd_version` | String vector (named) | Arguments to be used with `get_outputs()` and `get_draws()` for pulling estimates when raking to GBD. | Required to run raking, if `rake_to_gbd == T` |
| `raking_draws_dir` | String | Directory where `raking_area_var`-level draws are stored, to be used for raking. | Required to run raking, if `rake_to_gbd == F` |
| `raking_est_dir` | String | Directory where `raking_area_var`-level estimates are stored, to be used for raking. | Required to run raking, if `rake_to_gbd == F` |
| `prior_type` | String | Determines the prior type to be used on the standard deviation terms (either pc (penalized complexity), loggamma, or half_normal) | Optional, defaults to loggamma with a shape = 1, scale = 1000, and starting log standard deviation = -3 |
| `prior_list` | String vector | Parameters for the prior. If loggamma, the order is shape, scale, and starting log standard deviation. If pc, the order is sigma_0, alpha, and starting log standard deviation. If half normal, the order is mean (must be 0), SD (of distribution), and starting log standard deviation | Required if `prior_type` is specified |
| `race_togther` | Boolean | If the model fits all race groups together instead of fitting each of them separately. If not, in `pred_mx` the data will try to subset to race 99 | Optional, defaults to False |
| `edu_together` | Boolean | If the model fits all edu groups together instead of fitting each of them separately. Analogous to `race_together`| Optional, defaults to False |
| `misclassification_correction` | Boolean | If you want to adjust mx using misclassification ratios | Optional, defaults to False, always set to False when `by_race` = False |
| `age_knots_spec` | Integer vector | Age knots for spline model | Optional |
| `year_knots_num` | Integer | Number of year knots (they will be evenly spaced) for spline model | Optional |
| `covar_subpop_hyperpriors_settings` | String vector | Like `prior_list`, the hyperprior settings for random effects on race/ethnicity that are multiplied by the race/ethnicity-specific covariates. | Optional |



An example:

```
model_class,tmb_models
model,"spline_iid_race_all_one_variance"
area_var,mcnty
years,2015:2017
ages,"c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)"
sexes,"c(1, 2)"
by_race,TRUE
races,1:7
race_labels,"c(""NH White"", ""NH Black"", ""NH AIAN"", ""NH API"", ""NH Multiracial"", ""Hispanic"")"
covars,"c(""edu_hs"", ""income_median"", ""pop_density"")"
covars_trans,"c(income_median = ""log(x)"", pop_density = ""log(x)"")"
n.sims,1000
adjmat_file,"FILEPATH"
deaths_file,"FILEPATH"
pop_file,"FILEPATH"
covar_file,"FILEPATH"
geoagg_files,"c(natl = ""FILEPATH"", state = ""FILEPATH"")"
shape_file,"FILEPATH"
age_std_file,"FILEPATH"
ref_lt_file,"FILEPATH"
raking_area_var,"natl"
rake_to_gbd,TRUE
rake_to_gbd_version,"list(status = ""best"", gbd_round_id = 7, decomp_step = ""None"")"
```

## Code overview

All code listed below is in the `sae_models` folder in the mortality repo. More detailed documentation is available in the header for each script.

### Submission scripts:
  * `_submit_single_cause.r` -- submit all jobs required to fit models and generate predictions for a single cause.
  * `_submit_all_causes.r` -- submit all jobs required to fit models and generate predictions for all causes.
  * `_validate_lt.r` -- submit all jobs required to fit validation models and summarize model performance.
  * `raking/_run_all_raking_flexible.r` -- submit all jobs required to rake modeled estimates.

### Model code:
  * `models/prep_inputs.r` -- load, merge, and format the inputs files in preparation for modeling.
  * `tmb_models/fit_mod_#.r` (and `models/mod_#.cpp`) -- fit the SAE model. The `.r` files are run directly and call on the corresponding `.cpp` files.
  * `tmb_models/lcar_strmat.hpp` -- function for generating a LCAR structure matrix, used in `tmb_models/mod_#.cpp`
  * `tmb_models/prior_param_struct.hpp` -- create a structure in TMB for the various priors needed for the variance parameters.
  * `tmb_models/prior_penalty.hpp` -- calculate the contribution from the hyperpriors on the variance parameters
  * `tmb_models/pred_mx.r` -- generate posterior draws.
  * `tmb_models/complete_pred_mx_subdraw.r` -- combine draws of random and fixed effects.
  * `tmb_models/save_mx_pred.r` -- calculate summary estimates at the area-year-sex-race-age level.

### Functions:
  * `run_submission_scripts.r` -- convenience function for running any of the submission scripts.
  * `settings.r` -- functions for loading and checking settings.
  * `wait_for_job_ids.r` -- function to wait for other jobs to finish.
  * `prep_pred_mx_array_jobs.r` -- submit jobs for `complete_pred_mx_subdraw.r` and `save_mx_pred.r`
  * `read_misclassification_draws.r` -- copy misclassification draws into the model directory
  * `modeling_helpers.r` -- functions used across multiple modeling scripts
  * `load_sae_shared.r` -- load the sae.shared repo in order to get access to those functions
  * `add_gbd_ids.r` -- adds various GBD IDs for use in raking or comparison to GBD results
  * `delete_final_output.r` -- delete final files when resubmitting code
