##### STANDARD DEVIATION OPTIMIZATION


# STEP 1: Standard Deviation Optimization

### Thought Process
- From the SAE models, we have the prevalences of underweight, overweight, and obesity. We subsequently estimate mean BMI for each strata. Lastly, we have the characteristic shape of population distribution of BMI (ensemble weights). 
- The fundamental pieces you need to paramaterize a probability distribution are its: family (in our case a weighted ensemble of families), mean, and SD. From the definition of the distribution and it's mean and variance (first and second moments), we can solve for the shape and scale parameters with varying levels of difficulty.
- The only piece we're missing now is standard deviation. We now put the pieces we have into an optimization algorithm that will seek to find the SD value that results in a curve with our pre-specified shape and mean that minimizes the error for our overweight, obesity, and (optionally) underweight prevalences.

### Inputs
- For USHD, the exp_sd optimization pipeline is configured with a settings.csv file and run with a parent script.

The setting files live at `FILEPATH`

Example settings file:


| Arg                         | Example value                                                                                                                        | Explaination                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| :-------------------------- | :----------------------------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| draws_sd_output_dir         | FILEPATH                                             | Root directory for exp_sd outputs                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| mean_bmi_dir                | FILEPATH | Directory pointing to the version of the mean BMI estimates you would like to use; the script will eventually read in the paths to the prevalence estimates used to generate mean BMI. MUST use the database race codes                                                                                                                                                                                                                                                  |
| location_id                 | NULL                                                                                                                                 | If left NULL, will run for all locations. Can also provide a set of location_ids for mcnties (using the shared location IDs, not the mcnty indexes)                                                                                                                                                                                                                                                                                                                      |
| years                       | 2007:2019                                                                                                                            | Years to run for                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| sexes                       | c(1,2)                                                                                                                               | Sexes to run for                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| extra_dim                   | race                                                                                                                                 | Name of the extra dimension to use (must be race or edu)                                                                                                                                                                                                                                                                                                                                                                                                                 |
| extra_dim_values            | c(2,4,5,6,7)                                                                                                                         | Values of the "extra dim" variable, i.e., race or edu. ONLY race codes matching the database will be accepted because needs to merge with ensemble weights. codes                                                                                                                                                                                                                                                                                                        |
| n.imp                       | 10                                                                                                                                   | How many imputations to use                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| n_draws_per_imp             | 100                                                                                                                                  | Number of draws per imputation. Currently, there isn't a way to specify specific draws to run.                                                                                                                                                                                                                                                                                                                                                                           |
| incl_underweight            | FALSE                                                                                                                                | Logical: include underweight prevalence in the exp_sd optimization                                                                                                                                                                                                                                                                                                                                                                                                       |
| resub                       | TRUE                                                                                                                                 | Is this a resubmission? (If yes, run date must be provided)                                                                                                                                                                                                                                                                                                                                                                                                              |
| threshold_weights           | c(uw = 0, ov = 0.5, ob = 0.5)                                                                                                        | Relative weights to put on the error at each threshold (use if you want to prioritize fits at overweight vs obesity thresholds)                                                                                                                                                                                                                                                                                                                                          |
| .fx_length                  | 500                                                                                                                                  | Number of increments in the edensity approximation.                                                                                                                                                                                                                                                                                                                                                                                                                      |
| .control                    | list(iter.max=50)                                                                                                                    | Optimization control variables passed to nlminb.                                                                                                                                                                                                                                                                                                                                                                                                                         |
| round_digits                | list(exp_mean = 1, under_prev = 2, over_prev = 2, obes_prev = 2)                                                                     | Number of digits to round the exposure estimates to when identifying approximately equal strata                                                                                                                                                                                                                                                                                                                                                                          |
| area_var                    | mcnty                                                                                                                                | Name of area variable (mcnty, state, …)                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| run_date                    | 2022_06_29_16_34_11                                                                                                                  | Rundate (leave NULL if resub = FALSE)                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| save_score                  | FALSE                                                                                                                                | Logical: append the score (sum squared error between prevalences provided and the prevalences implied by the edensity for a given exp_sd). This is the same as the objective function used in the optimization. When TRUE, this increases runtime/storage, but useful to explore performance                                                                                                                                                                             |
| ens_weight_version_name     |                                                                                                                                      | Name corresponding to ens_weight_version in the database. Must exactly match. Optional if `ensemble_weight_version_ids` is provided.                                                                                                                                                                                                                                                                                                                                     |
| ensemble_weight_version_ids | 24                                                                                                                                   | Ensemble weight ID in database.                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| opt_raked_exp_mean          | FALSE                                                                                                                                | Indicator of whether to use the raked or unraked files from mean_bmi_dir                                                                                                                                                                                                                                                                                                                                                                                                 |
| opt_raked_prev              | FALSE                                                                                                                                | Indicator of whether to use the raked or unraked files from the SAE models. Applies to all of the overweight, obesity, and (when used) underweight files associated with mean BMI. **NOTE** This doesn't *necessarily* need to align with whether raked or unraked prevalence ests are used for mean BMI prediction. However, it's important to consider the consequences of using different types of prevalence estimates for estimating mean BMI and optimizing exp_sd |
| settings_desc               | {your explaination}                                                                                                                  | Something that is useful for the modeler, and this is also used in some of the markdowns comparing model runs.                                                                                                                                                                                                                                                                                                                                                           |


### Scripts

* `parent_ushd_sd_optimization.R`
    Parent script takes the settings version and creates/launches the array job that actually performs exp_sd optimization, resubmits failed jobs, and compiles the intermediate files into a more usable format.
    Based on the settings, this will launch an array broken up by:
    * year
    * sex
    * imputation
    * draw
    
    All counties specified in the settings and all race/edu variables will be run in the same script.
    Expect that this will take several days to run because there are many tasks, each of which takes several hours (depends a lot of the settings) to run.


    The parent script launches the child script for each of the tasks defined above:

  * `child_ushd_sd_optimization.R`
    This script is short because it focuses on parsing the settings file and the information in the array parameter map. It also sets some default values for the function call if they are not specified in the settings file.
    The script then calls `calc_metab_bmi_adult_exp_sd_ushd()` to do the actual optimization, and then saves the outputs into small, intermediate files.
    * `ushd_exp_sd_bmi.R`
    Contains source code for `calc_metab_bmi_adult_exp_sd_ushd()` and is the core component of optimization.
    * Reads and formats the mean BMI and prevalence exposure estimates. 
    * "rounds" the exposure values according to the `round_digits` argument, which avoids re-estimating the optimal exp_sd for rows with essentially the same exposure estimate. (It later recombines the exp_sd based on the rounded exposures with the original exposure values)
      * Note that the rounding is done separately by race -- this is b/c we don't want to use the same exp_sd value for rows with the same exposure values if they don't have the same edensity as defined by the weight set. This part of the code should be made more dynamic so that it avoids deduplicating based on which demographics actually have different ensemble weights.
    * Selects the appropriate row of the ensemble weight CSV (currently, there is only one)
    * Runs the optimizer to choose the best exp_sd for a given mean BMI, set of ensemble weights, and prevalence estimates. The optimizer uses `nlminb`. As we improved the ensemble distribution functions, we were able to change the optimization settings. The objective function in the optimizer calculates the prevalence of underweight, overweight, and obesity implied by a given exp_sd, exp_mean, and set of ensemble weights. The score is calculated in R based on the implied prevalences minus the prevalences from the SAE models. There are two implementations of this objective function: (1) using CPP  script with source code for `integ_bmi()`, which calulates prevalence by numerically integrating over the edensity. The other option is to use `get_edensity_prevs` (defined in `ushd_get_edensity_prevs.R`), which analytically solves the prevalences in the ensemble distribution using the CDF functions for each component distribution. This is the preferred method because it is faster and more accurate.
  * `resub_ushd_sd_optimization.R`
    Resubmits failed jobs -- determines which jobs failed based on whether or not the expected files are found. This could also be accomplished (perhaps more efficiently) by passing in the job_id of the array job and using the `sacct` command.
    This script will resubmit `child_ushd_sd_optimization.R`  for any jobs without the expected outputs. This script will then "sleep" until all of the child tasks are complete so that this script can serve as a hold for the combine script.
  * `combine_ushd_sd_optimization.R`
    * Reads in all of the intermediate output from the child scripts (initially saved separately by draw), and combines them into all-draws files with the format `./draws/draws_mcnty_{YEAR}_{SEX}_{RACE}_{EDU}.rds` *and* `./draws_mcnty/{MNCTY}/draws_{MCNTY}_{YEAR}_{SEX}_{RACE}_{EDU}.rds`. This is somewhat inefficient in terms of storage, but it speeds up the PAF calculator considerably to save the 1000 draw files separately by mcnty. This script also collapses the draws into summary files `./est/est_mcnty_{YEAR}_{SEX}_{RACE}_{EDU}.rds`
  * `ushd_pdf_families.R` -- defined components of ensemble distribution. Made important updates to the `pdf_families.R` script used in the central PAF calculator that will likely be incorported into GBD 2022. For now, use this script for ensemble distribution work on USHD.
 
* `update_obj_fxn.Rmd` -- Important script documenting changes to the objective function and pdf_families script.
* Vetting scripts: (`/vetting/`)
  * `fast_diagnostics.R` -- checks the exp_sd values are valid. Makes some plots useful for diagnosing optimizer issues (e.g., irregular clustering of values)
  * `draw_level_correlation_mean_sd.R` -- Plots the correlation between exp_mean and optimized `exp_sd`. Useful for checking optimization issues. Similar to `fast_diagnostics`, but works at the draw level (which is the only level at which exp_mean and exp_sd are directly comparable, but much slower to fun)
  * `evaluate_edensity_performance.R` -- Assesses how closely ensemble distribution matches the target overweight and obesity prevalence estimates it is supposed to match
  * `plot_curve_results.R` -- Visualizes the ensemble distribution and the prevalence of overweight and obesity in the relevant strata.
       
Older scripts used for model development (unlikely to need in the near future)

* `exp_sd_diagnostics.R`
  Provide the location of the exp_sd estimates, and this script will read in the corresponding mean BMI/prevalence estimates according to the settings files. It will then create plots to assess the validity of exp_sd estimates.
* `ushd_model_run_comparison.Rmd`
  Notebook used to compare model runs using different settings. Renders to ()
  Note that I want to improve this notebook to test draws from multiple imputations. Note that it's important to consider the versions of models included to make sure that the comparisons are meaningful (e.g., covering the same locations).
* `ushd_sd_model_selection.Rmd`
  Notebook used to explore the effect of changing rounding, optimizer settings, and edensity approximation on the speed and precision of exp_sd estimates.
* `exp_sd_aggregation_exploration.Rmd`
  Initial exploration of ways to calculate exp_sd for aggregate units (e.g., states). The exploration is not complete, but we may not need to return to it because we can aggregate in the PAF step.



### Outputs
```
FILEPATH
.
|-- 2022_06_29_16_34_11
|   |-- draws {all draws in one file}
|   |   | -- draws_{MCNTY}_{YEAR}_{SEX}_{RACE}_{EDU}.rds
|   |   | -- ...
|   |-- intermediate {output from child script}
|   |   | -- draws_mcnty_{YEAR}_{SEX}_{RACE}_{EDU}_{IMP}_draw{DRAW}.rds
|   |   | -- ...
|   |-- draws_mcnty  {draws combined, saved by county}
|   |-- | {MCNTY}
|   |   |-- draws_{MCNTY}_{YEAR}_{SEX}_{RACE}_{EDU}.rds
|   |   | -- ...
|   |   | ...
|   |-- est {collapsed draws}
|   |   |-- est_mcnty_{YEAR}_{SEX}_{RACE}_{EDU}.rds
|   |   | ...
|   |-- git_log.txt
|   |-- initial_run_sd_opt_tasks.csv
|   |-- sd_opt_resub_job_id.rds
|   |-- sd_opt_tasks.csv
|   |-- sd_opt_tasks_resub.csv
|   |-- settings.csv
|   `-- timelog_child_ushd_sd_optimization.csv

```


# Process diagram


Below is a visual map of the main R scripts and their relationships in the standard deviation optimization pipeline. This diagram reflects the actual workflow described above, including launch order and dependencies. Note that the objective function can be calculated using either a C++ integration method (`ushd_integ_bmi.cpp`) or an analytical R method (`ushd_get_edensity_prevs.R`), as specified in the settings file.

The pipeline diagram below is rendered from DOT code in `render_pipeline_diagram.R`. To update the diagram, edit the DOT code in that script and re-run it to regenerate the image.

![Main pipeline diagram](img/pipeline_main.png)

*If you update the DOT code in `render_pipeline_diagram.R`, re-run the script to regenerate this image.*


**Legend:**
- Blue: Launch/parent script
- Green: Child/core/processing scripts
- Orange: Objective function and ensemble distribution scripts
- Gray: Utility or aggregation scripts

