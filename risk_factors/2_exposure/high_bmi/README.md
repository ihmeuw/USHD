# High BMI exposure modeling

## Self-report adjustment/crosswalk

### Preparing data for crosswalk

The crosswalk models need the BMI data compiled in a standardized format across all surveys.

-   `risk_factors/2_exposure/high_bmi/sae_models/counties/01_input_data/prep_bmi_xwalk.r` Loads and standardizes BMI data from all surveys. Creates microdata file that can be used in crosswalk, and a summary of the BMI in each age/sex group. Produces plots of BMI defined in `/risk_factors/2_exposure/high_bmi/correction_models/viz_bmi.r`

    -   Outputs located in time-stamped directories within `FILEPATH`
    -   The output of the BMI crosswalk (in \``` high_bmi/sae_models/counties/01_input_data/prepped/` `` ) are the input to the FPG, LDL-c, and SBP crosswalk models. As a result, we need to prepare all covariates used in those crosswalks before passing them through the BMI crosswalk.

### Running crosswalk pipeline

Code to run crosswalk models is located in `/risk_factors/2_exposure/high_bmi/correction_models`.

## Running model-based BMI crosswalk (current method as of August 2024)

see `risk_factors/2_exposure/high_bmi/correction_models/model_based_cw/README.md`



## Running quantile-matching crosswalk crosswalk model (old method)

See `risk_factors/2_exposure/high_bmi/correction_models/quantile_matching_cw/README.md`




**Running/testing CANDIDATE crosswalk models:**

*Note that the candidate models were moved to the `correction_models/model_exploration` subdirectory to avoid confusion with the final code*

The results of the candidate crosswalk models are located at `FILEPATH`

Within the `correction_models` directory, there is a pipeline to run, test, and compare multiple crosswalk models.

*Defining, running, and tracking the model*

-   `_parent_crosswalk_model.r`

    -   In order to keep track of the crosswalk model metadata and output files, we run all candidate crosswalk models within `_parent_crosswalk_model.r`. This script should be sourced and run in a qlogin with minimal resources with \~5 hours of runtime (or longer if it takes a long time for jobs to get scheduled).

        -   This script reads in the crosswalk model specifications and metadata, as defined in `candidate_bmi_correction_model.r` `candidate_bmi_correction_model.r` can be edited directly. A snapshot of the model specifications is saved when running the parent file (`FILEPATH`).

        -   For each model specified in `candidate_bmi_correction_model.r` for which `run` is `TRUE`, `_parent_crosswalk_model.r` launches a separate job for `_child_crosswalk_model.r` Information about the candidate file is passed in as a command argument (refers to `all_models.rds` created when the parent script is launched).

            -   `_child_crosswalk_model.r` loads in relevant data, fits the crosswalk model as specified in `all_models.rds` (not applicable for the quantile normalization crosswalks),

            -   makes predicted values based on the crosswalk model with `predict_adjusted_bmi.R` (not applicable for the quantile normalization crosswalks), and

            -   runs the quantile normalization function (`quantile_normalization.R`), which takes 1-3 hours.

            -   Runs `quantile_correction()` and `pred_bmi()` , which are contained in the `quantile_correction.R` scripts. These functions predict respondents' measured BMI quantile based on their self-reported BMI quantile, and recalculates BMI based on the predicted quantile. **Note that this correction is only incorporated into the final model.**

            -   After the data are crosswalked, the script saves the data in a time-stamped subdirectory of a directory named based on the model (as defined in `all_models.rds` (e.g., `FILEPATH`.

            -   The script then runs `viz_bmi_adjustment.R`, which creates plots related to the model fits (if applicable) and saves them as `.png` in the same directory as the data. Note that this only displays the point estimate, which has a slightly different distribution than the draws (the quantile correction is performed in logit-space, so the point estimate is not the mean of the draws in quantile-space).

        -   `_parent_crosswalk_model.r` waits until all of the models have completed (or failed), it records the exit code of each child job and updates the metadata in `all_model.rds` for each model to specify where the output information is stored. This updated list is stored in `all_models_with_fits.rds` in the same directory as `all_models.rds`.

-   `results_candidate_bmi_correction_model.Rmd` knits together metadata for each candidate model and prints the related plots (if specified in the rmd document) based on the `all_models_with_fits.rds` that the user can specify. This document knits quickly and is saved as `FILEPATH`.

Additional scripts:

-   `missing_bmi_data.Rmd` creates a report on the level of missingness in BMI microdata that goes into the crosswalk models.
-   `calc_ks.R` performs a Kolmogorov-Smirnov test to compare distribution of measured BMI in NHANES to the quantile-normalization distribution produced by the crosswalks. This function relies on `weighted_ks_test_function.R`
-   `make_svydesigns.R` -- deprecated
-   `./uncertainty/` Contains documents used to understand propagation of uncertainty and sources of bias in the quantile normalization crosswalk. Led to the addition of quantile_correction step and imputations.
-   `./vetting/` Contains many useful vettings scripts. Some highlighted below:
    -   `misc_vetting_final_model.R` -- script to make additional vetting plots that we don't make for all of the candidate models (i.e., QN adjustment by strata, instead of globally)
    -   `summary_qn_bias_uncertainty.Rmd`: Summarizes key findings related to uncertainty and bias in the quantile normalization crosswalk for BMI.
    -   `compare_versions.R` **plots timetrends by selected demographic groups for different versions of the crosswalk. Very useful for selecting final specifications.**
    -   `reexamine_bias_obesity.R`: Notebook to explore why obesity was overestimated in some verions of the crosswalk.
