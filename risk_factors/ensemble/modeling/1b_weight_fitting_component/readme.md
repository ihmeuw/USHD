##### CHILD GROWTH FAILURE WEIGHT-FITTING

# STEP 1: Fit Ensemble Weights to Microdata

### Thought Process

-   ~~After modeling step 1a, we have estimates of CGF rates,~~ After the SAE and mean BMI models, we have the prevalence of overweight/underweight/obesity and mean BMI, but we do not have any models that speak to the shape of ~~CGF~~ BMI distributions. That's what we're trying to get here. The shape.

-   There are technically two strategies you can use to do this. We call them M1 and M2. These are described in a little more detail below. Know M1 is the old way that many people at IHME still use. For CGF, we use M2. We spent a lot of time developing it for GBD 2020, have strong evidence that it's better, and are working towards convincing other people to adopt it.

-   The two strategies are described below

-   M1 (outdated):

    -   Takes all microdata sources and fits the shape of them *individually*
    -   It uses an optimization algorithm to find the weighted average of 10 distribution families that best reflect the shape of the microdata source
    -   After fitting each microdata source separately and getting a "weight set" for each, it takes an average of those weight sets to get a final global weight set
    -   This averaging of weight sets has been found to produce weight sets that are sub-optimal and is the main reason we set out to develop M2

-   M2 (new, improved): *This is what we use on USHD*

    -   Takes all microdata sources and fits the shape of them *simultaneously*
    -   It tries a weight set and sums up the error for this weight set from ALL microdata sources. This summed error is what the algorithm tries to minimize
    -   In this way, the weight set is optimized on the collective body of microdata from the start, and no averaging across final weight sets is needed

-   There are also multiple fit "options" you can use. These range from 1-5, with 1 prioritizing the fit across the entire distribution, and 5 having the highes priority towards fit at specified points. We recommend using "thresholds_4" or (probably) "thresholds_5". *(As of 7/6/22, only `thresholds_5` was updated for USHD)*

##### FOR MORE INFORMATION/DOCUMENTATION ABOUT WEIGHT FITTING STRATEGIES AND OPTIONS, PLEASE LOOK AT THE POWERPOINT IN THIS REPO OR WATCH RYAN FITZGERALD'S RECORDED SCIENCE SEMINAR

### Inputs

-   Supply the filepath to the microdata for your ME. This data needs to have a column called nid_loc_yr_index which is used to differentiate microdata sources that will be fit. The data column should be called data.
-   Choose a strategy and option for optimization.
-   Specify the actual thresholds/tails for optimization along with their weights
-   Give the launch a name, note this doesn't need to include the ME name

**Notes for USHD:** For USHD, see the script `ushd_process_microdata.R` in this repo to set up microdata For USHD, we specify these settings in a settings.csv file -- located at `FILEPATH`. We then put the name of the settings file into the top of the launch script and it will do the rest. Note that the settings csv is used a bit unconventially in this pipeline (compared to other USHD scripts) because the child scripts prodominently use a csv produced by the launch script to specify the models. The settings file is mostly just used to set up another csv in the format the child script expects

This pipeline produces a ton of files, and there was a fairly complicated file structure set up by the CDF. I tried not to modify their file structure too much. However, I changed the structure in a few major ways: I reorganized the outputs to nest into model runs (the "launch date"). The launch date can actually be any string, but I tended to use the time stamp of when the model was launched (this is programmed inot the launch script, although it wouldn't be too hard to change). To fit race-specific weights, I created a subdirectory one level below the model directory that contained all of the usual files/subdirectories for an all-pop model run. To fit this option into the existing file struture, I passed `{DATE}/{RACE}` as the launch.date instead of the usual `{DATE}`. This is handled in the launch script IF `by_demo = T` is specified in the settings file.

**Example settings.csv**
Settings files live at `FILEPATH`

| Arg                       | Example value                                                                                    | Explaination                                                                                                                                                      |
| :------------------------ | :----------------------------------------------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| data_version              | 2022_06_27_15_58_40                                                                              | Version of the microdata to use. Will bombined with FILEPATH/{DATE_VERSION}                 |
| me_type                   | high_bmi                                                                                         | This must match the filepaths for each risk in the risks/2_exposure directory                                                                                     |
| weight_root               | sprintf("FILEPATH", me_type) | Formula to specify location of the weight set. This doesn't need to be modified                                                                                   |
| num_of_initial_conditions | 100                                                                                              | CGF team recommends >= 100. The is the number of starting weight sets to test in the optimizer. Each starting version is run as a separate task in the array job. |
| run_m1                    | FALSE                                                                                            | m1 is the old ensemble distribution approach                                                                                                                      |
| run_m2                    | TRUE                                                                                             | m2 is ens dist approach by CGF team                                                                                                                               |
| option                    | thresholds_5                                                                                     | See notes in the launch script describing methods                                                                                                                 |
| low_threshold             | 18.5                                                                                             | underweight                                                                                                                                                       |
| medium_threshold          | 25                                                                                               | overweight                                                                                                                                                        |
| high_threshold            | 30                                                                                               | obese                                                                                                                                                             |
| low_weight                | 0.33                                                                                             | The proportion of the overall score for a weight set that should be based on the error at this threshold                                                          |
| medium_weight             | 0.34                                                                                             | The proportion of the overall score for a weight set that should be based on the error at this threshold                                                          |
| high_weight               | 0.33                                                                                             | The proportion of the overall score for a weight set that should be based on the error at this threshold                                                          |
| by_demo                   | TRUE                                                                                             | Logical: should the weight sets be fit separately by some demographic variable?                                                                                   |
| demos                     | c("race", "sex")                                                                                 | The demographic variables on which we fit separate weight sets. Ignored if by_demo = FALSE.                                                                       |
| test_additional_dist      | TRUE                                                                                             | Logical: should we test additional distribution families that are included in the GBD ensemble weights but not the CGF code? (I recommend setting equal to TRUE)
| resub                     | TRUE                                                                                             | Is this a resubmission? (If yes, run date must be provided)                                                                                                       |
| run_date                  | 2022_07_05_12_50_43                                                                              | Run date (leave NULL if resub = FALSE)                                                                                                                            |
| short_desc                | {Your explaination}                                                                              | Something that is useful for the modeler, and this is also used in some of the markdowns comparing model runs.                                                    |


### Outputs

-   The log file located in the weight_fitting_files folder will be updated with this information
-   You will produce a lot of output files from this run, but the next step sifts through them easily

### Diagnostics

-   Wait until we've processed these weights in step 2 to view any diagnostics

### Things to Know

-   This step is pretty resource and time intensive. It could take a day or two for these models to complete. 
-   We launch 100 initial conditions for the optimization function. The first 11 use pre-specified weights that we think are valuable candidates, and additional initial conditions after that begin with random weight sets.

# STEP 2: Process Best Weights

### Thought Process

-   There's a ton of output files produced from step 1 and they all contain metrics on goodness of fit. Here we read through all those files and find the weight set that did the best. If the weight set is stratified by some demographic variable, the script will also compile all of the strata-specific weights into a single weight set.

### Inputs

-   ~~You have to provide the ME name, strategy (which should be m2), the option, and the launch.date. Keep in mind these should all be accessible from the log file if you forget.~~
-   Arguments: The launch date. The task will then read the settings file saved in the model directory. This was updated on 7/8/22 so that you don't need to treat all-pop weights or strata-specific weight sets differently -- as long as the information is in the settings file, the script will handle the rest.
-   This script will also create some diagnostic plots specific to that weight set. Those plots are defined in **`./vetting/check_implied_prevs.R`**. (See step 3 to make plots that compare across weight sets.)

Example:

`launch.date <- "2022_07_05_12_50_43"`

### Outputs

-   The best weights will be output to this filepath: ~~FILEPATH\_*option*.csv~~
-   `FILEPATH/bestweights.csv` (this is the best set of weights for a given strata)
-   `FILEPATH/_weightset.csv` (this version compiles all of the strata-specific weights).
-   `FILEPATH/check_implied_prevs.pdf`(diagnostics)
-   `FILEPATH/implied_distributions.rds`(diagnostics)
-   `FILEPATH/implied_prevs.rds`(diagnostics)

### Diagnostics

-   Now that we have processed the best weights, the next step is the diagnostic, which will use these weights.

### Things to Know

-   This function will run fairly quickly (the slower part is generating the diagnostics)

### Potential Areas for Updates/Edits/Improvements

# STEP 3: Compare Weight Sets (plotting/diagnostics)

\*Note that there are two scripts for diagnostic plots: `3_launch_comparison_ushd.R` accurately handles weights stratified by demographic variables.  but is not as thorough as `3d_launch_error_over_domain.R`. 

`3d_launch_error_over_domain.R` is not currently suitable for comparing weight sets with stratified weights, so it *should not be used except to compare all-population weights*. (see notes below)

### Thought Process

-   We want to pick the weight set that fits out microdata sources the best (prioritizing a shape that works best in the area of the BMI distribution associated with excess risk ~~HAZ/WAZ/WHZ domain that constitutes CGF~~)

Do not use `3d_launch_error_over_domain.R` to compare weight sets stratified by demographics.

### Inputs

To run `3_launch_comparison_ushd.R`, just specify the launch dates that you want to compare. The data used in the plots was already produced by the `vetting/check_implied_prevs.R` plots called in step 2 (process best weights)

To run `3d_launch_error_over_domain.R` (which produces slightly different plots that are not always appropriate for the USHD context -- see notes below), the following directions apply. Note recommended to run for USHD, but keeping instruction for posterity/in case you want to compare ensemble weights that are not stratified by demographics.

> This is the description for CGF [On USHD, only use if comparing unstratified weights]: \> - You will launch an array job, with one task of each HAZ/WAZ/WHZ. Each of these uses it's own sub-param map. This is advanced compared to a normal array job. \> - The sub-param maps for HAZ/WAZ/WHZ will each need to be given the strategy, option, and launch.date that was used in step 2 when it was run \> - Note that you can compare your weight set to a custom weight set by doing the following: \> + Set the strategy and option to "custom" \> + In this directory /weight_fitting_files/best_weights/*me*/ create a folder and name it anything. Enter this folder name as the launch.date in this launch script. \> + Create a file called "bestweights_custom.csv" in that folder you just made. Enter the weights with the same format as the other best weights csv's that have been output.
>
> For USHD, I'm in the process of adapting the launch script and plotting script to make it easier to examine the effects of the settings we care most about: different weights on the thresholds and the inclusion of race/sex/age on the weight set.
>
> To compare different settings on an all-race, all-sex, all-race version, specify multiple launch dates you'd like to compare. 
>
> Ex: `launch.dates = c("2022_06_28_08_27_15", "2022_06_30_16_18_12", "2022_06_28_15_04_49", "2022_06_28_14_30_04", "2022_07_01_14_34_27", "2022_07_05_12_12_28")`
>

### Outputs

If you run `3d_launch_error_over_domain.R`, you will get a diagnostic PDF at this directory: `FILEPATH`

If you run `3_launch_comparison_ushd.R`, you will get a diagnostic PDF at this directory: `FILEPATH`

### Diagnostics (applies to `3d_launch_error_over_domain`)

*(it's slightly different for USHD)* - The first few pages will show you the shapes of the PDFs and CDFs for different weight sets (centered at 0 with a constant SD) - The next  pages will show scatters of data prevalence vs modeled prevalence at the specified thresholds (-3, -2, and -1 Z scores). Weight sets that did the best will have their sum squared errors shown in bold. - The last several pages all show the same information in slightly different ways. The X axis is the domain of HAZ/WAZ/WHZ values. The y axis reflects the mean/median error across all input NIDs. This is shown for the Sum Squared Error and the Relative Percent Error. Each has a subsequent plot that zooms into the domain of CGF (below -1SD) to make it easier to distinguish lines in key areas.

### Things to Know

-   You want to consider which weight set looks best across the distribution, but especially consider which weight sets perform best in key areas of the distribution.
-   Keep in mind that weighting points differently will have different effects
-   Once you find a weight set that you think is best, run the next script to select it as our best weights

### Potential Areas for Updates/Edits/Improvements

-   We should see if there's a way to run some preliminary tests using step 2 infrastructure that could also influence ensemble weight set selection

# STEP 4: Select Best Weights

### Thought Process
-   It's helpful for organization to actually copy over "final" weights to another directory so we can remember which weights we're using.
### Inputs
-   Version of the weight set to designate "best".
-   Update the `prev_issues` and `description` fields in the database uploads
### Outputs
-   Copies the weightset to a directory easier to find and formats the weight sets in a standardized way (so that they can be uploaded to the database)
-   Also uploads a version of the ensemble weights to USHD's database
### Diagnostics
-   No diagnostics here
### Things to Know
-   These are the weights you should be using in modeling step 2.
-   In future modeling steps, you should retrieve the weights from the database rather than reading directly from here.

# Scripts

A more detailed explanation of running scripts -- concepts described above

-   `ushd_process_microdata.R`

    -   Reformat NHANES microdata into shape expected by the weight-fitting scripts. Saves to `FILEPATH`.
    -   This is an important step for deciding how many years to model, create new variables to use in stratifications, etc.

-   `1_launch_weight_fitting.R`

    -   This script reads in the settings file and configures the array job that creates the weight sets. Based on the settings, this script creates a paramater map used in the array. That paramater map is subsequently used in the child scripts (based on the settings, the correct child script will be called -- currently, only `1_parallel_m2_weight_fitting.R` is supported)

    -   `1_parallel_m1_weight_fitting.R`

        -   Script to fit ensemble weights using the `m1` method described above. Not currently adapted for USHD.

    -   `1_parallel_m2_weight_fitting.R`

        -   Script to fit ensemble weights using the `m2` method described above.\
        -   Runs the optimizer based on the settings in the parameter map and the settings csv. Will produce *many* output files that get sorted in step. 1 job per starting value.
        -   This script relies on functions defined in **`1_parallel_helper_functions.R`** to generate and score each weight set.

    -   `2_process_best_weights.R`

        -   This gets called from the `1_launch_weight_fitting.R`. It sorts through the outputs of the array jobs, above, to select the weight set with the best performance.
        -   Produces plots with `vetting/check_implied_prevs.R`

-   `3_launch_comparison_ushd.R`

    -   `compare_performance_ushd.R` compares the performance of ensemble weight sets.
    -   Uses outputs from `vetting/check_implied_prevs.R`, which is run in step 2.

> -   `3d_launch_error_over_domain.R` [Note recommended]
>
>    -   This is run separately from `1_launch_weight_fitting.R`. This sets up the paramaters passed to the child scripts. This script is configured as an array job because of how it's used on the CGF team, but we only run one child script on USHD. This script could be substantially simplified for our use, and is still under development as decribed above in Step 3.
>
>    -   `3d_parallel_error_over_domain.R`
>
>        -   Child script of `3d_launch_error_over_domain.R` produces a bunch of plots to visualize the distributions implied by the ensemble weights and their performance over the entire BMI distribution, and at the thresholds of interest (under/overweight, obese) in particular.
>
        -   In addition to the plots defined in the script and in `ensemble\other_functions\curve_creation_functions.R`, the following scripts are sourced to create plots I found particuarlly relevant for USHD:

            -   `vetting/plot_edensity_components.R`
            -   `vetting/plot_weights_score.R`

-   `4_select_best_weights.R`

    -   Puts ensemble weights into standard format regardless of stratification variables.
    -   Uploads ensemble weights to the USHD database

-   `background_intro.pptx`

    -   Contains useful overview of the methods (made by Ryan for Child Growth Failure team)
