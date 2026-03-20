# Model-based BMI crosswalk

# How to run crosswalk

Root: `risk_factors/2_exposure/high_bmi/correction_models/model_based_cw`

** Launch script**

`./run_bmi_correction_model_INLA.R` 
- Purpose: Sets up and runs BMI crosswalk (INLA-based crosswalk methodology). This script sets up the directories, launches the modeling script, and runs diagnostics on the results.
- Inputs: Specify the path of the setings file. The content of the settings file will be explained in the next section.
- Outputs: Create a run directory for the crosswalk models in `FILEPATH`

**Model script**

`.INLA_based_bmi_correction_binned_quantiles.R`
- Purpose: This is the main script for performing the crosswalk. It is launched by the `run` script above. The arguments/parameters for the model are set in the settings file. The methodology is explained in [OneDrive documentation].
- Inputs: Directory path. Will read the settings file contained in the directory to determine the other arguments. All of the model formulas we have tested are in the script. The specific model used is specified in the settings file.
- Outputs:
  - Collapsed/paired BMI data, input to the crosswalk (`pairs.rds`)
  - Microdata with crosswalk applied (`pred.rds`)
  - Model fits (`model.rds`)
  - Information about the inputs

`INLA_bmi_correction_ratios_gbd.R`
- Purpose: This script uses the model fits from the INLA-based crosswalk and creates an all-race version of the ratios, which GBD can apply to their data.
- Outputs:
  - Returns table with the correction factors (ratio measured/self-reported BMI) by age, sex, year, source, and BMI quantile.
  - Save to FILEPATH and FILEPATH so we can share with GBD.
  - Creates plots to show the difference between the self-report correction when applying race-specific corrections to microdata vs all-race corrections.


**Vetting scripts**

`./INLA_BMI_cw_plot_model.R`
- Purpose: 
  - Plot the model parameters for models (random effects and fixed effects)
  - Shows the "in-sample" performance of the BMI crosswalk, showing observed vs. predicted ratios of measured to self-reported BMI.
`/.plot_cw_effect_individuals.R`
- Purpose: Check the crosswalk effect on microdate (the level at which CW is applied)
`./compare_parameters_versions.R`
- Purpose: Compare the parameters of different versions of the crosswalk model. 


Additionally, two vetting scripts in `FILEPATH` are particuarlly useful:

`risk_factors/2_exposure/high_bmi/correction_models/vetting/detailed_compare_cw_sr_trends.R`
- Purpose: Creates nices plots showing the self-reported and crosswalked BMI trends over time, using a variety of stratifications. Shows mean BMI, prevalence of obesity, and prevalence overweight.

`risk_factors/2_exposure/high_bmi/correction_models/vetting/compare_versions.R`
- Purpose: compare multiple versions of the BMI crosswalk.

## Settings file for BMI crosswalk

Example settings file and usages

| 0                   | 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | 2                                                                                                                                                  |
| :------------------ | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------- |
| resub               | FALSE                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | If TRUE, need to specify a rundate in the next line -- e.g., run_date,YYYY_MM_DD_HH_MM_SS_formXX                                                   |
| age_var             | age20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | Which aggregate age group to use when crosswalking -- age10 or age 20                                                                              |
| strat_vars          | c("age20", "sex", "race_eth_code")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Stratification variables for the BMI crosswalk. Will collapse BMI by these categories (plus BMI quantile)                                          |
| num_quantiles       | 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Number of bins of BMI quantiles                                                                                                                    |
| n.sims              | 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Number of imputations                                                                                                                              |
| model_num           | form24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | Model formula number -- this determines which specification is used. See the specifications in the modeling script.                                |
| use_source_v2       | TRUE                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | If TRUE, splits BRFSS into BRFSS_LANDLINE_ONLY and BRFSS_LLCP                                                                                      |
| weight_inla         | TRUE                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | If TRUE, weights the likelihood function based on the sum of survey weights in each strata                                                         |
| year_subsets        | {see example 1 below}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | A list to determine the periods of NHANES to match to BRFSS/Gallup.                                                                                |
| fit_exclusions      | {see example 2 below}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | Stratifications to DROP before fitting the BMI crosswalk on the collapsed data. Include these if there are parts of the data considered unreliable |
| pred_exclusions     | {see example 3 below}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | Which populations to also exclude from the predictions. Can be the same as fit_exclusions, but does not need to be.                                |
| year_subsets_notes  | These are the 'chunks' of years we want to stratify by when fitting the crosswalk. Do NOT include exclusions here otherwise they will be dropped from prediction. I am pooling more years of NHANES than previous versions of the INLA CW. This is closer to year pooling in the quantile-correction CW. Compared to previous settings files- I modified the pooling here so that I am consistent with the number of years of BRFSS/Gallup pooled. Update on 7/18/24: I changed the pool for first year of BRFSS so that the time periods match. | Place for notes                                                                                                                                    |
| fit_exclusion_notes | These are subsets of the data excluded from the fitting process. Previously identifed as problematic matches. We will still predict out on them.Exclude race 97 from BRFSS/NHANES in 1999-2002 b/c of issues with sample in NHANES 1999-2000. Exclude race 97 from Gallup in all years b/c not comparable with NHANES. Keep BRFSS/NHANES from 1999 and 2019 even though matching different. Exclude race 97 in 2017_2020 prp bc of extreme pattern.                                                                                              | Place for notes                                                                                                                                    |
|                     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |                                                                                                                                                    |

Examples referred to in example setings file. Due to different special characters in CSV and markdown, it's easier to show the examples verbatim here.

```
example 1
"list(
  quote(((survey == ""brfss"" & year %in% 1999:2000) | (survey == ""nhanes"" & svyyear %in% c(""1999_2000"")))),
  quote((survey == ""brfss"" & svyyear == c(""2001_2002"") | (survey == ""nhanes"" & svyyear %in% c(""1999_2000"", ""2001_2002"", ""2003_2004"")))),
  quote((survey == ""brfss"" & svyyear == c(""2003_2004"") | (survey == ""nhanes"" & svyyear %in% c(""2001_2002"", ""2003_2004"", ""2005_2006"")))),
  quote((survey == ""brfss"" & svyyear == c(""2005_2006"") | (survey == ""nhanes"" & svyyear %in% c(""2003_2004"", ""2005_2006"", ""2007_2008"")))),
  quote((survey == ""brfss"" & svyyear == c(""2007_2008"") | (survey == ""nhanes"" & svyyear %in% c(""2005_2006"", ""2007_2008"", ""2009_2010"")))),
  quote((survey == ""brfss"" & svyyear == c(""2009_2010"") | (survey == ""nhanes"" & svyyear %in% c(""2007_2008"", ""2009_2010"", ""2011_2012"")))),
  quote((survey == ""brfss"" & svyyear == c(""2011_2012"") | (survey == ""nhanes"" & svyyear %in% c(""2009_2010"", ""2011_2012"", ""2013_2014"")))),
  quote((survey == ""brfss"" & svyyear == c(""2013_2014"") | (survey == ""nhanes"" & svyyear %in% c(""2011_2012"", ""2013_2014"", ""2015_2016"")))),
  quote((survey == ""brfss"" & svyyear %in% c(""2015_2016"") | (survey == ""nhanes"" & svyyear %in% c(""2013_2014"",""2015_2016"", ""2017_2018"")))),
  quote((survey == ""brfss"" & svyyear %in% c(""2017_2020prp"") | (survey == ""nhanes"" & svyyear %in% c(""2017_2020prp"")))),
  quote(((survey == ""gallup"" & svyyear == c(""2007_2008"")) | (survey == ""nhanes"" & svyyear %in% c(""2005_2006"", ""2007_2008"", ""2009_2010"")))),
  quote(((survey == ""gallup"" & svyyear == c(""2009_2010"")) | (survey == ""nhanes"" & svyyear %in% c(""2007_2008"", ""2009_2010"", ""2011_2012"")))),
  quote(((survey == ""gallup"" & svyyear == c(""2011_2012"")) | (survey == ""nhanes"" & svyyear %in% c(""2009_2010"", ""2011_2012"", ""2013_2014"")))),
  quote(((survey == ""gallup"" & svyyear == c(""2013_2014"")) | (survey == ""nhanes"" & svyyear %in% c(""2011_2012"", ""2013_2014"", ""2015_2016"")))),
  quote(((survey == ""gallup"" & svyyear == c(""2015_2016"")) | (survey == ""nhanes"" & svyyear %in% c(""2013_2014"", ""2015_2016"", ""2017_2018"")))),
  quote(((survey == ""gallup"" & svyyear == c(""2017_2018"")) | (survey == ""nhanes"" & svyyear %in% c(""2015_2016"", ""2017_2020prp""))))
)"

Example 2
"quote(
  (survey == ""brfss"" & (year_start %in% 1999:2002 | year_end %in% 1999:2002) & race_eth_code == 97) |
  (survey == ""gallup"" & race_eth_code == 97) |
  (survey == ""brfss"" & (year_start %in% 2003:2010 | year_end %in% 2003:2010) & age20 == ""60+"" & race_eth_code == 97) |
  (year_start %in% 2017:2019 & race_eth_code == 97)
)"

Example 3

"quote(
  (survey == ""gallup"" & race_eth_code == 97)
)"
```