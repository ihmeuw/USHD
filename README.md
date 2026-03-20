# BMI - Race Ethnicity - 2026

This repo contains cleaned code used to generate estimates in 'Cause-specific years of life lost attributable to non-optimal body mass index by county, sex, race, and ethnicity in the USA, 2000–2019: a systematic analysis of health disparities' published in BMC Medicine on March 18, 2026. 

This repo contains eight directories:

1) covariates: Used to derive estimates of sociodemographic covariates employed in the SAE models.
2) non_fatal: Used to fit SAE models of overweight and obesity prevalence.
3) population: Used to derive population estimates for use in aggregation of model predictions.
4) post_stratification_frame: Used to produce a post-stratification frame for use in addressing non-response bias and harmonizing survey weights across sources.
5) risk_factors: Used to model mean BMI.
6) sae.shared: Contains shared functions used by other repos.
7) tater/src: Used in calculation of attributable burden.
8) ushd.paf: Used for PAF calculation.

# Other USHD work

* To see other published code repositories for the US Health Disparities team, please visit our [code homepage](https://github.com/ihmeuw/USHD/tree/main).

* You may find [interactive US Health Disparities visualizations here](https://vizhub.healthdata.org/subnational/usa).