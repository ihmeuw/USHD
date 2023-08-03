# Cause of Death - Race Ethnicity - 2023

This repo contains cleaned code used to generate Cause of Death Mortality estimates in the United States by county, year, age, sex, race, and ethnicity as reported in 'Cause-specific mortality by county, race, and ethnicity in the USA, 2000–19: a systematic analysis of health disparities' published in The Lancet on August 3, 2023. 

This repo contains five directories:

1) cod_database contains code that processes raw deaths data, performing various adjustments and redistributing deaths that do not have usable ICD codes. 
2) covariates contains code to generate various covariates used in the modeling process.
3) mortality contains code to model mortality rates and life expectancy using the prepared deaths and population data.
4) population contains code to generate population by county, year, age, sex, race, and ethnicity.
5) sae.shared contains shared tools, scripts, and related utilities for modeling.

# Other USHD work

* To see other published code repositories for the US Health Disparities team, please visit our [code homepage](https://github.com/ihmeuw/USHD/tree/main).

* You may find [interactive US Health Disparities visualizations here](https://vizhub.healthdata.org/subnational/usa).