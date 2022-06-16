# Life Expectancy - Race Ethnicity - 2022

This repo contains cleaned code used to generate life expectancy estimates in the United States by county, year, age, sex, race, and ethnicity as reported in 'Life expectancy by county, race, and ethnicity in the USA,
2000–19: a systematic analysis of health disparities' published in The Lancet on June 16, 2022. 

This repo contains five directories:

1) cod_database contains code that processes raw deaths data, performing various adjustments and redistributing deaths that do not have usable ICD codes. 
2) covariates contains code to generate various covariates used in the modeling process.
3) mortality contains code to model mortality rates and life expectancy using the prepared deaths and population data.
4) population contains code to generate population by county, year, age, sex, race, and ethnicity.
5) sae.shared contains shared tools, scripts, and related utilities for modeling.

# Other USHD work

* To see other published code repositories for the US Health Disparities team, please visit our [code homepage](https://stash.ihme.washington.edu/projects/UC/repos/published_code/browse).

* You may find [interactive US Health Disparities visualizations here](https://vizhub.healthdata.org/subnational/usa).