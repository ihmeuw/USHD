# USHD Risk Factors

The repo will contain the code necesary to estimate exposure to certain risk factors in the US, estimate relative risks (or pull the risk curves from the GBD team), and calculate PAFs for the risk factors, all at the county-level by age, sex, and race/ethnicity or education. The code is either adopted from the previous US County project, GBD teams, or newly written. The repo is organized as follows:

*Note that this README is not comprehensive, and there are more detailed readme's within the sub-directories. Substantive documentation is the USHD Google Drive*


0. ```0_functions/```:
		This directory contains scripts for common operations, most importantly, for the SAE models.
1. ```1_data_prep/```: Code for prepping data necessary for estimating exposure to air pollution that has not already been processed in the ```population``` or ```covariates``` repo.
	* ```air_pollution/```: code related to air pollution rasters and state shapefiles
		* ```crop_pm_raster.R```: Script that subsets the PM2.5 rasters from the ERF team to just cells in the US.
		* ```sample_state/```: [Old version -- now use`2_exposure`] We are attempting to implement the air polluition data processing for MA before going onto the rest of the country because these tasks are computationally intensive.
	* ```counties/```
	  * ```survey_data/```: Contains code for extracting data from BRFSS, CPS, Gallup, NHANES, and NHIS. More details in the readme in the `counties` directory. This will eventually be moved to its own repo.
	* ```custom_pop/```: 
		* Code for loading and recoding the age/sex/race tabulations from the census and ACS.
		* Script to split tabulations into Hispanic/Non-Hispanic in the age/sex/race tabulations based on proportions from age/sex/ethnicity tabulations on the same admin units.
		* Script to run the above scripts for all states (and, eventually, years)
		* ```vetting/```: Scripts to vet the population counts by accessing internal consistency and comparing to other census tabulations
	* ```king_wa/```
2. ```2_exposure/```: contains subdirectors with code for estimating exposure to each risk factor. Processes raster and shapefile data to estimate exposure at the smallest administrative level available, and create aggregated tabulations by-demographic.
	* ```air_pollution/```: code for estimating exposure to three types of air pollution at the county level (estimated at the block or tract level and aggregated up). This directory includes a separate ```readme.md``` with more details about this process.
		* ```pm2_5/```: contains code to estimate overlap between air pollution rasters and census blocks to produce a link table. Combines link table with census/ACS tabulations to get county-level estimates for each demographic group, and overall.
			* ```vetting/```: contains extensive code for checking the PM2.5 results.
		* Crosswalks live here in the `high_bmi`, `fpg`, `hypertension`, and `cholesterol` sub directories.
4. ```3_risk/```: code for relative risk analysis

**Additional readme files may be inside the major subdirectories.**