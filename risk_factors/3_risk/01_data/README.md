# README

**Contains information related to screening/extracting results for BMI systematic review on US Health Disparities team**

Each file in this directory/subdirectories has a descriptive header with inputs/outputs. A high-level overview of each script and tips about run-order is below.

Repo organization:

```
FILEPATH
.
|-- README.md
|-- extraction
|   |-- add_nid_sr_extraction.R
|   |-- check_cohorts.R
|   |-- merge_extra_cols.R
|   |-- refresh_bmi_sr_extraction.R
|   |-- upload_extractions.R
|   `-- validate_extracts.R
|-- processing
|   `-- clean_extractions.R
`-- screening
    |-- rescreen_gbd.R
    `-- update_locations_nids.R
```


## `screening/`

Contains scripts needed to create screening sheet based on GBD's bundle (check that bundle version is best) and the screening sheet scripts used for systematic reviews on GBD. 

* `rescreen_gbd.R`: Creates a blank screening sheet based on the articles included in the latest GBD bundle. Note that the script needs to be run interactively and will guide you on manual steps (e.g., searches to run in the GHDx).
* `update_locations_nids.R` -Helper script for `rescreen_gbd.R` that returns NIDs in the latest bundle that are located in the US, but are assigned a "global" location ID by the bundle database.

The screening sheet scripts likely do not need to be rerun unless you are doing a new systematic review.

## `extraction/`

This directory contains scripts that create  an extraction sheet based on the USHD extraction template (based on the GBD 2020 template) and pulls as many results from GBD's extraction sheets as possible. 

* `refresh_bmi_sr_extraction.R` 
  * Description: Create an extraction sheet that includes the most-recent template with the previously-extracted results from GBD's systematic review(s).
    * 1) Load the extraction template and previous extractions from GBD
    * 2) Filter to just the NIDs that we've included for screening
    * 3) Add USHD-specific columns + information from screening sheet
    * 4) Add the old extractions to the extraction template and save
  
      NOTE: If you've started manually extracting results based on this output and need to add additional rows from GBD's extraction sheets to your working extraction sheet (e.g., because I accidentally left out an NID), use `add_nid_sr_extraction.R` instead.     
* `check_cohorts.R` -- checks that we are consistent with which cohort studies we mark as "effectively single race" in the extraction sheets. As of 6/6/2022, we have checked this for the BMI extractions so this script may not need to be rerun unless you do a major revamp of the extractions.
* `add_nid_sr_extraction.R` Add rows from GBD's extractions associated with certain NIDs to an extraction sheet you're already working on (was designed to add the NIDs identified in `check_cohorts.R`). Only needs to be rerun when adding new NIDs to the extraction sheet.
* `validate_extracts.R`: Performs validations for extractions to enforce certain logical consistency and data quality rules. It's a helper script that checks for internal consistency, but fundamentally the quality of extractions relies on careful manual extraction.
* `upload_extractions.R` Uploads extraction to bundle database (needs to be run *after* `validation_extracts.R`) and creates bundle versions. Retrieves extractions from database and saves to disc.

## `processing/`

* `clean_extractions.R` -- based on `clean_mr_brt.R` in DBD repo. Created prepped data for modeling with MR BRT. Saves to disc.

