####################################################################################################
## Description: Upload inputs needed for upload_high_bmi_input_data.R
##              NOTE: post-stratification frame and the cbsa-mcnty geographic crosswalk
##                    should be uploaded as they are produced.
##                    Use this script to upload versions ONLY if not already completed
####################################################################################################

library(R.utils)
library(data.table)
library(yaml)

## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
# running this line ensures successful database connection and prevents interference from other packages
ushd_client$save_covariate_population

is_best_ps = TRUE
prev_issues_ps = "Unstable population counts in some counties"
is_best_geo_crosswalk = TRUE
prev_issues_geo = "Based on old PS frame"

# Upload post-stratification frame ----------------------------------------


post_stratification_frame_id <- save_post_stratification_frame(
  post_stratification_type_id = 2, # no phone in current version (see get_post_stratification_type())
  ipums_nid = 99999, # TBD
  post_stratification_filepath = "FILEPATH",
  covariate_dataset_ids = list(167),
  is_best = is_best_ps,
  prev_issues = prev_issues_ps
)
post_stratification_frame_id

# Upload mcnty-CBSA crosswalk
covariate_dataset_id = 167 # id of population run used from ushd_shared.covariate_dataset (I checked get_covariate_version("pop_by_race_ethn_1977") and looked at what versionw as best when the crosswalk was created)
post_stratification_frame_id = post_stratification_frame_id # id of post stratification frame used from ushd_risk.post_stratification_frame
survey_extraction_version_id = 20 # id of survey extraction data from ushd_risk.survey_extraction_version -- get_survey_extraction(input_type = "brfss") -- confirmed version w/ input_versions.txt from FILEPATH
nids = list() # list of additional nids used in this script, leave as empty list if NA

mcnty_mapping_crosswalk_id <- save_mcnty_mapping_crosswalk(
  mcnty_mapping_type_id = 1,  # cbsa_mcnty; see get_mcnty_mapping_type()
  covariate_dataset_id = covariate_dataset_id, 
  mcnty_mapping_filepath = "FILEPATH",
  post_stratification_frame_id = post_stratification_frame_id,
  survey_extraction_version_id = survey_extraction_version_id, # corresponds to BRFSS
  nids = nids,
  is_best = is_best_geo_crosswalk,
  prev_issues = prev_issues_geo
)
mcnty_mapping_crosswalk_id
