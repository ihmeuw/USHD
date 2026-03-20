####################################################################################################
## Description: Upload the extraction data to the Bundle database/ELMO
## 
## Inputs: Validated extractions 
##              
## Outputs: Bundle upload; update tracker
##             3_risk/01_data/extraction/validate_extracts.R 
##             FILEPATH
##             Write out raw data: FILEPATH
##             Updates extraction bundle tracker
##
####################################################################################################

# Set up ------------------------------------------------------------------

source("FILEPATH/upload_bundle_data.R")
source("FILEPATH/get_bundle_data.R")
source("FILEPATH/save_bundle_version.R")
source("FILEPATH/get_bundle_version.R")
source("FILEPATH/get_ids.R")
source(paste0("FILEPATH/_versioning_functions.R"))
library(data.table)
library(openxlsx)
library(stringr)

bundle_id <- 9963

# Path to validated input data
file_in <- "FILEPATH"
out_raw_root <- "FILEPATH"

# Upload to epi database --------------------------------------------------
# "get" data to save it to FILEPATH (before uploading a new version)
bundle <- get_bundle_data(bundle_id = bundle_id,
                          export = T,
                          merge_id_columns = T)

result <- upload_bundle_data(bundle_id, filepath=file_in)

print(sprintf('Request status: %s', result$request_status))
print(sprintf('Request ID: %s', result$request_id))

# create a bundle version:
result_v <- save_bundle_version(bundle_id, 
                              include_clinical = "None",
                              automatic_crosswalk = "F")

print(sprintf('Request status: %s', result_v$request_status))
print(sprintf('Request ID: %s', result_v$request_id))
print(sprintf('Bundle version ID: %s', result_v$bundle_version_id))
date <- make_time_stamp()
# Track the versions! -----------------------------------------------------

if(result_v$request_status == "Successful"){
  is_best <- readline(prompt = "Should this version be uploaded as 'best'? [y/n]: ")
  while(!is_best %in% c("y","n")){
    is_best <- readline(prompt = "Should this version be uploaded as 'best'? [y/n]: ")
  }
  
  desc <- readline(prompt = "Write description of bundle version: ")
  
  df_tmp <- data.table(bundle_id = bundle_id,
                       bundle_version_id = result_v$bundle_version_id,
                       request_id = result_v$request_id,
                       current_best = ifelse(is_best == "y", 1, 0),
                       date = date,
                       description = desc)
  cv_tracker <- as.data.table(read.xlsx(paste0(dirname(file_in), '/_extraction_version_tracking.xlsx')))
  if(is_best == "y"){
    cv_tracker[bundle_id == bundle_id, current_best := 0]  # set previous versions to not best
  }
  cv_tracker <- rbind(cv_tracker, df_tmp, fill = T)
  write.xlsx(cv_tracker, paste0(dirname(file_in), '/_extraction_version_tracking.xlsx'))
}

# Save the raw extractions ------------------------------------------------

out_raw <- sprintf('%s/bundle%i/v%i', out_raw_root, bundle_id, result_v$bundle_version_id)
dir.create(out_raw, recursive = T)

bundle_raw <- as.data.table(get_bundle_version(bundle_version_id = result_v$bundle_version_id,
                                               export = FALSE,
                                               transform = F)) # don't calculate variables

# Save raw versions -------------------------------------------------------

causes <- bundle_raw[, unique(cause_id)]
cause_ids <- get_ids("cause")
lapply(causes, function(cc){
  DT <- bundle_raw[cause_id == cc]
  c_name <- cause_ids[cause_id == cc, acause]
  ff <- sprintf('%s/ushd_high_bmi_%s_%d.xlsx', out_raw, tolower(str_replace(c_name, " ", "_")), cc)
  write.xlsx(DT, file=ff)
  print(ff)
})
