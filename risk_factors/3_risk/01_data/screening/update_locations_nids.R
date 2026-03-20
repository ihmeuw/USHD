####################################################################################################
## Description: Create a list of NIDs that are in the USA according to extraction sheets
##                but not according to the GBD bundle. Save this list of NIDs
##                so that they can be added to the extraction sheet.
##                This issue is documented in 
##                FILEPATH
##              This script is set up as a function just so that the outputs can 
##                generated within rescreen_gbd.R            
## Input:   GBD location hierarchy 
##          GBD extractions
## Output:  Function to source in rescreen_gbd.R
## 
####################################################################################################

source("FILEPATH/get_bundle_data.R")
source("FILEPATH/get_bundle_version.R")
source("FILEPATH/get_ids.R")
source("FILEPATH/_versioning_functions.R")

run_update_location_nids <- function(){
  # USA locations -----------------------------------------------------------
  
  all_locs <- get_ids("location", return_all_columns = T)
  loc_ids = unique(c(102, # USA
                     all_locs[location_parent_id == 102, location_id], # US Admin 1
                     all_locs[location_parent_id %in% all_locs[location_parent_id == 102, location_id], location_id]) # US Admin 2
  )
  
  USA_locs <- all_locs[location_id %in% loc_ids]
  
  # GBD extractions ---------------------------------------------------------
  
  gbd_extraction_root <- "FILEPATH"
  
  # Compile GBD extractions -------------------------------------------------
  ff <- list.files(gbd_extraction_root,
                   pattern = "^cc",
                   full.names = T)
  
  my_read <- function(f){
    if(grepl(".csv", f)) fread(f) else as.data.table(read.xlsx(f))
  }
  all_extractions <- lapply(ff, my_read)
  
  all_extractions <- rbindlist(all_extractions, fill =T )
  
  # GBD's bundle ------------------------------------------------------------
  
  bundle_version_id = 40010
  bundle_version <- get_bundle_version(bundle_version_id = bundle_version_id, export = FALSE)
  
  # Look at descrepancies ---------------------------------------------------
  
  USA_extractions <- all_extractions[location_id %in% unique(USA_locs$location_id) & keep == 0] # only look at studies actually extracted
  
  USA_bundle <- bundle_version[location_id %in% unique(USA_locs$location_id)]
  
  USA_extractions_nids <- unique(USA_extractions[, .(location_id, nid, study_name, source = "extractions")])
  USA_bundle_nids <- unique(USA_bundle[, .(location_id, nid, source = "bundle")])
  
  nids <- rbind(USA_extractions_nids, USA_bundle_nids, fill = T)
  
  diffs <-setdiff(USA_extractions_nids[, unique(nid)],
                  USA_bundle_nids[, unique(nid)])
  
  nids[nid %in% diffs]
  
  return(diffs)
}
